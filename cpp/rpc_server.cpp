// Tutorial 6: Remote Procedure Call, server
//
// Consumes fib(n) requests from the `rpc_queue` queue and publishes each result
// back to the queue named in the request's reply-to property, echoing the
// correlation id so the client can match the reply to its request.

#include <rmqa_consumer.h>
#include <rmqa_producer.h>
#include <rmqa_rabbitcontext.h>
#include <rmqa_topology.h>
#include <rmqa_vhost.h>
#include <rmqp_messageguard.h>
#include <rmqp_producer.h>
#include <rmqt_confirmresponse.h>
#include <rmqt_consumerconfig.h>
#include <rmqt_fieldvalue.h>
#include <rmqt_message.h>
#include <rmqt_plaincredentials.h>
#include <rmqt_properties.h>
#include <rmqt_result.h>
#include <rmqt_simpleendpoint.h>

#include <bslmt_semaphore.h>

#include <bsl_memory.h>
#include <bsl_string.h>
#include <bsl_vector.h>

#include <iostream>
#include <string>

using namespace BloombergLP;

namespace {

long long fib(int n)
{
    long long a = 0;
    long long b = 1;
    for (int i = 0; i < n; ++i) {
        long long next = a + b;
        a = b;
        b = next;
    }
    return a;
}

} // namespace

int main()
{
    rmqa::RabbitContext rabbit;

    bsl::shared_ptr<rmqa::VHost> vhost = rabbit.createVHostConnection(
        "tutorial-six rpc server",
        bsl::make_shared<rmqt::SimpleEndpoint>("localhost", "/"),
        bsl::make_shared<rmqt::PlainCredentials>("guest", "guest"));

    rmqa::Topology topology;
    rmqt::FieldTable quorum;
    quorum["x-queue-type"] = rmqt::FieldValue(bsl::string("quorum"));
    rmqt::QueueHandle rpcQueue = topology.addQueue(
        "rpc_queue", rmqt::AutoDelete::OFF, rmqt::Durable::ON, quorum);

    // Replies are published through the default exchange, routed by the
    // reply-to queue name each client provides.
    rmqt::Result<rmqa::Producer> publisherResult = vhost->createProducer(
        topology, topology.defaultExchange(), /* maxOutstandingConfirms */ 10);
    if (!publisherResult) {
        std::cerr << "Failed to create publisher: " << publisherResult.error()
                  << "\n";
        return 1;
    }
    auto publisher = publisherResult.value();

    rmqt::Result<rmqa::Consumer> consumerResult = vhost->createConsumer(
        topology,
        rpcQueue,
        [publisher](rmqp::MessageGuard& guard) {
            const rmqt::Message& request = guard.message();
            const rmqt::Properties& props = request.properties();

            if (props.replyTo.isNull()) {
                guard.ack();
                return;
            }

            std::string body(reinterpret_cast<const char*>(request.payload()),
                             request.payloadSize());
            int n = 0;
            try {
                n = std::stoi(body);
            }
            catch (const std::exception&) {
                // An unhandled exception in this callback would terminate the
                // server, so reject requests that aren't an integer.
                std::cerr << " [!] Ignoring unparseable request" << std::endl;
                guard.nack(/* requeue */ false);
                return;
            }
            std::cout << " [.] fib(" << n << ")" << std::endl;

            const std::string answer = std::to_string(fib(n));
            rmqt::Message reply(bsl::make_shared<bsl::vector<uint8_t> >(
                answer.begin(), answer.end()));
            reply.properties().correlationId = props.correlationId;

            publisher->send(
                reply,
                props.replyTo.value(),
                [](const rmqt::Message&,
                   const bsl::string&,
                   const rmqt::ConfirmResponse&) {});
            guard.ack();
        },
        rmqt::ConsumerConfig()
            .setConsumerTag("tutorial-six rpc server")
            .setPrefetchCount(1));
    if (!consumerResult) {
        std::cerr << "Failed to create consumer: " << consumerResult.error()
                  << "\n";
        return 1;
    }

    std::cout << " [x] Awaiting RPC requests" << std::endl;

    bslmt::Semaphore stop;
    stop.wait();
    return 0;
}
