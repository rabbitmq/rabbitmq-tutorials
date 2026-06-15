// Tutorial 6: Remote Procedure Call, client
//
// Sends a fib(n) request to the `rpc_queue` queue and waits for the answer on a
// private reply queue. Replies arrive on a background thread, so `call` bridges
// the result back with a promise/future, matched to the request by correlation
// id.

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

#include <bsl_memory.h>
#include <bsl_string.h>
#include <bsl_vector.h>

#include <future>
#include <iostream>
#include <memory>
#include <mutex>
#include <stdexcept>
#include <string>

using namespace BloombergLP;

class FibonacciRpcClient {
  public:
    explicit FibonacciRpcClient(const bsl::shared_ptr<rmqa::VHost>& vhost)
    : d_replyQueueName("rpc.reply." + rmqt::ConsumerConfig::generateConsumerTag())
    {
        rmqt::FieldTable quorum;
        quorum["x-queue-type"] = rmqt::FieldValue(bsl::string("quorum"));
        d_topology.addQueue(
            "rpc_queue", rmqt::AutoDelete::OFF, rmqt::Durable::ON, quorum);

        // A private reply queue, auto-deleted once this client exits. Declared
        // durable because RabbitMQ no longer permits transient, non-exclusive
        // queues.
        rmqt::QueueHandle replyQueue = d_topology.addQueue(
            d_replyQueueName, rmqt::AutoDelete::ON, rmqt::Durable::ON);

        rmqt::Result<rmqa::Producer> publisherResult = vhost->createProducer(
            d_topology, d_topology.defaultExchange(), /* maxOutstandingConfirms */ 1);
        if (!publisherResult) {
            throw std::runtime_error("Failed to create publisher: " +
                                     std::string(publisherResult.error()));
        }
        d_publisher = publisherResult.value();

        rmqt::Result<rmqa::Consumer> consumerResult = vhost->createConsumer(
            d_topology,
            replyQueue,
            [this](rmqp::MessageGuard& guard) { onReply(guard); },
            rmqt::ConsumerConfig().setConsumerTag(d_replyQueueName));
        if (!consumerResult) {
            throw std::runtime_error("Failed to create consumer: " +
                                     std::string(consumerResult.error()));
        }
        d_consumer = consumerResult.value();
    }

    long long call(int n)
    {
        auto promise = std::make_shared<std::promise<long long> >();
        std::future<long long> future = promise->get_future();

        const bsl::string correlationId = bsl::to_string(++d_requestCount);
        {
            std::lock_guard<std::mutex> guard(d_mutex);
            d_correlationId = correlationId;
            d_pending       = promise;
        }

        const std::string payload = std::to_string(n);
        rmqt::Message request(
            bsl::make_shared<bsl::vector<uint8_t> >(payload.begin(), payload.end()));
        request.properties().correlationId = correlationId;
        request.properties().replyTo       = d_replyQueueName;

        d_publisher->send(
            request,
            "rpc_queue",
            [](const rmqt::Message&,
               const bsl::string&,
               const rmqt::ConfirmResponse&) {});

        return future.get();
    }

  private:
    void onReply(rmqp::MessageGuard& guard)
    {
        const rmqt::Message& message = guard.message();
        const bsl::string correlationId =
            message.properties().correlationId.value_or(bsl::string());

        std::shared_ptr<std::promise<long long> > pending;
        {
            std::lock_guard<std::mutex> g(d_mutex);
            if (d_pending && correlationId == d_correlationId) {
                pending = d_pending;
                d_pending.reset();
            }
        }

        if (pending) {
            std::string body(reinterpret_cast<const char*>(message.payload()),
                             message.payloadSize());
            pending->set_value(std::stoll(body));
        }
        guard.ack();
    }

    bsl::string d_replyQueueName;
    rmqa::Topology d_topology;
    int d_requestCount = 0;
    std::mutex d_mutex;
    bsl::string d_correlationId;
    std::shared_ptr<std::promise<long long> > d_pending;
    bsl::shared_ptr<rmqa::Producer> d_publisher;
    // Declared last so the consumer stops before the members its callback uses.
    bsl::shared_ptr<rmqa::Consumer> d_consumer;
};

int main(int argc, char** argv)
{
    const int n = argc > 1 ? std::stoi(argv[1]) : 30;

    rmqa::RabbitContext rabbit;
    bsl::shared_ptr<rmqa::VHost> vhost = rabbit.createVHostConnection(
        "tutorial-six rpc client",
        bsl::make_shared<rmqt::SimpleEndpoint>("localhost", "/"),
        bsl::make_shared<rmqt::PlainCredentials>("guest", "guest"));

    try {
        FibonacciRpcClient client(vhost);
        std::cout << " [x] Requesting fib(" << n << ")" << std::endl;
        const long long response = client.call(n);
        std::cout << " [.] Got " << response << std::endl;
    }
    catch (const std::exception& e) {
        std::cerr << e.what() << "\n";
        return 1;
    }
    return 0;
}
