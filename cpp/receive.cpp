// Tutorial 1: "Hello World!" consumer
//
// Consumes messages from the `hello` queue and prints them.

#include <rmqa_consumer.h>
#include <rmqa_rabbitcontext.h>
#include <rmqa_topology.h>
#include <rmqa_vhost.h>
#include <rmqp_messageguard.h>
#include <rmqt_consumerconfig.h>
#include <rmqt_fieldvalue.h>
#include <rmqt_message.h>
#include <rmqt_plaincredentials.h>
#include <rmqt_result.h>
#include <rmqt_simpleendpoint.h>

#include <bslmt_semaphore.h>

#include <bsl_memory.h>
#include <bsl_string.h>

#include <iostream>
#include <string>

using namespace BloombergLP;

int main()
{
    rmqa::RabbitContext rabbit;

    bsl::shared_ptr<rmqa::VHost> vhost = rabbit.createVHostConnection(
        "tutorial-one consumer",
        bsl::make_shared<rmqt::SimpleEndpoint>("localhost", "/"),
        bsl::make_shared<rmqt::PlainCredentials>("guest", "guest"));

    rmqa::Topology topology;
    rmqt::FieldTable quorum;
    quorum["x-queue-type"] = rmqt::FieldValue(bsl::string("quorum"));
    rmqt::QueueHandle queue =
        topology.addQueue("hello", rmqt::AutoDelete::OFF, rmqt::Durable::ON, quorum);

    // Each delivery is passed to this callback on a background thread. rmqcpp
    // does not auto-acknowledge: acking only once the message is handled means
    // an interrupted consumer leaves the message on the queue for redelivery.
    rmqt::Result<rmqa::Consumer> consumerResult = vhost->createConsumer(
        topology,
        queue,
        [](rmqp::MessageGuard& guard) {
            const rmqt::Message& message = guard.message();
            std::string body(reinterpret_cast<const char*>(message.payload()),
                             message.payloadSize());
            std::cout << " [x] Received " << body << std::endl;
            guard.ack();
        },
        rmqt::ConsumerConfig().setConsumerTag("tutorial-one consumer"));
    if (!consumerResult) {
        std::cerr << "Failed to create consumer: " << consumerResult.error()
                  << "\n";
        return 1;
    }

    std::cout << " [*] Waiting for messages. To exit press CTRL+C" << std::endl;

    // The consumer runs on background threads, so park the main thread here.
    bslmt::Semaphore stop;
    stop.wait();
    return 0;
}
