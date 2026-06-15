// Tutorial 5: Topics, log subscriber
//
// Subscribes to the `topic_logs` topic exchange, binding a private queue for
// each binding pattern passed on the command line, e.g. `kern.*` or `*.critical`.

#include <rmqa_consumer.h>
#include <rmqa_rabbitcontext.h>
#include <rmqa_topology.h>
#include <rmqa_vhost.h>
#include <rmqp_messageguard.h>
#include <rmqt_consumerconfig.h>
#include <rmqt_envelope.h>
#include <rmqt_exchangetype.h>
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

int main(int argc, char** argv)
{
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " [binding_key]...\n";
        return 1;
    }

    rmqa::RabbitContext rabbit;

    bsl::shared_ptr<rmqa::VHost> vhost = rabbit.createVHostConnection(
        "tutorial-five subscriber",
        bsl::make_shared<rmqt::SimpleEndpoint>("localhost", "/"),
        bsl::make_shared<rmqt::PlainCredentials>("guest", "guest"));

    rmqa::Topology topology;
    // Transient exchange, matching the convention used by the other tutorials.
    rmqt::ExchangeHandle topicLogs = topology.addExchange(
        "topic_logs",
        rmqt::ExchangeType::TOPIC,
        rmqt::AutoDelete::OFF,
        rmqt::Durable::OFF);

    // Private, auto-deleted queue with a unique name. Declared durable because
    // RabbitMQ no longer permits transient, non-exclusive queues.
    rmqt::QueueHandle queue = topology.addQueue(
        rmqt::ConsumerConfig::generateConsumerTag(),
        rmqt::AutoDelete::ON,
        rmqt::Durable::ON);

    for (int i = 1; i < argc; ++i) {
        topology.bind(topicLogs, queue, argv[i]);
    }

    rmqt::Result<rmqa::Consumer> consumerResult = vhost->createConsumer(
        topology,
        queue,
        [](rmqp::MessageGuard& guard) {
            const rmqt::Message& message = guard.message();
            std::string body(reinterpret_cast<const char*>(message.payload()),
                             message.payloadSize());
            std::cout << " [x] " << guard.envelope().routingKey() << ":" << body
                      << std::endl;
            guard.ack();
        },
        rmqt::ConsumerConfig().setConsumerTag("tutorial-five subscriber"));
    if (!consumerResult) {
        std::cerr << "Failed to create consumer: " << consumerResult.error()
                  << "\n";
        return 1;
    }

    std::cout << " [*] Waiting for logs. To exit press CTRL+C" << std::endl;

    bslmt::Semaphore stop;
    stop.wait();
    return 0;
}
