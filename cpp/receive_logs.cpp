// Tutorial 3: Publish/Subscribe, log subscriber
//
// Subscribes to the `logs` fanout exchange through a private, temporary queue
// and prints every message it receives.

#include <rmqa_consumer.h>
#include <rmqa_rabbitcontext.h>
#include <rmqa_topology.h>
#include <rmqa_vhost.h>
#include <rmqp_messageguard.h>
#include <rmqt_consumerconfig.h>
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

int main()
{
    rmqa::RabbitContext rabbit;

    bsl::shared_ptr<rmqa::VHost> vhost = rabbit.createVHostConnection(
        "tutorial-three subscriber",
        bsl::make_shared<rmqt::SimpleEndpoint>("localhost", "/"),
        bsl::make_shared<rmqt::PlainCredentials>("guest", "guest"));

    rmqa::Topology topology;
    // Transient exchange, matching the convention used by the other tutorials.
    rmqt::ExchangeHandle logs = topology.addExchange(
        "logs",
        rmqt::ExchangeType::FANOUT,
        rmqt::AutoDelete::OFF,
        rmqt::Durable::OFF);

    // A fresh, uniquely named queue, auto-deleted when this subscriber
    // disconnects, so each subscriber gets its own private copy of the logs.
    // Declared durable because RabbitMQ no longer permits transient,
    // non-exclusive queues.
    rmqt::QueueHandle queue = topology.addQueue(
        rmqt::ConsumerConfig::generateConsumerTag(),
        rmqt::AutoDelete::ON,
        rmqt::Durable::ON);
    topology.bind(logs, queue, "");

    rmqt::Result<rmqa::Consumer> consumerResult = vhost->createConsumer(
        topology,
        queue,
        [](rmqp::MessageGuard& guard) {
            const rmqt::Message& message = guard.message();
            std::string body(reinterpret_cast<const char*>(message.payload()),
                             message.payloadSize());
            std::cout << " [x] " << body << std::endl;
            guard.ack();
        },
        rmqt::ConsumerConfig().setConsumerTag("tutorial-three subscriber"));
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
