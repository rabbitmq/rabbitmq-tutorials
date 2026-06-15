// Tutorial 2: Work Queues, worker
//
// Consumes tasks from the `task_queue` queue and simulates work by sleeping one
// second per dot in the message body.

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

#include <algorithm>
#include <chrono>
#include <iostream>
#include <string>
#include <thread>

using namespace BloombergLP;

int main()
{
    rmqa::RabbitContext rabbit;

    bsl::shared_ptr<rmqa::VHost> vhost = rabbit.createVHostConnection(
        "tutorial-two worker",
        bsl::make_shared<rmqt::SimpleEndpoint>("localhost", "/"),
        bsl::make_shared<rmqt::PlainCredentials>("guest", "guest"));

    rmqa::Topology topology;
    rmqt::FieldTable quorum;
    quorum["x-queue-type"] = rmqt::FieldValue(bsl::string("quorum"));
    rmqt::QueueHandle queue = topology.addQueue(
        "task_queue", rmqt::AutoDelete::OFF, rmqt::Durable::ON, quorum);

    // A prefetch count of 1 spreads work fairly: the broker won't hand a worker
    // a new task until the previous one has been acknowledged.
    rmqt::Result<rmqa::Consumer> consumerResult = vhost->createConsumer(
        topology,
        queue,
        [](rmqp::MessageGuard& guard) {
            const rmqt::Message& message = guard.message();
            std::string body(reinterpret_cast<const char*>(message.payload()),
                             message.payloadSize());
            std::cout << " [x] Received " << body << std::endl;

            const long seconds = std::count(body.begin(), body.end(), '.');
            std::this_thread::sleep_for(std::chrono::seconds(seconds));

            std::cout << " [x] Done" << std::endl;
            // Ack after the work is finished, so an interrupted task is
            // redelivered to another worker rather than lost.
            guard.ack();
        },
        rmqt::ConsumerConfig()
            .setConsumerTag("tutorial-two worker")
            .setPrefetchCount(1));
    if (!consumerResult) {
        std::cerr << "Failed to create consumer: " << consumerResult.error()
                  << "\n";
        return 1;
    }

    std::cout << " [*] Waiting for messages. To exit press CTRL+C" << std::endl;

    bslmt::Semaphore stop;
    stop.wait();
    return 0;
}
