// Tutorial 7: Publisher Confirms
//
// rmqcpp uses publisher confirms for every message, so this tutorial focuses on
// three ways to wait for those confirms and the throughput trade-off between
// them: one at a time, in batches, and fully asynchronously.

#include <rmqa_producer.h>
#include <rmqa_rabbitcontext.h>
#include <rmqa_topology.h>
#include <rmqa_vhost.h>
#include <rmqp_producer.h>
#include <rmqt_confirmresponse.h>
#include <rmqt_consumerconfig.h>
#include <rmqt_fieldvalue.h>
#include <rmqt_message.h>
#include <rmqt_plaincredentials.h>
#include <rmqt_result.h>
#include <rmqt_simpleendpoint.h>

#include <bsl_memory.h>
#include <bsl_string.h>
#include <bsl_vector.h>

#include <atomic>
#include <chrono>
#include <iostream>
#include <stdexcept>
#include <string>

using namespace BloombergLP;

namespace {

const int MESSAGE_COUNT = 50000;

// A durable queue that the broker removes once it has been idle for a minute
// (x-expires), so repeated runs of this tutorial leave nothing behind. It is
// durable because RabbitMQ no longer permits transient, non-exclusive queues.
bsl::shared_ptr<rmqa::Producer> createPublisher(rmqa::VHost& vhost,
                                                rmqa::Topology& topology,
                                                const bsl::string& queueName,
                                                uint16_t maxOutstandingConfirms)
{
    rmqt::FieldTable args;
    args["x-expires"] = rmqt::FieldValue(uint32_t(60000));
    topology.addQueue(
        queueName, rmqt::AutoDelete::OFF, rmqt::Durable::ON, args);

    rmqt::Result<rmqa::Producer> result = vhost.createProducer(
        topology, topology.defaultExchange(), maxOutstandingConfirms);
    if (!result) {
        throw std::runtime_error("Failed to create publisher: " +
                                 std::string(result.error()));
    }
    return result.value();
}

bsl::shared_ptr<bsl::vector<uint8_t> > messageBody()
{
    const std::string text = "Hello World!";
    return bsl::make_shared<bsl::vector<uint8_t> >(text.begin(), text.end());
}

void ignoreConfirm(const rmqt::Message&,
                   const bsl::string&,
                   const rmqt::ConfirmResponse&)
{
}

long long elapsedMs(std::chrono::steady_clock::time_point start)
{
    const auto end = std::chrono::steady_clock::now();
    return std::chrono::duration_cast<std::chrono::milliseconds>(end - start)
        .count();
}

// Strategy 1: wait for each message to be confirmed before sending the next.
// Safe and simple, but the slowest: every message costs a full round trip.
long long publishIndividually(rmqa::VHost& vhost)
{
    rmqa::Topology topology;
    const bsl::string queue =
        "pc.individual." + rmqt::ConsumerConfig::generateConsumerTag();
    auto publisher =
        createPublisher(vhost, topology, queue, /* maxOutstandingConfirms */ 1);
    auto body = messageBody();

    const auto start = std::chrono::steady_clock::now();
    for (int i = 0; i < MESSAGE_COUNT; ++i) {
        rmqt::Message message(body);
        publisher->send(message, queue, &ignoreConfirm);
        publisher->waitForConfirms();
    }
    return elapsedMs(start);
}

// Strategy 2: send a batch, then wait for the whole batch to be confirmed.
// Far fewer round trips than strategy 1, at the cost of some bookkeeping.
long long publishInBatches(rmqa::VHost& vhost)
{
    const int batchSize = 100;
    rmqa::Topology topology;
    const bsl::string queue =
        "pc.batch." + rmqt::ConsumerConfig::generateConsumerTag();
    auto publisher = createPublisher(vhost, topology, queue, batchSize);
    auto body = messageBody();

    const auto start = std::chrono::steady_clock::now();
    int outstanding = 0;
    for (int i = 0; i < MESSAGE_COUNT; ++i) {
        rmqt::Message message(body);
        publisher->send(message, queue, &ignoreConfirm);
        if (++outstanding == batchSize) {
            publisher->waitForConfirms();
            outstanding = 0;
        }
    }
    publisher->waitForConfirms();
    return elapsedMs(start);
}

// Strategy 3: never block on a confirm; react to each one in the callback.
// The fastest, and the model rmqcpp is built around.
long long publishAsynchronously(rmqa::VHost& vhost)
{
    rmqa::Topology topology;
    const bsl::string queue =
        "pc.async." + rmqt::ConsumerConfig::generateConsumerTag();
    auto publisher =
        createPublisher(vhost, topology, queue, /* maxOutstandingConfirms */ 1000);
    auto body = messageBody();

    std::atomic<int> confirmed(0);
    rmqp::Producer::ConfirmationCallback onConfirm =
        [&confirmed](const rmqt::Message&,
                     const bsl::string&,
                     const rmqt::ConfirmResponse& response) {
            if (response.status() == rmqt::ConfirmResponse::ACK) {
                confirmed.fetch_add(1, std::memory_order_relaxed);
            }
        };

    const auto start = std::chrono::steady_clock::now();
    for (int i = 0; i < MESSAGE_COUNT; ++i) {
        rmqt::Message message(body);
        publisher->send(message, queue, onConfirm);
    }
    // waitForConfirms returns only once every confirm callback has run.
    publisher->waitForConfirms();
    if (confirmed.load() != MESSAGE_COUNT) {
        std::cerr << "Only " << confirmed.load() << " of " << MESSAGE_COUNT
                  << " messages were confirmed" << std::endl;
    }
    return elapsedMs(start);
}

} // namespace

int main()
{
    rmqa::RabbitContext rabbit;
    bsl::shared_ptr<rmqa::VHost> vhost = rabbit.createVHostConnection(
        "tutorial-seven publisher confirms",
        bsl::make_shared<rmqt::SimpleEndpoint>("localhost", "/"),
        bsl::make_shared<rmqt::PlainCredentials>("guest", "guest"));

    try {
        std::cout << "Published " << MESSAGE_COUNT << " messages individually in "
                  << publishIndividually(*vhost) << " ms" << std::endl;
        std::cout << "Published " << MESSAGE_COUNT << " messages in batch in "
                  << publishInBatches(*vhost) << " ms" << std::endl;
        std::cout << "Published " << MESSAGE_COUNT
                  << " messages and handled confirms asynchronously in "
                  << publishAsynchronously(*vhost) << " ms" << std::endl;
    }
    catch (const std::exception& e) {
        std::cerr << e.what() << "\n";
        return 1;
    }
    return 0;
}
