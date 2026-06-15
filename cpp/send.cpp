// Tutorial 1: "Hello World!" publisher
//
// Publishes a single message to the `hello` queue through the default exchange.

#include <rmqa_producer.h>
#include <rmqa_rabbitcontext.h>
#include <rmqa_topology.h>
#include <rmqa_vhost.h>
#include <rmqp_producer.h>
#include <rmqt_confirmresponse.h>
#include <rmqt_fieldvalue.h>
#include <rmqt_message.h>
#include <rmqt_plaincredentials.h>
#include <rmqt_result.h>
#include <rmqt_simpleendpoint.h>

#include <bsl_memory.h>
#include <bsl_string.h>
#include <bsl_vector.h>

#include <iostream>
#include <string>

using namespace BloombergLP;

int main()
{
    // The context owns the background threads and event loop. It must outlive
    // every connection, publisher and consumer created from it.
    rmqa::RabbitContext rabbit;

    // The connection is opened lazily, when the first publisher or consumer is
    // created on the vhost. rmqcpp keeps it alive and reconnects on its own.
    bsl::shared_ptr<rmqa::VHost> vhost = rabbit.createVHostConnection(
        "tutorial-one publisher",
        bsl::make_shared<rmqt::SimpleEndpoint>("localhost", "/"),
        bsl::make_shared<rmqt::PlainCredentials>("guest", "guest"));

    // rmqcpp re-declares the topology on every (re)connection, so the queue is
    // created if it is not there yet. Quorum queues are durable and replicated.
    rmqa::Topology topology;
    rmqt::FieldTable quorum;
    quorum["x-queue-type"] = rmqt::FieldValue(bsl::string("quorum"));
    topology.addQueue("hello", rmqt::AutoDelete::OFF, rmqt::Durable::ON, quorum);

    // Without an exchange of our own we publish through the default exchange,
    // which routes a message to the queue named by its routing key.
    rmqt::Result<rmqa::Producer> publisherResult = vhost->createProducer(
        topology, topology.defaultExchange(), /* maxOutstandingConfirms */ 1);
    if (!publisherResult) {
        std::cerr << "Failed to create publisher: " << publisherResult.error()
                  << "\n";
        return 1;
    }
    auto publisher = publisherResult.value();

    const std::string text = "Hello World!";
    rmqt::Message message(
        bsl::make_shared<bsl::vector<uint8_t> >(text.begin(), text.end()));

    publisher->send(
        message,
        "hello",
        [](const rmqt::Message&,
           const bsl::string&,
           const rmqt::ConfirmResponse&) {});
    std::cout << " [x] Sent 'Hello World!'" << std::endl;

    // Wait for the broker to confirm the message is safely stored before exit.
    publisher->waitForConfirms();
    return 0;
}
