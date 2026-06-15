// Tutorial 3: Publish/Subscribe, log publisher
//
// Publishes a log message to the `logs` fanout exchange, which broadcasts it to
// every bound queue.

#include <rmqa_producer.h>
#include <rmqa_rabbitcontext.h>
#include <rmqa_topology.h>
#include <rmqa_vhost.h>
#include <rmqp_producer.h>
#include <rmqt_confirmresponse.h>
#include <rmqt_exchangetype.h>
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

int main(int argc, char** argv)
{
    rmqa::RabbitContext rabbit;

    bsl::shared_ptr<rmqa::VHost> vhost = rabbit.createVHostConnection(
        "tutorial-three publisher",
        bsl::make_shared<rmqt::SimpleEndpoint>("localhost", "/"),
        bsl::make_shared<rmqt::PlainCredentials>("guest", "guest"));

    rmqa::Topology topology;
    // Transient exchange, matching the convention used by the other tutorials.
    rmqt::ExchangeHandle logs = topology.addExchange(
        "logs",
        rmqt::ExchangeType::FANOUT,
        rmqt::AutoDelete::OFF,
        rmqt::Durable::OFF);

    rmqt::Result<rmqa::Producer> publisherResult =
        vhost->createProducer(topology, logs, /* maxOutstandingConfirms */ 1);
    if (!publisherResult) {
        std::cerr << "Failed to create publisher: " << publisherResult.error()
                  << "\n";
        return 1;
    }
    auto publisher = publisherResult.value();

    std::string text = "info: Hello World!";
    if (argc > 1) {
        text.clear();
        for (int i = 1; i < argc; ++i) {
            if (i > 1) {
                text += ' ';
            }
            text += argv[i];
        }
    }

    rmqt::Message message(
        bsl::make_shared<bsl::vector<uint8_t> >(text.begin(), text.end()));

    // A fanout exchange ignores the routing key.
    publisher->send(
        message,
        "",
        [](const rmqt::Message&,
           const bsl::string&,
           const rmqt::ConfirmResponse&) {});
    std::cout << " [x] Sent " << text << std::endl;

    publisher->waitForConfirms();
    return 0;
}
