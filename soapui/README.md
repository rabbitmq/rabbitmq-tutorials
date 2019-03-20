# RabbitMQ tutorials ported to SoapUI

The code is based on the [Java tutorials](https://www.rabbitmq.com/tutorials),
with slight modifications to make it work in SoapUI.

## Prerequisites

 * [SoapUI](https://www.soapui.org/downloads/soapui.html) or [ReadyAPI](https://smartbear.com/product/ready-api/overview/)
 with the [Java Client](https://www.rabbitmq.com/download.html#clients) JAR file in the `bin/ext` directory.
 * local RabbitMQ broker instance running with all the defaults

## Running

You can import and run the `RabbitMQ-tutorials-soapui-project.xml` in the GUI.
The project does not have any asserts; all log messages can be viewed in the "script log" tab.

You can also use the [CLI runner](https://www.soapui.org/test-automation/running-from-command-line/functional-tests.html):
`$SOAPUI_HOME/bin/testrunner.sh RabbitMQ-tutorials-soapui-project.xml`.

Or you can use the provided Maven pom: `./mvnw verify`.

## Tutorials

The code to *send* messages to RabbitMQ is pretty much unchanged from
the Java tutorials. The biggest change is: there is no reading of the
text of the message from the command line - the text of the message
is included in each individual step.

There are two changes to the code to *consume* messages:

1. The program pauses for a bit to let the message get processed.
Without this, SoapUI ends the script step immediately and
kills the asynchronous process running in the background waiting
for the message. The exact pause will depend on the specific case;
it can be as simple as `sleep 10000`, or little more elaborate, e.g.:

   ```groovy
   int stop = 0
   while(message == null && stop++ < 20) {
   	log.info " [*] Waiting for messages."
   	sleep 500
   }
   ```

2. At the end of each read script step, the channel and connections
are closed explicitly:

   ```groovy
   channel.close()
   connection.close()
   ```

Without this, some background threads are still running and would make
the test hang.
