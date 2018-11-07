# RabbitMQ tutorials ported to SoapUI

The code is based on the [Java tutorials](http://www.rabbitmq.com/tutorials), with slight modifications to make it work in SoapUI.

## Prerequisites

[SoapUI](https://www.soapui.org/downloads/soapui.html) or [ReadyAPI](https://smartbear.com/product/ready-api/overview/), and you need to place the [Java Client](http://www.rabbitmq.com/download.html#clients) in your `bin/ext` directory.

## Running

You can open and run the `RabbitMQ-tutorials-soapui-project.xml` in the GUI. The project does not have any asserts; all log messages can be viewed in the "script log" tab.

You can use the CLI runner: `$SOAPUI_HOME/bin/testrunner.sh RabbitMQ-tutorials-soapui-project.xml`.

Or you can use the provided Maven pom: `mvn verify`.

## Tutorials

The code to send any message to RabbitMQ is pretty much unchanged from the Java tutorials. The biggest difference is: there is no reading of the text of the message from the command line - the text of the messages is included in the individual steps.

I had to make two changes to the code to read any message:

1. I had to pause for a bit to let the message get processed. Without this, SoapUI ends the script step immediatelly and would kill the asynchronous process running in the background waiting for the message. The pause will depend on your specific case; it can be as simple as `sleep 10000`, or little more elaborate:

   ```groovy
   int stop = 0
   while(message == null && stop++ < 20) {
   	log.info " [*] Waiting for messages."
   	sleep 500
   }
   ```

   This will depend on your specific situation.

2. I had to explicityly:

   ```
   channel.close()
   connection.close()
   ```

   Not including this would lock up RabbitMQ and no further messages could be read.
