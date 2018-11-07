# RabbitMQ tutorials ported to SoapUI

The code is based on the [Java tutorials](http://www.rabbitmq.com/tutorials), with some modifications to make it work in SoapUI.

## Prerequisites

SoapUI (tested with version 5.4.0) or ReadyAPI (tested with version 2.5.0), and you need to place the [Java Client](http://www.rabbitmq.com/download.html#clients) in your `bin/ext` directory.

## Running

You can open and run the `RabbitMQ-tutorials-soapui-project.xml` in the GUI. The project does not have any asserts; all log messages can be viewed in the "script log" tab.

You can use the CLI runner: `$SOAPUI_HOME/testrunner.sh -A -f target/readyapi-results RabbitMQ-tutorials-soapui-project.xml`.

You can use the provided Maven pom: `mvn verify`.
