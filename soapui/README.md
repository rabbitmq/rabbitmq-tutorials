# RabbitMQ tutorials ported to SoapUI

The code is based on the [Java tutorials](http://www.rabbitmq.com/tutorials), with slight modifications to make it work in SoapUI.

## Prerequisites

[SoapUI](https://www.soapui.org/downloads/soapui.html) or [ReadyAPI](https://smartbear.com/product/ready-api/overview/), and you need to place the [Java Client](http://www.rabbitmq.com/download.html#clients) in your `bin/ext` directory.

## Running

You can open and run the `RabbitMQ-tutorials-soapui-project.xml` in the GUI. The project does not have any asserts; all log messages can be viewed in the "script log" tab.

You can use the CLI runner: `$SOAPUI_HOME/testrunner.sh RabbitMQ-tutorials-soapui-project.xml`.

Or you can use the provided Maven pom: `mvn verify`.
