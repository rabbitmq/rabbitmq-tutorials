# RabbitMQ Tutorial Using Spring AMQP

This project implements each of the [6 RabbitMQ Tutorials][1] using Spring AMQP.

It is a CLI app that uses Spring Profiles to control its behavior.  Each tutorial is a trio of classes:
sender, receiver, and configuration.

[1]: https://www.rabbitmq.com/getstarted.html

## Prerequisites
These tutorials assume RabbitMQ is [installed](http://rabbitmq.com/download.html) and running
on `localhost` using the standard port (`5672`). In case you use
a different host, port or credentials, connections settings would require adjusting.

## Usage

These tutorials use Maven. To build them run

```
./mvnw clean package
```
The app uses Spring Profiles to control what tutorial it's running, and if it's a
Sender or Receiver. Choose which tutorial to run by using these profiles:

- {tut1|hello-world},{sender|receiver}
- {tut2|work-queues},{sender|receiver}
- {tut3|pub-sub|publish-subscribe},{sender|receiver}
- {tut4|routing},{sender|receiver}
- {tut5|topics},{sender|receiver}
- {tut6|rpc},{client|server}

After building with maven, run the app however you like to run boot apps.

For example:

```
java -jar target/rabbitmq-tutorials.jar --spring.profiles.active=work-queues,sender
```

will run the publisher part of tutorial 2 (Work Queues).

For tutorials 1-5, run the consumer (receiver) followed by the publisher (sender):

```
# shell 1
java -jar target/rabbit-tutorials-*.jar --spring.profiles.active=work-queues,receiver

# shell 2
java -jar target/rabbit-tutorials-*.jar --spring.profiles.active=work-queues,sender
```

For tutorial 6, run the server followed by the client.

##C onfiguration

When running receivers/servers it's useful to set the duration the app runs to a longer time.  Do this by setting
the `tutorial.client.duration` property.

```
java -jar rabbitmq-tutorials.jar --spring.profiles.active=tut2,receiver,remote --tutorial.client.duration=60000
```

By default, Spring AMQP uses localhost to connect to RabbitMQ.  In the
sample, the `remote` profile causes Spring to load the properties in
`application-remote.yml` that are used for testing with a non-local
server.  Set your own properties in the one in the project, or provide
your own on the command line when you run it.

To use to a remote RabbitMQ installation set the following properties:

```
spring:
  rabbitmq:
    host: <rabbitmq-server>
    username: <tutorial-user>
    password: <tutorial-user>
```

To use this at runtime create a file called `application-remote.yml` (or properties) and set the properties in there.  Then set the
remote profile as in the example above.  See the [Spring Boot](https://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/)
and [Spring AMQP documentation](http://docs.spring.io/spring-amqp/reference/html/) for more information on setting application
properties and AMQP properties specifically.
