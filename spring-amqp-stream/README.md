# RabbitMQ Stream Tutorial Using Spring AMQP

It is a CLI app that uses Spring Profiles to control its behavior. Each tutorial is a trio of classes:
sender, receiver, and configuration.

## Prerequisites

To successfully use the examples you will need a running RabbitMQ server with the
 [stream plugin enabled](https://www.rabbitmq.com/docs/stream#enabling-plugin).

See [First Application With RabbitMQ Streams](https://www.rabbitmq.com/blog/2021/07/19/rabbitmq-streams-first-application),
[Stream plugin documentation](https://www.rabbitmq.com/docs/stream) and
[how to preconfigure plugins](https://www.rabbitmq.com/docs/plugins#enabled-plugins-file).

## Usage

These tutorials use Maven. To build them run

```
./mvnw clean package
```
The app uses Spring Profiles to control what tutorial it's running, and if it's a
Sender or Receiver. Choose which tutorial to run by using these profiles:

- {tut1|hello-world},{sender|receiver}

You can find usage instructions by running the following command:

```bash
java -jar target/rabbitmq-stream-tutorials.jar
```

This will display the following message:

```bash
This app uses Spring Profiles to control its behavior.

Options are:
java -jar target/rabbitmq-stream-tutorials.jar --spring.profiles.active=hello-world,receiver
java -jar target/rabbitmq-stream-tutorials.jar --spring.profiles.active=hello-world,sender
```

## Configuration

When running receivers/servers it's useful to set the duration the app runs to a longer time.  Do this by setting
the `tutorial.client.duration` property.

```
java -jar target/rabbitmq-stream-tutorials.jar --spring.profiles.active=tut1,receiver,remote --tutorial.client.duration=60000
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
and [Spring AMQP documentation](https://docs.spring.io/spring-amqp/reference/html/) for more information on setting application
properties and AMQP properties specifically.
