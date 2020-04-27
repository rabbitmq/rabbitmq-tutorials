# RabbitMQ Tutorial Using SpringBoot and Spring-Retry 

This project implements examples using SpringBoot and Spring-Retry to demonstrate how can Retry automatic consume message when exceptions occur while processing the consumer messages RabbitMQ.

In this tutorial was create the 3 APIs to send the message to the RabbitMQ. All 3 APIs are:

- /send/success
- /send/retry-recovery
- /send/retry-unrecoverable

All detailed documentation about these APIs are available on Swagger documentation on this tutorial,
to see the swagger this tutorial, follow all steps to start the project detailed on the next steps.

[1]: http://localhost:8080/swagger-ui.html

## Start RabbitMQ
To execute the project, is required start the RabbitMQ before start the tutorial, 
to start the RabbitMQ you can start the docker-compose configured in this tutorial.

```
docker-compose up
```

## Build the project

You can build this tutorial, executing the following command

```
./mvnw clean package
```

## Start the project

After building the project, you can start the project with the command:

```
java -jar target/rabbitmq-tutorials.jar
```

To use another instance of RabbitMQ set the following properties in the application.yml:

```
spring:
  rabbitmq:
    host: <host>
    port: <port>
    username: <usarname>
    password: <password>
```

