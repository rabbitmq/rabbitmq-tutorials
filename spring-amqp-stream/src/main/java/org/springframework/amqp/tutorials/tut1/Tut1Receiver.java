package org.springframework.amqp.tutorials.tut1;

import org.springframework.amqp.rabbit.annotation.RabbitListener;

import com.rabbitmq.stream.Message;

public class Tut1Receiver {

    @RabbitListener(queues="hello-spring-stream", containerFactory="nativeFactory")
    public void listen(Message message) {
        System.out.println("Received message: " + new String(message.getBodyAsBinary()));
    }
}
