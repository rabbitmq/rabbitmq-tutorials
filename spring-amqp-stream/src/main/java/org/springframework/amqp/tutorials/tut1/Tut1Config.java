package org.springframework.amqp.tutorials.tut1;

import org.springframework.amqp.rabbit.listener.RabbitListenerContainerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.rabbit.stream.config.StreamRabbitListenerContainerFactory;
import org.springframework.rabbit.stream.listener.StreamListenerContainer;
import org.springframework.rabbit.stream.producer.RabbitStreamTemplate;

import com.rabbitmq.stream.ByteCapacity;
import com.rabbitmq.stream.Environment;
import com.rabbitmq.stream.OffsetSpecification;

@Profile({"tut1","hello-world"})
@Configuration
public class Tut1Config {

    private final String stream = "hello-spring-stream";

    @Bean
    public Environment rabbitStreamEnvironment() {
        Environment environment = Environment.builder().build();
        environment.streamCreator().stream(stream).maxLengthBytes(ByteCapacity.GB(5)).create();
        return environment;
    }

    @Bean
    RabbitStreamTemplate streamTemplate(Environment env) {
        return new RabbitStreamTemplate(env, stream);
    }

    @Bean
    RabbitListenerContainerFactory<StreamListenerContainer> nativeFactory(Environment env) {
        StreamRabbitListenerContainerFactory factory = new StreamRabbitListenerContainerFactory(env);
        factory.setNativeListener(true);
        factory.setConsumerCustomizer((id, builder) -> {
            builder.stream(stream)
            .offset(OffsetSpecification.first())
            .build();
        });
        return factory;
    }

    @Profile("receiver")
    @Bean
    public Tut1Receiver receiver() {
        return new Tut1Receiver();
    }

    @Profile("sender")
    @Bean
    public Tut1Sender sender() {
        return new Tut1Sender();
    }
}
