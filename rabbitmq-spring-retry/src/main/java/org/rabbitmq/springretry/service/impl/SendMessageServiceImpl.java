/*
 * Copyright 2015-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.rabbitmq.springretry.service.impl;

import org.rabbitmq.springretry.domain.Message;
import org.rabbitmq.springretry.service.SendMessageService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author Regis Rocha.
 *
 * Service interface to send message to the RabbitMQ.
 */
@Service
public class SendMessageServiceImpl implements SendMessageService {

    private static final Logger LOG = LoggerFactory.getLogger(SendMessageServiceImpl.class);

    @Autowired
    private RabbitTemplate rabbitTemplate;

    /**
     * Send message to the RabbitMQ.
     *
     * @param message - Message
     * @param routingKey - String
     *
     */
    @Override
    public void sendMessage(final Message message, final String routingKey) {
        try {
            LOG.info("Sending message to the Rabbit: {}", message);
            this.rabbitTemplate.convertAndSend(routingKey, message);
        } catch (Exception e) {
            LOG.error("", e);
            throw new RuntimeException("Error sending message to the RabbitMQ");
        }
    }
}
