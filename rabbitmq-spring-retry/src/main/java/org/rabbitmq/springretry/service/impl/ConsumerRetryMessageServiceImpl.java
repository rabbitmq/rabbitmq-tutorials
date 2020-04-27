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

import org.rabbitmq.springretry.config.QueueRetryConfig;
import org.rabbitmq.springretry.domain.Message;
import org.rabbitmq.springretry.service.ConsumerMessageService;
import org.rabbitmq.springretry.service.ConsumerRetryService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.stereotype.Service;

/**
 * @author Regis Rocha.
 *
 * Service to consume message RabbitMQ.
 */
@Service
public class ConsumerRetryMessageServiceImpl implements ConsumerMessageService {

    private static final Logger LOG = LoggerFactory.getLogger(ConsumerRetryMessageServiceImpl.class);

    @Autowired
    private ConsumerRetryService consumerRetryService;

    /**
     * Consume message RabbitMQ with Retry configuration.
     *
     * @param message - Message
     */
    @Override
    @RabbitListener(queues = {QueueRetryConfig.QUEUE_NAME})
    public void sendMessage(@Payload final Message message) {
        this.consumerRetryService.consumerWithRetry(message);
        LOG.info("Consuming message: {}", message);
    }
}
