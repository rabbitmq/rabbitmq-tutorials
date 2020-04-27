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
package org.rabbitmq.springretry.service;

import org.rabbitmq.springretry.domain.Message;

/**
 * @author Regis Rocha.
 *
 * Service interface to send message to the RabbitMQ.
 */
public interface SendMessageService {

    /**
     * Send message to the RabbitMQ.
     *
     * @param message - Message
     * @param routingKey - String
     *
     */
    void sendMessage(Message message, String routingKey);

}
