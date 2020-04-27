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
package org.rabbitmq.springretry.controller;

import io.swagger.annotations.*;
import org.rabbitmq.springretry.api.MessageRequest;
import org.rabbitmq.springretry.config.QueueRetryConfig;
import org.rabbitmq.springretry.config.QueueRetryUnrecoverableConfig;
import org.rabbitmq.springretry.config.QueueSuccessConfig;
import org.rabbitmq.springretry.domain.Message;
import org.rabbitmq.springretry.service.SendMessageService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Regis Rocha.
 *
 * REST Controller that send message to the RabbitMQ.
 *
 */
@RestController
@Api(value = "API that send message to the RabbitMQ.")
@RequestMapping(value = "/send", consumes = MediaType.APPLICATION_JSON_VALUE)
public class SendMessageRabbitController {

    private static final Logger LOG = LoggerFactory.getLogger(SendMessageRabbitController.class);

    @Autowired
    private SendMessageService sendMessageService;

    /**
     * API to send success message to the RabbitMQ.
     *
     * @param messages - List<MessageRequest>
     *
     * @return ResponseEntity<String>
     */
    @ApiOperation("API to send success message to the RabbitMQ.")
    @ApiResponses(value = {
        @ApiResponse(code = 200, message = "Success"),
        @ApiResponse(code = 500, message = "Erro to send message to the RabbitMQ.")})
    @PostMapping("/success")
    public ResponseEntity<String> sendSuccessMessages(@RequestBody final List<MessageRequest> messages) {
        try {
            LOG.info("Controller sending message: {}", messages);
            this.toMessageModel(messages)
                    .forEach(m -> this.sendMessageService.sendMessage(m, QueueSuccessConfig.ROUTER_QUEUE));
            return ResponseEntity.ok("Message sent successfully");
        } catch (Exception e) {
            LOG.error("", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Error when sending message to RabbitMQ.");
        }
    }

    /**
     * API to send message to the RabbitMQ.
     *
     * At the consuming message time, the force error will be throw to simulate retrying the consume messages.
     *
     * @param messages - List<MessageRequest>
     *
     * @return ResponseEntity<String>
     */
    @ApiOperation("API to send message to the RabbitMQ to test Retry Policy when error occur in consumer. This example will retry until success message be consumed")
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "Success"),
            @ApiResponse(code = 500, message = "Error to send message to the RabbitMQ.")})
    @PostMapping("/retry-recovery")
    public ResponseEntity<String> sendRetryMessages(@RequestBody final List<MessageRequest> messages) {
        try {
            LOG.info("Controller sending message: {}", messages);
            this.toMessageModel(messages)
                    .forEach(m -> this.sendMessageService.sendMessage(m, QueueRetryConfig.ROUTER_QUEUE));
            return ResponseEntity.ok("Message sent successfully");
        } catch (Exception e) {
            LOG.error("", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Error when sending message to RabbitMQ.");
        }
    }

    /**
     * API to send message to the RabbitMQ.
     *
     * At the consuming message time, the force error will be throw to simulate retrying the consume messages.
     *
     * @param messages - List<MessageRequest>
     *
     * @return ResponseEntity<String>
     */
    @ApiOperation("API to send message to the RabbitMQ to test Retry Policy when error occur in consumer. This example the message will not be consumed to simulate the max attempt the retry policy.")
    @ApiResponses(value = {
            @ApiResponse(code = 200, message = "Success"),
            @ApiResponse(code = 500, message = "Error to send message to the RabbitMQ.")})
    @PostMapping("/retry-unrecoverable")
    public ResponseEntity<String> sendRetryUnrecoverableMessages(@RequestBody final List<MessageRequest> messages) {
        try {
            LOG.info("Controller sending message: {}", messages);
            this.toMessageModel(messages)
                    .forEach(m -> this.sendMessageService.sendMessage(m, QueueRetryUnrecoverableConfig.ROUTER_QUEUE));
            return ResponseEntity.ok("Message sent successfully");
        } catch (Exception e) {
            LOG.error("", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Error when sending message to RabbitMQ.");
        }
    }

    /**
     * Convert request object to model.
     *
     * @param messages - List<MessageRequest>
     *
     * @return List<Message>
     */
    private List<Message> toMessageModel(final List<MessageRequest> messages) {
        if (!CollectionUtils.isEmpty(messages)) {
            return messages.stream().map(this::toModel).collect(Collectors.toList());
        }
        return Collections.emptyList();
    }

    private Message toModel(final MessageRequest request) {
       return new Message(UUID.randomUUID().toString(), LocalDateTime.now(), request.getMessage());
    }
}
