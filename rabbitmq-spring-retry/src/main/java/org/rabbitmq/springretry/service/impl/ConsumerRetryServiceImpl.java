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
import org.rabbitmq.springretry.exception.RetryConsumeMessageException;
import org.rabbitmq.springretry.service.ConsumerRetryService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;

import java.util.Random;

/**
 * @author Regis Rocha.
 *
 * Service to consume message with retry policy.
 */
@Service
public class ConsumerRetryServiceImpl implements ConsumerRetryService {
    private static final int BOUND_RANDOM = 3;
    private static final int LIMIT_ATTEMPTS = 10;
    private static final Logger LOG = LoggerFactory.getLogger(ConsumerRetryService.class);

    /**
     * Consume message with retry policy.
     *
     * To simulate Retry Policy, the conditional was provide if (Random number is even)
     * exception {@link RetryConsumeMessageException} will be throw otherwise message will be
     * consumed with success
     *
     * This example don't have the maximum attempts to retry.
     *
     * @param message - {@link Message}
     */
    @Override
    @Retryable(maxAttempts = Integer.MAX_VALUE, value = {RetryConsumeMessageException.class}, backoff = @Backoff(delay = 1000))
    public void consumerWithRetry(final Message message) {
        LOG.info("Retry Consuming message ID: {}", message.getId());
        if(new Random().nextInt(BOUND_RANDOM) % 2 == 0) {
            throw new RetryConsumeMessageException("Exception when consuming message to test Retry");
        }
    }

    /**
     * Consume message with retry policy.
     *
     * To simulate Retry Policy, the conditional was provide if (Random number is even)
     * exception {@link RetryConsumeMessageException} will be throw otherwise message will be
     * consumed with success.
     *
     * This example have the maximum attempts to retry.
     * When the maximum attempts the recoverable method will be invoked
     *
     * @param message - {@link Message}
     */
    @Override
    @Retryable(maxAttempts = LIMIT_ATTEMPTS, value = {RetryConsumeMessageException.class}, backoff = @Backoff(delay = 1000))
    public void consumerWithRetryUnRecoverable(final Message message) {
        LOG.info("Retry Consuming message ID: {}", message.getId());
        throw new RetryConsumeMessageException("Exception when consuming message to test Retry");
    }

    @Recover
    public void revocer(final Message message, RetryConsumeMessageException e) {
        LOG.info("The maximum number of attempts have reached");
        throw new RuntimeException(e);
    }
}
