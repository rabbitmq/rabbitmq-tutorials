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
import org.rabbitmq.springretry.exception.RetryConsumeMessageException;

/**
 * @author Regis Rocha.
 *
 * Service interface to consume message with retry policy.
 */
public interface ConsumerRetryService {

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
    void consumerWithRetry(Message message);

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
    void consumerWithRetryUnRecoverable(Message message);
}
