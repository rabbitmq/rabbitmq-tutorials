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
package org.rabbitmq.springretry.config;

import org.springframework.amqp.core.*;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author Regis Rocha.
 * 
 * Configuration class retry unrecoverable queue
 *
 */
@Configuration
public class QueueRetryUnrecoverableConfig {

	/**
	 * exchange name
	 */
	public static final String DIRECT_EXCHANGE = "spring.retry.direct.exchange";

	/**
	 * queue name
	 */
	public static final String QUEUE_NAME = "spring.retry.unrecoverable.queue";

	/**
	 * queue router
	 */
	public static final String ROUTER_QUEUE = "spring.retry.unrecoverable.queue";

	/**
	 * queue DLQ
	 */
	public static final String DEAD_LETTER_QUEUE_NAME= "spring.retry.unrecoverable.dlq.queue";

	/**
	 * Create Bean queue
	 * 
	 * @return Queue
	 */
	@Bean
	public Queue retryUnrecoverableQueue() {
		return QueueBuilder.durable(QUEUE_NAME)
				.withArgument("x-dead-letter-exchange", "")
				.withArgument("x-dead-letter-routing-key", DEAD_LETTER_QUEUE_NAME)
				.withArgument("x-message-ttl", 15000)
				.build();
	}

	/**
	 * Cria o Bean Direct Exchange
	 * 
	 * @return DirectExchange
	 */
	@Bean
	public DirectExchange retryUnrecoverableExchange() {
		return new DirectExchange(DIRECT_EXCHANGE);
	}

	/**
	 * Binding queue and exchange by router
	 * 
	 * @return Binding
	 */
	@Bean
	public Binding bindingRetryUnrecoverable() {
		return BindingBuilder
				.bind(this.retryUnrecoverableQueue())
				.to(this.retryUnrecoverableExchange()).with(ROUTER_QUEUE);
	}

	/**
	 * Create bean queue dead letter
	 */
	@Bean
	public Queue deadLetterQueue() {
		return QueueBuilder.durable(DEAD_LETTER_QUEUE_NAME).build();
	}

}
