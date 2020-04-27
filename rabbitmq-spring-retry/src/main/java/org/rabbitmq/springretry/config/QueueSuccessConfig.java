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

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.amqp.core.*;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.messaging.converter.MessageConverter;

/**
 * @author Regis Rocha.
 * 
 * Configuration class success queue
 *
 */
@Configuration
public class QueueSuccessConfig {

	/**
	 * exchange name
	 */
	public static final String DIRECT_EXCHANGE = "spring.retry.direct.exchange";

	/**
	 * queue name
	 */
	public static final String QUEUE_NAME = "spring.success.queue";

	/**
	 * queue router
	 */
	public static final String ROUTER_QUEUE = "spring.success.queue";

	/**
	 * Create Bean queue
	 * 
	 * @return Queue
	 */
	@Bean
	public Queue queue() { return QueueBuilder.durable(QUEUE_NAME).build(); }

	/**
	 * Cria o Bean Direct Exchange
	 * 
	 * @return DirectExchange
	 */
	@Bean
	public DirectExchange exchange() {
		return new DirectExchange(DIRECT_EXCHANGE);
	}

	/**
	 * Binding queue and exchange by router
	 * 
	 * @return Binding
	 */
	@Bean
	public Binding successBinding() {
		return BindingBuilder.bind(this.queue()).to(this.exchange()).with(ROUTER_QUEUE);
	}
	
	/**
	 * Create message converter
	 */
	@Bean
	public Jackson2JsonMessageConverter messageConverter() {
		final ObjectMapper mapper = new ObjectMapper().findAndRegisterModules();
		return new Jackson2JsonMessageConverter(mapper);
	}

}
