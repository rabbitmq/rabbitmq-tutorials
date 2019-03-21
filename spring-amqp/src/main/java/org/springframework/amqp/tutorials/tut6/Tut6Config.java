/*
 * Copyright 2015 the original author or authors.
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
package org.springframework.amqp.tutorials.tut6;

import org.springframework.amqp.core.Binding;
import org.springframework.amqp.core.BindingBuilder;
import org.springframework.amqp.core.DirectExchange;
import org.springframework.amqp.core.Queue;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

/**
 * @author Gary Russell
 * @author Scott Deeg
 *
 */
@Profile({"tut6","rpc"})
@Configuration
public class Tut6Config {

	@Profile("client")
	private static class ClientConfig {

		@Bean
		public DirectExchange exchange() {
			return new DirectExchange("tut.rpc");
		}

		@Bean
		public Tut6Client client() {
	 	 	return new Tut6Client();
		}

	}

	@Profile("server")
	private static class ServerConfig {

		@Bean
		public Queue queue() {
			return new Queue("tut.rpc.requests");
		}

		@Bean
		public DirectExchange exchange() {
			return new DirectExchange("tut.rpc");
		}

		@Bean
		public Binding binding(DirectExchange exchange, Queue queue) {
			return BindingBuilder.bind(queue).to(exchange).with("rpc");
		}

		@Bean
		public Tut6Server server() {
			return new Tut6Server();
		}

	}

}
