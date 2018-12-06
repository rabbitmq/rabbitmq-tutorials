/*
 * Copyright 2015-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.amqp.tutorials.tut2;

import org.springframework.amqp.core.Queue;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

/**
 * @author Gary Russell
 * @author Scott Deeg
 */
@Profile({"tut2", "work-queues"})
@Configuration
public class Tut2Config {

	@Bean
	public Queue hello() {
		return new Queue("tut.hello");
	}

	@Profile("receiver")
	private static class ReceiverConfig {

		@Bean
		public Tut2Receiver receiver1() {
			return new Tut2Receiver(1);
		}

		@Bean
		public Tut2Receiver receiver2() {
			return new Tut2Receiver(2);
		}

	}

	@Profile("sender")
	@Bean
	public Tut2Sender sender() {
		return new Tut2Sender();
	}

}
