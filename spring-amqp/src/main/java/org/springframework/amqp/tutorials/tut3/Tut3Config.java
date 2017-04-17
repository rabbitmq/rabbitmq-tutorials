/*
 * Copyright 2015 the original author or authors.
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
package org.springframework.amqp.tutorials.tut3;

import org.springframework.amqp.core.AnonymousQueue;
import org.springframework.amqp.core.Binding;
import org.springframework.amqp.core.BindingBuilder;
import org.springframework.amqp.core.FanoutExchange;
import org.springframework.amqp.core.Queue;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

/**
 * @author Gary Russell
 * @author Scott Deeg
 */
@Profile({"tut3", "pub-sub", "publish-subscribe"})
@Configuration
public class Tut3Config {

	@Bean
	public FanoutExchange fanout() {
		return new FanoutExchange("tut.fanout");
	}

	@Profile("receiver")
	private static class ReceiverConfig {

		@Bean
		public Queue autoDeleteQueue1() {
			return new AnonymousQueue();
		}

		@Bean
		public Queue autoDeleteQueue2() {
			return new AnonymousQueue();
		}

		@Bean
		public Binding binding1(FanoutExchange fanout, Queue autoDeleteQueue1) {
			return BindingBuilder.bind(autoDeleteQueue1).to(fanout);
		}

		@Bean
		public Binding binding2(FanoutExchange fanout, Queue autoDeleteQueue2) {
			return BindingBuilder.bind(autoDeleteQueue2).to(fanout);
		}

		@Bean
		public Tut3Receiver receiver() {
			return new Tut3Receiver();
		}

	}

	@Profile("sender")
	@Bean
	public Tut3Sender sender() {
		return new Tut3Sender();
	}

}
