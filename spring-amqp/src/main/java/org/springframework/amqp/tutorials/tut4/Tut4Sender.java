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
package org.springframework.amqp.tutorials.tut4;

import org.springframework.amqp.core.DirectExchange;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author Gary Russell
 * @author Scott Deeg
 * @author Arnaud Cogoluègnes
 */
public class Tut4Sender {

	@Autowired
	private RabbitTemplate template;

	@Autowired
	private DirectExchange direct;

	AtomicInteger index = new AtomicInteger(0);

	AtomicInteger count = new AtomicInteger(0);

	private final String[] keys = {"orange", "black", "green"};

	@Scheduled(fixedDelay = 1000, initialDelay = 500)
	public void send() {
		StringBuilder builder = new StringBuilder("Hello to ");
		if (this.index.incrementAndGet() == 3) {
			this.index.set(0);
		}
		String key = keys[this.index.get()];
		builder.append(key).append(' ');
		builder.append(this.count.incrementAndGet());
		String message = builder.toString();
		template.convertAndSend(direct.getName(), key, message);
		System.out.println(" [x] Sent '" + message + "'");
	}

}
