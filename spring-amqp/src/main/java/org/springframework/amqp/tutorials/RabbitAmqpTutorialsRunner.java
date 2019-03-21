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
package org.springframework.amqp.tutorials;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.ConfigurableApplicationContext;

/**
 * @author Gary Russell
 * @author Scott Deeg
 */
public class RabbitAmqpTutorialsRunner implements CommandLineRunner {

	@Value("${tutorial.client.duration:0}")
	private int duration;

	@Autowired
	private ConfigurableApplicationContext ctx;

	@Override
	public void run(String... arg0) throws Exception {
		System.out.println("Ready ... running for " + duration + "ms");
		Thread.sleep(duration);
		ctx.close();
	}

}
