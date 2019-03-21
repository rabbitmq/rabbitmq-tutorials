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

import org.springframework.amqp.rabbit.annotation.RabbitListener;

/**
 * @author Gary Russell
 * @author Scott Deeg
 */
public class Tut6Server {

	@RabbitListener(queues = "tut.rpc.requests")
	// @SendTo("tut.rpc.replies") used when the client doesn't set replyTo.
	public int fibonacci(int n) {
		System.out.println(" [x] Received request for " + n);
		int result = fib(n);
		System.out.println(" [.] Returned " + result);
		return result;
	}

	public int fib(int n) {
		return n == 0 ? 0 : n == 1 ? 1 : (fib(n - 1) + fib(n - 2));
	}

}
