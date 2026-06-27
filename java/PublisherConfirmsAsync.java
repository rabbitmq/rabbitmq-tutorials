// Copyright (c) 2007-2025 Broadcom. All Rights Reserved. The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
//
// This software, the RabbitMQ Java client library, is triple-licensed under the
// Mozilla Public License 2.0 ("MPL"), the GNU General Public License version 2
// ("GPL") and the Apache License version 2 ("ASL"). For the MPL, please see
// LICENSE-MPL-RabbitMQ. For the GPL, please see LICENSE-GPL2.  For the ASL,
// please see LICENSE-APACHE2.
//
// This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND,
// either express or implied. See the LICENSE file for specific language governing
// rights and limitations of this software.
//
// If you have any questions regarding licensing, please contact us at
// info@rabbitmq.com.

import com.rabbitmq.client.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * Example demonstrating asynchronous publisher confirmations with ConfirmationChannel.
 */
public class PublisherConfirmsAsync {

    public static void main(String[] args) throws Exception {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");

        try (Connection connection = factory.newConnection();
             Channel channel = connection.createChannel()) {

            // Create ConfirmationChannel with optional rate limiting
            RateLimiter rateLimiter = new ThrottlingRateLimiter(100); // Max 100 in-flight
            ConfirmationChannel confirmChannel = ConfirmationChannel.create(channel, rateLimiter);

            String queueName = confirmChannel.queueDeclare().getQueue();

            // Collect futures for all publishes
            List<CompletableFuture<String>> futures = new ArrayList<>();

            // Publish messages asynchronously with context for correlation
            for (int i = 0; i < 10; i++) {
                String messageId = "msg-" + i;
                String message = "Message " + i;

                CompletableFuture<String> future = confirmChannel.basicPublishAsync(
                        "",
                        queueName,
                        MessageProperties.PERSISTENT_TEXT_PLAIN,
                        message.getBytes(),
                        messageId  // Context parameter for correlation
                );

                // Handle confirmation
                future.thenAccept(id -> System.out.println("Confirmed: " + id))
                      .exceptionally(ex -> {
                          if (ex.getCause() instanceof PublishException) {
                              PublishException pe = (PublishException) ex.getCause();
                              System.err.println("Failed: " + pe.getContext() + " - " + ex.getMessage());
                          }
                          return null;
                      });

                futures.add(future);
            }

            // Wait for all confirmations
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
            System.out.println("All messages published and confirmed");
        }
    }
}
