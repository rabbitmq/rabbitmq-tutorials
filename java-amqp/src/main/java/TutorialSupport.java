import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.impl.AmqpEnvironmentBuilder;

/**
 * Shared connection defaults for the AMQP 1.0 tutorials (guest on localhost, default port).
 */
final class TutorialSupport {

    static final String BROKER_URI = "amqp://guest:guest@localhost:5672/%2f";

    private TutorialSupport() {
    }

    static Environment newEnvironment() {
        return new AmqpEnvironmentBuilder().build();
    }
}
