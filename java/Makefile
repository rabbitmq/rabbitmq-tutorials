JAVAC := javac -Xlint -Werror
JAVA_RUN := java -cp $(CURDIR):amqp-client-4.1.1.jar:slf4j-api-1.7.21.jar:slf4j-simple-1.7.22.jar

# http://www.rabbitmq.com/tutorials/tutorial-one-java.html
# Tutorial One: Hello World
tutorial_one: jars
	$(JAVAC) -cp amqp-client-4.1.1.jar Send.java Recv.java

run_tutorial_one: tutorial_one
	$(JAVA_RUN) Send
	$(JAVA_RUN) Recv

# http://www.rabbitmq.com/tutorials/tutorial-two-java.html
# Tutorial Two: Work Queues
tutorial_two: jars
	$(JAVAC) -cp amqp-client-4.1.1.jar NewTask.java Worker.java

run_worker: tutorial_two
	$(JAVA_RUN) Worker

run_task: tutorial_two
	$(JAVA_RUN) NewTask

# http://www.rabbitmq.com/tutorials/tutorial-six-java.html
# Tutorial Six: RPC
tutorial_six: jars
	$(JAVAC) -cp amqp-client-4.1.1.jar RPCClient.java RPCServer.java

jars: amqp-client-4.1.1.jar slf4j-api-1.7.21.jar slf4j-simple-1.7.22.jar

slf4j-api-1.7.21.jar:
	curl -sLO http://central.maven.org/maven2/org/slf4j/slf4j-api/1.7.21/slf4j-api-1.7.21.jar

slf4j-simple-1.7.22.jar:
	curl -sLO http://central.maven.org/maven2/org/slf4j/slf4j-simple/1.7.22/slf4j-simple-1.7.22.jar

amqp-client-4.1.1.jar:
	curl -sLO http://central.maven.org/maven2/com/rabbitmq/amqp-client/4.1.1/amqp-client-4.1.1.jar

clean:
	rm -f *.jar *.class
