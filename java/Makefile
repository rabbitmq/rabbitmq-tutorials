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

run_tutorial_two_worker: tutorial_two
	$(JAVA_RUN) Worker

run_tutorial_two_task: tutorial_two
	$(JAVA_RUN) NewTask

# http://www.rabbitmq.com/tutorials/tutorial-three-java.html
# Tutorial Three: Emit / Receive Logs
tutorial_three: jars
	$(JAVAC) -cp amqp-client-4.1.1.jar EmitLog.java ReceiveLogs.java

run_tutorial_three_receive: tutorial_three
	$(JAVA_RUN) ReceiveLogs

run_tutorial_three_emit: tutorial_three
	$(JAVA_RUN) EmitLog

# http://www.rabbitmq.com/tutorials/tutorial-four-java.html
# Tutorial Four: Routing
tutorial_four: jars
	$(JAVAC) -cp amqp-client-4.1.1.jar EmitLogDirect.java ReceiveLogsDirect.java

run_tutorial_four_receive: tutorial_four
	$(JAVA_RUN) ReceiveLogsDirect info warning error

run_tutorial_four_emit: tutorial_four
	$(JAVA_RUN) EmitLogDirect error "Run. Run. Or it will explode."

# http://www.rabbitmq.com/tutorials/tutorial-five-java.html
# Tutorial Five: Topics
tutorial_five: jars
	$(JAVAC) -cp amqp-client-4.1.1.jar EmitLogTopic.java ReceiveLogsTopic.java

run_tutorial_five_receive: tutorial_five
	$(JAVA_RUN) ReceiveLogsTopic 'kern.*'

run_tutorial_five_emit: tutorial_five
	$(JAVA_RUN) EmitLogTopic 'kern.critical' 'A critical kernel error'

# http://www.rabbitmq.com/tutorials/tutorial-six-java.html
# Tutorial Six: RPC
tutorial_six: jars
	$(JAVAC) -cp amqp-client-4.1.1.jar RPCClient.java RPCServer.java

run_tutorial_six_client: tutorial_six
	$(JAVA_RUN) RPCClient

run_tutorial_six_server: tutorial_six
	$(JAVA_RUN) RPCServer

jars: amqp-client-4.1.1.jar slf4j-api-1.7.21.jar slf4j-simple-1.7.22.jar

slf4j-api-1.7.21.jar:
	curl -sLO http://central.maven.org/maven2/org/slf4j/slf4j-api/1.7.21/slf4j-api-1.7.21.jar

slf4j-simple-1.7.22.jar:
	curl -sLO http://central.maven.org/maven2/org/slf4j/slf4j-simple/1.7.22/slf4j-simple-1.7.22.jar

amqp-client-4.1.1.jar:
	curl -sLO http://central.maven.org/maven2/com/rabbitmq/amqp-client/4.1.1/amqp-client-4.1.1.jar

clean:
	rm -f *.jar *.class
