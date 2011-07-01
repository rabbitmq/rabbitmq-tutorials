# Ignore this file, go straight to the directory with your language of
# choice and read the readme there.
#
# This makefile is for testing only. It's intended to install
# dependencies for all source code languages, which is an overkill for
# the great majority of users.
#

all:
	@echo "Review README in the directory with your langage of choice."


# Test all combinations of languages
test: dotnet/.ok erlang/.ok java/.ok python/.ok php/.ok ruby/.ok
	python test.py

R=http://www.rabbitmq.com/releases
dotnet/.ok:
	(cd dotnet && \
		mkdir lib && \
		cd lib && \
		wget $(R)/rabbitmq-dotnet-client/v2.4.1/rabbitmq-dotnet-client-2.4.1-dotnet-3.0.zip && \
		unzip rabbitmq-dotnet-client-2.4.1-dotnet-3.0.zip && \
		for f in *.cs; do \
			gmcs -r:lib/bin/RabbitMQ.Client.dll $$f; \
		done && \
		touch .ok)


erlang/.ok:
	(cd erlang && \
		wget $(R)/plugins/v2.5.0/rabbit_common-2.5.0.ez && \
		unzip rabbit_common-2.5.0.ez && \
		ln -s rabbit_common-2.5.0 rabbit_common && \
		wget $(R)/releases/plugins/v2.5.0/amqp_client-2.5.0.ez && \
		unzip amqp_client-2.5.0.ez && \
		ln -s amqp_client-2.5.0 amqp_client && \
		touch .ok)

java/.ok:
	(cd java && \
		wget $(R)/rabbitmq-java-client/v2.4.1/rabbitmq-java-client-bin-2.4.1.zip && \
		unzip rabbitmq-java-client-bin-2.4.1.zip && \
		cp rabbitmq-java-client-bin-2.4.1/*.jar . && \
		javac -cp rabbitmq-client.jar *.java && \
		touch .ok)

python/.ok:
	(cd python && \
		virtualenv venv && \
		./venv/bin/pip install pika==0.9.5 && \
		touch .ok)

php/.ok:
	(cd php && \
		git clone http://github.com/tnc/php-amqplib.git lib/php-amqplib && \
		touch .ok)

ruby/.ok:
	(cd ruby && \
		GEM_HOME=gems/gems RUBYLIB=gems/lib gem1.8 install amqp --pre --version "= 0.8.0.rc12" && \
		touch .ok)
