# Ignore this file, go straight to the directory with your language of
# choice and read the readme there.
#
# This makefile is for testing only. It's intended to install
# dependencies for all source code languages, which is an overkill for
# the great majority of users.
#

all:
	@echo "Review README in the directory with your langage of choice."

### Test all combinations of languages
#
#
# Running everything requires quite a lot of dependencies you need at
# least (as tested on debian 5.0):
#
#     apt-get install python-virtualenv git-core php5-cli \
#         ruby1.8 ruby1.8-dev rdoc1.8 unzip mono-gmcs sun-java5-jdk
#
#
# You also need recent erlang, you may install it from sources following
# this commands:
#     cd /usr/src
#     apt-get -y install libncurses-dev libssl-dev
#     [ -e otp_src_R14B03.tar.gz ] || wget http://www.erlang.org/download/otp_src_R14B03.tar.gz
#     [ -e otp_src_R14B03 ] || tar xzf otp_src_R14B03.tar.gz
#     cd otp_src_R14B03/
#     ./configure
#     make
#     make install
#
test: dotnet/.ok erlang/.ok java/.ok python/.ok php/.ok ruby/.ok
	RUBYVER=$(RUBYVER) python test.py

R=http://www.rabbitmq.com/releases
dotnet/.ok:
	(cd dotnet && \
		mkdir lib && \
		cd lib && \
		wget -c $(R)/rabbitmq-dotnet-client/v2.4.1/rabbitmq-dotnet-client-2.4.1-dotnet-3.0.zip && \
		unzip -q rabbitmq-dotnet-client-2.4.1-dotnet-3.0.zip && \
		cd .. && \
		for f in *.cs; do \
			gmcs -r:lib/bin/RabbitMQ.Client.dll $$f; \
		done && \
		touch .ok)
clean::
	(cd dotnet && \
		rm -rf .ok *.zip lib *.exe)

erlang/.ok:
	(cd erlang && \
		wget -c $(R)/plugins/v2.5.0/rabbit_common-2.5.0.ez \
			$(R)/plugins/v2.5.0/amqp_client-2.5.0.ez && \
		unzip -q rabbit_common-2.5.0.ez && \
		ln -s rabbit_common-2.5.0 rabbit_common && \
		unzip -q amqp_client-2.5.0.ez && \
		ln -s amqp_client-2.5.0 amqp_client && \
		touch .ok)
clean::
	(cd erlang && \
		rm -rf .ok *.ez amqp_client* rabbit_common*)

java/.ok:
	(cd java && \
		wget -c $(R)/rabbitmq-java-client/v2.4.1/rabbitmq-java-client-bin-2.4.1.zip && \
		unzip -q rabbitmq-java-client-bin-2.4.1.zip && \
		cp rabbitmq-java-client-bin-2.4.1/*.jar . && \
		javac -cp rabbitmq-client.jar *.java && \
		touch .ok)
clean::
	(cd java && \
		rm -rf .ok *.jar *.class *.zip rabbitmq-java-client-bin*)

python/.ok:
	(cd python && \
		virtualenv venv && \
		./venv/bin/easy_install pip && \
		./venv/bin/pip install pika==0.9.5 && \
		touch .ok)
clean::
	(cd python && \
		rm -rf .ok venv distribute*.tar.gz)

php/.ok:
	(cd php && \
		git clone http://github.com/tnc/php-amqplib.git lib/php-amqplib && \
		touch .ok)
clean::
	(cd php && \
		rm -rf .ok lib)

RUBYVER:=1.9
GEMSVER=1.8.5
TOPDIR:=$(PWD)
ruby/.ok:
	(cd ruby && \
		wget http://production.cf.rubygems.org/rubygems/rubygems-$(GEMSVER).tgz && \
		tar xzf rubygems-$(GEMSVER).tgz && \
		cd rubygems-$(GEMSVER) && \
		ruby$(RUBYVER) setup.rb --prefix=$(TOPDIR)/ruby/gems && \
		cd .. && \
		rm -r rubygems-$(GEMSVER).tgz rubygems-$(GEMSVER) && \
		GEM_HOME=gems/gems RUBYLIB=gems/lib gems/bin/gem$(RUBYVER) install amqp --version "0.8.0.rc13" && \
		touch .ok)
clean::
	(cd ruby && \
		rm -rf .ok gems)
