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
#         ruby1.9 ruby1.9-dev rdoc1.9 unzip mono-gmcs sun-java5-jdk \
#         cpan perl
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
setup: dotnet/.ok erlang/.ok java/.ok python/.ok php/.ok ruby-amqp/.ok ruby/.ok python-puka/.ok clojure/.ok

setup-travisci: dotnet/.ok erlang/.ok java/.ok python/.ok ruby/.ok php/.ok clojure/.ok

test: setup
	RUBY=$(RUBY) python test.py

test-travisci: setup-travisci
		SLOWNESS=4 RUBY=ruby python travisci.py

RABBITVER:=$(shell curl -s "http://www.rabbitmq.com/releases/rabbitmq-server/" | grep -oE '([0-9\.]{5,})' | tail -n 1)
R=http://www.rabbitmq.com/releases

# Default value assumes CI environment
RUBY?=ruby1.9.1

DVER=$(RABBITVER)
dotnet/.ok:
	(cd dotnet && \
		mkdir -p lib && \
		cd lib && \
		wget -qc $(R)/rabbitmq-dotnet-client/v$(DVER)/rabbitmq-dotnet-client-$(DVER)-dotnet-4.0.zip && \
		unzip -q rabbitmq-dotnet-client-$(DVER)-dotnet-4.0.zip && \
		cd .. && \
		for f in *.cs; do \
			gmcs -r:lib/bin/RabbitMQ.Client.dll $$f; \
		done && \
		touch .ok)
clean::
	(cd dotnet && \
		rm -rf .ok *.zip lib *.exe)

EVER=$(RABBITVER)
erlang/.ok:
	(cd erlang && \
		wget -qc $(R)/rabbitmq-erlang-client/v$(EVER)/rabbit_common-$(EVER).ez \
			$(R)/rabbitmq-erlang-client/v$(EVER)/amqp_client-$(EVER).ez && \
		unzip -q rabbit_common-$(EVER).ez && \
		ln -s rabbit_common-$(EVER) rabbit_common && \
		unzip -q amqp_client-$(EVER).ez && \
		ln -s amqp_client-$(EVER) amqp_client && \
		touch .ok)
clean::
	(cd erlang && \
		rm -rf .ok *.ez amqp_client* rabbit_common*)

JVER=$(RABBITVER)
java/.ok:
	(cd java && \
		wget -qc $(R)/rabbitmq-java-client/v$(JVER)/rabbitmq-java-client-bin-$(JVER).zip && \
		unzip -q rabbitmq-java-client-bin-$(JVER).zip && \
		cp rabbitmq-java-client-bin-$(JVER)/*.jar . && \
		javac -cp rabbitmq-client.jar *.java && \
		touch .ok)
clean::
	(cd java && \
		rm -rf .ok *.jar *.class *.zip rabbitmq-java-client-bin*)

PVER=0.9.5
python/.ok:
	(cd python && \
		virtualenv venv && \
		./venv/bin/easy_install pip && \
		./venv/bin/pip install pika==$(PVER) && \
		touch .ok)
clean::
	(cd python && \
		rm -rf .ok venv distribute*.tar.gz)

php/.ok:
	(cd php && \
		mkdir -p ./bin && \
		curl -sS https://getcomposer.org/installer | php -- --install-dir=bin && \
		php ./bin/composer.phar install --prefer-source && \
		touch .ok)
clean::
	(cd php && \
		rm -rf .ok lib)

GEM?=gem1.9.1
TOPDIR:=$(PWD)
ruby/.ok:
	(cd ruby && \
		GEM_HOME=gems/gems RUBYLIB=gems/lib $(GEM) install bunny --version "~> 1.7.0" --no-ri --no-rdoc && \
		touch .ok)
clean::
	(cd ruby && \
		rm -rf .ok gems)

ruby-amqp/.ok:
	(cd ruby-amqp && \
		GEM_HOME=gems/gems RUBYLIB=gems/lib $(GEM) install amqp --no-ri --no-rdoc && \
		touch .ok)
clean::
	(cd ruby-amqp && \
		rm -rf .ok gems)

python-puka/.ok:
	(cd python-puka && \
		virtualenv venv && \
		./venv/bin/easy_install pip && \
		./venv/bin/pip install puka && \
		touch .ok)

perl/.ok:
	(cd perl && \
		PERL_MM_USE_DEFAULT=1 cpan -i -f Net::RabbitFoot && \
		PERL_MM_USE_DEFAULT=1 cpan -i -f UUID::Tiny && \
		touch .ok)

clean::
	(cd python-puka && \
		rm -rf .ok venv distribute*.tar.gz)

clojure/.ok:
	(cd clojure && \
		mkdir -p bin && cd bin && \
		wget -qc https://raw.github.com/technomancy/leiningen/stable/bin/lein && \
		chmod +x ./lein && cd .. && \
		./bin/lein deps && \
		touch .ok)
clean::
	(cd clojure && \
		rm -rf .ok bin/*)

go/.ok:
	(cd go && \
		go get https://github.com/streadway/amqp && \
		touch .ok)

clean::
	(cd go && \
		rm -rf .ok)
