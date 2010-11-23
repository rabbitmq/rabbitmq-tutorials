# RabbitMQ - PHP Examples #

## Requirements ##

To run the examples you need:

- A running RabbitMQ server
- PHP 5.3
- php-amqplib

## Setup ##

To get the php-amqplib library execute the following command inside the present folder:

		$ git clone http://github.com/tnc/php-amqplib.git lib/php-amqplib
		
## Running the Examples ##

To run the examples do in one Terminal:

		$ php send.php some message to send

Then in another Terminal try:

		$ php receive.php