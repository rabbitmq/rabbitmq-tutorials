# Python code for RabbitMQ tutorials

Here you can find a Python code examples from [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

## Dependencies

Before running the examples you need to install
[pika](https://github.com/tonyg/pika) library. In order to do so run,
on Ubuntu:

    sudo apt-get install python-pip git-core
    sudo pip install -e git+http://github.com/tonyg/pika.git#egg=pika

on Debian:

    sudo apt-get install python-setuptools git-core
    sudo easy_install pip
    sudo pip install -e git+http://github.com/tonyg/pika.git#egg=pika


## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    python send.py
    python receive.py


[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html):

    python new_task.py
    python worker.py


[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html)

    python receive_logs.py
    python emit_log.py

