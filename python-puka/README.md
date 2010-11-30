# Puka code for RabbitMQ tutorials

Here you can find code examples from
[RabbitMQ tutorials](http://www.rabbitmq.com/getstarted.html) adapted
to [Puka](https://github.com/majek/puka) Python library.

## Requirements

First, you need Python Pip.

  * On Ubuntu:

    $ sudo apt-get install python-pip git-core

 * On Debian:

    $ sudo apt-get install python-setuptools git-core
    $ sudo easy_install pip


Now you can install Puka system-wide using Pip:

    $ sudo pip install -e git+http://github.com/majek/puka.git#egg=puka


## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    python send.py
    python receive.py


[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html):

    python new_task.py
    python worker.py



