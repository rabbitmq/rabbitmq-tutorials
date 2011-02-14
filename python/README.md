# Python code for RabbitMQ tutorials

Here you can find a Python code examples from [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

## Requirements

To run this code you need `pika` library version 0.5.2 (newer versions
may not work). To install it run

    pip install -e git+http://github.com/tonyg/pika.git@v0.5.2#egg=pika-v0.5.2

or

    easy_install pika==0.5.2


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

