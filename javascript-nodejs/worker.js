var amqp       = require('amqp');

var connection = amqp.createConnection({host: 'localhost'});

connection.on('ready', function(){
    connection.queue('task_queue', {autoDelete: false,
                                    durable: true}, function(queue){

        console.log(' [*] Waiting for messages. To exit press CTRL+C');

        queue.subscribe({ack: true, prefetchCount: 1}, function(msg){
            var body = msg.data.toString('utf-8');
            console.log(" [x] Received %s", body);
            setTimeout(function(){
                console.log(" [x] Done");
                queue.shift(); // basic_ack equivalent
            }, (body.split('.').length - 1) * 1000);
        });
    });
});
