var amqp = require('amqp');

var connection = amqp.createConnection({host: 'localhost'});

connection.on('ready', function(){
    connection.queue('hello', {autoDelete: false}, function(queue){

        console.log(' [*] Waiting for messages. To exit press CTRL+C')

        queue.subscribe(function(msg){
            console.log(" [x] Received %s", msg.data.toString('utf-8'));
        });
    });
});
