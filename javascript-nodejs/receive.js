var amqp       = require('amqp');
var amqp_hacks = require('./amqp-hacks');

var connection = amqp.createConnection({ host: 'localhost' });

connection.on('ready', function(){
    connection.queue('hello', {autoDelete: false}, function(queue){
        queue.subscribe(function(msg){
            console.log(" [x] Received %s", msg.data.toString('utf-8'));
        });
    });
    console.log(' [*] Waiting for messages. To exit press CTRL+C')
});
