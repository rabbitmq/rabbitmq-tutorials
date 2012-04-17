var amqp       = require('amqp');
var amqp_hacks = require('./amqp-hacks');

var connection = amqp.createConnection({host: 'localhost'});

var message = process.argv.slice(2).join(' ') || 'Hello World!';

connection.on('ready', function(){
    connection.queue('task_queue', {autoDelete: false,
                                    durable: true}, function(queue){
        connection.publish('task_queue', message, {deliveryMode: 2});
        console.log(" [x] Sent %s", message);

        amqp_hacks.safeEndConnection(connection);
    });
});
