var amqp       = require('amqp');
var amqp_hacks = require('./amqp-hacks');

var connection = amqp.createConnection({host: 'localhost'});

var message = process.argv.slice(2).join(' ') || 'Hello World!';

connection.on('ready', function(){
    connection.exchange('logs', {type: 'fanout',
                                 autoDelete: false}, function(exchange){
        exchange.publish('', message);
        console.log(" [x] Sent %s", message);

        amqp_hacks.safeEndConnection(connection);
    });
});
