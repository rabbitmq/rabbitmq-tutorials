var amqp       = require('amqp');
var amqp_hacks = require('./amqp-hacks');

var connection = amqp.createConnection({ host: 'localhost' });

connection.on('ready', function(){
    connection.publish('hello', 'Hello World!');
    console.log(" [x] Sent 'Hello World!'");

    amqp_hacks.safeEndConnection(connection);
});
