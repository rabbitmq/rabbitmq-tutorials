var amqp = require('amqp');

var connection = amqp.createConnection({host: 'localhost'});

connection.on('ready', function(){
    connection.exchange('logs', {type: 'fanout',
                                 autoDelete: false}, function(exchange){
        connection.queue('tmp-' + Math.random(), {exclusive: true},
                         function(queue){
            queue.bind('logs', '');
            console.log(' [*] Waiting for logs. To exit press CTRL+C')

            queue.subscribe(function(msg){
                console.log(" [x] %s", msg.data.toString('utf-8'));
            });
        })
    });
});
