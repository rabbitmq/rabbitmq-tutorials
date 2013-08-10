// requries
var amqp = require('amqp');
var amqp_hacks = require('./amqp-hacks');
 
var connection = amqp.createConnection({ host: 'localhost' });

var fib = function(n){
    // same implementation from python example
    if(n===0){ return 0; }
    else if(n===1){ return 1; }
    else { return fib(n-1) + fib(n-2); }
};
 
// 1. connect
connection.on('ready', function connectionReady(){
    // 2 .create queue 
    connection.queue('rpc-queue', {prefetchCount: 1}, function(queue){
        console.log('RPC Server started up, using the `rpc-queue` queue');

        // 3. subscribe to the queue, wait for messages
        queue.subscribe({prefetchCount: 1},function(message, headers, deliveryInfo, options){
            // 4. Message received from a RPC client, do stuff
            var response = fib(message);
            console.log('Received message: ', message);
            console.log('Sending response: ', response);
        
            // 5. Publish it to the `replyTo` queue that was passed in, along
            //  with passing the the correlationId
            connection.publish(
                options.replyTo,  // make sure to send it to the right queue
                response + '',  // send response as a string
                { 
                    correlationId:options.correlationId
                });
        });
    });
});
