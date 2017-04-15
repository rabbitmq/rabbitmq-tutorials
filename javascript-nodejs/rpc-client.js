// requries
var amqp = require('amqp');
var amqp_hacks = require('./amqp-hacks');

// util
var generateId = function(){
    // ideally you could use a UUID here, e.g., with the node-uuid library
    return Math.floor((1 + Math.random()) * 0x10000).toString(16);
};

var connection = amqp.createConnection({ host: 'localhost' });

// generate unique queue name (only do it once)
var queueName = 'rpc-queue' + generateId();

// 1. setup connection
connection.on('ready', function connectionReady(){
    // 2. setup a queue with a unique name
    // Create an exclusive callback queue. This queue is created once per client
    connection.queue(queueName, {exclusive: true}, function setupQueue(queue){
        
        // 3. subscribe to the created queue
        //  After the server receives, processes, and
        //  publishes a response, this callback will be called
        queue.subscribe(function finishedRPC(msg){
            // 6. Message has been received from the RPC server
            console.log('Message from RPC server: ', msg);
            console.log('To string:', 
                msg.data.toString('utf-8'));

            // 7. All done
            amqp_hacks.safeEndConnection(connection);
        });

        // 4.  After the connection and queue have been set up, we can
        // make RPC requests to the RPC server.
        readyToCall();
    });
});

var readyToCall = function(){
    // Send a message *after* the connection and queue above have been set up
    var message = ((Math.random() * 10) | 0) + '';

    // 5.  Send request to RPC server
    connection.publish('rpc-queue', message, {
        // use the queue name created above to identify this queue
        replyTo: queueName,
        // generate an ID on each RPC request
        correlationId: generateId(),
        contentType: 'application/json',
        contentEncoding: 'utf-8'
    });
};
