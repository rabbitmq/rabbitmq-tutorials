#import "ViewController.h"
@import RMQClient;

@interface ViewController ()
@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    [self send];
    [self receive];
}

- (void)send {
    NSLog(@"Attempting to connect to local RabbitMQ broker");
    RMQConnection *conn = [[RMQConnection alloc] initWithDelegate:[RMQConnectionDelegateLogger new]];
    [conn start];

    id<RMQChannel> ch = [conn createChannel];

    RMQQueue *q = [ch queue:@"hello"];

    [ch.defaultExchange publish:@"Hello World!" routingKey:q.name];
    NSLog(@"Sent 'Hello World!'");

    [conn close];
}

- (void)receive {
    NSLog(@"Attempting to connect to local RabbitMQ broker");
    RMQConnection *conn = [[RMQConnection alloc] initWithDelegate:[RMQConnectionDelegateLogger new]];
    [conn start];

    id<RMQChannel> ch = [conn createChannel];

    RMQQueue *q = [ch queue:@"hello"];
    NSLog(@"Waiting for messages.");
    [q subscribe:^(id<RMQMessage>  _Nonnull message) {
        NSLog(@"Received %@", message.content);
    }];
}

@end
