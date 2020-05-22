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

    [ch.defaultExchange publish:[@"Hello World!" dataUsingEncoding:NSUTF8StringEncoding] routingKey:q.name];
    NSLog(@"Sent 'Hello World!'");

    [NSThread sleepForTimeInterval:2.0f];

    [conn close];
}

- (void)receive {
    NSLog(@"Attempting to connect to local RabbitMQ broker");
    RMQConnection *conn = [[RMQConnection alloc] initWithDelegate:[RMQConnectionDelegateLogger new]];
    [conn start];

    id<RMQChannel> ch = [conn createChannel];

    RMQQueue *q = [ch queue:@"hello"];
    NSLog(@"Waiting for messages.");
    [q subscribe:^(RMQMessage * _Nonnull message) {
        NSLog(@"Received %@", [[NSString alloc] initWithData:message.body encoding:NSUTF8StringEncoding]);
    }];
}

@end
