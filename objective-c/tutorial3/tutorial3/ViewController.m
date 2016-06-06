#import "ViewController.h"
@import RMQClient;

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    [self receiveLogs];
    [self receiveLogs];
    sleep(1);
    [self emitLog];
}

- (void)emitLog {
    RMQConnection *conn = [[RMQConnection alloc] initWithDelegate:[RMQConnectionDelegateLogger new]];
    [conn start];

    id<RMQChannel> ch = [conn createChannel];
    RMQExchange *x = [ch fanout:@"logs"];

    NSString *msg = @"Hello World!";

    [x publish:msg];
    NSLog(@"Sent %@", msg);

    [conn close];
}

- (void)receiveLogs {
    RMQConnection *conn = [[RMQConnection alloc] initWithDelegate:[RMQConnectionDelegateLogger new]];
    [conn start];

    id<RMQChannel> ch = [conn createChannel];
    RMQExchange *x = [ch fanout:@"logs"];
    RMQQueue *q = [ch queue:@"" options:RMQQueueDeclareExclusive];

    [q bind:x];

    NSLog(@"Waiting for logs.");

    [q subscribe:^(RMQMessage * _Nonnull message) {
        NSLog(@"Received %@", message);
    }];
}

@end
