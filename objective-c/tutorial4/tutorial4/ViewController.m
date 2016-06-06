#import "ViewController.h"
@import RMQClient;

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    RMQConnection *conn = [[RMQConnection alloc] initWithDelegate:[RMQConnectionDelegateLogger new]];
    [conn start];

    [self receiveLogsDirect:conn];
    sleep(2);
    [self emitLogDirect:conn message:@"Hello World!" severity:@"info"];
    [self emitLogDirect:conn message:@"Missile button pressed" severity:@"warning"];
    [self emitLogDirect:conn message:@"Launch mechanism jammed" severity:@"error"];
}

- (void)receiveLogsDirect:(RMQConnection *)conn {
    id<RMQChannel> ch = [conn createChannel];
    RMQExchange *x    = [ch direct:@"direct_logs"];
    RMQQueue *q       = [ch queue:@"" options:RMQQueueDeclareExclusive];

    NSArray *severities = @[@"error", @"warning", @"info"];
    for (NSString *severity in severities) {
        [q bind:x routingKey:severity];
    }

    NSLog(@"Waiting for logs.");

    [q subscribe:^(RMQMessage * _Nonnull message) {
        NSLog(@"%@:%@", message.routingKey, message.content);
    }];
}

- (void)emitLogDirect:(RMQConnection *)conn message:(NSString *)msg severity:(NSString *)severity {
    id<RMQChannel> ch = [conn createChannel];
    RMQExchange *x    = [ch direct:@"direct_logs"];

    [x publish:msg routingKey:severity];
    NSLog(@"Sent '%@'", msg);
}

@end
