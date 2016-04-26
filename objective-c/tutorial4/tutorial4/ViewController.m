#import "ViewController.h"
@import RMQClient;

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    [self receiveLogsDirect];
    sleep(1);
    [self emitLogDirect:@"Hello World!" severity:@"info"];
    [self emitLogDirect:@"Missile button pressed" severity:@"warning"];
    [self emitLogDirect:@"Launch mechanism jammed" severity:@"error"];
}

- (void)receiveLogsDirect {
    RMQConnection *conn = [[RMQConnection alloc] initWithDelegate:[RMQConnectionDelegateLogger new]];
    [conn start];

    id<RMQChannel> ch = [conn createChannel];
    RMQExchange *x    = [ch direct:@"direct_logs"];
    RMQQueue *q       = [ch queue:@"" options:RMQQueueDeclareExclusive];

    NSArray *severities = @[@"error", @"warning", @"info"];
    for (NSString *severity in severities) {
        [q bind:x routingKey:severity];
    }

    NSLog(@"Waiting for logs.");

    [q subscribe:^(RMQDeliveryInfo * _Nonnull deliveryInfo, RMQMessage * _Nonnull message) {
        NSLog(@"%@:%@", deliveryInfo.routingKey, message.content);
    }];
}

- (void)emitLogDirect:(NSString *)msg severity:(NSString *)severity {
    RMQConnection *conn = [[RMQConnection alloc] initWithDelegate:[RMQConnectionDelegateLogger new]];
    [conn start];

    id<RMQChannel> ch = [conn createChannel];
    RMQExchange *x    = [ch direct:@"direct_logs"];

    [x publish:msg routingKey:severity];
    NSLog(@"Sent '%@'", msg);

    [conn close];
}

@end
