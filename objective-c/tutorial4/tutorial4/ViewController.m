#import "ViewController.h"
@import RMQClient;

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    [self receiveLogsDirect];
    sleep(2);
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

    [q subscribe:^(RMQMessage * _Nonnull message) {
        NSLog(@"%@:%@", message.routingKey, [[NSString alloc] initWithData:message.body encoding:NSUTF8StringEncoding]);
    }];
}

- (void)emitLogDirect:(NSString *)msg severity:(NSString *)severity {
    RMQConnection *conn = [[RMQConnection alloc] initWithDelegate:[RMQConnectionDelegateLogger new]];
    [conn start];

    id<RMQChannel> ch = [conn createChannel];
    RMQExchange *x    = [ch direct:@"direct_logs"];

    [x publish:[msg dataUsingEncoding:NSUTF8StringEncoding] routingKey:severity];
    NSLog(@"Sent '%@'", msg);

    [conn close];
}

@end
