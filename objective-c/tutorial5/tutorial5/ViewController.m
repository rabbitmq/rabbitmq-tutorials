#import "ViewController.h"
@import RMQClient;

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    RMQConnection *conn = [[RMQConnection alloc] initWithDelegate:[RMQConnectionDelegateLogger new]];
    [conn start];

    [self receiveLogsTopic:conn routingKeys:@[@"kern.*", @"*.critical"]];
    sleep(2);
    [self emitLogTopic:conn message:@"Hello World!" routingKey:@"kern.info"];
    [self emitLogTopic:conn message:@"A critical kernel error" routingKey:@"kern.critical"];
    [self emitLogTopic:conn message:@"Critical module error" routingKey:@"somemod.critical"];
    [self emitLogTopic:conn message:@"Just some module info. You won't get this." routingKey:@"somemod.info"];
}

- (void)receiveLogsTopic:(RMQConnection *)conn routingKeys:(NSArray *)routingKeys {
    id<RMQChannel> ch = [conn createChannel];
    RMQExchange *x    = [ch topic:@"topic_logs"];
    RMQQueue *q       = [ch queue:@"" options:RMQQueueDeclareExclusive];

    for (NSString *routingKey in routingKeys) {
        [q bind:x routingKey:routingKey];
    }

    NSLog(@"Waiting for logs.");

    [q subscribe:^(RMQMessage * _Nonnull message) {
        NSLog(@"%@:%@", message.routingKey, message.content);
    }];
}

- (void)emitLogTopic:(RMQConnection *)conn message:(NSString *)msg routingKey:(NSString *)routingKey {
    id<RMQChannel> ch = [conn createChannel];
    RMQExchange *x    = [ch topic:@"topic_logs"];

    [x publish:msg routingKey:routingKey];
    NSLog(@"Sent '%@'", msg);
}

@end
