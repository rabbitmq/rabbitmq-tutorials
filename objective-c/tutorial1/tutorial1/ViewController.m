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
    RMQConnection *conn = [RMQConnection new];
    [conn start];

    NSError *error = NULL;
    id<RMQChannel> ch = [conn createChannelWithError:&error];
    if (error) {
        NSLog(@"Error creating channel: %@", error);
        [conn close];
        exit(1);
    } else {
        RMQQueue *q = [ch queue:@"hello"];
        [q publish:@"Hello World!"];
        NSLog(@"Sent 'Hello World!'");
        [conn close];
    }
}

- (void)receive {
    RMQConnection *conn = [RMQConnection new];
    [conn start];

    NSError *error = NULL;
    id<RMQChannel> ch = [conn createChannelWithError:&error];
    if (error) {
        NSLog(@"Error creating channel: %@", error);
        [conn close];
        exit(1);
    } else {
        RMQQueue *q = [ch queue:@"hello"];
        NSLog(@"Waiting for messages.");
        [q subscribe:^(id<RMQMessage> _Nonnull m) {
            NSLog(@"Received %@", m.content);
            [conn close];
            exit(0);
        }];
    }
}

@end
