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
    RMQConnection *conn = [RMQConnection new];
    NSError *error = NULL;
    [conn startWithError:&error];
    if (error) {
        NSLog(@"Couldn't connect to local RabbitMQ broker: %@", error);
        exit(1);
    }

    id<RMQChannel> ch = [conn createChannelWithError:&error];
    if (error) {
        NSLog(@"Error creating channel: %@", error);
        [conn close];
        exit(1);
    }

    RMQQueue *q = [ch queue:@"hello"];
    [q publish:@"Hello World!"];
    NSLog(@"Sent 'Hello World!'");
    [conn close];
}

- (void)receive {
    NSLog(@"Attempting to connect to local RabbitMQ broker");
    RMQConnection *conn = [RMQConnection new];
    NSError *error = NULL;
    [conn startWithError:&error];
    if (error) {
        NSLog(@"Couldn't connect to local RabbitMQ broker: %@", error);
        exit(1);
    }

    id<RMQChannel> ch = [conn createChannelWithError:&error];
    if (error) {
        NSLog(@"Error creating channel: %@", error);
        [conn close];
        exit(1);
    }

    RMQQueue *q = [ch queue:@"hello"];
    NSLog(@"Waiting for messages.");
    [q subscribeWithError:&error handler:^(id<RMQMessage>  _Nonnull message) {
        NSLog(@"Received %@", message.content);
    }];
    if (error) {
        NSLog(@"Error subscribing: %@", error);
        [conn close];
        exit(1);
    }
}

@end
