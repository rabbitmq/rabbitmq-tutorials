#!/usr/bin/perl

use strict;
use warnings;

$|++;
use AnyEvent;
use Net::RabbitFoot;

my $conn = Net::RabbitFoot->new()->load_xml_spec()->connect(
    host => 'localhost',
    port => 5672,
    user => 'guest',
    pass => 'guest',
    vhost => '/',
);

my $channel = $conn->open_channel();

$channel->declare_queue(queue => 'rpc_queue');

sub fib {
    my $n = shift;
    my ($a, $b) = (0, 1);
    for (1..$n) {
        ($a, $b) = ($b, $a + $b);
    }
    return $a;
}

sub on_request {
    my $var = shift;
    my $body = $var->{body}->{payload};
    my $props = $var->{header};

    my $n = $body;
    print " [.] fib($n)\n";
    my $response = fib($n);

    $channel->publish(
        exchange => '',
        routing_key => $props->{reply_to},
        header => {
            correlation_id => $props->{correlation_id},
        },
        body => $response,
    );

    $channel->ack();
}

$channel->qos(prefetch_count => 1);
$channel->consume(
    on_consume => \&on_request,
);

print " [x] Awaiting RPC requests\n";

# Wait forever
AnyEvent->condvar->recv;
