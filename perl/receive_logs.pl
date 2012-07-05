#!/usr/bin/perl

use strict;
use warnings;

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

$channel->declare_exchange(
    exchange => 'logs',
    type => 'fanout',
);

my $result = $channel->declare_queue( exclusive => 1, );

my $queue_name = $result->{method_frame}->{queue};

$channel->bind_queue(
    exchange => 'logs',
    queue => $queue_name,
);

print " [*] Waiting for logs. To exit press CTRL-C\n";

sub callback {
    my $var = shift;
    my $body = $var->{body}->{payload};

    print " [x] $body\n";
}

$channel->consume(
    on_consume => \&callback,
    queue => $queue_name,
    no_ack => 1,
);

AnyEvent->condvar->recv;
