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

$channel->declare_exchange(
    exchange => 'topic_logs',
    type => 'topic',
);

my $result = $channel->declare_queue(exclusive => 1);

my $queue_name = $result->{method_frame}->{queue};

my @binding_keys = @ARGV or die "Usage: $0 [binding_key]...\n";

for my $key (@binding_keys) {
    $channel->bind_queue(
        exchange => 'topic_logs',
        queue => $queue_name,
        routing_key => $key,
    );
}

print " [*] Waiting for logs. To exit press CTRL-C\n";

sub callback {
    my $var = shift;
    my $body = $var->{body}->{payload};
    my $routing_key = $var->{deliver}->{method_frame}->{routing_key};
    print " [x] $routing_key:$body\n";
}

$channel->consume(
    on_consume => \&callback,
    no_ack => 1,
);

# Wait forever
AnyEvent->condvar->recv;

