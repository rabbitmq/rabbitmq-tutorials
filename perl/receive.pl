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

my $ch = $conn->open_channel();

$ch->declare_queue(queue => 'hello');

print " [*] Waiting for messages. To exit press CTRL-C\n";

sub callback {
    my $var = shift;
    my $body = $var->{body}->{payload};
    print " [x] Received $body\n";
}

$ch->consume(
    on_consume => \&callback,
    no_ack => 1,
);

# Wait forever
AnyEvent->condvar->recv;

