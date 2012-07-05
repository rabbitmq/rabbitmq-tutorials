#!/usr/bin/perl

use strict;
use warnings;

use Net::RabbitFoot;

my $conn = Net::RabbitFoot->new()->load_xml_spec()->connect(
    host => 'localhost',
    port => 5672,
    user => 'guest',
    pass => 'guest',
    vhost => '/',
);

my $ch = $conn->open_channel();

$ch->declare_queue(
    queue => 'task_queue',
    durable => 1,
);

print " [*] Waiting for messages. To exit press CTRL-C\n";

sub callback {
    my $var = shift;
    my $body = $var->{body}->{payload};
    print " [x] Received $body\n";

    my @c = $body =~ /\./g;
    sleep(scalar(@c));

    print " [x] Done\n";
    $ch->ack();
}

$ch->qos(prefetch_count => 1,);

$ch->consume(
    on_consume => \&callback,
    no_ack => 0,
);

# Wait forever
AnyEvent->condvar->recv;
