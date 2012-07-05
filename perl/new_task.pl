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
    timeout => 1,
);


my $chan = $conn->open_channel();

$chan->declare_queue(
    queue => 'task_queue',
    durable => 1,
);

my $msg = join(' ', @ARGV) || "Hello World!";

$chan->publish(
    exchange => '',
    routing_key => 'task_queue',
    body => $msg,
);

print " [x] Sent '$msg'\n";

$conn->close();

