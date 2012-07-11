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

my $routing_key = $ARGV[0] || 'anonymous.info';
my $msg = join(' ', @ARGV[1..$#ARGV]) || 'Hello World!';

$channel->publish(
    exchange => 'topic_logs',
    routing_key => $routing_key,
    body => $msg,
);

print " [x] Sent $routing_key:$msg\n";

$conn->close();

