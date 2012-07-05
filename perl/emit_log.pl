#!/usr/bin/perl

use strict;
use warnings;

use Net::RabbitFoot;

use Data::Dumper;

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

my $msg = join(' ', @ARGV) || "info: Hello World!";

$channel->publish(
    exchange => 'logs',
    routing_key => '',
    body => $msg,
);

print " [x] Sent $msg\n";

$conn->close();
