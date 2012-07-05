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

my $channel = $conn->open_channel();

$channel->declare_exchange(
    exchange => 'direct_logs',
    type => 'direct',
);

my $severity = delete $ARGV[0] || 'info';
my $msg = join(' ', @ARGV[1..$#ARGV]) || 'Hello World!';

$channel->publish(
    exchange => 'direct_logs',
    routing_key => $severity,
    body => $msg,
);

print " [x] Send $severity: $msg\n";

$conn->close();
