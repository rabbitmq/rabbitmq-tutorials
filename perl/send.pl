#!/usr/bin/perl

use strict;
use warnings;

$|++;
use Net::RabbitFoot;

my $conn = Net::RabbitFoot->new()->load_xml_spec()->connect(
    host => 'localhost',
    port => 5672,
    user => 'guest',
    pass => 'guest',
    vhost => '/',
);


my $chan = $conn->open_channel();

$chan->publish(
    exchange => '',
    routing_key => 'hello',
    body => 'Hello World!',
);

print " [x] Sent 'Hello World!'\n";

$conn->close();

