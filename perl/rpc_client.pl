#!/usr/bin/perl

use strict;
use warnings;

use Net::RabbitFoot;
use AnyEvent;
use UUID::Tiny;

use Data::Dumper;

my $cv = AnyEvent->condvar;
my $corr_id = UUID::Tiny::create_UUID_as_string(UUID::Tiny::UUID_V4);

my $conn = Net::RabbitFoot->new()->load_xml_spec()->connect(
    host => 'localhost',
    port => 5672,
    user => 'guest',
    pass => 'guest',
    vhost => '/',
);

my $channel = $conn->open_channel();

my $result = $channel->declare_queue(exclusive => 1);
my $callback_queue = $result->{method_frame}->{queue};

sub on_response {
    my $var = shift;
    my $body = $var->{body}->{payload};
    if ($corr_id eq $var->{header}->{correlation_id}) {
        $cv->send($body);
    }
}

$channel->consume(
    no_ack => 1,
    on_consume => \&on_response,
);

$channel->publish(
    exchange => '',
    routing_key => 'rpc_queue',
    header => {
        reply_to => $callback_queue,
        correlation_id => $corr_id,
    },
    body => 30,
);

print " [x] Requesting fib(30)\n";
my $response = $cv->recv;
print " [.] Got $response\n";

