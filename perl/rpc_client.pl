#!/usr/bin/perl

use strict;
use warnings;

$|++;
use AnyEvent;
use Net::RabbitFoot;
use UUID::Tiny;

sub fibonacci_rpc($) {
    my $n = shift;
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

    sub on_response_cb {
        my %a   = (
            condvar         => undef,
            correlation_id  => undef,
            @_
        );
        return  sub {
            my $var = shift;
            my $body = $var->{body}->{payload};
            if ($a{correlation_id} eq $var->{header}->{correlation_id}) {
                $a{condvar}->send($body);
            }
        };
    }

    $channel->consume(
        no_ack => 1,
        on_consume => on_response_cb(
            condvar         => $cv,
            correlation_id  => $corr_id,
        ),
    );

    $channel->publish(
        exchange => '',
        routing_key => 'rpc_queue',
        header => {
            reply_to => $callback_queue,
            correlation_id => $corr_id,
        },
        body => $n,
    );
    return $cv->recv;
}

print " [x] Requesting fib(30)\n";
my $response = fibonacci_rpc(30);
print " [.] Got $response\n";

print " [x] Requesting fib(32)\n";
$response = fibonacci_rpc(32);
print " [.] Got $response\n";
