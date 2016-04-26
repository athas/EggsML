#!/usr/bin/env perl
package EggsML::Daemon::Client;
use 5.012;
use warnings;

use Env qw/EGGS_DAEMON_SOCKET/;
use IO::Socket::UNIX;
use utf8::all;

sub _run {
    my ($self, @command) = @_;

    my $sock = IO::Socket::UNIX->new(Type => SOCK_STREAM, Peer => $EGGS_DAEMON_SOCKET);
    binmode $sock => ':encoding(utf-8)';
    { local $, = ' '; $sock->print("@command\n"); }

    my $resCode = <$sock>;
    die("Failed to communicate with eggsmld") unless defined $resCode;
    my $res = do { local $/; <$sock> };

    return (int($resCode), $res);
}

sub _run_list {
    my $self = shift;
    my ($resCode, $res) = $self->_run(@_);
    return ($resCode, [ split (/\n/, $res // '') ]);
}

sub _run_nested_list {
    my $self = shift;
    my ($resCode, $res) = $self->_run(@_);
    my @parts = split (/\n\n/, $res // '');
    return ($resCode, [ map { [ split (/\n/, $_) ] } @parts ]);
}

sub _run_hash {
    my $self = shift;
    my ($resCode, $res) = $self->_run_list(@_);
    return ($resCode, { map { split (/ /, $_) } @$res });
}

sub _run_bool {
    my $self = shift;
    my ($resCode) = $self->_run(@_);
    return $resCode ? 0 : 1;
}

sub aliases {
    my ($self, @names) = @_;
    my ($resCode, $res) = $self->_run_nested_list( 'aliases', @names );
    return if $resCode;
    return $res;
}

sub wishes {
    my $self = shift;
    my ($resCode, $res) = $self->_run_list( 'wishes' );
    return if $resCode;
    return $res;
}

sub balances {
    my $self = shift;
    my ($resCode, $res) = $self->_run_hash( 'balances' );
    return if $resCode;
    return { reverse %$res };
}

sub balance_of_payments {
    my $self = shift;
    my ($resCode, $res) = $self->_run( 'balance_of_payments' );
    return if $resCode;
    return $res;
}

sub allaliases {
    my $self = shift;
    my ($resCode, $res) = $self->_run_nested_list( 'allaliases' );
    return if $resCode;
    return $res;
}

sub lunches {
    my ($self, $name) = @_;
    my ($resCode, $res) = $self->_run_list( 'lunches', $name );
    return if $resCode;
    return $res;
}

sub eggsmates {
    my ($self, $name) = @_;
    my ($resCode, $res) = $self->_run( 'eggsmates', $name );
    return if $resCode;
    return $res;
}

sub eggscount {
    my ($self, $name) = @_;
    my ($resCode, $res) = $self->_run( 'eggscount', $name );
    return if $resCode;
    return $res;
}

sub consecutive {
    my ($self, $name) = @_;
    my ($resCode, $res) = $self->_run( 'consecutive', $name );
    return if $resCode;
    return $res;
}

sub cmpnames {
    my ($self, $name1, $name2) = @_;
    return $self->_run_bool( 'cmpnames', $name1, $name2 );
}

sub isTrusted {
    my ($self, $name) = @_;
    return $self->_run_bool( 'isTrusted', $name );
}

1;

