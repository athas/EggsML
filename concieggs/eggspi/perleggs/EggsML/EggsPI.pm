#!/usr/bin/env perl
package EggsML::EggsPI;
use 5.020;
use warnings;

use Exporter;
use IPC::Run;
use Sub::Install;

sub _install_sub_handler {
    my ($type, $caller, $code) = @_;

    for my $method (@{ $type // [] }) {
        Sub::Install::install_sub({
            code => sub { $code->($method, @_) },
            into => $caller,
            as   => $method,
        });
    }
}

sub _run {
    my $method = shift;
    my $args = ref $_[0] eq 'HASH' ? shift : {};
    $args->{stdin} = join("\n", @{$args->{stdin}}) . "\n" if ref $args->{stdin} eq 'ARRAY';

    my ($stdin, $stdout, $stderr);

    my $h = IPC::Run::start [$method, @_], \$stdin, \$stdout, \$stderr;
    $stdin .= $args->{stdin} if $args->{stdin};
    my $ret_code = IPC::Run::finish($h);

    print STDERR $stderr if $stderr;

    return { stdout => $stdout, return_code => $ret_code };
}

sub import {
    my ($package, %args) = @_;
    my ($caller) = caller;

    _install_sub_handler($args{boolean}, $caller, sub {
        my $res = _run(@_);
        return ! $res->{return_code};
    } );

    _install_sub_handler($args{list}, $caller, sub {
        my $res = _run(@_);
        chomp(my @lines = split(/\n/, $res->{stdout}));
        return @lines;
    } );

    _install_sub_handler($args{text}, $caller, sub {
        my $res = _run(@_);
        chomp(my $text = $res->{stdout});
        return $text;
    } );

    _install_sub_handler($args{raw}, $caller, sub {
        my $res = _run(@_);
        my $text = $res->{stdout};
        return $text;
    } );
}



1;

