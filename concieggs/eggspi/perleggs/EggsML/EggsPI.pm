#!/usr/bin/env perl
package EggsML::EggsPI;
use 5.020;
use warnings;

use Exporter;
use IPC::System::Simple qw/systemx capturex EXIT_ANY/;
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

sub import {
    my ($package, %args) = @_;
    my ($caller) = caller;

    _install_sub_handler($args{boolean}, $caller, sub {
        my $method = shift;
        my $exit_code = systemx(EXIT_ANY, $method, @_);
        return ! $exit_code;
    } );

    _install_sub_handler($args{list}, $caller, sub {
        my $method = shift;
        chomp(my @lines = capturex(EXIT_ANY, $method, @_));
        return @lines;
    } );

    _install_sub_handler($args{text}, $caller, sub {
        my $method = shift;
        chomp(my $text = capturex(EXIT_ANY, $method, @_));
        return $text;
    } );

    _install_sub_handler($args{raw}, $caller, sub {
        my $method = shift;
        my $text = capturex(EXIT_ANY, $method, @_);
        return $text;
    } );
}



1;

