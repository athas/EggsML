#!/usr/bin/env perl
package EggsML::Lunchfile;
use 5.012;
use warnings;

use Mojo::Base -base;
use utf8::all;

use DateTime;
use EggsML::Member;

=head1 NAME

EggsML::Lunchfile - Parse and extract data from slashdotfrokost.

=head2 ATTRIBUTES

=head3 filename

The filename of the slashdotfrokost-file.

=head3 members

The data about the members. Hashref, with key being the primary alias of the user.

=head3 wishes

The contents of the wishlist.

=head3 purchases

All the purchases that have been done.

=head3 lunches

All the lunches that've been conducted.

=cut

has [qw(
    filename members wishes
    purchases lunches
    last_parsed_time members_reverse
)];

=head2 PUBLIC METHODS

=head3 parse

Parses the lunchfile if it has changed since C<parse> was last called.

Must be called before using anything else.

=cut

sub parse {
    my $self = shift;

    my $last_modified = (stat($self->filename))[9];

    return unless ($last_modified > ($self->last_parsed_time // -1));
    $self->last_parsed_time($last_modified);

    my $data;
    {
        open(my $f, '<', $self->filename);
        local $/; # enable slurp mode
        $data = <$f>;
        close($f);
    }

    my @sections = split( qr/^::(.*?)::$/m, $data );
    shift @sections; # We don't care about "NY OG FORBEDRET"

    while (1) {
        my ($head, $body) = (shift @sections, shift @sections);
        last unless $head;

        # Strip comments
        $body =~ s/#.*$//gm unless $head eq 'FARVER';
        # Uniform newlines
        $body =~ s/\r/\n/g;
        # Remove trailing whitespace
        $body =~ s/\s+$//gm;
        # Remove blank lines
        $body =~ s/^\n+//gm;

        my @lines = split(/\n/, $body);

        if ($head eq 'ALIAS') {
            $self->_parse_aliases(\@lines);
        } elsif ($head eq 'FARVER') {
            $self->_parse_colors(\@lines);
        } elsif ($head eq 'SKÆBNER') {
            $self->_parse_destinies(\@lines);
        } elsif ($head eq 'MESTRE') {
            $self->_parse_masters(\@lines);
        } elsif ($head eq 'MÅLTIDER') {
            # Ignore
        } elsif ($head eq 'INDKØBSØNSKER') {
            $self->wishes(\@lines);
        } elsif ($head eq 'INDKØB') {
            $self->_parse_purchases(\@lines);
        } elsif ($head eq 'MÅLTIDSDATA') {
            $self->_parse_lunches(\@lines);
        } else {
            print STDERR "Unknown section $head\n";
        }
    }

    $self->_add_balances;
}

sub _parse_aliases {
    my ($self, $lines) = @_;

    my %members;
    my %members_reverse;
    for my $line (@$lines) {
        my @aliases = split(/[\s,]+/, $line);
        my $primary = $aliases[0];
        $members{fc $primary} = EggsML::Member->new(
            canonical => $primary,
            aliases   => \@aliases,
        );
        $members_reverse{fc $_} = $primary for @aliases;
    }

    $self->members(\%members);
    $self->members_reverse(\%members_reverse);
}

sub _parse_colors {
    my ($self, $lines) = @_;

    for my $lines (@$lines) {
        next unless $lines =~ /([^:]*): (#\w{6})/;
        $self->member($1)->color($2);
    }
}

sub _parse_destinies {
    my ($self, $lines) = @_;

    for my $line (@$lines) {
        my ($name, $destiny) = split(/:\s*/, $line);
        $self->member($name)->destiny($destiny);
    }
}

sub _parse_masters {
    my ($self, $lines) = @_;
    for my $alias (@$lines) {
        $self->member($alias)->master(1);
    }
}

sub _parse_purchases {
    my ($self, $lines) = @_;

    my @purchases;

    for my $line (@$lines) {
        my ($date, $alias, $price) = $line =~ qr/(\d{4}-\d{2}-\d{2}), (.*?): (.*?)$/;
        next unless $date;

        my $purchase = { date => $date, alias => $alias, price => $price };
        push( @purchases, $purchase );
        $self->member($alias)->add_purchase( $purchase );
    }

    $self->purchases( \@purchases );
}

sub _parse_lunches {
    my ($self, $lines) = @_;

    my @lunches;
    for my $line (@$lines) {
        my @data = split(qr/,\s*/, $line);
        my $date = shift @data;

        my @lunchers;
        for my $entry (@data) {
            my ($alias, $weight) = $entry =~ qr/^(.*?)(?:\(\*(.*?)\))?$/;
            $weight //= 1;

            push (@lunchers, { alias => $alias, weight => $weight });
        }

        my $lunch_entry = { date => $date, lunchers => \@lunchers };

        push( @lunches, $lunch_entry );

        for my $luncher (@lunchers) {
            $self->member( $luncher->{alias} )->add_lunch( $luncher, $lunch_entry );
        }
    }

    $self->lunches( \@lunches );
}

sub _add_balances {
    my $self = shift;
    my $balances = EggsML::Daemon::Client->balances;
    for my $alias (keys %$balances) {
        $self->member($alias)->balance( $balances->{$alias} );
    }
}

=head3 member

Resolves an alias to the entry on the user.

=cut

sub member {
    my ($self, $alias) = @_;
    $alias =~ s/^\s*(.*?)\s*$/$1/g;
    my $canonical = $self->members_reverse->{fc $alias};
    return $self->members->{fc $canonical};
}

sub all_members {
    my $self = shift;

    my @members = sort { DateTime->compare( $b->latest_lunch, $a->latest_lunch ) ||
                        $a->canonical cmp $b->canonical }
                  grep { ! $_->retired } (values %{ $self->members });
    return \@members;
}

1;

