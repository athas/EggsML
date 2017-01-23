package EggsML::Daemon::Client;
use 5.012;
use warnings;

use Env qw/EGGS_DAEMON_SOCKET/;
use IO::Socket::UNIX;
use utf8::all;

=head1 NAME

EggsML::Daemon::Client - Access the EggsML daemon directly from perl.

=head1 METHODS

=cut

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

=head2 aliases

Get a list of the aliases of te people given.

=cut


sub aliases {
    my ($self, @names) = @_;
    my ($resCode, $res) = $self->_run_nested_list( 'aliases', @names );
    return if $resCode;
    return $res;
}

=head2 wishes

Get a list of all the wishes.

=cut

sub wishes {
    my $self = shift;
    my ($resCode, $res) = $self->_run_list( 'wishes' );
    return if $resCode;
    return $res;
}

=head2 balances

Get a hash of all the people in Eggs' balances.

=cut

sub balances {
    my $self = shift;
    my ($resCode, $res) = $self->_run_hash( 'balances' );
    return if $resCode;
    return { reverse %$res };
}

=head2 balance_of_payments

=cut

sub balance_of_payments {
    my $self = shift;
    my ($resCode, $res) = $self->_run( 'balance_of_payments' );
    return if $resCode;
    return $res;
}

=head2 allaliases

Get a list of lists; each containing all aliases for one member.
=cut

sub allaliases {
    my $self = shift;
    my ($resCode, $res) = $self->_run_nested_list( 'allaliases' );
    return if $resCode;
    return $res;
}

=head2 lunches

=cut

sub lunches {
    my ($self, $name) = @_;
    my ($resCode, $res) = $self->_run_list( 'lunches', $name );
    return if $resCode;
    return $res;
}

=head2 eggsmates

=cut

sub eggsmates {
    my ($self, $name) = @_;
    my ($resCode, $res) = $self->_run( 'eggsmates', $name );
    return if $resCode;
    return $res;
}

=head2 eggscount

Get the eggscount for a given person.

=cut

sub eggscount {
    my ($self, $name) = @_;
    my ($resCode, $res) = $self->_run( 'eggscount', $name );
    return if $resCode;
    return $res;
}

=head2 consecutive

=cut

sub consecutive {
    my ($self, $name) = @_;
    my ($resCode, $res) = $self->_run( 'consecutive', $name );
    return if $resCode;
    return $res;
}

=head2 cmpnames

Is person1 equal to person2?
=cut

sub cmpnames {
    my ($self, $name1, $name2) = @_;
    return $self->_run_bool( 'cmpnames', $name1, $name2 );
}

=head2 isTrusted

Is the given person trusted?

=cut

sub isTrusted {
    my ($self, $name) = @_;
    return $self->_run_bool( 'isTrusted', $name );
}

1;

