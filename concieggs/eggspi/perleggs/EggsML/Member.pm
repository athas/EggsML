package EggsML::Member;
use 5.012;
use warnings;

use Mojo::Base -base;
use utf8::all;

use DateTime;
use EggsML::Daemon::Client;

has [qw(
    canonical aliases color destiny
)];

has purchases => sub { [] };
has lunches => sub { [] };

has [qw(master lunch_count total_purchased balance)] => sub { 0 };
has latest_lunch => sub { DateTime->new( year => 1970, month => 1, day => 1 ) };


sub add_purchase {
    my ($self, $purchase) = @_;


    $self->total_purchased( $self->total_purchased + $purchase->{price} );
    push( @{ $self->purchases }, $purchase );
}

sub add_lunch {
    my ($self, $luncher, $lunch) = @_;
    $self->lunch_count( $self->lunch_count + $luncher->{weight} );
    push( @{ $self->lunches }, $lunch );

    my $date = $self->parse_date( $lunch->{date} );
    unless ($date < $self->latest_lunch) {
        $self->latest_lunch( $date );
    }
}

sub parse_date {
    my ($self, $date) = @_;
    my ($y, $m, $d) = $date =~ /(\d{4})-(\d{2})-(\d{2})/;
    return DateTime->new( year => $y, month => $m, day => $d );
}

sub days_since_latest_lunch {
    my $self = shift;

    return DateTime->now->delta_days($self->latest_lunch)->in_units('days');
}

sub retired {
    my $self = shift;
    return $self->days_since_latest_lunch > 30 && abs($self->balance) < 10;
}

1
