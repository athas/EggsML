#!/usr/bin/env perl
package EggsML::Website;
use Mojolicious::Lite;
use Env qw/EGGS_DIR/;
use EggsML::Lunchfile;

my $lunchfile = EggsML::Lunchfile->new( filename => "$EGGS_DIR/slashdotfrokost" );

get '/' => sub {
    my $self = shift;

    $lunchfile->parse;

    $self->stash(
        wishes  => $lunchfile->wishes,
        members => $lunchfile->all_members,
    );

    return $self->render('index');
};

push (@{ app->renderer->paths }, "$EGGS_DIR/spindel/skabeloner");
push (@{ app->static->paths }, "$EGGS_DIR/spindel/statisk");

app->defaults(
    layout => 'bootstrap',
    title  => 'EggsML',
);

app->config(hypnotoad => {
    pid => "$EGGS_DIR/spindel/hypnotoad.pid",
    listen => [ 'http://127.0.0.1:14500' ],
});

app->start;
