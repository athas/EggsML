#!/usr/bin/env perl
use 5.012;
use warnings;
no warnings 'experimental::smartmatch';

use Env qw/CONCIEGGS_DIR CONCIEGGS_DB_DIR CONCIEGGS_LIB_DIR/;
use File::Basename;
use File::Copy qw/copy/;
use File::Find::Rule;
use File::Path qw/make_path/;
use File::Spec::Functions qw/catfile/;
use File::Temp qw/tempdir/;
use IPC::System::Simple qw/capturex EXIT_ANY $EXITVAL/;

unless (@ARGV) {
    say STDERR "Usage: $0 <srcfile> [args...]\n";
    exit(1);
}

my $cache_dir = catfile($CONCIEGGS_DB_DIR, "compiled-cache");
my $source_dir = catfile($CONCIEGGS_DIR, "compiled");

my $command = basename(shift @ARGV);

my $cached_file = catfile($cache_dir, $command);
my ($source_file) = File::Find::Rule->file()->name("$command.*")->in($source_dir);

unless ($source_file) {
    say STDERR "No source file exists for '$command'.\n";
    exit(1);
}

# If source file is newer than cached version; recompile.
if ((stat($source_file))[9] > ((stat($cached_file))[9] // 0)) {
    my $compile_dir = tempdir(CLEANUP => 1);
    copy($source_file, $compile_dir);
    $source_file = catfile($compile_dir, basename($source_file));

    make_path($cache_dir);
    chdir($compile_dir);

    given ($source_file) {
        when (/\.go$/) {
            my $golang_lib_dir = catfile($CONCIEGGS_LIB_DIR, 'golangbiblioteggs');
            $ENV{'GOPATH'} = "$source_dir:$golang_lib_dir";
            say STDERR capturex(EXIT_ANY, qw(go build), $source_file);
        }
        when (/\.c$/) {
            say STDERR capturex(EXIT_ANY, qw(gcc -std=c99), $source_file, '-o', $cached_file);
        }
        when (/\.sml$/) {
            say STDERR capturex(EXIT_ANY, qw(mosmlc -P full -toplevel -o), $cached_file, $source_file);
        }
        when (/\.pas$/) {
            say STDERR capturex(EXIT_ANY, 'fpc', $source_file, qq{-o"$cached_file"});
        }
        when (/\.hs$/) {
            my $haskell_lib_dir = catfile($CONCIEGGS_LIB_DIR, 'haskeggs');
            my $include_path = "$source_dir:$haskell_lib_dir";
            say STDERR capturex(EXIT_ANY, 'ghc', $source_file, '-o', $cached_file, qq{-i"$include_path"});
        }
        when (/\.kex$/) {
            say STDERR capturex(EXIT_ANY, qw(repg compile), $source_file, '--srcout', "$source_file.c");
            last if $EXITVAL;

            say STDERR capturex(EXIT_ANY, qw(gcc -o), $cached_file, qw(-O3 -xc -D FLAG_WORDALIGNED -w), "$source_file.c");
        }
        default {
            say "Cannot compile file $source_file - unknown extension.";
            exit(2);
        }
    }

    if ($EXITVAL) {
        say STDERR "Compilation produced an error code ($EXITVAL). Aborting.";
        exit(3);
    }
}

exec ($cached_file, @ARGV);
