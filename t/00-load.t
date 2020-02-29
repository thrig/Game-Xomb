#!perl -T
use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'Game::Xomb' ) || print "Bail out!\n";
}

diag( "Testing Game::Xomb $Game::Xomb::VERSION, Perl $], $^X" );
