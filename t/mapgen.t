#!perl
#
# is the map (and player) generation doing sane things?
#
#   AUTHOR_TEST_JMATES=1 prove t/mapgen.t

use 5.24.0;
use warnings;
use Data::Dumper;
use Game::Xomb;
use Test::Most;

use lib qw(t/lib);
use Stats;

my $deeply = \&eq_or_diff;

ok @Game::Xomb::LMap == 0;
Game::Xomb::init_map;
$deeply->($Game::Xomb::LMap[1][1][Game::Xomb::WHERE], [ 1, 1 ]);

my ($col, $row) = Game::Xomb::make_player;

ok $Game::Xomb::LMap[$row][$col][Game::Xomb::ANIMAL][Game::Xomb::SPECIES] ==
  Game::Xomb::HERO;

my $trials = $ENV{XOMB_MAPGEN_TRIALS} || 1;

# how many seeds is the level gen using up? (too few available is really
# bad, too many is less efficient but will better allow one to find a
# close point to some other point...)
my @seeds;

my $minlvl = $ENV{XOMB_MAPGEN_MINLVL} || 1;
my $maxlvl = $ENV{XOMB_MAPGEN_MAXLVL} || 5;
lives_ok sub {
    for my $level ($minlvl .. $maxlvl) {
        for (1 .. $trials) {
            push @seeds, Game::Xomb::generate_map;
            audit_map() if $ENV{AUTHOR_TEST_JMATES};
        }
    }
};

my $mean = mean(\@seeds);
my $sd = sd(\@seeds, $mean);
diag sprintf "free map seeds %.2f %.2f", $mean, $sd;

done_testing;

sub audit_map {
    my $anicount = 0;
    for my $r (0 .. Game::Xomb::MAP_ROWS - 1) {
        for my $c (0 .. Game::Xomb::MAP_COLS - 1) {
            ok defined $Game::Xomb::LMap[$r][$c][Game::Xomb::MINERAL]
              or diag Dumper $Game::Xomb::LMap[$r][$c];
            ok( !defined $Game::Xomb::LMap[$r][$c][Game::Xomb::VEGGIE]
                  or $Game::Xomb::LMap[$r][$c][Game::Xomb::VEGGIE]->@*
            ) or diag Dumper $Game::Xomb::LMap[$r][$c];
            my $ani = $Game::Xomb::LMap[$r][$c][Game::Xomb::ANIMAL];
            ok(!defined $ani or $ani->@*) or diag Dumper $Game::Xomb::LMap[$r][$c];
            $anicount++ if defined $ani;
        }
    }
    ok scalar @Game::Xomb::Animates == $anicount
      or diag "Animates "
      . scalar @Game::Xomb::Animates
      . " but found in map $anicount";
}
