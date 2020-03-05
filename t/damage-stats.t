#!perl
#
# how much damage do the damage functions dish out?
#  cpanm App::Prove
#  XOMB_STATS=1 prove t/damage-stats.t

use 5.24.0;
use warnings;
use Game::Xomb;
use Scalar::Util qw(looks_like_number);
use Test::Most;

my $trials = 1000;

my $count     = keys %Game::Xomb::Damage_From;
my $testcount = $count * 3;

plan tests => $testcount;

my $hero = Game::Xomb::make_player();

SKIP: {
    skip "no stats without XOMB_STATS set", $testcount
      unless $ENV{XOMB_STATS};

    my %args = (
        acidburn => [qw/10/],    # $duration
        attackby => [$hero],
        falling  => [qw//],      # smaller body does not matter to Gravity
    );

    my @outcomes;
    for my $name (sort { $a cmp $b } keys %Game::Xomb::Damage_From) {
        my $fn  = $Game::Xomb::Damage_From{$name};
        my $ret = $fn->($args{$name}->@*);
        # was there somewhat viable output from the fn?
        ok looks_like_number($ret);
        is $ret, int $ret;

        $name = $Game::Xomb::Things{ $name }->[Game::Xomb::DISPLAY] if exists $Game::Xomb::Things{ $name };

        my @ret = map { $fn->($args{$name}->@*) } 1 .. $trials;
        my ($mean, $min, $max) = mean(\@ret);
        my $sd = sd(\@ret, $mean);
        push @outcomes, sprintf "DAMAGE %s %.2f %.2f [%d,%d]\n", $name, $mean,
          $sd, $min, $max;

        # it isn't good to be negative
        ok $min >= 0;
    }
    diag "sample damage - mean sd [min,max]:\n", @outcomes;

    # and what do the to-hit values look like? (environmental factors
    # make things even worse)
    for my $name (sort { $a cmp $b } keys %Game::Xomb::Xarci_Bedo) {
        my $s = 'TO-HIT ' . $Game::Xomb::Things{ $name }->[Game::Xomb::DISPLAY];
        my $range = $Game::Xomb::Xarci_Bedo{$name}[1];
        for my $dist (1..$range) {
            my $odds = tohit($range, $dist, $Game::Xomb::Xarci_Bedo{$name}[0]);
            $s .= ' ' . $odds;
        }
        diag $s;
    }
}

sub mean {
    my ($ref) = @_;
    my $N     = $ref->@*;
    my $sum   = 0;
    my $min   = ~0;
    my $max   = -1;
    for my $x ($ref->@*) {
        $sum += $x;
        if    ($x < $min) { $min = $x }
        elsif ($x > $max) { $max = $x }
    }
    return $sum / $N, $min, $max;
}

sub sd {
    my ($ref, $mean) = @_;
    return sqrt mean([ map { ($_ - $mean)**2 } $ref->@* ]);
}

# KLUGE extracted from update_* for monsters. these probably should be
# non-linear so can have lower odds to-hit closer in for some, different
# rates of change, etc
sub tohit {
    my ($range, $dist, $bonus) = @_;
    my $distf = $dist / $range;
    return $bonus + int((1 - $distf) * 100);
}
