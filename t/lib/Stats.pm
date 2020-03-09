# -*- Perl -*-
#
# probably should find a stats module on CPAN one of these years...

use 5.24.0;
use warnings;

package Stats;
use Exporter 'import';
our @EXPORT = qw(mean sd);

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

1;
