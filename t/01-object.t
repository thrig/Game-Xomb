
use Test::More tests => 10;

BEGIN {
use_ok( 'Game::Xomb' );
}

my $obj;

ok( $obj = Game::Xomb->new(), "no initializer");
isa_ok($obj,"Game::Xomb");

ok( $obj = Game::Xomb->new(1), "initial numeric value");
ok($obj->{value} == 1, "implicit initializer");

ok( $obj = Game::Xomb->new("fish"), "initial string value");
ok($obj->{value} eq "fish", "implicit initializer");

ok( $obj = Game::Xomb->new(color => "red", flavor => "sour"), 
	"hash as initializer");
ok( $obj->{color} eq "red", "first hash key");
ok( $obj->{flavor} eq "sour", "first hash key");
