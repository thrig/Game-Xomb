use Test::More tests => 5;

BEGIN {
use_ok( 'Game::Xomb' );
}

my $obj = Game::Xomb->new(1);
ok( $obj->increment );
ok( $obj->{value} == 2);

$obj = Game::Xomb->new(value => 3);
ok( $obj->{value} == 3 );
ok( $obj->increment == 4 );
