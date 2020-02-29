use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Game::Xomb',
    license             => 'bsd',
    dist_author         => 'Jeremy Mates <jeremy.mates@gmail.com>',
    dist_version_from   => 'lib/Game/Xomb.pm',
    include_dirs        => ['.'],
    requires => {
        'Test::More' => 0,
    },
    add_to_cleanup      => [ 'Game-Xomb-*' ],
);

$builder->create_build_script();
