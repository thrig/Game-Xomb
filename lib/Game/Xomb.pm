package Game::Xomb;

use warnings;
use strict;

=head1 NAME

Game::Xomb - The great new Game::Xomb!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

require XSLoader;
XSLoader::load('Game::Xomb', $VERSION);

=head1 SYNOPSIS

Quick summary of what the module does.

Perhaps a little code snippet.

    use Game::Xomb;

    my $foo = Game::Xomb->new();
    ...

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.

=head1 FUNCTIONS

=head2 new

Creates a new Game::Xomb object.  Takes the following optional parameters:

=over 4

=item value

If you pass a single numeric value, it will be stored in the 'value' slot
of the object hash.

=item key/value pair

A generic input method which takes an unlimited number of key/value pairs
and stores them in the object hash.  Performs no validation.

=back

=cut

#sub new {
# Defined in the XS code
#}

=head2 increment

An object method which increments the 'value' slot of the the object hash,
if it exists.  Called like this:

  my $obj = Game::Xomb->new(5);
  $obj->increment(); # now equal to 6

=cut

#sub function2 {
# Defined in the XS code
#}

=head1 AUTHOR

Jeremy Mates, C<< <jeremy.mates@gmail.com> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-game-xomb@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Game-Xomb>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2020 Jeremy Mates, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1; # End of Game::Xomb
