# -*- Perl -*-
#
# Game::Xomb - this is a terminal-based game. run the `xomb` command
# that should be installed with this module to begin

package Game::Xomb;

our $VERSION = '0.01';

use 5.24.0;
use warnings;
use open ':encoding(UTF-8)';
use POSIX qw(STDIN_FILENO TCIFLUSH tcflush);
use Scalar::Util qw(weaken);
use Term::ReadKey qw(GetTerminalSize ReadKey ReadMode);
use Time::HiRes qw(gettimeofday sleep tv_interval);

require XSLoader;
XSLoader::load('Game::Xomb', $VERSION);

# ANSI or XTerm control sequences
sub at              { "\e[" . $_[1] . ';' . $_[0] . 'H' }
sub at_col          { "\e[" . $_[0] . 'G' }
sub alt_screen ()   { "\e[?1049h" }
sub clear_screen () { "\e[1;1H\e[2J" }
sub clear_right ()  { "\e[K" }
sub hide_cursor ()  { "\e[?25l" }
sub hide_pointer () { "\e[>3p" }
sub show_cursor ()  { "\e[?25h" }
sub term_norm ()    { "\e[m" }
sub unalt_screen () { "\e[?1049l" }

sub MAX_ROWS () { 24 }
sub MAX_COLS () { 80 }
# TODO probably want full-er level design, maybe M to bring up full
# messages like in Brogue
sub COLS ()         { 23 }
sub ROWS ()         { 23 }
sub MAP_DISP_OFF () { 1 }

# level map is row, col while points are [ col, row ]
sub PROW () { 1 }
sub PCOL () { 0 }

sub MSG_MAX () { 4 }

# WHAT Animates and such can be
sub HERO () { 0 }
#sub MONST ()  { 1 }
#sub BOMB ()   { 2 }
#sub GEM ()    { 3 }
sub FLOOR () { 4 }
sub WALL ()  { 5 }
#sub LADDER () { 6 }
sub GATE () { 7 }
#sub STATUE () { 8 }

# for Animates (and also some Things for the first few slots)
sub WHAT () { 0 }
sub DISP () { 1 }
# NOTE that GROUND use TYPE to distinguish between different types of
# those (FLOOR, GATE) so something can only look at WHAT for
# whether motion is possible in that cell; ANI and ITEM instead use TYPE
# to tell ANI apart from ITEM
sub TYPE ()       { 2 }
sub STASH ()      { 3 }
sub UPDATE ()     { 4 }
sub LMC ()        { 5 }
sub BLACK_SPOT () { 6 }

# for the Level Map Cell (LMC)
sub WHERE ()  { 0 }
sub GROUND () { 1 }
sub ITEM ()   { 2 }
sub ANI ()    { 3 }

sub MOVE_FAILED () { 0 }
sub MOVE_OK ()     { 1 }
sub MOVE_NEWLVL () { 2 }

sub DEFAULT_COST () { 10 }
sub DIAG_COST () { 14 }

our (@Animates, @RedrawA, @RedrawB, $Hero_Pos, $LMap, $Redraw_Delay);
our $Level = 0;

our %CharMap = (
    '.' => FLOOR,
    '@' => HERO,
    '%' => GATE,
    '#' => WALL,
);

our %Examine_Offsets = (
    'h' => [ -1, +0 ],    # left
    'j' => [ +0, +1 ],    # down
    'k' => [ +0, -1 ],    # up
    'l' => [ +1, +0 ],    # right
    'y' => [ -1, -1 ],
    'u' => [ +1, -1 ],
    'b' => [ -1, +1 ],
    'n' => [ +1, +1 ],
);

our %Things = (
    FLOOR, [ FLOOR, "\e[33m.\e[0m", FLOOR ],
    GATE,  [ FLOOR, "\e[37m%\e[0m", GATE ],
    WALL,  [ WALL,  "\e[35m#\e[0m", WALL ],
);

our %Descriptions = (
    FLOOR, 'Empty cell.', HERO, 'You.', GATE, 'Gate to another level.',
    WALL, 'A wall.',
);

$Animates[HERO]->@[ WHAT, DISP, TYPE, UPDATE ] =
  (HERO, "\e[1;37m\@\e[0m", ANI, \&update_hero);

our %Key_Commands = (
    'h' => move_player(-1, +0, DEFAULT_COST ),    # left
    'j' => move_player(+0, +1, DEFAULT_COST ),    # down
    'k' => move_player(+0, -1, DEFAULT_COST ),    # up
    'l' => move_player(+1, +0, DEFAULT_COST ),    # right
    'y' => move_player(-1, -1, DIAG_COST),
    'u' => move_player(+1, -1, DIAG_COST),
    'b' => move_player(-1, +1, DIAG_COST),
    'n' => move_player(+1, +1, DIAG_COST),
    '.' => \&move_nop,             # rest
    ' ' => \&move_nop,            # also rest
    'v' => sub { post_message('Version ' . $VERSION); return MOVE_FAILED },
    'x' => \&move_examine,
    # TODO use these to go up/down at gates
    #'<' => sub {
    #},
    #'>' => sub {
    #},
    '?' => sub {
        post_message("TODO write help");
        return MOVE_FAILED;
    },
    '@' => sub {
        local $" = ',';
        post_message("\@ $Animates[HERO][LMC][WHERE]->@*");
        return MOVE_FAILED;
    },
    # by way of history '%' is what rogue (version 3.6) uses to display
    # stairs (actually one-way optional chutes), except the '>' (or very
    # rarely '<') keys are used to interact with that symbol
    '%' => sub {
        if ($Animates[HERO][LMC][GROUND][TYPE] == GATE) {
            #load_level();
            # TODO gen new level
            print clear_screen(), draw_level();
            post_message('Welcome to ' . $Level);
            return MOVE_NEWLVL;
        } else {
            post_message('PKC-0037 No gate found.');
            return MOVE_FAILED;
        }
    },
    'G' => sub {
        print clear_screen, show_cursor, <<"BOSS_SCREEN", "\n:";

LS(1)                     BSD General Commands Manual                    LS(1)

\e[1mNAME\e[0m
     \e[1mls\e[0m -- list directory contents

SYNOPSIS
     \e[1mls\e[0m [-\e[1mABCFGHLOPRSTUW\@abcdefghiklmnopqrstuwx1\e[0m] [\e[4mfile\e[0m \e[4m...\e[0m]

\e[1mDESCRIPTION\e[0m
     For each operand that names a \e[4mfile\e[0m of a type other than directory, ls
     displays its name as well as any requested, associated information.  For
     each operand that names a \e[4mfile\e[0m of type directory, \e[1mls\e[0m displays the names
     of files contained within that directory, as well as any requested, asso-
     ciated information.

     If no operands are given, the contents of the current directory are dis-
     played.  If more than one operand is given, non-directory operands are
     displayed first; directory and non-directory operands are sorted sepa-
     rately and in lexicographical order.

     The following options are available:

BOSS_SCREEN
        while (1) {
            my $key = ReadKey(0);
            last if $key eq "\033" or $key eq 'q';
        }
        print hide_cursor;
        redraw_level();
        return MOVE_FAILED;
    },
    'q'    => sub { game_over('Be seeing you...') },
    'Q'    => sub { game_over('Be seeing you...') },
    "\003" => sub {                                    # <C-c>
        # DBG
        game_over();
        #post_message('Enough with these silly interruptions!');
        #return MOVE_FAILED;
    },
    "\014" => sub {                                    # <C-l>
        redraw_level();
        return MOVE_FAILED;
    },
    "\032" => sub {                                    # <C-z>
        post_message('You hear a strange noise in the background.');
        return MOVE_FAILED;
    },
    "\033" => sub {
        post_message('You cannot escape quite so easily.');
        return MOVE_FAILED;
    },
);

sub bad_terminal {
    my ($TCols, $TRows) = (GetTerminalSize(*STDOUT))[ 0, 1 ];
    return (not defined $TCols or $TCols < MAX_COLS or $TRows < MAX_ROWS);
}

sub bail_out {
    restore_term();
    print "\n", at_col(0), clear_right;
    warn $_[0] if @_;
    game_over("The game yet again collapses about you...");
}

{
    my @log;

    sub clear_messages { @log = () }

    sub post_message {
        my ($msg) = @_;
        while (@log >= MSG_MAX) { shift @log }
        push @log, $msg;
        show_messages();
    }

    sub show_messages {
        for my $i (0 .. $#log) {
            # TODO
            #print at(MSG_COL, MSG_ROW + $i), clear_right, $log[$i];
            *STDERR->say("MSG $i: $log[$i]");
        }
    }
}

sub draw_level {
    my $s = '';
    for my $rownum (0 .. ROWS - 1) {
        $s .= at(MAP_DISP_OFF, MAP_DISP_OFF + $rownum);
        # TODO may need unicode indicator where ani+item or ani+gate present?
        # or gates are items that interact with items thrown on them?
        for my $lmc ($LMap->[$rownum]->@*) {
            if (defined $lmc->[ANI]) {
                $s .= $lmc->[ANI][DISP];
            } elsif (defined $lmc->[ITEM]) {
                $s .= $lmc->[ITEM][DISP];
            } else {
                $s .= $lmc->[GROUND][DISP];
            }
        }
    }
    return $s;
}

sub game_loop {
    game_over('Terminal must be at least ' . MAX_COLS . 'x' . MAX_ROWS)
      if bad_terminal();
    $Redraw_Delay = shift;
    generate_level();
    $SIG{$_} = \&bail_out for qw(INT HUP TERM PIPE QUIT USR1 USR2 __DIE__);
    STDOUT->autoflush(1);
    ReadMode 'raw';
    print term_norm, alt_screen, hide_cursor, hide_pointer, clear_screen,
      draw_level;
    $SIG{CONT}  = \&redraw_level;
    $SIG{WINCH} = sub {
        post_message('The terminal is too small!') if bad_terminal();
        redraw_level();
    };

    # TODO prolly needs changes for an energy based system...
    while (1) {
        next            if $Animates[HERO][UPDATE]->() == MOVE_NEWLVL;
        for my $ent (@Animates[ 1 .. $#Animates ]) {
            $ent->[UPDATE]->($ent) unless $ent->[BLACK_SPOT] ;
        }
        @Animates = grep { !$_->[BLACK_SPOT] } @Animates;
        redraw_movers();
    }
}

sub game_over {
    my ($msg, $code) = @_;
    $code //= 1;
    restore_term();
    print "\n", at_col(0), clear_right, $msg, "\n", clear_right;
    exit $code;
}

sub generate_level {
    splice @Animates, 1;
    undef $Animates[HERO][LMC];
    $LMap = [];
    for my $r (0 .. ROWS - 1) {
        for my $c (0 .. COLS - 1) {
            # should lmap in LMC have c,r broken out instead of as an array ref?
            # could slice it out...
            my $point = [ $c, $r ];    # PCOL, PROW (x, y)
            push $LMap->[$r]->@*, [ $point, $Things{ FLOOR, } ];
        }
    }
    my $r = int rand COLS;
    my $c = int rand ROWS;
    $Hero_Pos = [ $c, $r ];
    $LMap->[$r][$c][ANI] = $Animates[HERO];
    $Animates[HERO][LMC] = $LMap->[$r][$c];
    weaken($Animates[HERO][LMC]);
}

sub interact {
    my ($mover, $dest) = @_;
    return 0;
# TODO will need suitable interactions with stuff...
#   for my $i (ANI, ITEM) {
#       my $target = $LMap->[ $dest->[PROW] ][ $dest->[PCOL] ][$i];
#       if (defined $target) {
#           # this code is assumed to take care of everything and be the
#           # final say on the interaction
#           $Interact_With{ $target->[WHAT] }->($mover, $target);
#           return 1;
#       }
#   }
#   return 0;
}

sub move_animate {
    my ($ent, $cols, $rows, $cost) = @_;
    my $lmc = $ent->[LMC];
    my $dest = [ $lmc->[WHERE][PCOL] + $cols, $lmc->[WHERE][PROW] + $rows ];
    return MOVE_FAILED, 0 if $dest->[PCOL] < 0 or $dest->[PCOL] >= COLS or $dest->[PROW] < 0 or $dest->[PROW] >= ROWS;
    relocate($ent, $dest) unless interact($ent, $dest);
    return MOVE_OK, $cost;
}

# TODO maybe things with ani+item get a unicode marker to show that?
# draw level would need to peek when ANI if ITEM or !FLOOR (gate)
sub move_examine {
    my $key;
    my $row = $Animates[HERO][LMC][WHERE][PROW];
    my $col = $Animates[HERO][LMC][WHERE][PCOL];
    # TODO
    #print at(MSG_COL, MSG_ROW + $_), clear_right for 1 .. MSG_MAX;
    #print at(MSG_COL, MSG_ROW), clear_right,
    #  'Move cursor to view a cell. Esc exits', show_cursor;
  PEEK: while (1) {
        #print at(MSG_COL, MSG_ROW + $_), clear_right for 3 .. 5;
        my $disp_row = 2;
        for my $i (ANI, ITEM) {
            my $x = $LMap->[$row][$col][$i];
            if (defined $x) {
                #print at(MSG_COL, MSG_ROW + $disp_row++), clear_right, $x->[DISP],
                #  ' - ', $Descriptions{ $x->[WHAT] };
            }
        }
        my $g = $LMap->[$row][$col][GROUND];
        #print at(MSG_COL, MSG_ROW + $disp_row), clear_right, $g->[DISP],
        #  ' - ', $Descriptions{ $g->[TYPE] },
        #  at(MAP_DISP_OFF + $col, MAP_DISP_OFF + $row);
        $key = ReadKey(0);
        last PEEK if $key eq "\033";
        # TODO probably figure out/convert numpad to correct hjkl here
        # or have xmodmap docs on that translation
        my $distance = 1;
        if (ord $key < 97) {    # SHIFT moves faster
            $key      = lc $key;
            $distance = 5;
        }
        my $dir = $Examine_Offsets{$key} // next PEEK;
        $row = between(0, ROWS - 1, $row + $dir->[PROW] * $distance);
        $col = between(0, COLS - 1, $col + $dir->[PCOL] * $distance);
    }
    print hide_cursor;
    show_messages();
    return MOVE_FAILED;
}

sub move_nop { return MOVE_OK, DEFAULT_COST }

sub move_player {
    my ($cols, $rows, $mvcost) = @_;
    return sub {
        my ($status, $cost, $msg) = move_animate($Animates[HERO], $cols, $rows, $mvcost);
        post_message($msg) if $msg;
        return $status, $cost;
    };
}

sub redraw_level { print clear_screen, draw_level; show_messages() }

sub redraw_movers {
    redraw_ref(\@RedrawA);
    sleep($Redraw_Delay);
    redraw_ref(\@RedrawB);
    @RedrawA = ();
    @RedrawB = ();
}

sub redraw_ref {
  CELL: for my $point ($_[0]->@*) {
        for my $i (ANI, ITEM) {
            my $ent = $LMap->[ $point->[PROW] ][ $point->[PCOL] ][$i];
            if (defined $ent and !$ent->[BLACK_SPOT]) {
                print at(map { MAP_DISP_OFF + $_ } $point->@*), $ent->[DISP];
                next CELL;
            }
        }
        print at(map { MAP_DISP_OFF + $_ } $point->@*),
          $LMap->[ $point->[PROW] ][ $point->[PCOL] ][GROUND][DISP];
    }
}

# TODO Redraw might be hash so not overdrawing things esp. after some
# animation run knocks a bunch of cells?
sub relocate {
    my ($ent, $dest) = @_;
    my $src = $ent->[LMC][WHERE];
    push @RedrawA, $src;
    push @RedrawB, $dest;
    my $lmc = $LMap->[ $dest->[PROW] ][ $dest->[PCOL] ];
    $lmc->[ $ent->[TYPE] ] = $ent;
    undef $LMap->[ $src->[PROW] ][ $src->[PCOL] ][ $ent->[TYPE] ];
    $ent->[LMC] = $lmc;
    weaken($ent->[LMC]);
}

sub restore_term {
    ReadMode 'restore';
    print term_norm, show_cursor, unalt_screen;
}

sub update_hero {
    my ($cost, $status);
    tcflush(STDIN_FILENO, TCIFLUSH);
    while (1) {
        my $key;
        while (1) {
            $key = ReadKey(0);
            last if exists $Key_Commands{$key};
            post_message(sprintf "Illegal command \\%03o", ord $key);
        }
        ($status, $cost, my $msg) = $Key_Commands{$key}->();
        post_message($msg) if defined $msg;
        last               if $status == MOVE_OK or $status == MOVE_NEWLVL;
    }
    return $status, $cost;
}

1;
__END__

=head1 NAME

Game::Xomb - a game featuring @ versus the Xarci Bedo

=head1 SYNOPSIS

This is a terminal-based game. Assuming that your development tools,
perl, L<App::cpanminus>, and possibly also L<local::lib> are installed
and setup, in a suitable terminal install and run the game via:

    cpanm Game::Xomb
    xomb

Use the C<?> key in game to show the help text (TODO implement), or
peruse the (TODO write) documentation below.

The L<xomb(1)> documentation details commands and other useful game
information; it should be available once the module is installed either
as a L<man(1)> page or by running:

    perldoc xomb

=head1 DESCRIPTION

    Know then on Minos III sacred to Mars
    do grow the spawn of Cadmus from afar

    as towers tall, monoliths for Clotho
    to spread as she would. The Xarci Bedo

    we call this foe. Do you now seek the stone
    below of Dragon born to call your own?

=head1 BUGS

Patches might best be applied towards:

L<https://github.com/thrig/Game-Xomb>

=head1 SEE ALSO

L<Game::PlatformsOfPeril> upon which this code is adapted from and then
various roguelikes available on the Internet.

7DRL

=head1 AUTHOR

Jeremy Mates

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2020 by Jeremy Mates

This program is distributed under the (Revised) BSD License:
L<http://www.opensource.org/licenses/BSD-3-Clause>

=cut
