# -*- Perl -*-
#
# Game::Xomb - this is a terminal-based game. run the xomb(1) command
# that should be installed with this module to begin

package Game::Xomb;

our $VERSION = '0.02';

use 5.24.0;
use warnings;
use open IO => ':locale';
use Carp qw(croak confess);    # DBG
use Data::Dumper;              # DBG
use List::Util qw(min);
use POSIX qw(STDIN_FILENO TCIFLUSH tcflush);
use Scalar::Util qw(weaken);
use Term::ReadKey qw(GetTerminalSize ReadKey ReadMode);
use Time::HiRes qw(gettimeofday sleep tv_interval);

require XSLoader;
XSLoader::load('Game::Xomb', $VERSION);

# ANSI or XTerm control sequences - http://invisible-island.net/xterm/
#
# points are col,row (x,y) while terminal uses row,col hence the reverse
# argument order here
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

sub MAP_COLS ()     { 78 }
sub MAP_ROWS ()     { 22 }
sub MAP_DISP_OFF () { 2 }

sub MSG_COL ()      { 0 }
sub MSG_ROW ()      { 0 }
sub MSG_MAX ()      { MAX_ROWS - 1 }
sub STATUS_COL ()   { 0 }
sub STATUS_ROW ()   { MAX_ROWS }
sub ERR_CODE_COL () { 70 }

# level map is row, col while points are [ col, row ]
sub PROW () { 1 }
sub PCOL () { 0 }

# WHAT Animates and such can be
sub HERO ()   { 0 }
sub AMULET () { 1 }
sub FLOOR ()  { 4 }
sub WALL ()   { 5 }
sub COLUMN () { 6 }
sub GATE ()   { 7 }

# for Animates (and also some Things for the first few slots)
sub WHAT () { 0 }
sub DISP () { 1 }
# NOTE that GROUND use TYPE to distinguish between different types of
# those (FLOOR, GATE, WALL, COLUMN) so something can only look at WHAT
# for whether motion is possible in that cell; ANI and ITEM instead use
# TYPE to tell ANI apart from ITEM. TODO maybe fields for solid, opaque
# like over in prentice but without SQLite?
sub TYPE ()       { 2 }
sub STASH ()      { 3 }
sub UPDATE ()     { 4 }
sub LMC ()        { 5 }
sub BLACK_SPOT () { 6 }
sub ENERGY ()     { 7 }

# player stash items
sub INVENTORY () { 0 }

# for the Level Map Cell (LMC)
sub WHERE ()  { 0 }
sub GROUND () { 1 }
sub ITEM ()   { 2 }
sub ANI ()    { 3 }

sub MOVE_LVLUP ()   { -1 }
sub MOVE_FAILED ()  { 0 }
sub MOVE_LVLDOWN () { 1 }
sub MOVE_OK ()      { 2 }

sub CAN_MOVE ()     { 0 }
sub DEFAULT_COST () { 10 }
sub DIAG_COST ()    { 14 }
sub NLVL_COST ()    { 15 }

our (@Animates, @RedrawA, @RedrawB, $LMap, $Redraw_Delay, $TCols,
    $TRows);
our $Level      = 1;
our $Turn_Count = 0;
our $Time_Spent = 0;

# by way of history '%' is what rogue (version 3.6) uses to display
# stairs (which really are actualy one-way optional chutes)
our %Things = (
    AMULET, [ AMULET, "\e[1;33;40m,\e[m",        AMULET ],
    COLUMN, [ WALL,   "\e[0;37;40m\x{2B29}\e[m", COLUMN ],
    FLOOR,  [ FLOOR,  "\e[2;37;40m.\e[m",        FLOOR ],
    GATE,   [ FLOOR,  "\e[1;36;40m%\e[m",        GATE ],
    WALL,   [ WALL,   "\e[0;37;40m#\e[m",        WALL ],
);

# NOTE these must be short as they must all fit in status line and there
# can be three of them, see move_examine
our %Descript = (
    AMULET, 'Dragonstone', COLUMN, 'Column',
    FLOOR, 'Floor', HERO, 'You', GATE, 'Gate to another level',
    WALL, 'Wall',
);

# make the hero
$Animates[HERO]->@[ WHAT, DISP, TYPE, ENERGY, STASH, UPDATE ] =
  (HERO, "\e[1;37;40m\@\e[m", ANI, CAN_MOVE, [], \&update_hero);

# for looking around with examine mode
our %Examine_Offsets = (
    'h' => [ -1, +0 ],
    'j' => [ +0, +1 ],
    'k' => [ +0, -1 ],
    'l' => [ +1, +0 ],
    'y' => [ -1, -1 ],
    'u' => [ +1, -1 ],
    'b' => [ -1, +1 ],
    'n' => [ +1, +1 ],
);

# these define what happens when various keys are mashed
our %Key_Commands = (
    'g' => \&move_pickup,
    ',' => \&move_pickup,
    'i' => \&show_inventory,
    'h' => move_player(-1, +0, DEFAULT_COST),    # left
    'j' => move_player(+0, +1, DEFAULT_COST),    # down
    'k' => move_player(+0, -1, DEFAULT_COST),    # up
    'l' => move_player(+1, +0, DEFAULT_COST),    # right
    'y' => move_player(-1, -1, DIAG_COST),
    'u' => move_player(+1, -1, DIAG_COST),
    'b' => move_player(-1, +1, DIAG_COST),
    'n' => move_player(+1, +1, DIAG_COST),
    '.' => \&move_nop,                           # rest
    ' ' => \&move_nop,                           # also rest
    'v' => sub {
        log_message('Xomb v.' . $VERSION . ' PID ' . $$);
        return MOVE_FAILED, 0;
    },
    'x' => \&move_examine,
    '<' => sub {
        log_message('TODO check if have amulet');
        if ($Animates[HERO][LMC][GROUND][TYPE] != GATE) {
            log_message('not a gate!!');
            return MOVE_FAILED, 0, 'PKC-0037';
        }
        log_message('Gate activated.');
        return MOVE_LVLUP, NLVL_COST;
    },
    '>' => sub {
        if ($Animates[HERO][LMC][GROUND][TYPE] != GATE) {
            log_message('not a gate!!');
            return MOVE_FAILED, 0, 'PKC-0037';
        }
        log_message('Gate activated.');
        return MOVE_LVLDOWN, NLVL_COST;
    },
    '?' => sub {
        log_message("TODO write help or exec perldoc ...");
        return MOVE_FAILED, 0;
    },
    'G' => sub { boss_screen();   return MOVE_FAILED, 0 },
    'M' => sub { show_messages(); return MOVE_FAILED, 0 },
    'q' => sub { game_over('Be seeing you...') },
    'Q' => sub { game_over('Be seeing you...') },
    "\003" => sub {    # <C-c>
                       # DBG
        game_over();
        #log_message('Enough with these silly interruptions!');
        #return MOVE_FAILED;
    },
    "\014" => sub {    # <C-l>
        refresh_board();
        return MOVE_FAILED, 0;
    },
    "\032" => sub {    # <C-z>
        log_message('You hear a strange noise in the background.');
        return MOVE_FAILED, 0;
    },
    "\033" => sub {
        log_message('You cannot escape so easily.');
        return MOVE_FAILED, 0;
    },
);

sub await_quit {
    while (1) {
        my $key = ReadKey(0);
        last if $key eq "\033" or $key eq 'q';
    }
}

sub bad_terminal {
    return 0 unless -t *STDOUT;
    ($TCols, $TRows) = (GetTerminalSize(*STDOUT))[ 0, 1 ];
    !defined $TCols or $TCols < MAX_COLS or $TRows < MAX_ROWS;
}

sub bail_out {
    restore_term();
    print "\n", at_col(0), clear_right;
    warn $_[0] if @_;
    game_over("The game yet again collapses about you...");
}

sub between {
    my ($min, $max, $value) = @_;
    if ($value < $min) {
        $value = $min;
    } elsif ($value > $max) {
        $value = $max;
    }
    return $value;
}

sub boss_screen {
    print clear_screen, at(0, 2), <<"BOSS_SCREEN", "\n:", show_cursor;
LS(1)                     BSD General Commands Manual                    LS(1)

\e[1mNAME\e[m
     \e[1mls\e[m -- list directory contents

SYNOPSIS
     \e[1mls\e[m [-\e[1mABCFGHLOPRSTUW\@abcdefghiklmnopqrstuwx1\e[m] [\e[4mfile\e[m \e[4m...\e[m]

\e[1mDESCRIPTION\e[m
     For each operand that names a \e[4mfile\e[m of a type other than directory, ls
     displays its name as well as any requested, associated information.  For
     each operand that names a \e[4mfile\e[m of type directory, \e[1mls\e[m displays the names
     of files contained within that directory, as well as any requested, asso-
     ciated information.

     If no operands are given, the contents of the current directory are dis-
     played.  If more than one operand is given, non-directory operands are
     displayed first; directory and non-directory operands are sorted sepa-
     rately and in lexicographical order.

     The following options are available:
BOSS_SCREEN
    await_quit();
    print hide_cursor;
    refresh_board();
}

sub can_win {
    my $inventory = $Animates[HERO][STASH][INVENTORY] // [];
    for my $item ($inventory->@*) {
        return 1 if $item->[TYPE] == AMULET;
    }
    return 0;
}

{
    my $code_clear = 1;
    my $last_code  = '';
    my @log;

    sub clear_code {
        print at(ERR_CODE_COL, STATUS_ROW), clear_right;
        $code_clear = 1;
    }
    sub clear_messages { @log = () }

    sub log_code {
        my ($code) = @_;
        if ($code ne $last_code) {
            $last_code  = $code;
            $code_clear = 0;
            show_code();
        }
    }

    sub log_message {
        my ($msg) = @_;
        while (@log >= MSG_MAX) { shift @log }
        push @log, $Turn_Count . ' ' . $msg;    # DBG
        show_top_message();
    }

    sub show_code {
        return if $code_clear or $last_code eq '';
        print at(ERR_CODE_COL, STATUS_ROW), "\e[1;31;40m*", $last_code, '*',
          term_norm;
    }

    sub show_messages {
        print clear_screen, show_cursor, "\e[0;37;40m";
        while (my ($i, $msg) = each @log) {
            print at(MSG_COL, MSG_ROW + $i), $msg;
        }
        print at(MSG_COL, MSG_ROW + @log), "-- press Esc to continue --",
          term_norm;
        await_quit();
        print hide_cursor;
        refresh_board();
    }

    sub show_top_message {
        my $msg = @log ? $log[-1] : '';
        print at(MSG_COL, MSG_ROW), clear_right, "\e[0;37;40m", $msg, term_norm;
    }
}

sub draw_level {
    my $s = '';
    for my $rownum (0 .. MAP_ROWS - 1) {
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
    print $s;
}

sub fisher_yates_shuffle {
    my $array = $_[0];
    my $i;
    for ($i = @$array; --$i;) {
        my $j = int rand($i + 1);
        next if $i == $j;
        @$array[ $i, $j ] = @$array[ $j, $i ];
    }
}

sub game_loop {
    game_over('Terminal must be at least ' . MAX_COLS . 'x' . MAX_ROWS)
      if bad_terminal();

    ($Redraw_Delay) = @_;

    ReadMode 'raw';
    $SIG{$_} = \&bail_out for qw(INT HUP TERM PIPE QUIT USR1 USR2 __DIE__);
    $SIG{CONT}  = \&refresh_board;
    $SIG{WINCH} = sub {
        log_message('The terminal is too small!') if bad_terminal();
        refresh_board();
    };
    STDOUT->autoflush(1);

    generate_level();

    print term_norm, alt_screen, hide_cursor, hide_pointer, clear_screen;
    paint_black();
    draw_level();
    show_status_bar();
    log_message('Welcome to xomb');

    while (1) {
        my $min_cost = min map { $_->[ENERGY] } @Animates;
        $Time_Spent += $min_cost;
        log_message('DBG min energy ' . $min_cost);

        my @movers;
        for my $ent (@Animates) {
            $ent->[ENERGY] -= $min_cost;
            push @movers, $ent if $ent->[ENERGY] <= CAN_MOVE;
        }
        fisher_yates_shuffle \@movers if @movers > 1;

        my $new_level = 0;
        for my $ent (@movers) {
            #use Data::Dumper; warn Dumper $ent; # DBG
            my ($status, $cost) = $ent->[UPDATE]->($ent);
            $ent->[ENERGY] += $cost;
            $new_level = $status
              if $status == MOVE_LVLDOWN or $status == MOVE_LVLUP;
            die "DBG bad cost $cost\n" . Dumper($ent) if $cost <= CAN_MOVE;
        }

        if ($new_level != 0) {
            $Level += $new_level;
            if ($Level <= 0) {
                if (can_win()) {
                    game_over('Victory TODO improve this', 0);
                } else {
                    log_message('You need the Dragonstone to escape.');
                    $Level = 1;
                    goto CLEANUP;
                }
            }
            generate_level();
            draw_level();
            log_message('Welcome to level ' . $Level);
            show_status_bar();
            next;
        }

      CLEANUP:
        @Animates = grep { !$_->[BLACK_SPOT] } @Animates;
        redraw_movers();
    }
}

sub game_over {
    my ($msg, $code) = @_;
    $code //= 1;
    restore_term();
    print "\n", at_col(0), clear_right, $msg,
      " ($Turn_Count, $Time_Spent)\n",
      clear_right;
    exit $code;
}

sub generate_level {
    splice @Animates, 1;
    undef $Animates[HERO][LMC];
    $LMap = [];

    for my $r (0 .. MAP_ROWS - 1) {
        for my $c (0 .. MAP_COLS - 1) {
            my $point = [ $c, $r ];    # PCOL, PROW (x, y)
            push $LMap->[$r]->@*, [ $point, $Things{ FLOOR, } ];
        }
    }

    # DBG KLUGE test out stuff on level map
    my $c = 1;
    my $r = 1;
    $LMap->[$r][$c][ANI] = $Animates[HERO];
    $Animates[HERO][LMC] = $LMap->[$r][$c];
    weaken $Animates[HERO][LMC];
    $c = $r = 2;
    $LMap->[$r][$c][GROUND] = $Things{ GATE, };

    if ($Level == 1) {
        $c                      = $r = 3;
        $LMap->[$r][$c][GROUND] = $Things{ WALL, };
        $c                      = $r = 4;
        $LMap->[$r][$c][GROUND] = $Things{ COLUMN, };
    } else {
        $c = $r = 5;
        $LMap->[$r][$c][ITEM] = $Things{ AMULET, };
    }
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
    my $lmc  = $ent->[LMC];
    my $dest = [ $lmc->[WHERE][PCOL] + $cols, $lmc->[WHERE][PROW] + $rows ];
    if (   $dest->[PCOL] < 0
        or $dest->[PCOL] >= MAP_COLS
        or $dest->[PROW] < 0
        or $dest->[PROW] >= MAP_ROWS) {
        return MOVE_FAILED, 0, 'PKC-0001';
    }
    relocate($ent, $dest) unless interact($ent, $dest);
    return MOVE_OK, $cost;
}

# TODO will need to honor FOV
# oh yeah TODO FOV support
sub move_examine {
    my $row = $Animates[HERO][LMC][WHERE][PROW];
    my $col = $Animates[HERO][LMC][WHERE][PCOL];
    print at(MSG_COL, MSG_ROW), clear_right, show_cursor,
      "\e[0;37;40m-- move cursor to view a cell. Esc exits --";
    while (1) {
        my $s = '';
        for my $i (ANI, ITEM) {
            my $x = $LMap->[$row][$col][$i];
            $s .= $x->[DISP] . $Descript{ $x->[WHAT] } . ' '
              if defined $x;
        }
        my $g = $LMap->[$row][$col][GROUND];
        $s .= $g->[DISP] . $Descript{ $g->[TYPE] } if defined $g;
        print at(STATUS_COL, STATUS_ROW), clear_right, $s,
          at(map { MAP_DISP_OFF + $_ } $col, $row);
        my $key = ReadKey(0);
        last if $key eq "\033" or $key eq 'q';
        my $distance = 1;
        if (ord $key < 97) {    # SHIFT moves faster
            $key      = lc $key;
            $distance = 5;
        }
        my $dir = $Examine_Offsets{$key} // next;
        $row = between(0, MAP_ROWS - 1, $row + $dir->[PROW] * $distance);
        $col = between(0, MAP_COLS - 1, $col + $dir->[PCOL] * $distance);
    }
    print hide_cursor, term_norm, at(STATUS_COL, STATUS_ROW), clear_right;
    show_top_message();
    show_status_bar();
    return MOVE_FAILED, 0;
}

sub move_nop { return MOVE_OK, DEFAULT_COST }

sub move_pickup {
    my $lmc = $Animates[HERO][LMC];
    if (!defined $lmc->[ITEM]) {
        return MOVE_FAILED, 0, 'PKC-0004';
    }
    log_message('got item');
    push $Animates[HERO][STASH][INVENTORY]->@*, $lmc->[ITEM];
    $lmc->[ITEM] = undef;
    return MOVE_OK, DEFAULT_COST;
}

sub move_player {
    my ($cols, $rows, $mvcost) = @_;
    sub { move_animate($Animates[HERO], $cols, $rows, $mvcost) }
}

# KLUGE improve or just require black bg terminal
sub paint_black {
    for my $r (0 .. $TRows) {
        print at(MSG_COL, $r), "\e[40m", " " x $TCols;
    }
    print term_norm;
}

sub redraw_movers {
    my %seen;
  CELL: for my $point (@RedrawA) {
        next if $seen{ $point->[PROW] . ',' . $point->[PCOL] }++;
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
    for my $key (keys %seen) {
        if ($seen{$key} > 1) {
            die "DBG STATS point redrawn twice $key $seen{$key}\n";
        }
    }
    @RedrawA = ();

    sleep($Redraw_Delay);
    %seen = ();

  CELL: for my $point (@RedrawB) {
        next if $seen{ $point->[PROW] . ',' . $point->[PCOL] }++;
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
    for my $key (keys %seen) {
        if ($seen{$key} > 1) {
            die "DBG STATS point redrawn twice $key $seen{$key}\n";
        }
    }
    @RedrawB = ();
}

sub refresh_board {
    print clear_screen;
    draw_level();
    show_top_message();
    show_status_bar();
}

sub relocate {
    my ($ent, $dest) = @_;
    my $src = $ent->[LMC][WHERE];
    push @RedrawA, $src;
    push @RedrawB, $dest;
    my $lmc = $LMap->[ $dest->[PROW] ][ $dest->[PCOL] ];
    $lmc->[ $ent->[TYPE] ] = $ent;
    undef $LMap->[ $src->[PROW] ][ $src->[PCOL] ][ $ent->[TYPE] ];
    $ent->[LMC] = $lmc;
    weaken $ent->[LMC];
}

sub restore_term {
    ReadMode 'restore';
    print term_norm, show_cursor, unalt_screen;
}

sub show_inventory {
    print clear_screen, show_cursor;
    my $inventory = $Animates[HERO][STASH][INVENTORY] // [];
    my $offset;
    my $s = "\e[0;37;40m";
    if ($inventory->@*) {
        while (my ($i, $item) = each $inventory->@*) {
            $s .=
                at(MSG_COL, MSG_ROW + $i)
              . $item->[DISP] . ' '
              . $Descript{ $item->[WHAT] };
        }
        $offset = $inventory->@* + 1;
    } else {
        $s .= at(MSG_COL, MSG_ROW) . "You have nothing in your inventory.";
        $offset = 2;
    }
    print $s, at(MSG_COL, MSG_ROW + $offset), "-- press Esc to continue --",
      term_norm;
    await_quit();
    print hide_cursor;
    refresh_board();
    return MOVE_FAILED, 0;
}

sub show_status_bar {
    print at(STATUS_COL, STATUS_ROW), "\e[0;37;40mLevel: ", $Level,
      term_norm;
    # TODO show "HP" maybe other things
    show_code();
}

sub update_hero {
    my ($cost, $ret);
    tcflush(STDIN_FILENO, TCIFLUSH);
    while (1) {
        my $key;
        while (1) {
            $key = ReadKey(0);
            last if exists $Key_Commands{$key};
            # TODO probably ignore for production release so typos don't
            # generate too much noise
            log_message(sprintf "Unknown command \\%03o", ord $key);
        }
        #warn "DBG key $key " . sprintf "%03o\n", ord $key;
        ($ret, $cost, my $code) = $Key_Commands{$key}->();
        confess "DBG no cost set?? $key" unless defined $cost;
        if ($ret != MOVE_FAILED) {
            clear_code();
            last;
        } else {
            log_code($code) if defined $code;
        }
    }
    $Turn_Count++;
    return $ret, $cost;
}

1;
__END__

=head1 NAME

Game::Xomb - a game featuring @ versus the Xarci Bedo

=head1 SYNOPSIS

This is a terminal-based game. Assuming that your development
tools, B<perl>, L<App::cpanminus>, and possibly also L<local::lib>
are installed and setup, in a suitable terminal install and run
the game via:

    cpanm Game::Xomb
    xomb

Use the C<?> key in game to show the help text (TODO implement), or
peruse the (TODO write) documentation below.

The L<xomb(1)> documentation details commands and other useful game
information; it should be available once the module is installed via:

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
