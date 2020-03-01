# -*- Perl -*-
#
# Game::Xomb - this is a terminal-based game. run the xomb(1) command
# that should be installed with this module to begin

package Game::Xomb;

our $VERSION = '0.03';

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

########################################################################
#
# CONSTANTS

# ANSI or XTerm control sequences - http://invisible-island.net/xterm/
#
# points are col,row (x,y) while terminal uses row,col hence the reverse
# argument order here. these two not-CONSTANTS move the cursor around
sub at              { "\e[" . $_[1] . ';' . $_[0] . 'H' }
sub at_col          { "\e[" . $_[0] . 'G' }
sub ALT_SCREEN ()   { "\e[?1049h" }
sub CLEAR_SCREEN () { "\e[1;1H\e[2J" }
sub CLEAR_RIGHT ()  { "\e[K" }
sub HIDE_CURSOR ()  { "\e[?25l" }     # this gets toggled on/off
sub HIDE_POINTER () { "\e[>3p" }      # no dead rat in way of things
sub SHOW_CURSOR ()  { "\e[?25h" }
sub TERM_NORM ()    { "\e[m" }
sub UNALT_SCREEN () { "\e[?1049l" }

# no more than this is used of the terminal (hey it's traditional)
sub MAX_ROWS () { 24 }
sub MAX_COLS () { 80 }

# where to put the map
sub MAP_COLS ()     { 78 }
sub MAP_ROWS ()     { 22 }
sub MAP_DISP_OFF () { 2 }

# where the message (top) and status (bottom) lines are
sub MSG_COL ()      { 0 }
sub MSG_ROW ()      { 0 }
sub MSG_MAX ()      { MAX_ROWS - 1 }
sub STATUS_COL ()   { 0 }
sub STATUS_ROW ()   { MAX_ROWS }
sub ERR_CODE_COL () { 70 }

# level map is row, col while points are [ col, row ]
sub PROW () { 1 }
sub PCOL () { 0 }

# a point in the LMC so Animates can find where they are at
sub WHERE () { 0 }
# GENUS is involved with interactions between things and where the
# thingy is slotted under the LMC
sub MINERAL () { 1 }    # walls, floors, etc
sub VEGGIE ()  { 2 }    # items
sub ANIMAL ()  { 3 }    # Animates

# and these are various species
sub HERO ()   { 0 }     # NOTE also used for Animates slot
sub AMULET () { 1 }     # goal of the game. technically is a vegetable
sub COLUMN () { 2 }     # copied from ZAngband, I like how they look
sub FLOOR ()  { 3 }
sub GATE ()   { 4 }     # stairs, done in the rogue 3.6 fashion
sub WALL ()   { 5 }     # regular wall

# for Animates (shared with Things for the first few slots)
sub GENUS ()      { 0 }
sub SPECIES ()    { 1 }
sub DISPLAY ()    { 2 }  # how to show 'em on the screen
sub STASH ()      { 3 }  # kitchen drawer, unique to species that use it
sub UPDATE ()     { 4 }  # what happens when their turn comes up
sub LMC ()        { 5 }  # link back to the level map
sub BLACK_SPOT () { 6 }  # if marked for death
sub ENERGY ()     { 7 }  # how long until their turn comes up

# player stash slots
sub LOOT () { 0 }

sub LOOT_MAX () { MAX_ROWS - 1 } # greedy packrat prevention

sub MOVE_LVLUP ()   { -1 }    # do not change
sub MOVE_FAILED ()  { 0 }
sub MOVE_LVLDOWN () { 1 }     # do not change
sub MOVE_OK ()      { 2 }

# energy constants
sub CAN_MOVE ()     { 0 }
sub DEFAULT_COST () { 10 }
sub DIAG_COST ()    { 14 }
sub NLVL_COST ()    { 15 }

our (@Animates, $Draw_Delay, $LMap, @RedrawA, @RedrawB, $TCols, $TRows);
our $Level      = 1;
our $Turn_Count = 0;          # player moves
our $Time_Spent = 0;          # energy spent

# by way of history '%' is what rogue (version 3.6) uses to display
# stairs (which really are actualy one-way optional chutes)
#             GENUS    SPECIES DISPLAY
# TODO how often is DISPLAY used? can we just stick term-norm after
# all those calls so not in memory here?
our %Things = (
    AMULET, [ VEGGIE,  AMULET, "\e[1;33;40m," . TERM_NORM ],
    COLUMN, [ MINERAL, COLUMN, "\e[0;37;40m\x{2B29}" . TERM_NORM ],
    FLOOR,  [ MINERAL, FLOOR,  "\e[2;37;40m." . TERM_NORM ],
    GATE,   [ MINERAL, GATE,   "\e[1;36;40m%" . TERM_NORM ],
    WALL,   [ MINERAL, WALL,   "\e[0;37;40m#" . TERM_NORM ],
);

# NOTE these must be short as they must all fit in status line and there
# can be three of them; see move_examine
our %Descript = (
    AMULET, 'Dragonstone', COLUMN, 'Column',
    FLOOR, 'Floor', HERO, 'You', GATE, 'Gate to another level',
    WALL, 'Wall',
);

# make the hero
$Animates[HERO]->@[ GENUS, SPECIES, DISPLAY, ENERGY, STASH, UPDATE ] = (
    ANIMAL, HERO, "\e[1;37;40m\@" . TERM_NORM,
    CAN_MOVE, [ [] ], \&update_hero
);

# handle bumps by GENUS (allow, fisticuffs, etc.) from move_animate
our %Interact_With = (
    ANIMAL,
    sub {
        my ($mover, $dpoint, $target, $cost) = @_;
        log_message('TODO fisticuffs');
        return MOVE_FAILED, $cost;
    },
    VEGGIE,
    sub {
        my ($mover, $dpoint, $target, $cost) = @_;
        log_message('DBG nice item here');
        relocate($mover, $dpoint);
        return MOVE_OK, $cost;
    },
    MINERAL,
    sub {
        my ($mover, $dpoint, $target, $cost) = @_;
        if ($target->[SPECIES] == WALL or $target->[SPECIES] == COLUMN) {
            return MOVE_FAILED, 0, 'PKC-0002';
        }
        log_message('DBG nice floor here');
        relocate($mover, $dpoint);
        return MOVE_OK, $cost;
    },
);

# for looking around with, see move_examine
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
    'h' => move_player_maker(-1, +0, DEFAULT_COST),    # left
    'j' => move_player_maker(+0, +1, DEFAULT_COST),    # down
    'k' => move_player_maker(+0, -1, DEFAULT_COST),    # up
    'l' => move_player_maker(+1, +0, DEFAULT_COST),    # right
    'y' => move_player_maker(-1, -1, DIAG_COST),
    'u' => move_player_maker(+1, -1, DIAG_COST),
    'b' => move_player_maker(-1, +1, DIAG_COST),
    'n' => move_player_maker(+1, +1, DIAG_COST),
    # TODO may need to interact with item/floor for status effects etc
    '.' => \&move_nop,                                 # rest
    ' ' => \&move_nop,                                 # also rest
    'v' => sub {
        log_message('Xomb v.' . $VERSION . ' PID ' . $$);
        return MOVE_FAILED, 0;
    },
    'x' => \&move_examine,
    '<' => sub {
        log_message('TODO check if have amulet');
        if ($Animates[HERO][LMC][MINERAL][SPECIES] != GATE) {
            log_message('not a gate!!');
            return MOVE_FAILED, 0, 'PKC-0037';
        }
        log_message('Gate activated.');
        return MOVE_LVLUP, NLVL_COST;
    },
    '>' => sub {
        if ($Animates[HERO][LMC][MINERAL][SPECIES] != GATE) {
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
        # TODO these should be log codes so not spamming messages
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
    print "\n", at_col(0), CLEAR_RIGHT;
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
    print CLEAR_SCREEN, at(0, 2), <<"BOSS_SCREEN", "\n:", SHOW_CURSOR;
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
    print HIDE_CURSOR;
    refresh_board();
}

sub can_win {
    my $loot = $Animates[HERO][STASH][LOOT];
    for my $item ($loot->@*) {
        return 1 if $item->[SPECIES] == AMULET;
    }
    return 0;
}

{
    my $code_clear = 1;
    my $last_code  = '';
    my @log;

    sub clear_code {
        print at(ERR_CODE_COL, STATUS_ROW), CLEAR_RIGHT;
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
          TERM_NORM;
    }

    sub show_messages {
        print CLEAR_SCREEN, SHOW_CURSOR, "\e[0;37;40m";
        while (my ($i, $msg) = each @log) {
            print at(MSG_COL, MSG_ROW + $i), $msg;
        }
        print at(MSG_COL, MSG_ROW + @log), "-- press Esc to continue --",
          TERM_NORM;
        await_quit();
        print HIDE_CURSOR;
        refresh_board();
    }

    sub show_top_message {
        my $msg = @log ? $log[-1] : '';
        print at(MSG_COL, MSG_ROW), CLEAR_RIGHT, "\e[0;37;40m", $msg, TERM_NORM;
    }
}

sub draw_level {
    my $s = '';
    for my $rownum (0 .. MAP_ROWS - 1) {
        $s .= at(MAP_DISP_OFF, MAP_DISP_OFF + $rownum);
        # TODO may need unicode indicator where ani+item or ani+gate present?
        # or gates are items that interact with items thrown on them?
        for my $lmc ($LMap->[$rownum]->@*) {
            if (defined $lmc->[ANIMAL]) {
                $s .= $lmc->[ANIMAL][DISPLAY];
            } elsif (defined $lmc->[VEGGIE]) {
                $s .= $lmc->[VEGGIE][DISPLAY];
            } else {
                $s .= $lmc->[MINERAL][DISPLAY];
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

    ($Draw_Delay) = @_;

    ReadMode 'raw';
    $SIG{$_} = \&bail_out for qw(INT HUP TERM PIPE QUIT USR1 USR2 __DIE__);
    $SIG{CONT}  = \&refresh_board;
    $SIG{WINCH} = sub {
        log_message('The terminal is too small!') if bad_terminal();
        refresh_board();
    };
    STDOUT->autoflush(1);

    generate_level();

    print TERM_NORM, ALT_SCREEN, HIDE_CURSOR, HIDE_POINTER, CLEAR_SCREEN;
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
    print "\n", at_col(0), CLEAR_RIGHT, $msg,
      " ($Turn_Count, $Time_Spent)\n",
      CLEAR_RIGHT;
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
    $LMap->[$r][$c][ANIMAL] = $Animates[HERO];
    $Animates[HERO][LMC] = $LMap->[$r][$c];
    weaken $Animates[HERO][LMC];
    $c = $r = 2;
    $LMap->[$r][$c][MINERAL] = $Things{ GATE, };

    if ($Level == 1) {
        $c                       = $r = 3;
        $LMap->[$r][$c][MINERAL] = $Things{ WALL, };
        $c                       = $r = 4;
        $LMap->[$r][$c][MINERAL] = $Things{ COLUMN, };
    } else {
        $c = $r = 5;
        $LMap->[$r][$c][VEGGIE] = $Things{ AMULET, };
    }
}

sub move_animate {
    my ($ent, $cols, $rows, $cost) = @_;
    my $lmc  = $ent->[LMC];
    my $dcol = $lmc->[WHERE][PCOL] + $cols;
    my $drow = $lmc->[WHERE][PROW] + $rows;
    return MOVE_FAILED, 0, 'PKC-0001'
      if $dcol < 0
      or $dcol >= MAP_COLS
      or $drow < 0
      or $drow >= MAP_ROWS;
    for my $i (ANIMAL, VEGGIE, MINERAL) {
        my $target = $LMap->[$drow][$dcol][$i];
        if (defined $target) {
            @_ = ($ent, [ $dcol, $drow ], $target, $cost);
            goto $Interact_With{ $target->[GENUS] }->&*;
        }
    }
    die "DBG no interaction at $dcol,$drow ??\n";
}

# TODO will need to honor FOV
# oh yeah TODO FOV support
sub move_examine {
    my $row = $Animates[HERO][LMC][WHERE][PROW];
    my $col = $Animates[HERO][LMC][WHERE][PCOL];
    print at(MSG_COL, MSG_ROW), CLEAR_RIGHT, SHOW_CURSOR,
      "\e[0;37;40m-- move cursor to view a cell. Esc exits --";
    while (1) {
        my $s = '';
        for my $i (ANIMAL, VEGGIE) {
            my $x = $LMap->[$row][$col][$i];
            $s .= $x->[DISPLAY] . $Descript{ $x->[SPECIES] } . ' '
              if defined $x;
        }
        my $g = $LMap->[$row][$col][MINERAL];
        $s .= $g->[DISPLAY] . $Descript{ $g->[SPECIES] } if defined $g;
        print at(STATUS_COL, STATUS_ROW), CLEAR_RIGHT, $s,
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
    print HIDE_CURSOR, TERM_NORM, at(STATUS_COL, STATUS_ROW), CLEAR_RIGHT;
    show_top_message();
    show_status_bar();
    return MOVE_FAILED, 0;
}

sub move_nop { return MOVE_OK, DEFAULT_COST }

sub move_pickup {
    my $lmc = $Animates[HERO][LMC];
    if (!defined $lmc->[VEGGIE]) {
        return MOVE_FAILED, 0, 'PKC-0004';
    }
    log_message('got item');
    my $loot = $Animates[HERO][STASH][LOOT];
    return MOVE_FAILED, 0, 'PKC-0005' if $loot->@* >= LOOT_MAX;
    push $loot->@*, $lmc->[VEGGIE];
    $lmc->[VEGGIE] = undef;
    return MOVE_OK, DEFAULT_COST;
}

sub move_player_maker {
    my ($cols, $rows, $mvcost) = @_;
    sub { move_animate($Animates[HERO], $cols, $rows, $mvcost) }
}

# KLUGE improve or just require black bg terminal
sub paint_black {
    for my $r (0 .. $TRows) {
        print at(MSG_COL, $r), "\e[40m", " " x $TCols;
    }
    print TERM_NORM;
}

sub redraw_movers {
    my %seen;
  CELL: for my $point (@RedrawA) {
        next if $seen{ $point->[PROW] . ',' . $point->[PCOL] }++;
        for my $i (ANIMAL, VEGGIE) {
            my $ent = $LMap->[ $point->[PROW] ][ $point->[PCOL] ][$i];
            if (defined $ent and !$ent->[BLACK_SPOT]) {
                print at(map { MAP_DISP_OFF + $_ } $point->@*), $ent->[DISPLAY];
                next CELL;
            }
        }
        print at(map { MAP_DISP_OFF + $_ } $point->@*),
          $LMap->[ $point->[PROW] ][ $point->[PCOL] ][MINERAL][DISPLAY];
    }
    for my $key (keys %seen) {
        if ($seen{$key} > 1) {
            die "DBG STATS point redrawn twice $key $seen{$key}\n";
        }
    }
    @RedrawA = ();

    sleep($Draw_Delay);
    %seen = ();

  CELL: for my $point (@RedrawB) {
        next if $seen{ $point->[PROW] . ',' . $point->[PCOL] }++;
        for my $i (ANIMAL, VEGGIE) {
            my $ent = $LMap->[ $point->[PROW] ][ $point->[PCOL] ][$i];
            if (defined $ent and !$ent->[BLACK_SPOT]) {
                print at(map { MAP_DISP_OFF + $_ } $point->@*), $ent->[DISPLAY];
                next CELL;
            }
        }
        print at(map { MAP_DISP_OFF + $_ } $point->@*),
          $LMap->[ $point->[PROW] ][ $point->[PCOL] ][MINERAL][DISPLAY];
    }
    for my $key (keys %seen) {
        if ($seen{$key} > 1) {
            die "DBG STATS point redrawn twice $key $seen{$key}\n";
        }
    }
    @RedrawB = ();
}

sub refresh_board {
    print CLEAR_SCREEN;
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
    $lmc->[ANIMAL] = $ent;
    undef $LMap->[ $src->[PROW] ][ $src->[PCOL] ][ANIMAL];
    $ent->[LMC] = $lmc;
    weaken $ent->[LMC];
}

sub restore_term {
    ReadMode 'restore';
    print TERM_NORM, SHOW_CURSOR, UNALT_SCREEN;
}

sub show_inventory {
    print CLEAR_SCREEN, SHOW_CURSOR;
    my $loot = $Animates[HERO][STASH][LOOT];
    my $offset;
    my $s = "\e[0;37;40m";
    if ($loot->@*) {
        while (my ($i, $item) = each $loot->@*) {
            $s .=
                at(MSG_COL, MSG_ROW + $i)
              . $item->[DISPLAY] . ' '
              . $Descript{ $item->[SPECIES] };
        }
        $offset = $loot->@* + 1;
    } else {
        $s .= at(MSG_COL, MSG_ROW) . "You have nothing in your inventory.";
        $offset = 2;
    }
    print $s, at(MSG_COL, MSG_ROW + $offset), "-- press Esc to continue --",
      TERM_NORM;
    await_quit();
    print HIDE_CURSOR;
    refresh_board();
    return MOVE_FAILED, 0;
}

sub show_status_bar {
    print at(STATUS_COL, STATUS_ROW), "\e[0;37;40mLevel: ", $Level,
      TERM_NORM;
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
