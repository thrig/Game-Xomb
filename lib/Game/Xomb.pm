# -*- Perl -*-
#
# Game::Xomb - this is a terminal-based roguelike. run the xomb(1)
# command that should be installed with this module to start a game

package Game::Xomb;

our $VERSION = '0.04';

use 5.24.0;
use warnings;
use open IO => ':locale';
use Carp qw(croak confess);    # DBG
use Data::Dumper;              # DBG
use List::Util qw(min);
use POSIX qw(STDIN_FILENO TCIFLUSH tcflush);
use Scalar::Util qw(weaken);
use Term::ReadKey qw(GetTerminalSize ReadKey ReadMode);
use Time::HiRes qw(sleep);

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
sub CLEAR_LINE ()   { "\e[2K" }
sub CLEAR_RIGHT ()  { "\e[K" }
sub CLEAR_SCREEN () { "\e[1;1H\e[2J" }
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
sub MSG_COL ()       { 1 }
sub MSG_ROW ()       { 1 }
sub MSG_MAX ()       { MAX_ROWS - 2 }
sub STATUS_COL ()    { 1 }
sub STATUS_ROW ()    { MAX_ROWS }
sub CELL_HP_COL ()   { 11 }
sub CELL_INFO_COL () { 64 }
sub ERR_CODE_COL ()  { 69 }

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
sub HERO ()   { 0 }     # NOTE also used for @Animates slot
sub AMULET () { 1 }     # goal of the game. technically is a vegetable
sub COLUMN () { 2 }     # copied from ZAngband, I like how they look
sub FLOOR ()  { 3 }
sub GATE ()   { 4 }     # stairs, in the rogue 3.6 fashion
sub WALL ()   { 5 }     # regular wall
sub GEM ()    { 6 }
sub HOLE ()   { 7 }     # quicker than gate but causes damage
sub TROLL ()  { 8 }

# for ANIMALS (shared with VEGGIES and MINERALS for the first few slots)
sub GENUS ()      { 0 }
sub SPECIES ()    { 1 }
sub DISPLAY ()    { 2 }  # how to show 'em on the screen
sub STASH ()      { 3 }  # kitchen drawer, unique to species that use it
sub UPDATE ()     { 4 }  # what happens when their turn comes up
sub LMC ()        { 5 }  # link back to the level map
sub BLACK_SPOT () { 6 }  # if marked for death
sub ENERGY ()     { 7 }  # how long until their next update call

# player stash slots
sub HITPOINTS () { 0 }
sub LOOT ()      { 1 }

sub START_HP () { 100 }             # see Damage_From and related
sub LOOT_MAX () { MAX_ROWS - 2 }    # greedy packrat prevention

sub MOVE_LVLUP ()   { -1 }          # NOTE tied to level change math
sub MOVE_FAILED ()  { 0 }
sub MOVE_LVLDOWN () { 1 }           # NOTE tied to level change math
sub MOVE_OK ()      { 2 }

# energy constants
sub CAN_MOVE ()     { 0 }
sub DEFAULT_COST () { 10 }
sub DIAG_COST ()    { 14 }
sub NLVL_COST ()    { 16 }

our (@Animates, $Draw_Delay, $LMap, @RedrawA, @RedrawB, $TCols, $TRows);
our $Level      = 1;
our $Turn_Count = 0;                # player moves
our $Time_Spent = 0;                # energy spent

# these are "class objects" by default; in other words there is only one
# WALL in all WALL cells unless efforts are taken otherwise (see reify
# or the various make_* routines)
#             GENUS    SPECIES DISPLAY
our %Things = (
    AMULET, [ VEGGIE,  AMULET, "\e[1;33;40m," . TERM_NORM ],
    COLUMN, [ MINERAL, COLUMN, "\e[0;37;40m\x{2B29}" . TERM_NORM ],
    FLOOR,  [ MINERAL, FLOOR,  "\e[2;37;40m." . TERM_NORM ],
    GATE,   [ MINERAL, GATE,   "\e[1;36;40m%" . TERM_NORM ],
    HOLE,   [ MINERAL, HOLE,   "\e[0;37;40m " . TERM_NORM ],
    WALL,   [ MINERAL, WALL,   "\e[0;37;40m#" . TERM_NORM ],
);

# NOTE these must be short as they must all fit in status line and there
# can be three of them; see move_examine
our %Descript = (
    AMULET, 'Dragonstone',   COLUMN, 'Column',
    FLOOR,  'Floor',         GATE,   'Stair',
    GEM,    'Gemstone',      HERO,   'You',
    TROLL,  'Railgun Tower', WALL,   'Wall',
);

our %Damage_From =
  ('falling' => sub { 3 + int(rand 6 + rand 6 + rand 6) },);

# what happens when moving into the given GENUS (allow the move,
# fisticuffs, etc.). called from move_animate
our %Bump_Into = (
    ANIMAL,
    sub {
        my ($mover, $dpoint, $target, $cost) = @_;
        log_message('TODO implement fisticuffs');
        return MOVE_FAILED, $cost;
    },
    VEGGIE,
    sub {
        my ($mover, $dpoint, $target, $cost) = @_;
        apply_passives($mover, $cost / 2, 0);
        relocate($mover, $dpoint);
        apply_passives($mover, $cost / 2, 1);
        return MOVE_OK, $cost;
    },
    MINERAL,
    sub {
        my ($mover, $dpoint, $target, $cost) = @_;
        if ($target->[SPECIES] == WALL or $target->[SPECIES] == COLUMN) {
            return MOVE_FAILED, 0, 'PKC-0002';
        }
        if ($target->[SPECIES] == HOLE) {
            return MOVE_FAILED, 0
              if nope_regarding("Falling may cause damage");
            apply_passives($mover, $cost / 2, 0);
            log_code('PKC-0098');
            apply_damage($mover, 'falling');
            return MOVE_LVLDOWN, $cost;
        }
        apply_passives($mover, $cost / 2, 0);
        relocate($mover, $dpoint);
        apply_passives($mover, $cost / 2, 1);
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
    'e' => sub { clear_code(); return MOVE_FAILED, 0 },
    'g' => \&move_pickup,
    ',' => \&move_pickup,
    'i' => \&manage_inventory,
    'd' => \&move_drop,
    'h' => move_player_maker(-1, +0, DEFAULT_COST),       # left
    'j' => move_player_maker(+0, +1, DEFAULT_COST),       # down
    'k' => move_player_maker(+0, -1, DEFAULT_COST),       # up
    'l' => move_player_maker(+1, +0, DEFAULT_COST),       # right
    'y' => move_player_maker(-1, -1, DIAG_COST),
    'u' => move_player_maker(+1, -1, DIAG_COST),
    'b' => move_player_maker(-1, +1, DIAG_COST),
    'n' => move_player_maker(+1, +1, DIAG_COST),
    '.' => \&move_nop,                                    # rest
    ' ' => \&move_nop,                                    # also rest
    'v' => sub { log_message('.xomb ' . $VERSION); return MOVE_FAILED, 0 },
    'x' => \&move_examine,
    '<' => sub {
        return MOVE_FAILED, 0, 'PKC-0004'
          if $Animates[HERO][LMC][MINERAL][SPECIES] != GATE;
        return MOVE_FAILED, 0, 'PKC-0010' unless has_amulet();
        log_message('Gate activated.');
        return MOVE_LVLUP, NLVL_COST;
    },
    '>' => sub {
        return MOVE_FAILED, 0, 'PKC-0004'
          if $Animates[HERO][LMC][MINERAL][SPECIES] != GATE;
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
    "\003" => sub { return MOVE_FAILED, 0, 'PKC-1203' },    # <C-c>
    "\014" => sub {                                         # <C-l>
        refresh_board();
        return MOVE_FAILED, 0;
    },
    "\032" => sub { return MOVE_FAILED, 0, 'PKC-1220' },    # <C-z>
    "\033" => sub { return MOVE_FAILED, 0 },
);

########################################################################
#
# SUBROUTINES

sub apply_damage {
    my ($ani, $cause) = @_;
    warn "HP $ani->[STASH][HITPOINTS]\n";
    $ani->[STASH][HITPOINTS] -= $Damage_From{$cause}->();
    warn "HP $ani->[STASH][HITPOINTS]\n";
    if ($ani->[STASH][HITPOINTS] <= 0) {
        if ($ani->[SPECIES] == HERO) {
            log_code('PKC-0099');
            $ani->[DISPLAY] = "\e[1;37;40m\&" . TERM_NORM;
            $ani->[UPDATE]  = \&update_gameover;
        } else {
            $ani->[BLACK_SPOT] = 1;
        }
    }
}

# TODO would a not-ANIMAL ever get into Animates and need UPDATE for
# that instead of how it is used here?
sub apply_passives {
    my ($ani, $duration, $newcell) = @_;
    for my $i (grep defined $ani->[LMC][$_], VEGGIE, MINERAL) {
        my $fn = $ani->[LMC][$i][UPDATE];
        $fn->($ani, $ani->[LMC][$i], $duration, $newcell) if defined $fn;
    }
}

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

{
    my @log;

    sub clear_code     { print at(ERR_CODE_COL, STATUS_ROW), CLEAR_RIGHT }
    sub clear_messages { @log = () }

    sub log_code {
        print at(ERR_CODE_COL, STATUS_ROW), "\e[0;31;40m*", $_[0], '*',
          TERM_NORM;
    }

    sub log_message {
        my ($message) = @_;
        while (@log >= MSG_MAX) { shift @log }
        push @log, $message;
        show_top_message();
    }

    sub show_messages {
        print SHOW_CURSOR, "\e[0;37;40m";
        while (my ($i, $message) = each @log) {
            print at(MSG_COL, MSG_ROW + $i), CLEAR_RIGHT, $message;
        }
        print at(MSG_COL, MSG_ROW + @log), CLEAR_RIGHT,
          "-- press Esc to continue --",
          TERM_NORM;
        await_quit();
        print HIDE_CURSOR;
        refresh_board(scalar @log);
    }

    # much of the complication is to dim progressively older information
    sub show_top_message {
        state $last_message = '';
        state($last_call, $calls);
        my $message = @log ? $log[-1] : '';
        if ($message ne $last_message) {
            $last_message = $message;
            $calls        = $last_call = 0;
        }
        my $d      = $calls - $last_call;
        my $weight = $d == 0 ? 1 : $d == 1 ? 0 : 2;
        print at(MSG_COL, MSG_ROW), CLEAR_RIGHT, "\e[$weight;37;40m", $message,
          TERM_NORM;
        $calls++;
    }
}

# for status bar so player can see what is in cell they are in
sub display_cellobjs {
    at(CELL_INFO_COL, STATUS_ROW) . "\e[0;37;40m[" . TERM_NORM . join(
        '',
        map {
            my $obj = $Animates[HERO][LMC][$_];
            (defined $obj and $obj->@*) ? $obj->[DISPLAY] : ' '
        } MINERAL,
        VEGGIE
      )
      . "\e[0;37;40m]"
      . TERM_NORM;
}

sub display_hitpoints {
    my $hp = $Animates[HERO][STASH][HITPOINTS];
    $hp = 0 if $hp < 0;
    log_message('Shield module failure.') if $hp == 0;
    # TODO probably want options to set red/yellow thresholds
    my $color = $hp < 25 ? 31 : $hp < 50 ? 33 : 32;
    my $ticks = int $hp / 2;
    my $hpbar = '=' x $ticks;
    $hpbar .= '-' if ($ticks & 1) == 1;
    my $len = length $hpbar;
    $hpbar .= ' ' x (50 - $len) if $len < 50;
    at(CELL_HP_COL, STATUS_ROW)
      . "\e[0;37;40m[\e[1;${color}m"
      . $hpbar
      . "\e[0;37m]"
      . TERM_NORM;
}

sub draw_level {
    my ($lines) = @_;
    $lines //= MAP_ROWS;
    my $s = '';
    for my $rownum (0 .. MAP_ROWS - 1) {
        $s .= at(MAP_DISP_OFF, MAP_DISP_OFF + $rownum) . CLEAR_LINE;
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
        last if $rownum > $lines;
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

    make_hero();
    generate_level();

    print TERM_NORM, ALT_SCREEN, HIDE_CURSOR, HIDE_POINTER, CLEAR_SCREEN;
    log_message('Welcome to .xomb');
    draw_level();
    show_status_bar();

    while (1) {
        my $min_cost = min map { $_->[ENERGY] } @Animates;
        $Time_Spent += $min_cost;
        warn('DBG min energy ' . $min_cost . "\n");

        my @movers;
        for my $ani (@Animates) {
            $ani->[ENERGY] -= $min_cost;
            push @movers, $ani if $ani->[ENERGY] <= CAN_MOVE;
        }
        fisher_yates_shuffle \@movers if @movers > 1;

        my $new_level = 0;
        for my $ani (@movers) {
            #use Data::Dumper; warn Dumper $ani; # DBG
            my ($status, $cost) = $ani->[UPDATE]->($ani);
            $ani->[ENERGY] += $cost;
            $new_level = $status
              if $status == MOVE_LVLDOWN or $status == MOVE_LVLUP;
            die "DBG bad cost $cost\n" . Dumper($ani) if $cost <= CAN_MOVE;
        }

        if ($new_level != 0) {
            $Level += $new_level;
            if ($Level <= 0) {
                if (has_amulet()) {
                    game_over('Victory TODO improve this', 0);
                } else {
                    log_message('You need the Dragonstone to escape.');
                    $Level = 1;
                    goto CLEANUP;
                }
            }
            generate_level();
            draw_level();
            show_status_bar();
            next;
        }

      CLEANUP:
        @Animates = grep { !$_->[BLACK_SPOT] } @Animates;
        redraw_movers();
    }
}

sub game_over {
    my ($message, $code) = @_;
    $code //= 1;
    restore_term();
    print "\n", at_col(0), CLEAR_RIGHT, $message,
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
            make_gem($c, $r) if rand() < 0.2;
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

    # TODO level gen will need to figure out where the player is and
    # make that square part of the pathable map (or allow digging...)
    # this thing resets their position due to above setup
    if ($Level == 1) {
        $c                       = $r = 3;
        $LMap->[$r][$c][MINERAL] = $Things{ WALL, };
        $c                       = $r = 4;
        $LMap->[$r][$c][MINERAL] = $Things{ COLUMN, };
        $c                       = $r = 5;
        $LMap->[$r][$c][MINERAL] = $Things{ HOLE, };
    } elsif (!has_amulet()) {
        $c = $r = 5;
        $LMap->[$r][$c][VEGGIE] = $Things{ AMULET, };
    }

    make_monster(
        0, 3,
        species => TROLL,
        hp      => 200,
        energy  => 10,
        display => "\e[1;38;2;255;153;0;40mT" . TERM_NORM
    );

    # how to attach passive functions to (now) specific cell objects
    #reify($LMap->[0][0], MINERAL, passive_msg_maker('bla blah I'));
    #reify($LMap->[0][1], MINERAL, passive_msg_maker('bla blah A'));
    #reify($LMap->[1][0], MINERAL, passive_msg_maker('bla blah B'));
    #reify($LMap->[0][4], MINERAL, passive_msg_maker('oneshot 1', 1));
    #reify($LMap->[4][0], MINERAL, passive_msg_maker('oneshot 2', 1));
}

sub has_amulet {
    for my $item ($Animates[HERO][STASH][LOOT]->@*) {
        return 1 if $item->[SPECIES] == AMULET;
    }
    return 0;
}

sub make_gem {
    my ($col, $row) = @_;
    my @colors = qw/32 35 36/;
    my ($name, $value);
    if (int rand 16 == 0) {
        $name  = "Bloodstone";
        $value = 43 + int(rand 4 + rand 4 + rand 4);
    } elsif (int rand 8 == 0) {
        $name  = "Sunstone";
        $value = 23 + int(rand 4 + rand 4 + rand 4);
    } else {
        $name  = "Moonstone";
        $value = 3 + int(rand 4 + rand 4 + rand 4);
    }
    my @adj = qw/Imperial Mystic Rose Smoky Warped/;
    if (int rand 3 == 0) {
        $name = $adj[ rand @adj ] . ' ' . $name;
        $value += 11 + int rand 10;
    }
    my $gem;
    $gem->@[ GENUS, SPECIES, DISPLAY, STASH ] = (
        VEGGIE, GEM,
        "\e[0;" . $colors[ rand @colors ] . ';40m*' . TERM_NORM,
        [ $name, $value ]
    );
    $LMap->[$row][$col][VEGGIE] = $gem;
}

sub make_hero {
    $Animates[HERO]->@[ GENUS, SPECIES, DISPLAY, ENERGY, STASH, UPDATE ] = (
        ANIMAL, HERO, "\e[1;37;40m\@" . TERM_NORM,
        CAN_MOVE, [ START_HP, [] ],
        \&update_hero
    );
}

sub make_monster {
    my ($col, $row, %params) = @_;
    my $monst;
    $monst->@[ GENUS, SPECIES, DISPLAY, ENERGY, STASH, UPDATE, LMC ] = (
        ANIMAL, $params{species}, $params{display}, $params{energy},
        [ $params{hp}, [] ],
        \&update_monster, [ $col, $row ]
    );
    push @Animates, $monst;
    $LMap->[$row][$col][ANIMAL] = $monst;
    weaken $monst->[LMC];
}

sub manage_inventory {
    my ($command, $message) = @_;
    print SHOW_CURSOR;
    my $loot = $Animates[HERO][STASH][LOOT];
    my $offset;
    my $s        = "\e[0;37;40m";
    my $has_loot = 0;
    if ($loot->@*) {
        $has_loot = 1;
        my $label = 'A';
        while (my ($i, $item) = each $loot->@*) {
            $s .=
                at(MSG_COL, MSG_ROW + $i)
              . CLEAR_RIGHT
              . $label++ . ') '
              . $item->[DISPLAY]
              . " \e[0;37;40m";
            if ($item->[SPECIES] == GEM) {
                $s .= join "\t", $item->[STASH]->@[ 1, 0 ];
            } else {
                $s .= $Descript{ $item->[SPECIES] };
            }
        }
        $offset = $loot->@*;
    } else {
        $s .= at(MSG_COL, MSG_ROW) . CLEAR_RIGHT . "Inventory is empty.";
        $offset = 1;
    }
    $s .= at(MSG_COL, MSG_ROW + $offset) . CLEAR_RIGHT . '-- ';
    if ($message) {
        $s .= $message;
    } else {
        $s .= 'press Esc to continue';
        $s .= ' or (d)rop' if $has_loot;
    }
    print $s, ' --', TERM_NORM;
  CMD: while (1) {
        my $key = $command // ReadKey(0);
        last if $key eq "\033" or $key eq 'q';
        undef $command;
        next unless $has_loot;
        if ($key eq 'd') {
            if (!defined $message) {
                print at(MSG_COL, MSG_ROW + $offset), CLEAR_RIGHT,
                  "\e[0;37;40m-- drop item L)able or Esc to exit --", TERM_NORM;
            }
            if (defined $Animates[HERO][LMC][VEGGIE]) {
                log_code('PKC-0104');
                last CMD;
            }
            while (1) {
                my $drop = ReadKey(0);
                last CMD if $drop eq "\033" or $drop eq 'q';
                if ($drop =~ m/^[A-X]$/) {    # NOTE related to LOOT_MAX
                    my $i = ord($drop) - 65;
                    if ($i < $loot->@*) {
                        $Animates[HERO][LMC][VEGGIE] = splice $loot->@*, $i, 1;
                        print display_cellobjs();
                        last CMD;
                    }
                }
            }
        }
    }
    print HIDE_CURSOR;
    refresh_board(MSG_ROW + $offset);
    return MOVE_FAILED, 0;
}

sub move_animate {
    my ($ani, $cols, $rows, $cost) = @_;
    my $lmc  = $ani->[LMC];
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
            @_ = ($ani, [ $dcol, $drow ], $target, $cost);
            goto $Bump_Into{ $target->[GENUS] }->&*;
        }
    }
    die "DBG no interaction at $dcol,$drow ??\n";
}

sub move_drop {
    return MOVE_FAILED, 0, 'PKC-0104'
      if defined $Animates[HERO][LMC][VEGGIE];
    return MOVE_FAILED, 0, 'PKC-0112'
      unless $Animates[HERO][STASH][LOOT]->@*;
    @_ = ('d', 'drop item L)abel or Esc to exit');
    goto &manage_inventory;
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
            $s .=
                $x->[DISPLAY]
              . " \e[0;37;40m"
              . $Descript{ $x->[SPECIES] }
              . TERM_NORM . ' '
              if defined $x;
        }
        my $g = $LMap->[$row][$col][MINERAL];
        $s .=
            $g->[DISPLAY]
          . " \e[0;37;40m"
          . $Descript{ $g->[SPECIES] }
          . TERM_NORM
          if defined $g;
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
    return MOVE_FAILED, 0, int rand 5000 == 0 ? 'PKC-1202' : ();
}

sub move_nop {
    apply_passives($Animates[HERO], DEFAULT_COST, 0);
    return MOVE_OK, DEFAULT_COST;
}

sub move_pickup {
    my $lmc = $Animates[HERO][LMC];
    if (!defined $lmc->[VEGGIE]) {
        return MOVE_FAILED, 0, 'PKC-0101';
    }
    log_message('got item');
    my $loot = $Animates[HERO][STASH][LOOT];
    return MOVE_FAILED, 0, 'PKC-0102' if $loot->@* >= LOOT_MAX;
    push $loot->@*, $lmc->[VEGGIE];
    $lmc->[VEGGIE] = undef;
    print display_cellobjs();
    return MOVE_OK, DEFAULT_COST;
}

sub move_player_maker {
    my ($cols, $rows, $mvcost) = @_;
    sub {
        my @ret = move_animate($Animates[HERO], $cols, $rows, $mvcost);
        show_top_message();
        print display_cellobjs();
        return @ret;
    }
}

sub nope_regarding {
    my ($message) = @_;
    print at(MSG_COL, MSG_ROW), "\e[1;35;40m", $message, ' (Y/N)',
      TERM_NORM;
    my $key;
    while (1) {
        $key = ReadKey(0);
        return 0 if $key eq 'Y';
        return 1 if $key eq "\033" or $key eq 'N';
    }
}

sub redraw_movers {
    my %seen;
  CELL: for my $point (@RedrawA) {
        next if $seen{ $point->[PROW] . ',' . $point->[PCOL] }++;
        for my $i (ANIMAL, VEGGIE) {
            my $ani = $LMap->[ $point->[PROW] ][ $point->[PCOL] ][$i];
            if (defined $ani and $ani->@*) {
                print at(map { MAP_DISP_OFF + $_ } $point->@*), $ani->[DISPLAY];
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
            my $ani = $LMap->[ $point->[PROW] ][ $point->[PCOL] ][$i];
            if (defined $ani and $ani->@*) {
                print at(map { MAP_DISP_OFF + $_ } $point->@*), $ani->[DISPLAY];
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
    my ($lines) = @_;
    print CLEAR_SCREEN unless $lines;
    draw_level($lines);
    show_top_message();
    show_status_bar() if !defined $lines or ($lines and $lines >= STATUS_ROW);
}

sub relocate {
    my ($ani, $dest) = @_;
    my $src = $ani->[LMC][WHERE];
    push @RedrawA, $src;
    push @RedrawB, $dest;
    my $dest_lmc = $LMap->[ $dest->[PROW] ][ $dest->[PCOL] ];
    $dest_lmc->[ANIMAL] = $ani;
    undef $LMap->[ $src->[PROW] ][ $src->[PCOL] ][ANIMAL];
    $ani->[LMC] = $dest_lmc;
    weaken $ani->[LMC];
}

sub restore_term {
    ReadMode 'restore';
    print TERM_NORM, SHOW_CURSOR, UNALT_SCREEN;
}

sub show_status_bar {
    print at(STATUS_COL, STATUS_ROW), "\e[0;37;40mLevel: ", $Level,
      display_hitpoints(), display_cellobjs();
}

sub passive_msg_maker {
    my ($message, $oneshot) = @_;
    sub {
        my ($ani, $obj, $duration, $newcell) = @_;
        warn sprintf "APAMM %s %d,%d\n", $ani->[DISPLAY],
          $ani->[LMC][WHERE]->@*;
        if ($newcell) {
            log_message($message);
            undef $obj->[UPDATE] if $oneshot;
        }
    }
}

# similar to tu'a in Lojban
sub reify {
    my ($lmc, $i, $update) = @_;
    $lmc->[$i] = [ $lmc->[$i]->@* ];
    $lmc->[$i][UPDATE] = $update if defined $update;
}

sub update_gameover {
    state $count = 0;
    my $key = ReadKey(0);
    if ($count > 2) {
        log_message('-- press Esc to continue --');
        game_over('Congratulations! You have died.')
          if $key eq "\033" or $key eq 'q';
    } else {
        log_message('Communication lost with remote unit.');
    }
    $count++;
    return MOVE_OK, DEFAULT_COST;
}

sub update_hero {
    my ($cost, $ret);
    tcflush(STDIN_FILENO, TCIFLUSH);
    while (1) {
        my $key;
        while (1) {
            $key = ReadKey(0);
            last if exists $Key_Commands{$key};
            log_message(sprintf "DBG unknown key \\%03o", ord $key);
        }
        #warn "DBG key $key " . sprintf "%03o\n", ord $key;
        ($ret, $cost, my $code) = $Key_Commands{$key}->();
        confess "DBG no cost set?? $key" unless defined $cost;
        log_code($code) if defined $code;
        last            if $ret != MOVE_FAILED;
    }
    $Turn_Count++;
    return $ret, $cost;
}

sub update_monster {
    warn "monster does something\n";
    return MOVE_OK, DEFAULT_COST / 2;
}

1;
__END__

=head1 NAME

Game::Xomb - a game featuring @ versus the Xarci Bedo

=head1 SYNOPSIS

This is a terminal-based roguelike. Assuming that the development
tools, L<perl(1)>, L<App::cpanminus>, and possibly also
L<local::lib> are installed and setup, in a suitable terminal
install and run the game via:

    cpanm Game::Xomb
    xomb

Use the C<?> key in game to show the help text (TODO implement). The
L<xomb(1)> documentation details commands and other useful game
information; it should be available once the module is installed via:

    perldoc xomb

=head1 DESCRIPTION

The implementation makes heavy use of arrays (and references to such)
with particular named slots (accessed via obscure CONSTANTS) standing in
(however poorly) for a more typical (if slower) Object Oriented design.
There is some overlap between the Animal, Vegetable, and Mineral arrays
(called "polymorphism" if one wishes to be all fancy); these three genus
have various species, so the Hero is an Animal, and so are the Xarci
Bedo; items all fall into the Vegetable genus, and the floor, walls
I<etc> are all Minerals.

The design beyond that is much like B<rogue> where everything is mostly
mixed up all together.

=head1 BUGS

Ouch.

    HP 100
    HP -140314714594444

L<https://github.com/thrig/Game-Xomb>

=head1 SEE ALSO

L<Game::PlatformsOfPeril> upon which this code evolved from and also
various roguelikes available on the Internet.

7DRL 2020

=head1 AUTHOR

Jeremy Mates

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2020 by Jeremy Mates

This program is distributed under the (Revised) BSD License:
L<http://www.opensource.org/licenses/BSD-3-Clause>

=cut
