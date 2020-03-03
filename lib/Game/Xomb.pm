# -*- Perl -*-
#
# Game::Xomb - this is a terminal-based roguelike. run the xomb(1)
# command that is installed with this module to start a game

package Game::Xomb;

our $VERSION = '0.05';

use 5.24.0;
use warnings;
#use Carp::Always;           # DBG
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

sub NEED_ROWS () { 24 }
sub NEED_COLS () { 80 }

# ANSI or XTerm control sequences - http://invisible-island.net/xterm/
sub ALT_SCREEN ()   { "\e[?1049h" }
sub CLEAR_LINE ()   { "\e[2K" }
sub CLEAR_RIGHT ()  { "\e[K" }
sub CLEAR_SCREEN () { "\e[1;1H\e[2J" }
sub HIDE_CURSOR ()  { "\e[?25l" }        # this gets toggled on/off
sub HIDE_POINTER () { "\e[>3p" }         # remove screen gnat
sub SHOW_CURSOR ()  { "\e[?25h" }
sub TERM_NORM ()    { "\e[m" }
sub UNALT_SCREEN () { "\e[?1049l" }

# these not-CONSTANT move the cursor around. points are col,row (x,y)
# while terminal uses row,col hence the reverse argument order here
sub at     { "\e[" . $_[1] . ';' . $_[0] . 'H' }
sub at_row { "\e[" . $_[0] . ';1H' }
sub at_col { "\e[" . $_[0] . 'G' }

# where the message (top) and status (bottom) lines are
sub AT_MSG_ROW ()    { "\e[1;1H" }
sub MSG_ROW ()       { 1 }
sub MSG_MAX ()       { NEED_ROWS - 2 }
sub STATUS_ROW ()    { 24 }
sub AT_STATUS_ROW () { "\e[24;1H" }
sub CELL_HP_COL ()   { 14 }
sub CELL_INFO_COL () { 68 }
sub ERR_CODE_COL ()  { 72 }

# how big and where to put the level map
sub MAP_COLS ()     { 78 }
sub MAP_ROWS ()     { 22 }
sub MAP_DISP_OFF () { 2 }

# level map is row, col while points are [ col, row ]
sub PROW () { 1 }
sub PCOL () { 0 }

# a point in the LMC so Animates can find where they are at
sub WHERE () { 0 }
# GENUS is involved with interactions between things and where the
# thingy is slotted under the LMC
sub MINERAL () { 1 }    # floor, walls, etc
sub VEGGIE ()  { 2 }    # amulet, gems, etc
sub ANIMAL ()  { 3 }    # Animates

# SPECIES
sub HERO ()   { 0 }     # NOTE also used for @Animates slot
sub AMULET () { 1 }     # goal of the game. technically is a vegetable
sub FLOOR ()  { 2 }
sub GATE ()   { 3 }     # stairs, rogue 3.6 style
sub WALL ()   { 4 }     # regular wall
sub GEM ()    { 5 }
sub HOLE ()   { 6 }     # quicker than gate but causes damage
sub TROLL ()  { 7 }
sub ACID ()   { 8 }
sub RUBBLE () { 9 }

# for ANIMALS (shared with VEGGIES and MINERALS for the first few slots)
sub GENUS ()      { 0 }
sub SPECIES ()    { 1 }
sub DISPLAY ()    { 2 }    # how to show 'em on the screen
sub UPDATE ()     { 3 }    # what happens when their turn comes up
sub STASH ()      { 4 }    # kitchen drawer
sub LMC ()        { 5 }    # link back to the level map
sub BLACK_SPOT () { 6 }    # marked for death
sub ENERGY ()     { 7 }    # how long until their next update call

# Animates stash slots
sub HITPOINTS () { 0 }
sub LOOT ()      { 1 }
sub ECOST ()     { 2 }

sub START_HP () { 100 }           # see Damage_From and related
sub LOOT_MAX () { NEED_ROWS - 2 } # avoids scrolling, status bar wipeout

sub MOVE_LVLUP ()   { -1 }        # NOTE tied to level change math
sub MOVE_FAILED ()  { 0 }
sub MOVE_LVLDOWN () { 1 }         # NOTE tied to level change math
sub MOVE_OK ()      { 2 }

# energy constants
sub CAN_MOVE ()     { 0 }
sub DEFAULT_COST () { 10 }
sub DIAG_COST ()    { 14 }
sub NLVL_COST ()    { 16 }
sub TIME_TO_DIE ()  { 2 }    # turns before quit possible after death

our (@Animates, $Draw_Delay, $LMap, @RedrawA, @RedrawB);
our $Level      = 1;
our $Turn_Count = 0;         # player moves
our $Time_Spent = 0;         # energy spent

# these are "class objects" by default; in other words there is only one
# WALL in all WALL cells unless efforts are taken otherwise (see reify
# and the make_* routines)
#
#             GENUS    SPECIES DISPLAY UPDATE (passive effects)
our %Things = (
    ACID,   [ MINERAL, ACID,   '~', \&passive_burn ],
    AMULET, [ VEGGIE,  AMULET, ',' ],
    FLOOR,  [ MINERAL, FLOOR,  '.' ],
    GATE,   [ MINERAL, GATE,   '%' ],
    HOLE,   [ MINERAL, HOLE,   ' ' ],
    RUBBLE, [ MINERAL, RUBBLE, '^' ],
    WALL,   [ MINERAL, WALL,   '#' ],
);

# mostly so that causes of damage are all kept in one place
our %Damage_From = (
    acidburn => sub {
        my ($duration) = @_;
        my $burn = 0;
        $burn += int rand 2 for 1 .. $duration;
        return $burn;
    },
    attackby => sub {
        # TODO put their damage function into their STASH?
        my ($ani) = @_;    # the attacker
        return int(rand 4 + rand 4 + rand 4);
    },
    falling => sub {
        # COSMETIC this may need to be higher depending on len of game,
        # healing sources etc
        if (int(rand 6 + rand 6) == 0) {
            return 0;
        } else {
            return 1 + int rand 6;
        }
    },
);

# NOTE these must be fairly short as they must all fit in status line
# and there can be three of them; see move_examine
our %Descript = (
    ACID,   'shallow acid pool',
    AMULET, 'Dragonstone',
    FLOOR,  'floor',
    GATE,   'Stair to next level',
    GEM,    'Gemstone',
    HERO,   'You',
    HOLE,   'a hole in the ground',
    RUBBLE, 'some rubble',
    TROLL,  'Railgun Tower',
    WALL,   'just another brick in the wall',
);

# what happens when moving into the given GENUS (allow the move,
# fisticuffs, etc.). called from move_animate. vegetables are ignored
# for reasons discussed over in said function (also less code, yay!)
our %Bump_Into = (
    ANIMAL,
    sub {
        my ($mover, $dpoint, $target, $cost) = @_;
        # TODO improve combat with found items or something, or
        # different enemy types lower to-hit odds somehow, but E_TIME
        if (int rand 2 == 0) {
            apply_damage($target, 'attackby', $mover);
        } else {
            log_code('PKC-0302') if $mover->[SPECIES] == HERO;
        }
        # try not to fight while in an acid pond? of course any dungeon
        # master worth their salt would immediately build railgun towers
        # surrounded by acid ponds...
        $cost += rubble_delay($cost)
          if $mover->[LMC][MINERAL][SPECIES] == RUBBLE;
        apply_passives($mover, $cost, 0);
        return MOVE_OK, $cost;
    },
    MINERAL,
    sub {
        my ($mover, $dpoint, $target, $cost) = @_;
        return MOVE_FAILED, 0, 'PKC-0002' if $target->[SPECIES] == WALL;
        # NOTE the rubble delay is applied *before* they can move out of
        # that acid pond that they are in:
        #   "Yes, we really hate players, damn their guts."
        #     -- Dungeon Crawl Stone Soup, cloud.cc
        $cost += rubble_delay($cost) if $target->[SPECIES] == RUBBLE;
        if ($target->[SPECIES] == HOLE) {
            return MOVE_FAILED, 0
              if nope_regarding("Falling may cause damage");
            apply_passives($mover, $cost / 2, 0);
            my $ret = MOVE_OK;
            if ($mover->[SPECIES] == HERO) {
                log_message('You plunge into the hole.');
                log_code('PKC-0099');
                $ret = MOVE_LVLDOWN;
            } else {
                # just remove them from play, for now
                push @RedrawA, $mover->[LMC][WHERE];
                $mover->[BLACK_SPOT] = 1;
            }
            apply_damage($mover, 'falling');
            return $ret, $cost;
        } else {
            apply_passives($mover, $cost / 2, 0);
            relocate($mover, $dpoint);
            apply_passives($mover, $cost / 2, 1);
            return MOVE_OK, $cost;
        }
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
    'v' => sub { log_message('xomb ' . $VERSION); return MOVE_FAILED, 0 },
    'x' => \&move_examine,
    '<' => sub {
        return MOVE_FAILED, 0, 'PKC-0004'
          if $Animates[HERO][LMC][MINERAL][SPECIES] != GATE;
        unless (has_amulet()) {
            log_message('You need the Dragonstone to escape.') if $Level == 1;
            return MOVE_FAILED, 0, 'PKC-0010';
        }
        log_message('Gate activated.');
        return MOVE_LVLUP, NLVL_COST;
    },
    '>' => sub {
        return MOVE_FAILED, 0, 'PKC-0004'
          if $Animates[HERO][LMC][MINERAL][SPECIES] != GATE;
        log_message('Gate activated.');
        return MOVE_LVLDOWN, NLVL_COST;
    },
    '?' => sub { help_screen();   return MOVE_FAILED, 0 },
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
    my ($ani, $cause, @rest) = @_;
    #warn "HP A for $ani->[SPECIES] $ani->[STASH][HITPOINTS]\n";
    $ani->[STASH][HITPOINTS] -= $Damage_From{$cause}->(@rest);
    #warn "HP B for $ani->[SPECIES] $ani->[STASH][HITPOINTS]\n";
    if ($ani->[STASH][HITPOINTS] <= 0) {
use Data::Dumper; warn Dumper $ani;
        push @RedrawA, $ani->[LMC][WHERE];
        if ($ani->[SPECIES] == HERO) {
            $ani->[DISPLAY] = '&';                 # the @ got unravelled
            $ani->[UPDATE]  = \&update_gameover;
        } else {
            # KLUGE assume source of damage was the player, otherwise
            # would need to always have a source object in @rest to
            # peek at...
# TODO why getting a "." overwrite of Y in You? is that the ground being
# drawn there from LMC?
            log_message('You destroy the ' . $Descript{ $ani->[SPECIES] });
            $ani->[BLACK_SPOT] = 1;
        }
    }
}

# KLUGE would a not-ANIMAL ever get into Animates and need UPDATE for
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
    my ($cols, $rows) = (GetTerminalSize(*STDOUT))[ 0, 1 ];
    !defined $cols or $cols < NEED_COLS or $rows < NEED_ROWS;
}

sub bail_out {
    restore_term();
    print "\n", at_col(0), CLEAR_LINE;
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

# something something Panopticon
sub boss_screen {
    print CLEAR_SCREEN, at(1, 2), <<"BOSS_SCREEN", "\n:", SHOW_CURSOR;
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
    my $mcount = 0;

    sub clear_code     { print at(ERR_CODE_COL, STATUS_ROW), CLEAR_RIGHT }
    sub clear_messages { @log = () }
    sub log_code       { print at(ERR_CODE_COL, STATUS_ROW), $_[0] }

    sub log_message {
        my ($message) = @_;
        while (@log >= MSG_MAX) { shift @log }
        push @log, $message;
        $mcount++;
        # COSMETIC dimming problems
        #show_top_message();
    }

    sub show_messages {
        my $s = SHOW_CURSOR;
        while (my ($i, $message) = each @log) {
            $s .= at_row(MSG_ROW + $i) . CLEAR_RIGHT . $message;
        }
        print $s, at_row(MSG_ROW + @log), CLEAR_RIGHT,
          "-- press Esc to continue --";
        await_quit();
        print HIDE_CURSOR;
        refresh_board(scalar @log);
    }

    # much of the complication is to dim progressively older information
    sub show_top_message {
        state $last_message = -1;
        state($last_call, $now);
        my $message = @log ? $log[-1] : '';
        if ($last_message != $mcount) {
            $last_message = $mcount;
            $last_call    = $now = 0;
        }
        my $delta = $now - $last_call;
        my $w     = $delta == 0 ? 1 : 0;
        print AT_MSG_ROW, CLEAR_RIGHT,
          ($delta < 2) ? ("\e[", $w, 'm', $message, TERM_NORM) : ();
        $now++;
    }
}

# so player can see what else is in cell, top to bottom to match the
# examine ordering
sub display_cellobjs {
    my $s = at(CELL_INFO_COL, STATUS_ROW) . '[';
    for my $i (VEGGIE, MINERAL) {
        my $obj = $Animates[HERO][LMC][$i];
        $s .= (defined $obj and $obj->@*) ? $obj->[DISPLAY] : ' ';
    }
    $s .= ']';
    return $s;
}

sub display_hitpoints {
    my $hp = $Animates[HERO][STASH][HITPOINTS];
    $hp = 0 if $hp < 0;
    # COSMETIC sentence gen grammer to vary these in Star Trek fashion?
    log_message('Shield module failure.') if $hp == 0;
    my $ticks = int $hp / 2;
    my $hpbar = '=' x $ticks;
    $hpbar .= '-' if $ticks & 1;
    my $len = length $hpbar;
    $hpbar .= ' ' x (50 - $len) if $len < 50;
    at(CELL_HP_COL, STATUS_ROW) . "SP[\e[1m" . $hpbar . TERM_NORM . ']';
}

sub draw_level {
    my ($lines) = @_;
    $lines //= MAP_ROWS;
    my $s = '';
    for my $rownum (0 .. MAP_ROWS - 1) {
        $s .= at(MAP_DISP_OFF, MAP_DISP_OFF + $rownum) . CLEAR_LINE;
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
    game_over('Terminal must be at least ' . NEED_COLS . 'x' . NEED_ROWS)
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

    make_player();
    generate_level();

    print ALT_SCREEN, HIDE_CURSOR, HIDE_POINTER, CLEAR_SCREEN, TERM_NORM;
    log_message('Welcome to xomb');
    show_top_message();
    draw_level();
    show_status_bar();

  GLOOP: while (1) {
        my $min_cost = min map { $_->[ENERGY] } @Animates;
        $Time_Spent += $min_cost;
        #warn('DBG min energy ' . $min_cost . "\n");

        my @movers;
        for my $ani (@Animates) {
            $ani->[ENERGY] -= $min_cost;
            push @movers, $ani if $ani->[ENERGY] <= CAN_MOVE;
        }
        # simultaneous move shuffle; this may be important in edge cases
        # such as when a player is trying to jump down a hole before
        # they get shot. otherwise all simultaneous movers get to go,
        # and then afterwards cleanup happens
        fisher_yates_shuffle(\@movers) if @movers > 1;

        my $new_level = 0;
        for my $ani (@movers) {
            #use Data::Dumper; warn Dumper $ani; # DBG
            my ($status, $cost) = $ani->[UPDATE]->($ani);
            #warn('DBG move cost ' . $cost . ' for ' . $ani->[DISPLAY] . "\n");
            die "DBG bad cost $cost\n" . Dumper($ani) if $cost <= CAN_MOVE;
            $ani->[ENERGY] += $ani->[STASH][ECOST] = $cost;
            $new_level = $status
              if $status == MOVE_LVLDOWN or $status == MOVE_LVLUP;
            if ($ani->[SPECIES] == HERO) {
                #warn "HERO RET COST $cost\n";
                show_top_message();
                show_status_bar();
            }
        }

        if ($new_level != 0) {
            $Level += $new_level;
            # COSMETIC actual victory screen, score, etc
            game_over('Congratulations! You did not die.', 0) if $Level <= 0;
            generate_level();
            draw_level();
            # NOTE other half of this is applied in the Bump-into-HOLE
            # logic, elsewhere. this last half happens here as the new
            # level is not yet available prior to the fall
            apply_passives($Animates[HERO], $Animates[HERO][STASH][ECOST] / 2, 1);
            show_status_bar();    # COSMETIC only update Level counter
            next GLOOP;
        }

      CLEANUP:
        #*STDERR->print("ANI BEF ");
        #warn Dumper \@Animates;
        @Animates =
          grep { $_->[BLACK_SPOT] ? undef $_->[LMC][ANIMAL] : 1 } @Animates;
# ok after T is indeed removed ... what about LMC for the Troll?
        #*STDERR->print("ANI AFT ");
        #warn Dumper \@Animates;
    warn "in cleanup...\n";
        warn Dumper \@RedrawA, \@RedrawB;
        redraw_movers();
    }
}

sub game_over {
    my ($message, $code) = @_;
    $code //= 1;
    restore_term();
    print "\n", at_col(0), CLEAR_LINE, $message,
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
            make_gem($c, $r) if rand() < 0.05;
        }
    }

    # DBG KLUGE test out stuff on level map
    my $c = 1;
    my $r = 1;
    $LMap->[$r][$c][ANIMAL] = $Animates[HERO];
    $Animates[HERO][LMC] = $LMap->[$r][$c];
    weaken($Animates[HERO][LMC]);
    $c = $r = 2;
    $LMap->[$r][$c][MINERAL] = $Things{ GATE, };

    # TODO level gen will need to figure out where the player is and
    # make that square part of the pathable map (or allow digging...)
    # this thing resets their position due to above setup; ideally new
    # level should preserve vertical harmony with position above
    if ($Level == 1) {
        $c                       = $r = 3;
        $LMap->[$r][$c][MINERAL] = $Things{ WALL, };
        $c                       = $r = 4;
        $LMap->[$r][$c][MINERAL] = $Things{ HOLE, };
    } elsif (!has_amulet()) {
        $c = $r = 5;
        $LMap->[$r][$c][VEGGIE] = $Things{ AMULET, };
    }

    $LMap->[0][7][MINERAL] = $Things{ ACID,   };
    $LMap->[0][8][MINERAL] = $Things{ RUBBLE, };

    $LMap->[4][0][MINERAL] = $Things{ RUBBLE, };
    $LMap->[3][1][MINERAL] = $Things{ RUBBLE, };
    $LMap->[4][1][MINERAL] = $Things{ RUBBLE, };
    $LMap->[2][0][MINERAL] = $Things{ ACID,   };
    make_monster(
        0, 3,
        species => TROLL,
        hp      => 10,
        energy  => 10,
        display => 'T'
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

sub help_screen {
    print CLEAR_SCREEN, at(1, 1), <<'HELP_SCREEN', "\n:", SHOW_CURSOR;
                     Xomb Commands Reference Manual

     y  k  u     Motion is traditional to rogue(6) as shown in the
      \ | /      compass to the left. Other commands, of which some
    h - @ - l    take time to complete:
      / | \             
     b  j  n               . - wait a turn      d - drop item
                         g , - pick up item     i - show inventory
    x - examine board    < > - activate gate    M - show messages
    e - clear error      C-l - redraw screen    ? - show help
    v - show version
    
    Esc or q will exit from sub-displays such as this one. Prompts
    must be answered with Y to carry out the action; N or n or Esc
    will reject the action. Map symbols include:

      @ - you   % - gate    * - gemstone  # - wall  . - empty cell
      ~ - acid  ^ - rubble

    Consult xomb(1) or `perldoc xomb` for additional documentation.
HELP_SCREEN
    await_quit();
    print HIDE_CURSOR;
    refresh_board();
}

sub make_gem {
    my ($col, $row) = @_;
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
    $gem->@[ GENUS, SPECIES, DISPLAY, STASH ] =
      (VEGGIE, GEM, '*', [ $name, $value ]);
    $LMap->[$row][$col][VEGGIE] = $gem;
}

sub make_monster {
    my ($col, $row, %params) = @_;
    my $monst;
    $monst->@[ GENUS, SPECIES, DISPLAY, ENERGY, UPDATE, STASH, LMC ] = (
        ANIMAL, $params{species}, $params{display}, $params{energy},
        \&update_monster,
        [ $params{hp}, [], ENERGY ],
        $LMap->[$row][$col]
    );
    push @Animates, $monst;
    die "DBG already occupied??" if defined $LMap->[$row][$col][ANIMAL];
    $LMap->[$row][$col][ANIMAL] = $monst;
    weaken($monst->[LMC]);
}

sub make_player {
    $Animates[HERO]->@[ GENUS, SPECIES, DISPLAY, ENERGY, UPDATE, STASH ] = (
        ANIMAL, HERO, '@', CAN_MOVE, \&update_player,
        [ START_HP, [], CAN_MOVE ],
    );
}

sub manage_inventory {
    my ($command, $message) = @_;
    print SHOW_CURSOR;
    my $loot = $Animates[HERO][STASH][LOOT];
    my $offset;
    my $s        = '';
    my $has_loot = 0;
    if ($loot->@*) {
        $has_loot = 1;
        my $label = 'A';
        while (my ($i, $item) = each $loot->@*) {
            $s .=
                at_row(MSG_ROW + $i)
              . CLEAR_RIGHT
              . $label++ . ') '
              . $item->[DISPLAY] . ' ';
            if ($item->[SPECIES] == GEM) {
                $s .= join "\t", $item->[STASH]->@[ 1, 0 ];
            } else {
                $s .= $Descript{ $item->[SPECIES] };
            }
        }
        $offset = $loot->@*;
    } else {
        $s .= AT_MSG_ROW . CLEAR_RIGHT . "Inventory is empty.";
        $offset = 1;
    }
    $s .= at_row(MSG_ROW + $offset) . CLEAR_RIGHT . '-- ';
    if ($message) {
        $s .= $message;
    } else {
        $s .= 'press Esc to continue';
        $s .= ' or (d)rop' if $has_loot;
    }
    print $s, ' --';
  CMD: while (1) {
        my $key = $command // ReadKey(0);
        last if $key eq "\033" or $key eq 'q';
        undef $command;
        next unless $has_loot;
        if ($key eq 'd') {
            if (!defined $message) {
                print at_row(MSG_ROW + $offset), CLEAR_RIGHT,
                  "-- drop item L)able or Esc to exit --";
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
    # NOTE vegetables are never considered when moving into a cell as
    # otherwise veggies in rubble never allow the rubble passive
    # effect to fire
    for my $i (ANIMAL, MINERAL) {
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
    print AT_MSG_ROW, CLEAR_RIGHT, SHOW_CURSOR,
      "-- move cursor to view a cell. SHIFT moves faster. Esc to exit --";
    while (1) {
        my $s = '';
        for my $i (ANIMAL, VEGGIE) {
            my $x = $LMap->[$row][$col][$i];
            $s .= $x->[DISPLAY] . ' ' . $Descript{ $x->[SPECIES] } . ' '
              if defined $x;
        }
        my $g = $LMap->[$row][$col][MINERAL];
        $s .= $g->[DISPLAY] . ' ' . $Descript{ $g->[SPECIES] }
          if defined $g;
        print at_row(STATUS_ROW), CLEAR_RIGHT, $s,
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
    print HIDE_CURSOR, at_row(STATUS_ROW), CLEAR_RIGHT;
    show_top_message();
    show_status_bar();
    return MOVE_FAILED, 0, int rand 5000 == 0 ? 'PKC-1202' : ();
}

sub move_nop {
    apply_passives($Animates[HERO], DEFAULT_COST, 0);
    # NOTE constant amount of time even if they idle in rubble
    return MOVE_OK, DEFAULT_COST;
}

sub move_pickup {
    my $lmc = $Animates[HERO][LMC];
    return MOVE_FAILED, 0, 'PKC-0101' unless defined $lmc->[VEGGIE];
    my $loot = $Animates[HERO][STASH][LOOT];
    return MOVE_FAILED, 0, 'PKC-0102' if $loot->@* >= LOOT_MAX;
    # TODO need a "name-of" call also called by manage_inventory
    log_message('Picked something up.');
    push $loot->@*, $lmc->[VEGGIE];
    $lmc->[VEGGIE] = undef;
    print display_cellobjs();
    my $cost = DEFAULT_COST;
    $cost += rubble_delay($cost) if $lmc->[MINERAL][SPECIES] == RUBBLE;
    return MOVE_OK, $cost;
}

sub move_player_maker {
    my ($cols, $rows, $mvcost) = @_;
    sub {
        my @ret = move_animate($Animates[HERO], $cols, $rows, $mvcost);
        warn "DBG COST @ret\n";
        print display_cellobjs();
        return @ret;
    }
}

sub nope_regarding {
    my ($message) = @_;
    print AT_MSG_ROW, " \e[1m/!\\ ", $message, ' (Y/N)', TERM_NORM;
    my $key;
    while (1) {
        $key = ReadKey(0);
        return 0 if $key eq 'Y';
        return 1 if $key eq "\033" or $key eq 'N' or $key eq 'n';
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
    show_status_bar()
      if !defined $lines
      or ($lines and $lines >= STATUS_ROW);
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
    weaken($ani->[LMC]);
}

sub restore_term {
    ReadMode 'restore';
    print TERM_NORM, SHOW_CURSOR, UNALT_SCREEN;
}

sub rubble_delay {
    my ($cost) = @_;
    warn "in RUBBLE yo\n";
    my $mod = 0;
    if (int rand 4 == 0) {
        log_message('Slow progress!');
        $mod = $cost / 2;
    }
    return $mod;    # added to overall cost
}

# COSMETIC inline display_ calls here for speeds?
sub show_status_bar {
    #warn "ECOST IN HERO IS $Animates[HERO][STASH][ECOST]\n";
    print at_row(STATUS_ROW),
      sprintf('Level %02d E%02d', $Level, $Animates[HERO][STASH][ECOST]),
      display_hitpoints(), display_cellobjs();
}

sub passive_burn {
    my ($ani, $obj, $duration, $newcell) = @_;
    warn "DBG ACID duration $duration\n";
    log_code('PKC-007E') if $ani->[SPECIES] == HERO;
    apply_damage($ani, 'acidburn', $duration);
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
    tcflush(STDIN_FILENO, TCIFLUSH);
    my $key = ReadKey(0);
    if ($count > TIME_TO_DIE) {
        log_message('-- press Esc to continue --');
        game_over('Congratulations! You have died.')
          if $key eq "\033" or $key eq 'q';
    } else {
        log_message('Communication lost with remote unit.');
    }
    $count++;
    return MOVE_OK, DEFAULT_COST;
}

sub update_monster {
    warn "monster twiddles thumbs waiting to be implemented\n";
    # TODO apply passives (except not messages to the player...) may
    # need to apply to monsters, unless map gen never puts them in a)
    # rubble or b) acid or c) on a hole or d) on a WALL (well wall might
    # be okay, they'll just be replaced with it when killed, like a
    # turret in Brogue)
    return MOVE_OK, DEFAULT_COST * 4;
}

sub update_player {
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
        # COSMETIC maybe clear Energy cost display here if MOVE_FAILED
        # as previous move is now some error... err why not do all
        # energy cost updates here (except for initial setup or redraw
        # of board?)
        last if $ret != MOVE_FAILED;
    }
    $Turn_Count++;
    return $ret, $cost;
}

1;
__END__
=encoding utf8

=head1 NAME

Game::Xomb - a game featuring @ versus the Xarci Bedo

=head1 SYNOPSIS

Xomb is a terminal-based roguelike. Assuming that the development
tools, L<perl(1)>, L<App::cpanminus>, and possibly also
L<local::lib> are installed and setup, in a suitable terminal
install and run the game via:

    cpanm Game::Xomb
    xomb

Use the C<?> key in game to show the help text. The L<xomb(1)>
documentation details other useful game information; it should be
available once the module is installed via:

    perldoc xomb

=head1 BUGS

    HP 100
    You plunge into the hole.
    HP -140314714594444

'tis but a flesh wound.

L<https://github.com/thrig/Game-Xomb>

=head1 SEE ALSO

L<Game::PlatformsOfPeril> from which this code evolved.

7DRL 2020

Vektor - Terminal Redux

=head1 AUTHOR

Jeremy Mates

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2020 by Jeremy Mates

This program is distributed under the (Revised) BSD License:
L<http://www.opensource.org/licenses/BSD-3-Clause>

=cut
