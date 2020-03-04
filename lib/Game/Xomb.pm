# -*- Perl -*-
#
# Game::Xomb - this is a terminal-based roguelike. run the xomb(1)
# command that is installed with this module to start a game

package Game::Xomb;

our $VERSION = '0.10';

use 5.24.0;
use warnings;
#use Carp::Always;           # DBG
use Carp qw(croak confess);    # DBG
use Data::Dumper;              # DBG
use List::Util qw(min);
use List::UtilsBy qw(nsort_by);
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

# these not-CONSTANTs move the cursor around. points are col,row (x,y)
# while terminal uses row,col hence the reverse argument order here.
# some at(...) calls have been made into AT_* constants for frequently
# used locations
sub at     { "\e[" . $_[1] . ';' . $_[0] . 'H' }
sub at_row { "\e[" . $_[0] . ';1H' }
sub at_col { "\e[" . $_[0] . 'G' }

# where the message (top) and status (bottom) lines are
sub MSG_ROW ()       { 1 }
sub AT_MSG_ROW ()    { "\e[1;1H" }
sub MSG_MAX ()       { NEED_ROWS - 2 }
sub STATUS_ROW ()    { 24 }
sub AT_STATUS_ROW () { "\e[24;1H" }
sub AT_HPBAR ()      { "\e[24;14H" }
sub AT_CELLOBJS ()   { "\e[24;68H" }
sub AT_ERR_CODE ()   { "\e[24;72H" }

# NOTE also set in Xomb.xs for map-aware functions
sub MAP_COLS () { 78 }
sub MAP_ROWS () { 22 }
sub MAP_DOFF () { 2 }    # display offset for map on screen

# NOTE level map is row, col while points are [ col, row ]
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
sub GHAST ()  { 1 }
sub TROLL ()  { 2 }
sub AMULET () { 3 }     # goal of the game. technically is a vegetable
sub GEM ()    { 4 }     # and so are these
sub HOLE ()   { 5 }     # quicker than a gate but has downsides
sub FLOOR ()  { 6 }
sub GATE ()   { 7 }     # stairs, rogue 3.6 style
sub ACID ()   { 8 }     # irresistible damage as a service
sub RUBBLE () { 9 }     # SLOW PROGRESS!!
sub WALL ()   { 10 }    # regular wall

sub AMULET_NAME () { 'Dragonstone' }

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
sub ECOST ()     { 1 }     # cost of last move made
sub LOOT ()      { 2 }     # inventory
sub SHIELDUP ()  { 3 }     # a gem can recharge the shield
# GEM stash slots
sub GEM_NAME ()  { 0 }
sub GEM_VALUE () { 1 }

sub START_HP () { 100 }           # see Damage_From and related
sub LOOT_MAX () { NEED_ROWS - 2 } # avoids scrolling, status bar wipeout

sub MOVE_LVLUP ()   { -1 }        # NOTE tied to level change math
sub MOVE_FAILED ()  { 0 }         # for zero-cost player moves
sub MOVE_LVLDOWN () { 1 }         # NOTE tied to level change math
sub MOVE_OKAY ()    { 2 }         # non-level-change costly moves

# energy constants, see game_loop for the system
sub CAN_MOVE ()     { 0 }
sub DEFAULT_COST () { 10 }
sub DIAG_COST ()    { 14 }
sub NLVL_COST ()    { 20 }    # time to gate to next level
sub TIME_TO_DIE ()  { 2 }     # turns before quit possible after death

########################################################################
#
# VARIABLES

our (@Animates, @LMap);
our $Energy_Spent = 0;
our $GGV          = 0;    # economy regulation index
our $Level        = 1;    # current level
our $Turn_Count   = 0;    # player moves

our $FOV;                 # FOV cache
our %Visible_Cell;

# mostly so that causes of damage are all kept in one (testable) place
#   cpanm App::Prove
#   XOMB_STATS=1 prove t/damage-stats.t
our %Damage_From = (
    acidburn => sub {
        my ($duration) = @_;
        my $burn = 0;
        $burn += int rand 2 for 1 .. $duration;
        return $burn;
    },
    attackby => sub {
        my ($ani) = @_;    # the attacker
        return roll(3, 6);
    },
    falling => sub {
        my $dice   = 1;
        my $damage = 0;
        while (1) {
            my $roll = roll($dice, 4);
            $damage += $roll;
            last if $roll <= 2 or $dice > 4;
            $dice++;
        }
        return $damage;
    },
);

# these are "class objects"; in other words there is only one WALL in
# all WALL cells unless efforts are taken otherwise (see reify and the
# make_* routines)
#
#             GENUS    SPECIES DISPLAY UPDATE (passive effects)
our %Things = (
    ACID,   [ MINERAL, ACID,   '~', \&passive_burn ],
    FLOOR,  [ MINERAL, FLOOR,  '.' ],
    GATE,   [ MINERAL, GATE,   '%' ],
    HOLE,   [ MINERAL, HOLE,   'o' ],
    RUBBLE, [ MINERAL, RUBBLE, '^' ],
    WALL,   [ MINERAL, WALL,   '#' ],
);

# NOTE these need to be fairly short as they must all fit in status line
# and there could be three of them; see move_examine
our %Descript = (
    ACID,   'shallow acid pool',
    AMULET, AMULET_NAME,
    FLOOR,  'floor',
    GATE,   'Gate to next level',
    GEM,    'gemstone',
    GHAST,  'Gatling Autocannon',
    HERO,   'You',
    HOLE,   'Hole',
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
        if (int rand 10 < 8) {
            apply_damage($target, 'attackby', $mover);
        } else {
            log_code('PKC-0302') if $mover->[SPECIES] == HERO;
        }
        # try not to fight while in an acid pond? of course any dungeon
        # master worth their salt would immediately build railgun towers
        # surrounded by acid ponds...
        $cost += rubble_delay($mover, $cost)
          if $mover->[LMC][MINERAL][SPECIES] == RUBBLE;
        apply_passives($mover, $cost, 0);
        return MOVE_OKAY, $cost;
    },
    MINERAL,
    sub {
        my ($mover, $dpoint, $target, $cost) = @_;
        return MOVE_FAILED, 0, 'PKC-0002' if $target->[SPECIES] == WALL;
        # NOTE the rubble delay is applied *before* they can move out of
        # that acid pond that they are in:
        #   "Yes, we really hate players, damn their guts."
        #     -- Dungeon Crawl Stone Soup, cloud.cc
        $cost += rubble_delay($mover, $cost) if $target->[SPECIES] == RUBBLE;
        if ($target->[SPECIES] == HOLE) {
            return MOVE_FAILED, 0
              if nope_regarding("Falling may cause damage");
            apply_passives($mover, $cost / 2, 0);
            my $ret = MOVE_OKAY;
            if ($mover->[SPECIES] == HERO) {
                # TODO this log needs for example expedited display...
                log_message('You plunge into the hole.');
                animate_plunge($mover, $dpoint);
                relocate($mover, $dpoint);
                log_code('PKC-0099');
                undef $FOV;
                $ret = MOVE_LVLDOWN;
            } else {
                # remove from play, for now
                $mover->[BLACK_SPOT] = 1;
                undef $mover->[LMC][ANIMAL];
            }
            apply_damage($mover, 'falling');
            return $ret, $cost;
        } else {
            apply_passives($mover, $cost / 2, 0);
            relocate($mover, $dpoint);
            apply_passives($mover, $cost / 2, 1);
            return MOVE_OKAY, $cost;
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
    'E' => sub { clear_code(); return MOVE_FAILED, 0 },
    'R' => \&move_remove,
    'i' => \&manage_inventory,
    'd' => \&move_drop,
    'e' => \&move_use,
    'h' => move_player_maker(-1, +0, DEFAULT_COST),
    'j' => move_player_maker(+0, +1, DEFAULT_COST),
    'k' => move_player_maker(+0, -1, DEFAULT_COST),
    'l' => move_player_maker(+1, +0, DEFAULT_COST),
    'y' => move_player_maker(-1, -1, DIAG_COST),
    'u' => move_player_maker(+1, -1, DIAG_COST),
    'b' => move_player_maker(-1, +1, DIAG_COST),
    'n' => move_player_maker(+1, +1, DIAG_COST),
    ',' => \&move_pickup,
    'g' => \&move_pickup,
    '.' => \&move_nop,                                    # rest
    ' ' => \&move_nop,                                    # also rest
    'v' => sub { log_message('xomb ' . $VERSION); return MOVE_FAILED, 0 },
    'x' => \&move_examine,
    '<' => sub {
        return MOVE_FAILED, 0, 'PKC-0004'
          if $Animates[HERO][LMC][MINERAL][SPECIES] != GATE;
        unless (has_amulet()) {
            log_message('You need the ' . AMULET_NAME . ' to ascend.');
            return MOVE_FAILED, 0, 'PKC-0010';
        }
        log_message('Gate activated.');
        sleep(0.05 + rand() / 3);
        undef $FOV;
        return MOVE_LVLUP, NLVL_COST;
    },
    '>' => sub {
        return MOVE_FAILED, 0, 'PKC-0004'
          if $Animates[HERO][LMC][MINERAL][SPECIES] != GATE;
        log_message('Gate activated.');
        sleep(0.05 + rand() / 3);
        undef $FOV;
        return MOVE_LVLDOWN, NLVL_COST;
    },
    '?' => sub { help_screen();   return MOVE_FAILED, 0 },
    'G' => sub { hide_screen();   return MOVE_FAILED, 0 },    # Gerente
    'M' => sub { show_messages(); return MOVE_FAILED, 0 },
    'q'    => sub { game_over('Be seeing you...') },          # DBG
    'Q'    => sub { game_over('Be seeing you...') },          # DBG
    "\003" => sub { return MOVE_FAILED, 0, 'PKC-1203' },      # <C-c>
    "\014" => sub {                                           # <C-l>
        refresh_board();
        return MOVE_FAILED, 0;
    },
    "\032" => sub { return MOVE_FAILED, 0, 'PKC-1220' },      # <C-z>
    "\033" => sub { return MOVE_FAILED, 0 },
);
# and a weak effort at numpad support
@Key_Commands{qw/1 2 3 4 5 6 7 8 9/} =
  @Key_Commands{qw/b j n h . l y k u/};

########################################################################
#
# SUBROUTINES

sub animate_plunge {
    my ($mover, $dpoint) = @_;
    my $lmc = $mover->[LMC];
    my $cell   = $lmc->[VEGGIE] // $lmc->[MINERAL];
    print at(map { MAP_DOFF + $_ } $lmc->[WHERE]->@*)
      . $cell->[DISPLAY]
      . at(map { MAP_DOFF + $_ } $dpoint->@*)
      . $mover->[DISPLAY];
    # FOV is going to be wiped out for level change, anyways
    delete $Visible_Cell{ join ',', $dpoint->@* };
    for my $point (values %Visible_Cell) {
        print at(map { MAP_DOFF + $_ } $point->@*), ' ';
    }
    sleep(0.05 + rand() / 3);
}

sub apply_damage {
    my ($ani, $cause, @rest) = @_;
    #warn "HP A for $ani->[SPECIES] $ani->[STASH][HITPOINTS]\n";
    $ani->[STASH][HITPOINTS] -= $Damage_From{$cause}->(@rest);
    #warn "HP B for $ani->[SPECIES] $ani->[STASH][HITPOINTS]\n";
    if ($ani->[STASH][HITPOINTS] <= 0) {
        if ($ani->[SPECIES] == HERO) {
            $ani->[DISPLAY] = '&';                 # the @ got unravelled
            $ani->[UPDATE]  = \&update_gameover;
        } else {
            # KLUGE assume source of damage was the player, otherwise
            # would need to always have a source object in @rest to
            # peek at...
            sleep(0.05 + rand() / 3);
            log_message('You destroy the ' . $Descript{ $ani->[SPECIES] });
            $ani->[BLACK_SPOT] = 1;
            undef $ani->[LMC][ANIMAL];
        }
        undef $FOV;
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
    game_over('The game yet again collapses about you.');   # DBG change rel
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

{
    my @log;
    my $mcount = 0;

    sub clear_code     { print AT_ERR_CODE, CLEAR_RIGHT }
    sub clear_messages { @log = () }
    sub log_code       { print AT_ERR_CODE, $_[0] }

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

# so can see what else is in cell, ordering matches that of examine sub
sub display_cellobjs {
    my $s = AT_CELLOBJS . '[';
    for my $i (VEGGIE, MINERAL) {
        my $obj = $Animates[HERO][LMC][$i];
        $s .= (defined $obj and $obj->@*) ? $obj->[DISPLAY] : ' ';
    }
    return $s . ']';
}

sub display_hitpoints {
    my $hp = $Animates[HERO][STASH][HITPOINTS];
    $hp = 0 if $hp < 0;
    log_message('Shield module failure.') if $hp == 0;
    my $ticks = int $hp / 2;
    my $hpbar = '=' x $ticks;
    $hpbar .= '-' if $ticks & 1;
    my $len = length $hpbar;
    $hpbar .= ' ' x (50 - $len) if $len < 50;
    return AT_HPBAR . "SP[\e[1m" . $hpbar . TERM_NORM . ']';
}

sub fisher_yates_shuffle {
    my ($array) = @_;
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
    raycast_fov(1);
    show_top_message();
    show_status_bar();

  GLOOP: while (1) {
        my $min_cost = min(map { $_->[ENERGY] } @Animates);
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
                #raycast_fov() if $new_level == 0;
                # XXXX probably don't need these
                show_top_message();
                show_status_bar();
            }
        }

        if ($new_level != 0) {
            $Level += $new_level;
            has_won() if $Level <= 0;
            generate_level();
            raycast_fov(1);
            # NOTE other half of this is applied in the Bump-into-HOLE
            # logic, elsewhere. this last half happens here as the new
            # level is not yet available prior to the fall
            apply_passives($Animates[HERO], $Animates[HERO][STASH][ECOST] / 2, 1);
            show_status_bar();    # COSMETIC only update Level counter
            next GLOOP;
        }

      CLEANUP:
        @Animates = grep { !$_->[BLACK_SPOT] } @Animates;
    }
}

# DBG mostly for abnormal exits such as uncaught signal or internal error
sub game_over {
    my ($message) = @_;
    restore_term();
    print "\n", at_col(0), CLEAR_LINE, $message, "\n", CLEAR_LINE;
    exit(1);
}

# "somos comparables al hechicero que teje un laberinto y que se ve
# forzado a errar en él hasta el fin de sus días"
#   -- Borges, Deutsches Requiem
sub generate_level {
    splice @Animates, 1;
    undef $Animates[HERO][LMC];
    @LMap = ();

    for my $r (0 .. MAP_ROWS - 1) {
        for my $c (0 .. MAP_COLS - 1) {
            my $point = [ $c, $r ];    # PCOL, PROW (x, y)
            push $LMap[$r]->@*, [ $point, $Things{ FLOOR, } ];
            # NOTE on level destruct may need to tally how much of the
            # GGV the player left on the floor, as they do not have that
            # to heal with (or include in forecast that GGV is a high
            # watermark, player probably has less)
            make_gem($c, $r) if rand() < 0.01;
        }
    }

    # DBG KLUGE test out stuff on level map
    my $c = 1;
    my $r = 1;
    $LMap[$r][$c][ANIMAL] = $Animates[HERO];
    $Animates[HERO][LMC] = $LMap[$r][$c];
    weaken($Animates[HERO][LMC]);
    $c = $r = 2;
    $LMap[$r][$c][MINERAL] = $Things{ GATE, };

    if ($Level == 1) {
        $c                     = $r = 3;
        $LMap[$r][$c][MINERAL] = $Things{ WALL, };
        $c                     = $r = 4;
        $LMap[$r][$c][MINERAL] = $Things{ HOLE, };
    } elsif ($Level == 2) {
        make_amulet(5, 5);
    }

    $LMap[0][7][MINERAL] = $Things{ ACID,   };
    $LMap[0][8][MINERAL] = $Things{ RUBBLE, };

    $LMap[4][0][MINERAL] = $Things{ RUBBLE, };
    $LMap[3][1][MINERAL] = $Things{ RUBBLE, };
    $LMap[4][1][MINERAL] = $Things{ RUBBLE, };
    $LMap[2][0][MINERAL] = $Things{ ACID,   };

    for my $x (10 .. 16) {
        for my $y (10 .. 16) {
            $LMap[$y][$x][MINERAL] = $Things{ RUBBLE, };
            $LMap[ $y ][ $x + 10 ][MINERAL] = $Things{ ACID, };
        }
    }

    make_monster(
        0, 3,
        species => TROLL,
        hp      => 10,
        energy  => 10,
        display => 'T'
    );

    make_monster(
        7, 8,
        species => GHAST,
        hp      => 4,
        energy  => 10,
        display => 'G'
    );

    # how to attach passive functions to (now) specific cell objects
    #reify($LMap[0][0], MINERAL, passive_msg_maker('bla blah I'));
    #reify($LMap[0][1], MINERAL, passive_msg_maker('bla blah A'));
    #reify($LMap[1][0], MINERAL, passive_msg_maker('bla blah B'));
    #reify($LMap[0][4], MINERAL, passive_msg_maker('oneshot 1', 1));
    #reify($LMap[4][0], MINERAL, passive_msg_maker('oneshot 2', 1));
}

sub has_amulet {
    for my $item ($Animates[HERO][STASH][LOOT]->@*) {
        return 1 if $item->[SPECIES] == AMULET;
    }
    # also must check shield regen slot; could set a flag but then they
    # could drop the damn thing or it could burn up repairing the shield
    # argh so complicated
    return 1
      if defined $Animates[HERO][STASH][SHIELDUP]
      and $Animates[HERO][STASH][SHIELDUP][STASH][GEM_NAME] eq AMULET_NAME;
    return 0;
}

sub has_lost {
    restore_term();
    my $score = score();
    print CLEAR_SCREEN, "Alas, you did not win this time.\n\n$score\n";
    exit(1);
}

sub has_won {
    restore_term();
    my $score = score();
    # some of this is borrowed from rogue 3.6.3
    print CLEAR_SCREEN, <<"WIN_SCREEN";

  @   @               @   @           @          @@@  @     @
  @   @               @@ @@           @           @   @     @
  @   @  @@@  @   @   @ @ @  @@@   @@@@  @@@      @  @@@    @
   @@@@ @   @ @   @   @   @     @ @   @ @   @     @   @     @
      @ @   @ @   @   @   @  @@@@ @   @ @@@@@     @   @     @
  @   @ @   @ @  @@   @   @ @   @ @   @ @         @   @  @
   @@@   @@@   @@ @   @   @  @@@@  @@@@  @@@     @@@   @@   @

    Congratulations. You have emerged victorious!

$score
WIN_SCREEN
    exit(0);
}

sub help_screen {
    print CLEAR_SCREEN, at(1, 1), <<'HELP_SCREEN', "\n:", SHOW_CURSOR;
                     Xomb Commands Reference Manual

     y  k  u     Motion is traditional to rogue(6) as shown in the
      \ | /      compass to the left. Other commands, of which some
    h - @ - l    take time to complete, include:
      / | \             
     b  j  n                . - wait a turn      x - examine board
                          g , - pick up item     i - show inventory
    M - show messages     < > - activate gate    e - equip a gem
    E - clear PKC code    C-l - redraw screen    R - remove a gem
    ? - show help         v   - show version     d - drop a gem

    Esc or q will exit from sub-displays such as this one. Prompts
    must be answered with Y to carry out the action; N or n or Esc
    will reject the action. Map symbols include:

      @  you     % gate    * gemstone    o hole in ground
      #  wall    ~ acid    ^ rubble      . empty cell

    Consult xomb(1) or `perldoc xomb` for additional documentation.
HELP_SCREEN
    await_quit();
    print HIDE_CURSOR;
    refresh_board(20);
}

# something something Panopticon
sub hide_screen {
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

# basically an expensive gem (vegetable) with speciation involved
sub make_amulet {
    my ($col, $row) = @_;
    state $made = 0;
    # DBG will need to make it multiple times if they jump through a
    # hole and miss where we put it, but not if they have one in
    # stock?
    if ($made) {
        warn "already made an amulet\n";
        return;
    }
    my $amulet;
    $amulet->@[ GENUS, SPECIES, DISPLAY ] = (VEGGIE, AMULET, ',');
    $amulet->[STASH][GEM_NAME] = AMULET_NAME;
    $GGV += $amulet->[STASH][GEM_VALUE] = 1000;
    # meh, nuke the lowly gem already there
    #die "already a veggie $col,$row??\n" if defined $LMap[$row][$col][VEGGIE];
    $LMap[$row][$col][VEGGIE] = $amulet;
    $made = 1;
}

sub make_gem {
    my ($col, $row) = @_;
    my ($name, $value);
    if (int rand 16 == 0) {
        $name  = "Bloodstone";
        $value = 80 + roll(4, 10);
    } elsif (int rand 8 == 0) {
        $name  = "Sunstone";
        $value = 40 + roll(2, 10);
    } else {
        $name  = "Moonstone";
        $value = 10 + roll(2, 10);
    }
    my @adj = qw/Imperial Mystic Rose Smoky Warped/;
    if (int rand 3 == 0) {
        $name = $adj[ rand @adj ] . ' ' . $name;
        $value += 10 + roll(2, 20);
    }
    my $gem;
    $gem->@[ GENUS, SPECIES, DISPLAY ] = (VEGGIE, GEM, '*');
    $gem->[STASH][GEM_NAME]  = $name;
    $gem->[STASH][GEM_VALUE] = $value;
    die "already a veggie $col,$row??\n"
      if defined $LMap[$row][$col][VEGGIE];
    $LMap[$row][$col][VEGGIE] = $gem;
    $GGV += $value;
}

sub make_monster {
    my ($col, $row, %params) = @_;
    my $monst;
    $monst->@[ GENUS, SPECIES, DISPLAY, ENERGY, UPDATE, LMC ] = (
        ANIMAL,          $params{species}, $params{display},
        $params{energy}, \&update_monster, $LMap[$row][$col]
    );
    $monst->[STASH][HITPOINTS] = $params{hp};
    $monst->[STASH][ECOST]     = CAN_MOVE;      # previous move cost
    push @Animates, $monst;
    die "DBG already occupied??" if defined $LMap[$row][$col][ANIMAL];
    $LMap[$row][$col][ANIMAL] = $monst;
    weaken($monst->[LMC]);
}

# TODO may need x,y if calling this from generate fn instead of game_l
sub make_player {
    $Animates[HERO]->@[ GENUS, SPECIES, DISPLAY, ENERGY, UPDATE ] =
      (ANIMAL, HERO, '@', CAN_MOVE, \&update_player,);
    $Animates[HERO][STASH][HITPOINTS] = START_HP;
    $Animates[HERO][STASH][ECOST]     = CAN_MOVE;    # prev. move cost
    $Animates[HERO][STASH][LOOT]      = [];
}

# KLUGE too long. break out and abstractify... but that clock be tickin
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
              . $item->[DISPLAY] . ' '
              . veggie_name($item);
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
        $s .= ' or (d)rop, (e)quip' if $has_loot;
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
        } elsif ($key eq 'e') {
            if (!defined $message) {
                print at_row(MSG_ROW + $offset), CLEAR_RIGHT,
                  "-- equip item L)able or Esc to exit --";
            }
            while (1) {
                my $use = ReadKey(0);
                last CMD if $use eq "\033" or $use eq 'q';
                if ($use =~ m/^[A-X]$/) {    # NOTE related to LOOT_MAX
                    my $i = ord($use) - 65;
                    if ($i < $loot->@*) {
                        use_item($loot, $i, $Animates[HERO][STASH]);
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
        my $target = $LMap[$drow][$dcol][$i];
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

sub move_examine {
    my $row = $Animates[HERO][LMC][WHERE][PROW];
    my $col = $Animates[HERO][LMC][WHERE][PCOL];
    print AT_MSG_ROW, CLEAR_RIGHT, SHOW_CURSOR,
      "-- move cursor to view a cell. SHIFT moves faster. Esc to exit --";
    while (1) {
        my $loc = $col . ',' . $row;
        my $s   = '';
        if (exists $Visible_Cell{$loc}) {
            for my $i (ANIMAL, VEGGIE) {
                my $x = $LMap[$row][$col][$i];
                $s .= $x->[DISPLAY] . ' ' . $Descript{ $x->[SPECIES] } . ' '
                  if defined $x;
            }
            my $g = $LMap[$row][$col][MINERAL];
            $s .= $g->[DISPLAY] . ' ' . $Descript{ $g->[SPECIES] }
              if defined $g;
        }
        print at_row(STATUS_ROW), CLEAR_RIGHT, $s,
          at(map { MAP_DOFF + $_ } $col, $row);
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
    return MOVE_OKAY, DEFAULT_COST;
}

sub move_pickup {
    my $lmc = $Animates[HERO][LMC];
    return MOVE_FAILED, 0, 'PKC-0101' unless defined $lmc->[VEGGIE];
    my $loot = $Animates[HERO][STASH][LOOT];
    return MOVE_FAILED, 0, 'PKC-0102' if $loot->@* >= LOOT_MAX;
    if ($lmc->[VEGGIE][SPECIES] == AMULET) {
        log_message('Obtained ' . AMULET_NAME . '! Ascend to win!');
    } else {
        log_message('Picked up ' . veggie_name($lmc->[VEGGIE]));
    }
    push $loot->@*, $lmc->[VEGGIE];
    $lmc->[VEGGIE] = undef;
    print display_cellobjs();
    my $cost = DEFAULT_COST;
    $cost += rubble_delay($Animates[HERO], $cost)
      if $lmc->[MINERAL][SPECIES] == RUBBLE;
    return MOVE_OKAY, $cost;
}

sub move_player_maker {
    my ($cols, $rows, $mvcost) = @_;
    sub {
        my @ret = move_animate($Animates[HERO], $cols, $rows, $mvcost);
        #warn "DBG COST @ret\n";
        raycast_fov($ret[0] == MOVE_FAILED ? 0 : 1);
        print display_cellobjs();
        return @ret;
    }
}

sub move_remove {
    return MOVE_FAILED, 0, 'PKC-0113'
      unless defined $Animates[HERO][STASH][SHIELDUP];
    my $loot = $Animates[HERO][STASH][LOOT];
    return MOVE_FAILED, 0, 'PKC-0102' if $loot->@* >= LOOT_MAX;
    push $loot->@*, $Animates[HERO][STASH][SHIELDUP];
    undef $Animates[HERO][STASH][SHIELDUP];
    return MOVE_FAILED, 0;
}

sub move_use {
    return MOVE_FAILED, 0, 'PKC-0112'
      unless $Animates[HERO][STASH][LOOT]->@*;
    @_ = ('e', 'equip item L)abel or Esc to exit');
    goto &manage_inventory;
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

sub raycast_fov {
    my ($refresh, $lines) = @_;
    if (!$refresh and defined $FOV) {
        print $FOV;
        return;
    }

    my (%blocked, %byrow);
    my ($cx, $cy) = $Animates[HERO][LMC][WHERE]->@*;
    %Visible_Cell = ();

    # radius 7 points taken from Game:RaycastFOV cache
    for my $ep (
        [ 7,  0 ],  [ 7,  1 ],  [ 7,  2 ],  [ 6,  2 ],
        [ 6,  3 ],  [ 6,  4 ],  [ 5,  4 ],  [ 5,  5 ],
        [ 4,  5 ],  [ 4,  6 ],  [ 3,  6 ],  [ 2,  6 ],
        [ 2,  7 ],  [ 1,  7 ],  [ 0,  7 ],  [ -1, 7 ],
        [ -2, 7 ],  [ -2, 6 ],  [ -3, 6 ],  [ -4, 6 ],
        [ -4, 5 ],  [ -5, 5 ],  [ -5, 4 ],  [ -6, 4 ],
        [ -6, 3 ],  [ -6, 2 ],  [ -7, 2 ],  [ -7, 1 ],
        [ -7, 0 ],  [ -7, -1 ], [ -7, -2 ], [ -6, -2 ],
        [ -6, -3 ], [ -6, -4 ], [ -5, -4 ], [ -5, -5 ],
        [ -4, -5 ], [ -4, -6 ], [ -3, -6 ], [ -2, -6 ],
        [ -2, -7 ], [ -1, -7 ], [ 0,  -7 ], [ 1,  -7 ],
        [ 2,  -7 ], [ 2,  -6 ], [ 3,  -6 ], [ 4,  -6 ],
        [ 4,  -5 ], [ 5,  -5 ], [ 5,  -4 ], [ 6,  -4 ],
        [ 6,  -3 ], [ 6,  -2 ], [ 7,  -2 ], [ 7,  -1 ]
    ) {
        linecb(
            sub {
                my ($col, $row, $iters) = @_;

                # "the moon is a harsh mistress" -- FOV degrades at range
                return -1 if $iters - 4 > int rand 7;

                my $loc = $col . ',' . $row;
                return -1 if $blocked{$loc};

                # FOV may also be blocked by walls, etc.
                my $cell = $LMap[$row][$col][MINERAL];
                if ($cell->[SPECIES] == WALL) {
                    $blocked{$loc} = 1;
                } elsif ($cell->[SPECIES] == RUBBLE) {
                    $blocked{$loc} = 1 if 0 == int rand 2;
                } elsif ($cell->[SPECIES] == ACID) {
                    $blocked{$loc} = 1 if 0 == int rand 200;
                }

                return 0 if $Visible_Cell{$loc};
                $Visible_Cell{$loc} = [$col,$row];
                for my $i (ANIMAL, VEGGIE) {
                    if (defined $LMap[$row][$col][$i]) {
                        push $byrow{$row}->@*, [ $col, $LMap[$row][$col][$i][DISPLAY] ];
                        return 0;
                    }
                }
                push $byrow{$row}->@*, [ $col, $cell->[DISPLAY] ];
                return 0;
            },
            $cx,
            $cy,
            $cx + $ep->[0],
            $cy + $ep->[1]
        );
    }

    my $s = '';
    for my $r (0 .. MAP_ROWS - 1) {
        $s .= at_row(MAP_DOFF + $r) . CLEAR_RIGHT;
    }
    for my $r (nsort_by { $byrow{$_} } keys %byrow) {
        $s .= at_row(MAP_DOFF + $r);
        for my $ref (nsort_by { $_->[0] } $byrow{$r}->@*) {
            $s .= at_col(MAP_DOFF + $ref->[0]) . $ref->[1];
        }
    }

    # ensure @ is shown as FOV may not touch that cell
    $FOV =
        $s
      . at(map { MAP_DOFF + $_ } $cx, $cy)
      . $LMap[$cy][$cx][ANIMAL][DISPLAY];
    print $FOV;
    $Visible_Cell{ $cx . ',' . $cy} = [$cx,$cy];
}

sub refresh_board {
    my ($lines) = @_;
    print CLEAR_SCREEN unless $lines;
    raycast_fov(0, $lines);
    show_top_message();
    show_status_bar()
      if !defined $lines
      or ($lines and $lines >= STATUS_ROW);
}

sub relocate {
    my ($ani, $dest) = @_;
    my $src      = $ani->[LMC][WHERE];
    my $dest_lmc = $LMap[ $dest->[PROW] ][ $dest->[PCOL] ];
    $dest_lmc->[ANIMAL] = $ani;
    undef $LMap[ $src->[PROW] ][ $src->[PCOL] ][ANIMAL];
    $ani->[LMC] = $dest_lmc;
    weaken($ani->[LMC]);
}

sub restore_term {
    ReadMode 'restore';
    print TERM_NORM, SHOW_CURSOR, UNALT_SCREEN;
}

sub roll {
    my ($times, $sides) = @_;
    my $sum = $times;
    $sum += int rand $sides while $times-- > 0;
    return $sum;
}

sub rubble_delay {
    my ($ani, $cost) = @_;
    if (int rand 2 == 0) {
        if ($ani->[SPECIES] == HERO) {
            # Ultima IV does this. too annoying?
            sleep(0.05 + rand() / 3);
            log_message('Slow progress!');
        }
        return int($cost / 2) + 2 + int rand 4;
    } else {
        return 2 + int rand 4;
    }
}

sub score {
    my $score = loot_value();
    return
      "Score: $score in $Turn_Count turns and $Energy_Spent energy use";
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
    #warn "DBG ACID duration $duration\n";
    log_code('PKC-007E') if $ani->[SPECIES] == HERO;
    # TUNING about 20 turns to die from sitting in acid? is that too high?
    apply_damage($ani, 'acidburn', $duration) unless $newcell;
}

sub passive_msg_maker {
    my ($message, $oneshot) = @_;
    sub {
        my ($ani, $obj, $duration, $newcell) = @_;
        #warn sprintf "APAMM %s %d,%d\n", $ani->[DISPLAY], $ani->[LMC][WHERE]->@*;
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

sub loot_value {
    my $value = 0;
    for my $item ($Animates[HERO][STASH][LOOT]->@*) {
        if ($item->[SPECIES] == AMULET) {
            $value += 1000;
        } elsif ($item->[SPECIES] == GEM) {
            $value += $item->[STASH][GEM_VALUE];
        } else {
            warn "DBG item with no score value $item->[SPECIES] ??\n";
        }
    }
    # they probably won't need to charge their shield after the game
    # is over?
    $value += $Animates[HERO][STASH][SHIELDUP][STASH][GEM_VALUE]
      if defined $Animates[HERO][STASH][SHIELDUP];
    return $value;
}

sub update_gameover {
    state $count = 0;
    tcflush(STDIN_FILENO, TCIFLUSH);
    my $key = ReadKey(0);
    if ($count > TIME_TO_DIE) {
        print AT_MSG_ROW, CLEAR_RIGHT, '-- press Esc to continue --';
        has_lost() if $key eq "\033" or $key eq 'q';
    } else {
        log_message('Communication lost with remote unit.');
    }
    $count++;
    return MOVE_OKAY, DEFAULT_COST;
}

sub update_monster {
    warn "monster twiddles thumbs waiting to be implemented\n";
    return MOVE_OKAY, DEFAULT_COST * 4;
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
    if (defined $Animates[HERO][STASH][SHIELDUP]
        and $Animates[HERO][STASH][HITPOINTS] < START_HP) {
        my $need  = START_HP - $Animates[HERO][STASH][HITPOINTS];
        my $offer = between(
            0,
            int($cost / 3),    # max heal rate over "time"
            $Animates[HERO][STASH][SHIELDUP][STASH][GEM_VALUE]
        );
        my $heal = between(0, $need, $offer);
        #warn "HEALUP $need $offer $heal\n";
        $Animates[HERO][STASH][SHIELDUP][STASH][GEM_VALUE] -= $heal;
        $Animates[HERO][STASH][HITPOINTS]                  += $heal;
        $GGV                                               -= $heal;
        # <= 0 and meh if hit release time
        if ($Animates[HERO][STASH][SHIELDUP][STASH][GEM_VALUE] < 0) {
            die "DBG math is hard??";
        }
        if ($Animates[HERO][STASH][SHIELDUP][STASH][GEM_VALUE] == 0) {
            log_code('PKC-0113');
            undef $Animates[HERO][STASH][SHIELDUP];
        }
    }
    $Energy_Spent += $cost;
    $Turn_Count++;
    return $ret, $cost;
}

sub use_item {
    my ($loot, $i, $stash) = @_;
    if (!($loot->[$i][SPECIES] == GEM or $loot->[$i][SPECIES] == AMULET)) {
        log_code('PKC-0111');
        return;
    }
    if (defined $stash->[SHIELDUP]) {
        warn "DBG DOIN SWAP\n";
        ($stash->[SHIELDUP], $loot->[$i]) = ($loot->[$i], $stash->[SHIELDUP]);
    } else {
        warn "DBG INSERT ($i)\n";
        warn Dumper $loot->[$i], $stash->[SHIELDUP];
        $stash->[SHIELDUP] = splice $loot->@*, $i, 1;
        warn Dumper $loot->[$i], $stash->[SHIELDUP];
        warn "DONE INSERT ($i)\n";
    }
}

sub veggie_name {
    my ($veg) = @_;
    my $s;
    if ($veg->[SPECIES] == GEM or $veg->[SPECIES] == AMULET) {
        $s = sprintf "(%d) %s", $veg->[STASH]->@[ GEM_VALUE, GEM_NAME ];
    } else {
        $s = $Descript{ $veg->[SPECIES] } // die "DBG uh what name??";
    }
    return $s;
}

sub with_adjacent {
    my ($col, $row, $fn) = @_;
    for my $adj (
        [ -1, -1 ], [ -1, 0 ],  [ -1, 1 ], [ 0, -1 ],
        [ 0,  1 ],  [ 1,  -1 ], [ 1,  0 ], [ 1, 1 ]
    ) {
        $fn->($col + $adj->[PCOL], $row + $adj->[PROW]);
    }
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
