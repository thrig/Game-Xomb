# -*- Perl -*-
#
# Game::Xomb - this is a terminal-based roguelike. run the xomb(1)
# command that is installed with this module to start a game

package Game::Xomb;

our $VERSION = '0.20';

use 5.24.0;
use warnings;
use List::Util qw(min max);
use List::UtilsBy qw(min_by nsort_by);
use POSIX qw(STDIN_FILENO TCIFLUSH tcflush);
use Scalar::Util qw(weaken);
use Term::ReadKey qw(GetTerminalSize ReadKey ReadMode);
use Time::HiRes qw(sleep);

require XSLoader;
XSLoader::load('Game::Xomb', $VERSION);    # linecb, walkcb

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
sub AT_SHIELDUP ()   { "\e[24;72H" }
sub AT_ERR_CODE ()   { "\e[24;76H" }

# NOTE also set in Xomb.xs for map-aware functions
sub MAP_COLS () { 78 }
sub MAP_ROWS () { 22 }
sub MAP_SIZE () { MAP_COLS * MAP_ROWS }
sub MAP_DOFF () { 2 }                     # display offset for map on screen

# NOTE level map is row, col while points are [ col, row ]
sub PROW () { 1 }
sub PCOL () { 0 }

# a point in the LMC so Animates can find where they are at
sub WHERE () { 0 }
# GENUS is involved with interactions between thingies and where the
# thing is slotted under the LMC
sub MINERAL () { 1 }    # floor, walls, etc
sub VEGGIE ()  { 2 }    # amulet, gems, etc
sub ANIMAL ()  { 3 }    # Animates

# SPECIES
sub HERO ()    { 0 }    # NOTE also used for @Animates slot
sub FUNGI ()   { 1 }
sub GHAST ()   { 2 }
sub MIMIC ()   { 3 }
sub STALKER () { 4 }
sub TROLL ()   { 5 }
sub AMULET ()  { 6 }
sub GEM ()     { 7 }
sub HOLE ()    { 8 }
sub FLOOR ()   { 9 }
sub GATE ()    { 10 }
sub ACID ()    { 11 }
sub RUBBLE ()  { 12 }
sub WALL ()    { 13 }

sub AMULET_NAME ()  { 'Dragonstone' }
sub AMULET_REGEN () { 5 }               # slow so less likely to burn out
sub AMULET_VALUE () { 1000 }

# for ANIMALS (shared with VEGGIES and MINERALS for the first few slots)
sub GENUS ()      { 0 }
sub SPECIES ()    { 1 }
sub DISPLAY ()    { 2 }                 # how to show 'em on the screen
sub UPDATE ()     { 3 }                 # what happens when their turn comes up
sub STASH ()      { 4 }                 # kitchen drawer
sub LMC ()        { 5 }                 # link back to the level map
sub BLACK_SPOT () { 6 }                 # marked for death
sub ENERGY ()     { 7 }                 # how long until their next update call

# Animates stash slots
sub HITPOINTS () { 0 }                  # player, monsters
sub ECOST ()     { 1 }                  # cost of previous move
sub WEAPON ()    { 2 }                  # mostly only for monsters
sub LOOT ()      { 3 }                  # player inventory
sub SHIELDUP ()  { 4 }                  # player shield recharge gem
# GEM stash slots
sub GEM_NAME ()  { 0 }
sub GEM_VALUE () { 1 }
sub GEM_REGEN () { 2 }

sub START_HP () { 100 }                 # player start (and max) HP
sub LOOT_MAX () { NEED_ROWS - 2 }       # avoids scrolling, status bar wipeout

sub WEAP_DMG () { 0 }    # for WEAPON stash slot (mostly for monsters)
sub W_RANGE ()  { 1 }    # max shooting range
sub W_COST ()   { 2 }    # recharge time after shot
sub W_TOHIT ()  { 3 }    # to-hit values ...

sub MOVE_LVLUP ()   { -1 }    # NOTE tied to level change math
sub MOVE_FAILED ()  { 0 }     # for zero-cost player moves
sub MOVE_LVLDOWN () { 1 }     # NOTE tied to level change math
sub MOVE_OKAY ()    { 2 }     # non-level-change costly moves

# energy constants, see game_loop for the system
sub CAN_MOVE ()     { 0 }
sub DEFAULT_COST () { 10 }
sub DIAG_COST ()    { 14 }
sub NLVL_COST ()    { 20 }    # time to gate to next level

########################################################################
#
# VARIABLES

our $Violent_Sleep_Of_Reason = 0;

our @Animates;    # things with energy, HERO always in first slot
our @LMap;        # level map. array of array of array of ...

our $Draw_Delay   = 0.15;
our $Energy_Spent = 0;
our $Level        = 1;      # current level
our $Seed;
our $Turn_Count = 0;        # player moves
our %Visible_Cell;          # x,y => [x,y] of cells visible in FOV
our %Warned_About;          # limit annoying messages

#  cpanm App::Prove
#  XOMB_STATS=1 prove t/damage-stats.t
our %Damage_From = (
    acidburn => sub {
        my ($duration) = @_;
        my $burn = 0;
        $burn += int rand 2 for 1 .. $duration;
        return $burn;
    },
    attackby => sub {
        my ($ani) = @_;
        goto $ani->[STASH][WEAPON][WEAP_DMG]->&*;
    },
    # this can hurt
    falling => sub {
        my $dice   = 1;
        my $damage = 0;
        while (1) {
            my $roll = roll($dice, 4);
            $damage += $roll;
            last if $roll <= 2 or $dice >= 4;
            $dice++;
        }
        return $damage;
    },
    # FUNGI does direct or splash damage; pretty sure that splash will
    # only be friendly fire (check the fungi update logic)
    plburn => sub {
        my ($range) = @_;
        my $dice    = 4 - $range;
        my $damage  = 0;
        do {
            $damage = roll($dice, 3 * $dice);
        } until ($damage <= 15);
        return $damage;
    },
    plsplash => sub { roll(2, 4) },
    # listed here for easy access but get called to through 'attackby'.
    # distance and environment effects will reduce this damage rate
    # over time
    GHAST,
    sub { roll(3, 2) - 2 },
    HERO,
    sub { roll(4, 3) + 4 },
    MIMIC,
    sub { roll(2, 4) },
    STALKER,
    sub { roll(4, 2) },
    TROLL,
    sub {
        my $damage = 0;
        do {
            $damage = roll(3, 6);
        } until ($damage <= 15);
        return $damage;
    },
);

# fairly low so not too much bumping needed
our %Hit_Points = (FUNGI, 32, GHAST, 18, MIMIC, 22,, STALKER, 32, TROLL, 48,);
# NOTE these MUST be kept in sync with the W_RANGE max
our %To_Hit = (
    FUNGI, [ 100, 100, 100 ],
    GHAST, [ 65, 50, 35, 25, 10 ],
    MIMIC, [ 5, 20, 35, 45, 45, 35, 20, 5 ],
    STALKER, [ 65, 75, 70, 65, 60, 55, 50, 45, 45, 30, 25, 10 ],
    TROLL,   [ 75, 70, 55, 40, 25, 15, 5 ],
);
# W_RANGE how far the monster will shoot; W_COST is how long the weapon
# takes to recharge after a successful shot
#
#   W_RANGE  W_COST
our %Weap_Stats = (
    FUNGI,   [ 3,  31 ], GHAST, [ 5, 6 ], MIMIC, [ 8, 21 ],
    STALKER, [ 12, 21 ], TROLL, [ 7, 37 ],
);

# these are "class objects"; in other words there is only one WALL in
# all WALL cells (see reify and the make_* routines)
#
#              GENUS    SPECIES DISPLAY UPDATE (passive effects)
our %Thingy = (
    # animals
    FUNGI,   [ ANIMAL, FUNGI,   'F', \&update_fungi ],
    GHAST,   [ ANIMAL, GHAST,   'G', \&update_ghast ],
    HERO,    [ ANIMAL, HERO,    '@', \&update_player ],
    MIMIC,   [ ANIMAL, MIMIC,   'M', \&update_mortar ],
    STALKER, [ ANIMAL, STALKER, 'Q', \&update_stalker ],
    TROLL,   [ ANIMAL, TROLL,   'T', \&update_troll ],
    # vegetables
    AMULET, [ VEGGIE, AMULET, ',' ],
    GEM,    [ VEGGIE, GEM,    '*' ],
    # minerals
    ACID,  [ MINERAL, ACID,  '~', \&passive_burn ],
    FLOOR, [ MINERAL, FLOOR, '.' ],
    GATE,   [ MINERAL, GATE,   '%' ],    # stair, rogue 3.6 style
    HOLE,   [ MINERAL, HOLE,   ' ' ],    # shaft
    RUBBLE, [ MINERAL, RUBBLE, '^' ],
    WALL,   [ MINERAL, WALL,   '#' ],
);

# NOTE these need to be fairly short as they must all fit in status line
# and there could be three of them; see move_examine
our %Descript = (
    ACID,   'shallow acid pool',  AMULET,  AMULET_NAME,
    FLOOR,  'floor',              FUNGI,   'Plasma Tower',
    GATE,   'Gate',               GEM,     'gemstone',
    GHAST,  'Gatling Autocannon', HERO,    'You',
    HOLE,   'Crevasse',           MIMIC,   'Mortar',
    RUBBLE, 'bunch of rubble',    STALKER, 'Quad-laser Array',
    TROLL,  'Railgun Tower',      WALL,    'wall',
);

# what happens when moving into the given GENUS (allow the move,
# fisticuffs, etc.). called from move_animate. vegetables are ignored
# for reasons discussed over in said function (also less code, yay!)
our %Bump_Into = (
    ANIMAL,
    sub {
        my ($mover, $dpoint, $target, $cost) = @_;
        # NOTE only for the player, none of the monsters move
        if (int rand 100 < 95) {
            apply_damage($target, 'attackby', $mover);
        } else {
            log_code('0302') if $mover->[SPECIES] == HERO;
        }
        # try not to fight whilst in acid? of course any dungeon master
        # worth their salt would immediately build railgun towers
        # surrounded by acid ponds...
        $cost += rubble_delay($mover, $cost)
          if $mover->[LMC][MINERAL][SPECIES] == RUBBLE;
        apply_passives($mover, $cost, 0);
        return MOVE_OKAY, $cost;
    },
    MINERAL,
    sub {
        my ($mover, $dpoint, $target, $cost) = @_;
        return MOVE_FAILED, 0, '0002' if $target->[SPECIES] == WALL;
        # NOTE the rubble delay is applied *before* they can move out of
        # that acid pond that they are in:
        #   "Yes, we really hate players, damn their guts."
        #     -- Dungeon Crawl Stone Soup, cloud.cc
        $cost += rubble_delay($mover, $cost) if $target->[SPECIES] == RUBBLE;
        if ($target->[SPECIES] == HOLE) {
            return MOVE_FAILED, 0
              if nope_regarding('Falling may cause damage');
            apply_passives($mover, $cost / 2, 0);
            my $ret = MOVE_OKAY;
            if ($mover->[SPECIES] == HERO) {
                log_message('You plunge into the hole.') unless $Warned_About{falling}++;
                relocate($mover, $dpoint);
                log_code('0099');
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
    # motion
    'h' => move_player_maker(-1, +0, DEFAULT_COST),
    'j' => move_player_maker(+0, +1, DEFAULT_COST),
    'k' => move_player_maker(+0, -1, DEFAULT_COST),
    'l' => move_player_maker(+1, +0, DEFAULT_COST),
    'y' => move_player_maker(-1, -1, DIAG_COST),
    'u' => move_player_maker(+1, -1, DIAG_COST),
    'b' => move_player_maker(-1, +1, DIAG_COST),
    'n' => move_player_maker(+1, +1, DIAG_COST),
    # misc
    ' '    => \&move_nop,
    ','    => \&move_pickup,
    '.'    => \&move_nop,
    '<'    => \&move_gate_up,
    '>'    => \&move_gate_down,
    '?'    => sub { help_screen(); return MOVE_FAILED, 0 },
    '@'    => \&report_position,
    'E'    => \&move_equip,
    'G'    => sub { hide_screen(); return MOVE_FAILED, 0 },
    'M'    => sub { show_messages(); return MOVE_FAILED, 0 },
    'Q'    => \&move_quit,
    'R'    => \&move_remove,
    'd'    => \&move_drop,
    'g'    => \&move_pickup,
    'i'    => \&manage_inventory,
    'p'    => sub { clear_code(); return MOVE_FAILED, 0 },
    'v'    => \&report_version,
    'x'    => \&move_examine,
    "\003" => sub { return MOVE_FAILED, 0, '1203' },            # <C-c>
    "\014" => sub { refresh_board(); MOVE_FAILED, 0 },          # <C-l>
    "\032" => sub { return MOVE_FAILED, 0, '1220' },            # <C-z>
    "\033" => sub { return MOVE_FAILED, 0, '121B' },
);
# and an effort at numpad support, however weak
@Key_Commands{qw/1 2 3 4 5 6 7 8 9/} = @Key_Commands{qw/b j n h . l y k u/};

my @Level_Features = (
    {   ACID, 50, GATE, 2, HOLE, 200, RUBBLE, 400, WALL, 100, xarci => [ GHAST, MIMIC ],
    },
    {   ACID, 100, GATE, 2, HOLE, 100, RUBBLE, 200, WALL, 200,
        xarci => [ GHAST, MIMIC, STALKER, TROLL ],
    },
    {   ACID, 400, GATE, 2, RUBBLE, 400, WALL, 50,
        xarci => [ FUNGI, GHAST, MIMIC, STALKER, TROLL ],
    },
    {   ACID, 100, AMULET, 1, GATE, 2, RUBBLE, 0, WALL, 300,
        xarci => [ FUNGI, GHAST, STALKER, TROLL ],
    },
);

########################################################################
#
# SUBROUTINES

sub apply_damage {
    my ($ani, $cause, @rest) = @_;
    $ani->[STASH][HITPOINTS] -= $Damage_From{$cause}->(@rest);
    if ($ani->[STASH][HITPOINTS] <= 0) {
        if ($ani->[SPECIES] == HERO) {
            $ani->[DISPLAY] = '&';                 # the @ got unravelled
            $ani->[UPDATE]  = \&update_gameover;
        } else {
            # KLUGE assume source of damage was the player, otherwise
            # would need to always have a source object in @rest to
            # peek at...
            log_message($Descript{ $ani->[SPECIES] } . ' destroyed.');
            $ani->[BLACK_SPOT] = 1;
            undef $ani->[LMC][ANIMAL];
        }
    }
    if ($ani->[SPECIES] == HERO) {
        print display_hitpoints();
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
    print at_col(0), CLEAR_LINE;
    warn $_[0] if @_;
    game_over('Minos III was unexpectedly hit by a rogue planet, the end.');
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
        show_top_message();
    }

    sub show_messages {
        my $s = SHOW_CURSOR;
        while (my ($i, $message) = each @log) {
            $s .= at_row(MSG_ROW + $i) . CLEAR_RIGHT . $message;
        }
        print $s, at_row(MSG_ROW + @log), CLEAR_RIGHT, "-- press Esc to continue --";
        await_quit();
        print HIDE_CURSOR;
        refresh_board();
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

sub display_shieldup {
    my $ch = ' ';
    if (defined $Animates[HERO][STASH][SHIELDUP]) {
        if ($Animates[HERO][STASH][SHIELDUP][STASH][GEM_NAME] eq AMULET_NAME) {
            $ch = $Thingy{ AMULET, }->[DISPLAY];
        } else {
            $ch = $Thingy{ GEM, }->[DISPLAY];
        }
    }
    AT_SHIELDUP . '[' . $ch . ']';
}

sub distance {
    my ($pcol, $prow, $mcol, $mrow) = @_;
    return int sqrt(($pcol - $mcol)**2 + abs($prow - $mrow)**2);
}

# does a monster hit? -1 for out of range, 0 for miss, 1 for hit
sub does_hit {
    my ($dist, $weap) = @_;
    if ($dist > $weap->[W_RANGE]) {
        # snooze monster for minimum time for player to be in range
        my $away = $dist - $weap->[W_RANGE];
        return -1, DEFAULT_COST * $away;
    }
    return ($weap->[ W_TOHIT + $dist - 1 ] > int rand 100), $weap->[W_COST];
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
    $SIG{$_}    = \&bail_out for qw(INT HUP TERM PIPE QUIT USR1 USR2 __DIE__);
    $SIG{CONT}  = \&refresh_board;
    $SIG{WINCH} = sub {
        if (bad_terminal()) {
            log_message('The terminal is too small!') unless $Warned_About{badterm}++;
        }
        refresh_board();
    };
    STDOUT->autoflush(1);

    $Animates[HERO] = make_player();
    generate_map();

    print ALT_SCREEN, HIDE_CURSOR, HIDE_POINTER, CLEAR_SCREEN, TERM_NORM;
    log_message('Welcome to xomb.');
    show_top_message();
    show_status_bar();

    # simple integer-based energy system; Animates with 0 cost all have
    # a go, and their energy is incremented by how costly the move they
    # make is. next up is the Animate with the lowest cost; this lowest
    # cost is subtracted off each Animate and then those at 0 ...
  GLOOP: while (1) {
        my $min_cost = min(map { $_->[ENERGY] } @Animates);

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
            my ($status, $cost) = $ani->[UPDATE]->($ani);
            $ani->[ENERGY] += $ani->[STASH][ECOST] = $cost;
            $new_level = $status
              if $status == MOVE_LVLDOWN or $status == MOVE_LVLUP;
            if ($ani->[SPECIES] == HERO) {
                show_top_message();
                show_status_bar();
            }
        }

        if ($new_level != 0) {
            $Level += $new_level;
            has_won() if $Level <= 0;
            generate_map();
            raycast_fov(1);
            # NOTE other half of this is applied in the Bump-into-HOLE
            # logic, elsewhere. this last half happens here as the new
            # level is not yet available prior to the fall
            apply_passives($Animates[HERO], $Animates[HERO][STASH][ECOST] / 2, 1);
            show_status_bar();
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
    print at_col(0), CLEAR_LINE, $message, "\n", CLEAR_LINE;
    exit(1);
}

# "somos comparables al hechicero que teje un laberinto y que se ve
# forzado a errar en él hasta el fin de sus días"
#   -- Borges, Deutsches Requiem
sub generate_map {
    my ($herop, @goodp, %seen);

    # reuse the bottom features if they get below that. there should not
    # be holes on the bottom level, and the gate activation code should
    # prevent them from going too deep
    my $findex = min($Level, scalar @Level_Features) - 1;

    # preserve where player is, otherwise clear the board and reduce
    # animates to just the player
    if (defined $Animates[HERO][LMC]) {
        $herop = $Animates[HERO][LMC][WHERE];
        $seen{ join ',', $herop->@* } = 1;
        undef $Animates[HERO][LMC];
    }
    splice @Animates, 1;
    @LMap = ();

    # basement of WHERE and FLOOR plus some white noise seed points for
    # future decorations
    my @seeds;
    my $left  = 200;        # hopefully overkill
    my $total = MAP_SIZE;
    for my $r (0 .. MAP_ROWS - 1) {
        for my $c (0 .. MAP_COLS - 1) {
            my $point = [ $c, $r ];    # PCOL, PROW (x, y)
            push $LMap[$r]->@*, [ $point, $Thingy{ FLOOR, } ];
            if (int rand $total < $left) {
                next if exists $seen{ join ',', $point->@* };
                push @seeds, $point;
                $left--;
            }
            $total--;
        }
    }

    # place player (back) on the map
    my ($col, $row);
    $herop = splice @seeds, rand @seeds, 1 unless defined $herop;
    ($col, $row) = $herop->@*;
    $LMap[$row][$col][ANIMAL] = $Animates[HERO];
    $Animates[HERO][LMC] = $LMap[$row][$col];
    weaken $Animates[HERO][LMC];

    # gems (vegetables) get scattered hither and yon
    if (exists $Level_Features[$findex]{ AMULET, } and !has_amulet()) {
        my $gem   = (make_amulet())[0];
        my $point = splice @seeds, rand @seeds, 1;
        ($col, $row) = $point->@*;
        push @goodp, $point;
        $LMap[$row][$col][VEGGIE]  = $gem;
        $LMap[$row][$col][MINERAL] = $Thingy{ FLOOR, };
        $seen{ $col . ',' . $row } = 1;
        log_message('Proximal ' . AMULET_NAME . ' readings detected.');
    }
    my $GGV    = 0;
    my $gcount = 0;
    my $gmax   = 250 + int(50 * exp($Level / 2.5));
    my $make   = !has_amulet();
    while ($make) {
        my ($gem, $value) = make_gem();
        my $point = splice @seeds, rand @seeds, 1;
        ($col, $row) = $point->@*;
        push @goodp, $point;
        $LMap[$row][$col][VEGGIE]  = $gem;
        $LMap[$row][$col][MINERAL] = $Thingy{ FLOOR, };
        $seen{ $col . ',' . $row } = 1;
        # influences max score and how much shield repair is possible
        $GGV += $value;
        $gcount++;
        last if $GGV > $gmax;
    }

    # brown noise for these features to make them clump together-ish
    #
    # NOTE rubble MUST be done first here as it ignores %seen
    for my $species (RUBBLE, ACID, HOLE, WALL) {
        my $want = $Level_Features[$findex]{$species} // 0;
        $want += int rand(2 + int($want / 2)) if $want > 0;
        while ($want > 0) {
            my $goal = max($want, min(20, int($want / 10)));
            my $seed = splice @seeds, rand @seeds, 1;
            $want -= place_things(
                $seed->@*,
                MINERAL, $species, $goal, 60,
                [   [ -1, 0 ], [ -1, 1 ], [ 0,  -1 ], [ 0, 1 ],  [ 1, -1 ], [ 1, 0 ],
                    [ 1,  1 ], [ -2, 0 ], [ -2, 2 ],  [ 0, -2 ], [ 0, 2 ],  [ 2, -2 ],
                    [ 2,  0 ], [ 2,  2 ], [ 3,  0 ],
                ],
                \%seen
            );
        }
        bail_out("Conditions on Minos III proved too harsh.") unless @seeds;
    }

    for (1 .. $Level_Features[$findex]{ GATE, }) {
        my $point = splice @seeds, rand @seeds, 1;
        ($col, $row) = $point->@*;
        push @goodp, $point;
        $LMap[$row][$col][MINERAL] = $Thingy{ GATE, };
        $seen{ $col . ',' . $row } = 1;
        bail_out("Conditions on Minos III proved too harsh.") unless @seeds;
    }

    # pick a special point and ensure that there is a walkable path
    # between all of the important points and that seed
    # NOTE they could jump down to somewhere unpathable but A) jumping
    # is risky and B) there are zero holes on the level before the
    # amulet level. possible, but unlikely?
    ($col, $row) = splice(@seeds, rand @seeds, 1)->@*;
    $LMap[$row][$col][MINERAL] = $Thingy{ FLOOR, };
    pathable($col, $row, $herop, @goodp);
    reify(
        $LMap[$row][$col],
        MINERAL,
        passive_msg_maker(
            'Something is written here, but you can\'t quite make it out.')
    );

    for (1 .. $Level + roll(3, 2)) {
        place_monster($Level_Features[$findex]{xarci}, \@seeds);
    }

    # be nice and put a gem close (but not too close) to the player so
    # they are more likely to have something to heal with
    if ($Level == 1) {
        my $mindist = 4 + int rand 4;
        my $gem     = (make_gem())[0];
        my $point   = min_by {
            my $d = distance($Animates[HERO][LMC][WHERE]->@*, $_->@*);
            $d < $mindist ? ~0 : $d
        }
        @seeds;
        # and that they can get to said gem.
        ($col, $row) = $point->@*;
        $LMap[$row][$col][VEGGIE]  = $gem;
        $LMap[$row][$col][MINERAL] = $Thingy{ FLOOR, };
        pathable($col, $row, $herop);
    }

    # be naughty and (maybe) drop monsters near the good points
    for my $gp (@goodp) {
        next if 0 == int rand 3;
        my $mindist = 1 + int rand 4;
        my $point =
          min_by { my $d = distance($gp->@*, $_->@*); $d < $mindist ? ~0 : $d } @seeds;
        place_monster($Level_Features[$findex]{xarci}, [$point]);
        ($col, $row) = $point->@*;
        pathable($col, $row, $herop);
    }
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
    print CLEAR_SCREEN, "Alas, victory was not to be yours.\n\n$score\n";
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

    Congratulations. Victory is yours.

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
    M - show messages     < > - activate gate    E - equip a gem
    p - clear PKC code    C-l - redraw screen    R - remove a gem
    ? - show help         v   - show version     d - drop a gem
    @ - show location     Q   - quit the game

    Esc or q will exit from sub-displays such as this one. Prompts
    must be answered with Y to carry out the action; N or n or Esc
    will reject the action. Map symbols include:

      @  you     % gate    * gemstone    . empty cell
      #  wall    ~ acid    ^ rubble        crevasse

    Consult xomb(1) or `perldoc xomb` for additional documentation.
HELP_SCREEN
    await_quit();
    print HIDE_CURSOR;
    refresh_board();
}

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

sub load_map {
    my ($mapf) = @_;
    open my $fh, '<', $mapf or bail_out("could not open '$mapf': $!\n");

    my %charid = map { $Thingy{$_}->[DISPLAY] => $_ } keys %Thingy;

    while (my $line = readline $fh) {
        chomp $line;
        my $len = length $line;
        bail_out("$mapf:$. wrong number columns $len\n") if $len != MAP_COLS;
        my $colnum = 0;
        for my $ch (split //, $line) {
            my $id = $charid{$ch}
              // bail_out("$mapf:$. unknown character $ch at index $colnum\n");
            my $point = [ $colnum++, $. - 1 ];
            if ($Thingy{$id}->[GENUS] == MINERAL) {
                push $LMap[ $. - 1 ]->@*, [ $point, $Thingy{$id} ];
            } else {
                bail_out("This section of Minos III is still under construction.\n");
            }
        }
    }
    bail_out("$mapf:$. incorrect row count\n") if $. != MAP_ROWS;
}

sub loot_value {
    my $value = 0;
    for my $item ($Animates[HERO][STASH][LOOT]->@*) {
        if ($item->[SPECIES] == AMULET) {
            $value += 1000;
        } elsif ($item->[SPECIES] == GEM) {
            $value += $item->[STASH][GEM_VALUE];
        }
    }
    # they probably won't need to charge their shield after the game
    # is over?
    $value += $Animates[HERO][STASH][SHIELDUP][STASH][GEM_VALUE]
      if defined $Animates[HERO][STASH][SHIELDUP];
    return $value;
}

# expensive gem (vegetable) that speciated
sub make_amulet {
    my $gem;
    $gem->@[ GENUS, SPECIES, DISPLAY ] = $Thingy{ AMULET, }->@*;
    $gem->[STASH]->@[ GEM_NAME, GEM_VALUE, GEM_REGEN ] =
      (AMULET_NAME, AMULET_VALUE, AMULET_REGEN);
    return $gem, AMULET_VALUE;
}

sub make_gem {
    my ($name, $value, $regen);
    # lower regen is better and thus more rare. higher value makes for a
    # higher score, or more shield that can be repaired. rare gems might
    # actuallly be a curse as there will be fewer to find on a given
    # level due to the GGV limit
    if (int rand 100 == 0) {
        $name  = "Bloodstone";
        $value = 80 + roll(2, 10);
        $regen = 3;
    } elsif (int rand 20 == 0) {
        $name  = "Sunstone";
        $value = 60 + roll(2, 10);
        $regen = 4;
    } else {
        $name  = "Moonstone";
        $value = 40 + roll(2, 10);
        $regen = 4;
    }
    # flavor text makes things better
    my @adj = qw/Imperial Mystic Rose Smoky Warped/;
    if (int rand 1000 == 0) {
        $name  = 'Pearl ' . $name;
        $regen = 2;
        $value += 40 + roll(2, 10);
    } elsif (int rand 3 == 0) {
        $name = $adj[ rand @adj ] . ' ' . $name;
        $value += 40 + roll(2, 10);
    }
    my $gem;
    $gem->@[ GENUS, SPECIES, DISPLAY ] = $Thingy{ GEM, }->@*;
    $gem->[STASH]->@[ GEM_NAME, GEM_VALUE, GEM_REGEN ] =
      ($name, $value, $regen);
    return $gem, $value;
}

sub make_monster {
    my (%params) = @_;
    my $monst;
    $monst->@[ GENUS, SPECIES, DISPLAY, UPDATE, ENERGY ] =
      ($Thingy{ $params{species} }->@*, $params{energy});
    $monst->[STASH]->@[ HITPOINTS, ECOST ] =
      ($Hit_Points{ $params{species} }, CAN_MOVE);
    $monst->[STASH][WEAPON]->@[ WEAP_DMG, W_RANGE, W_COST ] =
      ($Damage_From{ $params{species} }, $Weap_Stats{ $params{species} }->@*,);
    push $monst->[STASH][WEAPON]->@*, $To_Hit{ $params{species} }->@*;
    return $monst;
}

sub make_player {
    my $hero;
    $hero->@[ GENUS, SPECIES, DISPLAY, UPDATE, ENERGY ] =
      ($Thingy{ HERO, }->@*, CAN_MOVE,);
    $hero->[STASH]->@[ HITPOINTS, ECOST, LOOT ] = (START_HP, CAN_MOVE, []);

    # KLUGE always put floor below them so FOV not eff'd up
    #$LMap[$row][$col][MINERAL] = $Thingy{ FLOOR, };

    # bascially a bulldozer, unlike the other weapons
    $hero->[STASH][WEAPON][WEAP_DMG] = $Damage_From{ HERO, };
    return $hero;
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
        $s .= ' or (d)rop, (E)quip' if $has_loot;
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
                log_code('0104');
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
        } elsif ($key eq 'E') {
            if (!defined $message) {
                print at_row(MSG_ROW + $offset), CLEAR_RIGHT,
                  "-- Equip item L)able or Esc to exit --";
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
    refresh_board();
    return MOVE_FAILED, 0;
}

sub move_animate {
    my ($ani, $cols, $rows, $cost) = @_;
    my $lmc  = $ani->[LMC];
    my $dcol = $lmc->[WHERE][PCOL] + $cols;
    my $drow = $lmc->[WHERE][PROW] + $rows;
    return MOVE_FAILED, 0, '0001'
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
}

sub move_drop {
    return MOVE_FAILED, 0, '0104'
      if defined $Animates[HERO][LMC][VEGGIE];
    return MOVE_FAILED, 0, '0112'
      unless $Animates[HERO][STASH][LOOT]->@*;
    @_ = ('d', 'drop item L)abel or Esc to exit');
    goto &manage_inventory;
}

sub move_equip {
    return MOVE_FAILED, 0, '0112'
      unless $Animates[HERO][STASH][LOOT]->@*;
    @_ = ('E', 'Equip item L)abel or Esc to exit');
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
        } else {
            $s .= '-- negative return on FOV scanner query --';
        }
        print at_row(STATUS_ROW), CLEAR_RIGHT, $s, at(map { MAP_DOFF + $_ } $col, $row);
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
    return MOVE_FAILED, 0, int rand 5000 == 0 ? '1202' : ();
}

sub move_gate_down {
    return MOVE_FAILED, 0, '0004'
      if $Animates[HERO][LMC][MINERAL][SPECIES] != GATE;
    return MOVE_FAILED, 0, '0014' if $Level > @Level_Features;
    log_message('Gate activated.') unless $Warned_About{gateact}++;
    $Violent_Sleep_Of_Reason = 1;
    return MOVE_LVLDOWN, NLVL_COST;
}

sub move_gate_up {
    return MOVE_FAILED, 0, '0004'
      if $Animates[HERO][LMC][MINERAL][SPECIES] != GATE;
    unless (has_amulet()) {
        log_message('You need the ' . AMULET_NAME . ' to ascend.');
        return MOVE_FAILED, 0, '0010';
    }
    log_message('Gate activated.') unless $Warned_About{gateact}++;
    $Violent_Sleep_Of_Reason = 1;
    return MOVE_LVLUP, NLVL_COST;
}

sub move_nop {
    apply_passives($Animates[HERO], DEFAULT_COST, 0);
    # NOTE constant amount of time even if they idle in rubble
    return MOVE_OKAY, DEFAULT_COST;
}

sub move_pickup {
    my $lmc = $Animates[HERO][LMC];
    return MOVE_FAILED, 0, '0101' unless defined $lmc->[VEGGIE];
    my $loot = $Animates[HERO][STASH][LOOT];
    return MOVE_FAILED, 0, '0102' if $loot->@* >= LOOT_MAX;
    if ($lmc->[VEGGIE][SPECIES] == AMULET) {
        log_message('Obtained ' . AMULET_NAME . '! Ascend to win!');
        $Violent_Sleep_Of_Reason = 1;
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
        print display_cellobjs();
        return @ret;
    }
}

sub move_quit {
    return MOVE_FAILED, 0 if nope_regarding('Really quit game?');
    has_lost();
}

sub move_remove {
    return MOVE_FAILED, 0, '0113'
      unless defined $Animates[HERO][STASH][SHIELDUP];
    my $loot = $Animates[HERO][STASH][LOOT];
    return MOVE_FAILED, 0, '0102' if $loot->@* >= LOOT_MAX;
    push $loot->@*, $Animates[HERO][STASH][SHIELDUP];
    undef $Animates[HERO][STASH][SHIELDUP];
    print display_shieldup();
    return MOVE_FAILED, 0;
}

sub nope_regarding {
    my ($message) = @_;
    print AT_MSG_ROW, '/!\ ', $message, ' (Y/N)';
    my ($key, $ret);
    while (1) {
        $key = ReadKey(0);
        if ($key eq 'Y') {
            $ret = 0;
            last;
        } elsif ($key eq "\033" or $key eq 'N' or $key eq 'n') {
            $ret = 1;
            last;
        }
    }
    print AT_MSG_ROW, CLEAR_RIGHT;
    return $ret;
}

sub passive_burn {
    my ($ani, $obj, $duration, $newcell) = @_;
    log_code('007E') if $ani->[SPECIES] == HERO;
    # half damage if they move quick. perhaps the acidic fog does not
    # have time to settle, or something?
    unless ($newcell) {
        log_message('Acid burn reported by shield module.')
          unless $Warned_About{acidburn}++;
        apply_damage($ani, 'acidburn', $duration) unless $newcell;
    }
}

sub passive_msg_maker {
    my ($message, $oneshot) = @_;
    sub {
        my ($ani, $obj, $duration, $newcell) = @_;
        if ($newcell) {
            log_message($message);
            undef $obj->[UPDATE] if $oneshot;
        }
    }
}

sub pathable {
    my ($col, $row, @rest) = @_;
    for my $point (@rest) {
        linecb(
            sub {
                my ($c, $r) = @_;
                my $cell = $LMap[$r][$c][MINERAL];
                if ($cell->[SPECIES] == WALL or $cell->[SPECIES] == HOLE) {
                    $LMap[$r][$c][MINERAL] = $Thingy{ 0 == int rand 10 ? RUBBLE : FLOOR };
                }
            },
            $col,
            $row,
            $point->@*
        );
    }
}

sub place_monster {
    my ($species, $seeds) = @_;

    # could be fancy and try for "away from player" but let's just go
    # with random drop for now
    my $point = splice $seeds->@*, rand $seeds->@*, 1;
    my ($col, $row) = $point->@*;

    my $monst = make_monster(
        species => $species->[ rand $species->@* ],
        energy  => CAN_MOVE,
    );

    # they can camp atop gates... too hard?
    $LMap[$row][$col][MINERAL] = $Thingy{ FLOOR, }
      unless $LMap[$row][$col][MINERAL][SPECIES] == GATE;
    $LMap[$row][$col][ANIMAL] = $monst;

    push @Animates, $monst;
    $Animates[-1][LMC] = $LMap[$row][$col];
    weaken $monst->[LMC];

    return $point;
}

sub place_things {
    my ($col, $row, $genus, $species, $count, $odds, $offsets, $seen) = @_;
    my $placed = 0;
    while ($count-- > 0) {
        my ($ncol, $nrow) = $offsets->[ rand $offsets->@* ]->@*;
        $ncol += $col;
        $nrow += $row;
        next
          if $ncol < 0
          or $ncol >= MAP_COLS
          or $nrow < 0
          or $nrow >= MAP_ROWS;
        my $loc = $ncol . ',' . $nrow;
        # rubble is fine under the player or gems though should be
        # placed first so it does not swap out other types
        if ($species != RUBBLE) {
            next if $seen->{$loc}++;
        }
        $LMap[$nrow][$ncol][$genus] = $Thingy{$species};
        $placed++;
        if (int rand 100 < $odds) { ($col, $row) = ($ncol, $nrow) }
    }
    return $placed;
}

sub plasma_annihilator {
    my ($seen, $spread, $depth, $max) = @_;

    return if $depth >= $max or !$spread->@*;

    my ($col, $row) = $spread->[ rand $spread->@* ]->@*;
    my $loc = $col . ',' . $row;
    $seen->{$loc} = 1;

    my $lmc = $LMap[$row][$col];
    if (defined $lmc->[ANIMAL]) {
        apply_damage($lmc->[ANIMAL], 'plsplash') if 0 == int rand 2;
    } elsif ($lmc->[MINERAL][SPECIES] == WALL) {
        reduce($lmc) if 0 == int rand 100;
        return;
    }
    print at(map { MAP_DOFF + $_ } $col, $row),
      (int rand 1000) ? 'x' : $Thingy{ AMULET, }->[DISPLAY]
      if exists $Visible_Cell{$loc};

    with_adjacent(
        $col, $row,
        sub {
            my ($point) = @_;
            my $adj = join ',', $point->@*;
            return if $seen->{$adj};
            push $spread->@*, $point;
            @_ = ($seen, $spread, $depth + 1, $max);
            goto &plasma_annihilator;
        }
    );
}

sub raycast_fov {
    my ($refresh) = @_;
    state $FOV;
    if (!$refresh and defined $FOV) {
        print $FOV;
        return;
    }

    my (%blocked, %byrow);
    my ($cx, $cy) = $Animates[HERO][LMC][WHERE]->@*;
    %Visible_Cell = ($cx . ',' . $cy => [ $cx, $cy ]);

    # radius 7 points taken from Game:RaycastFOV cache
    for my $ep (
        [ -2, -7 ], [ -1, -7 ], [ 0,  -7 ], [ 1,  -7 ], [ 2,  -7 ], [ -4, -6 ],
        [ -3, -6 ], [ -2, -6 ], [ 2,  -6 ], [ 3,  -6 ], [ 4,  -6 ], [ -5, -5 ],
        [ -4, -5 ], [ 4,  -5 ], [ 5,  -5 ], [ -6, -4 ], [ -5, -4 ], [ 5,  -4 ],
        [ 6,  -4 ], [ -6, -3 ], [ 6,  -3 ], [ -7, -2 ], [ -6, -2 ], [ 6,  -2 ],
        [ 7,  -2 ], [ -7, -1 ], [ 7,  -1 ], [ -7, 0 ],  [ 7,  0 ],  [ -7, 1 ],
        [ 7,  1 ],  [ -7, 2 ],  [ -6, 2 ],  [ 6,  2 ],  [ 7,  2 ],  [ -6, 3 ],
        [ 6,  3 ],  [ -6, 4 ],  [ -5, 4 ],  [ 5,  4 ],  [ 6,  4 ],  [ -5, 5 ],
        [ -4, 5 ],  [ 4,  5 ],  [ 5,  5 ],  [ -4, 6 ],  [ -3, 6 ],  [ -2, 6 ],
        [ 2,  6 ],  [ 3,  6 ],  [ 4,  6 ],  [ -2, 7 ],  [ -1, 7 ],  [ 0,  7 ],
        [ 1,  7 ],  [ 2,  7 ]
    ) {
        linecb(
            sub {
                my ($col, $row, $iters) = @_;

                # "the moon is a harsh mistress" -- FOV degrades at range
                return -1 if $iters - 4 > int rand 7;

                my $loc = $col . ',' . $row;
                return -1 if exists $blocked{$loc};

                # walls MUST block, other features may due to the "harsh
                # environment" (vim on the 2009 MacBook, at the moment).
                # similar restrictions are applied to monster LOS walks
                # to the player (see update_*). hopefully.
                my $cell = $LMap[$row][$col][MINERAL];
                if ($cell->[SPECIES] == WALL) {
                    $blocked{$loc} = 1;
                    push $byrow{$row}->@*, [ $col, $cell->[DISPLAY] ];
                    $Visible_Cell{$loc} = [ $col, $row ];
                    return -1;
                } elsif ($cell->[SPECIES] == RUBBLE) {
                    $blocked{$loc} = 1 if 0 == int rand 20;
                } elsif ($cell->[SPECIES] == ACID) {
                    $blocked{$loc} = 1 if 0 == int rand 200;
                }

                return 0 if exists $Visible_Cell{$loc};
                $Visible_Cell{$loc} = [ $col, $row ];
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

    # ensure @ is shown as FOV should not touch that cell
    print $FOV =
      $s . at(map { MAP_DOFF + $_ } $cx, $cy) . $LMap[$cy][$cx][ANIMAL][DISPLAY];
}

sub reduce {
    my ($lmc) = @_;
    if (exists $Visible_Cell{ join ',', $lmc->[WHERE]->@* }) {
        log_message('A '
              . $Descript{ $lmc->[MINERAL][SPECIES] }
              . ' explodes in a shower of fragments!');
    }
    # rubble reification
    $lmc->[MINERAL] = [ $lmc->[MINERAL]->@* ];
    $lmc->[MINERAL]->@[ SPECIES, DISPLAY ] =
      $Thingy{ RUBBLE, }->@[ SPECIES, DISPLAY ];
}

sub refresh_board {
    print CLEAR_SCREEN;
    raycast_fov(0);
    show_top_message();
    show_status_bar();
}

# similar to tu'a in Lojban
sub reify {
    my ($lmc, $i, $update) = @_;
    $lmc->[$i] = [ $lmc->[$i]->@* ];
    $lmc->[$i][UPDATE] = $update if defined $update;
}

sub relocate {
    my ($ani, $dpoint) = @_;
    my $lmc = $ani->[LMC];

    my $src = $lmc->[WHERE];

    my $dest_lmc = $LMap[ $dpoint->[PROW] ][ $dpoint->[PCOL] ];
    $dest_lmc->[ANIMAL] = $ani;
    undef $LMap[ $src->[PROW] ][ $src->[PCOL] ][ANIMAL];

    $ani->[LMC] = $dest_lmc;
    weaken $ani->[LMC];

    my $cell = $lmc->[VEGGIE] // $lmc->[MINERAL];
    print at(map { MAP_DOFF + $_ } $src->@*), $cell->[DISPLAY],
      at(map { MAP_DOFF + $_ } $dpoint->@*),
      $ani->[DISPLAY];
}

sub report_position {
    log_message(
        'Transponder reports [' . join(',', $Animates[HERO][LMC][WHERE]->@*) . ']');
    return MOVE_FAILED, 0;
}

sub report_version {
    log_message('xomb ' . $VERSION . ' seed ' . $Seed);
    return MOVE_FAILED, 0;
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
            $Violent_Sleep_Of_Reason = 1;
            log_message('Slow progress!');
        }
        return int($cost / 2) + 2 + int rand 4;
    } else {
        return 2 + int rand 4;
    }
}

sub score {
    my $score = loot_value();
    return "Score: $score in $Turn_Count turns";
}

sub show_status_bar {
    print at_row(STATUS_ROW),
      sprintf('Level %02d T%02d', $Level, $Animates[HERO][STASH][ECOST]),
      display_hitpoints(), display_cellobjs(), display_shieldup();
}

sub update_gameover {
    state $count = 0;
    raycast_fov(1);
    tcflush(STDIN_FILENO, TCIFLUSH);
    my $key = ReadKey(0);
    if ($count == 4) {
        has_lost();
    } elsif ($count >= 2) {
        print AT_MSG_ROW, CLEAR_RIGHT, '-- press Esc to continue --';
        has_lost() if $key eq "\033" or $key eq 'q';
    } elsif ($count == 1) {
        log_message('Communication lost with remote unit.');
    }
    $count++;
    return MOVE_OKAY, DEFAULT_COST;
}

sub update_fungi {
    my ($self) = @_;
    my ($mcol, $mrow) = $self->[LMC][WHERE]->@*;
    my ($tcol, $trow) = $Animates[HERO][LMC][WHERE]->@*;
    my $weap = $self->[STASH][WEAPON];

    my ($hits, $cost) =
      does_hit(int sqrt(($tcol - $mcol)**2 + abs($trow - $mrow)**2), $weap);
    return MOVE_OKAY, $cost if $hits == -1;

    my %seen;
    my $loc = $mcol . ',' . $mrow;
    print at(map { MAP_DOFF + $_ } $mcol, $mrow), 'X'
      if exists $Visible_Cell{$loc};
    $seen{$loc} = 1;

    my @spread;
    with_adjacent(
        $mcol, $mrow,
        sub {
            my $loc = join ',', $_[0]->@*;
            $seen{$loc} = 1;
            return if int rand 10 < 8;
            print at(map { MAP_DOFF + $_ } $_[0]->@*), 'X'
              if exists $Visible_Cell{$loc};
            push @spread, $_[0];
        }
    );

    walkcb(
        sub {
            my ($col, $row, $iters) = @_;
            my $lmc = $LMap[$row][$col];
            my $loc = $col . ',' . $row;
            $seen{$loc} = 1;
            if (defined $lmc->[ANIMAL]) {
                apply_damage($lmc->[ANIMAL], 'plburn', $iters) if 0 == int rand $iters;
            } elsif ($lmc->[MINERAL][SPECIES] == WALL) {
                reduce($lmc) if 0 == int rand 20;
                return -1;
            }
            print at(map { MAP_DOFF + $_ } $col, $row), (int rand $iters <= 1) ? 'X' : 'x'
              if exists $Visible_Cell{$loc};
            push @spread, [ $col, $row ];
            return $iters >= $weap->[W_RANGE] ? -1 : 0;
        },
        $mcol,
        $mrow,
        $tcol,
        $trow
    );

    if (@spread) {
        my $max = 3;
        $max = 5 if 0 == int rand 100;
        $max = 7 if 0 == int rand 1000;
        plasma_annihilator(\%seen, \@spread, 2, $max);
    }

    $Violent_Sleep_Of_Reason = 1;

    return MOVE_OKAY, $cost;
}

sub update_ghast {
    my ($self) = @_;
    my ($mcol, $mrow) = $self->[LMC][WHERE]->@*;
    my ($tcol, $trow) = $Animates[HERO][LMC][WHERE]->@*;
    my $weap = $self->[STASH][WEAPON];

    my ($hits, $cost) =
      does_hit(int sqrt(($tcol - $mcol)**2 + abs($trow - $mrow)**2), $weap);
    return MOVE_OKAY, $cost if $hits == -1;

    # but gatling gun is often trigger happy ...
    my $missed = 0;
    if ($hits == 0) {
        return MOVE_OKAY, $cost if 0 == int rand 8;
        my @nearby;
        with_adjacent($tcol, $trow, sub { push @nearby, $_[0] });
        ($tcol, $trow) = $nearby[ rand @nearby ]->@*;
        $missed = 1;
    }

    my @path;
    linecb(
        sub {
            my ($col, $row, $iters) = @_;
            push @path, [ $col, $row ];
            if (defined $LMap[$row][$col][ANIMAL]
                and $LMap[$row][$col][ANIMAL][SPECIES] != HERO) {
                ($tcol, $trow) = ($col, $row) if $missed and 0 == int rand 3;
                return -1;
            }
            my $cell = $LMap[$row][$col][MINERAL];
            if ($cell->[SPECIES] == WALL) {
                $missed = 1;
                return -1;
            } elsif ($cell->[SPECIES] == RUBBLE) {
                if (0 == int rand 3) {
                    $missed = 1;
                    return -1;
                }
            }
            return 0;
        },
        $mcol,
        $mrow,
        $tcol,
        $trow
    );

    return MOVE_OKAY, $cost unless @path;

    for my $point (@path) {
        my $loc = join ',', $point->@*;
        print at(map { MAP_DOFF + $_ } $point->@*), '-'
          if exists $Visible_Cell{$loc};
    }
    my $loc = $tcol . ',' . $trow;
    my $lmc = $LMap[$trow][$tcol];
    if ($missed) {
        my $buddy = $LMap[$trow][$tcol][ANIMAL];
        apply_damage($buddy, 'attackby', $self) if defined $buddy;
    } else {
        apply_damage($Animates[HERO], 'attackby', $self);
    }

    if (exists $Visible_Cell{$loc}) {
        my $cell = $lmc->[ANIMAL] // $lmc->[VEGGIE] // $lmc->[MINERAL];
        print at(map { MAP_DOFF + $_ } $tcol, $trow), $cell->[DISPLAY];
    }

    $Violent_Sleep_Of_Reason = 1;

    return MOVE_OKAY, $cost;
}

sub update_mortar {
    my ($self) = @_;
    my ($mcol, $mrow) = $self->[LMC][WHERE]->@*;
    my ($tcol, $trow) = $Animates[HERO][LMC][WHERE]->@*;
    my $weap = $self->[STASH][WEAPON];

    my ($hits, $cost) =
      does_hit(int sqrt(($tcol - $mcol)**2 + abs($trow - $mrow)**2), $weap);
    return MOVE_OKAY, $cost if $hits == -1;

    my @nearby;
    if ($hits == 0) {
        return MOVE_OKAY, $cost if 0 == int rand 3;
        with_adjacent($tcol, $trow, sub { push @nearby, $_[0] });
    }

    # Mortars could, in theory, lob shells over walls but that would
    # allow Mortars to abuse things like ### that the player could
    # not get into.                      #M#
    my $take_shot = 1;    #              ### therefore LOS is required
    linecb(
        sub {
            my ($col, $row) = @_;
            my $cell = $LMap[$row][$col][MINERAL];
            if ($cell->[SPECIES] == WALL) {
                $take_shot = 0;
                return -1;
            }
            return 0;
        },
        $mcol,
        $mrow,
        $tcol,
        $trow
    );
    return MOVE_OKAY, $cost unless $take_shot;

    if (@nearby) {
        log_message('A mortar shell explodes nearby!');
        my ($ncol, $nrow) = $nearby[ rand @nearby ]->@*;
        my $lmc   = $LMap[$nrow][$ncol];
        my $buddy = $lmc->[ANIMAL];
        if (defined $buddy) {
            apply_damage($buddy, 'attackby', $self);
        } elsif ($lmc->[SPECIES] == WALL and 0 == int rand 20) {
            reduce($lmc);
        }
    } else {
        log_message('A mortar shell strikes you!');
        apply_damage($Animates[HERO], 'attackby', $self);
    }

    return MOVE_OKAY, $cost;
}

sub update_player {
    my ($self) = @_;
    my ($cost, $ret);

    # pre-move tasks
    if ($Violent_Sleep_Of_Reason == 1) {
        sleep($Draw_Delay);
        $Violent_Sleep_Of_Reason = 0;
    }
    raycast_fov(1);
    show_top_message();
    show_status_bar();

    tcflush(STDIN_FILENO, TCIFLUSH);
    while (1) {
        my $key;
        while (1) {
            $key = ReadKey(0);
            last if exists $Key_Commands{$key};
            #log_message(sprintf "DBG unknown key \\%03o", ord $key);
        }
        ($ret, $cost, my $code) = $Key_Commands{$key}->($self);
        log_code($code) if defined $code;
        last            if $ret != MOVE_FAILED;
    }

    if (defined $self->[STASH][SHIELDUP]
        and $self->[STASH][HITPOINTS] < START_HP) {
        my $need  = START_HP - $self->[STASH][HITPOINTS];
        my $offer = between(
            0,
            int($cost / $self->[STASH][SHIELDUP][STASH][GEM_REGEN]),
            $self->[STASH][SHIELDUP][STASH][GEM_VALUE]
        );

        my $heal = between(0, $need, $offer);
        $self->[STASH][SHIELDUP][STASH][GEM_VALUE] -= $heal;
        $self->[STASH][HITPOINTS] += $heal;

        if ($self->[STASH][SHIELDUP][STASH][GEM_VALUE] <= 0) {
            # ooops
            log_message('The ' . AMULET_NAME . ' burns up!')
              if $self->[STASH][SHIELDUP][STASH][GEM_NAME] eq AMULET_NAME;
            log_code('0113');
            undef $self->[STASH][SHIELDUP];
            print display_shieldup();
        }
    }

    $Energy_Spent += $cost;
    $Turn_Count++;
    return $ret, $cost;
}

# when player is in range try to shoot them
sub update_troll {
    my ($self) = @_;
    my ($mcol, $mrow) = $self->[LMC][WHERE]->@*;
    my ($tcol, $trow) = $Animates[HERO][LMC][WHERE]->@*;
    my $weap = $self->[STASH][WEAPON];

    my ($hits, $cost) =
      does_hit(int sqrt(($tcol - $mcol)**2 + abs($trow - $mrow)**2), $weap);
    return MOVE_OKAY, $cost if $hits == -1;

    my $missed = 0;

    if ($hits == 0) {
        return MOVE_OKAY, DEFAULT_COST if 0 == int rand 2;
        $missed = 1;
    }

    my $take_shot = 1;
    my @path;
    my $property_damage = 0;
    walkcb(
        sub {
            my ($col, $row, $iters) = @_;
            push @path, [ $col, $row ];
            if ($iters >= $weap->[W_RANGE]) {
                ($tcol, $trow) = ($col, $row) if $missed;
                return -1;
            }
            if (defined $LMap[$row][$col][ANIMAL]
                and $LMap[$row][$col][ANIMAL][SPECIES] != HERO) {
                if ($missed) {
                    ($tcol, $trow) = ($col, $row);
                } else {
                    $take_shot = 0;
                }
                return -1;
            }
            my $cell = $LMap[$row][$col][MINERAL];
            if ($cell->[SPECIES] == WALL) {
                if (0 == int rand 20) {
                    ($tcol, $trow) = ($col, $row);
                    $missed          = 1;
                    $property_damage = 1;
                } else {
                    $take_shot = 0;
                }
                return -1;
            } elsif ($cell->[SPECIES] == RUBBLE) {
                # similar FOV problem as for player, see raycast. also
                # should mean that rubble is good cover for the hero
                if (0 == int rand 4) {
                    if (0 == int rand 200) {
                        $property_damage = 1;
                    } else {
                        $take_shot = 0 unless $missed;
                    }
                    return -1;
                }
            }
            return 0;
        },
        $mcol,
        $mrow,
        $tcol,
        $trow
    );
    return MOVE_OKAY, $cost unless $take_shot and @path;

    for my $point (@path) {
        my $loc = join ',', $point->@*;
        print at(map { MAP_DOFF + $_ } $point->@*), '-'
          if exists $Visible_Cell{$loc};
    }
    my $loc = $tcol . ',' . $trow;
    my $lmc = $LMap[$trow][$tcol];
    if ($property_damage) {
        reduce($lmc);
    } else {
        if ($missed) {
            my $buddy = $LMap[$trow][$tcol][ANIMAL];
            apply_damage($buddy, 'attackby', $self) if defined $buddy;
        } else {
            apply_damage($Animates[HERO], 'attackby', $self);
        }
    }

    if (exists $Visible_Cell{$loc}) {
        my $cell = $lmc->[ANIMAL] // $lmc->[VEGGIE] // $lmc->[MINERAL];
        print at(map { MAP_DOFF + $_ } $tcol, $trow), $cell->[DISPLAY];
    }

    $Violent_Sleep_Of_Reason = 1;

    return MOVE_OKAY, $cost;
}

# like shooter but can only fire across totally open ground. advanced
# targetting arrays prevent friendly fire and property damage
sub update_stalker {
    my ($self) = @_;
    my ($mcol, $mrow) = $self->[LMC][WHERE]->@*;
    my ($tcol, $trow) = $Animates[HERO][LMC][WHERE]->@*;
    my $weap = $self->[STASH][WEAPON];

    my ($hits, $cost) =
      does_hit(int sqrt(($tcol - $mcol)**2 + abs($trow - $mrow)**2), $weap);
    return MOVE_OKAY, $cost if $hits < 1;

    my $take_shot = 1;
    my @path;
    linecb(
        sub {
            my ($col, $row) = @_;
            if ($col == $tcol and $row == $trow) {    # gotcha
                push @path, [ $col, $row ];
                return 0;
            }
            my $cell = $LMap[$row][$col][MINERAL];
            if (   defined $LMap[$row][$col][ANIMAL]
                or $cell->[SPECIES] == WALL
                or $cell->[SPECIES] == RUBBLE
                or ($cell->[SPECIES] == ACID and 0 == int rand 4)) {
                $take_shot = 0;
                return -1;
            }
            push @path, [ $col, $row ];
        },
        $mcol,
        $mrow,
        $tcol,
        $trow
    );
    return MOVE_OKAY, $cost unless $take_shot and @path;

    for my $point (@path) {
        my $loc = join ',', $point->@*;
        print at(map { MAP_DOFF + $_ } $point->@*), '-'
          if exists $Visible_Cell{$loc};
    }
    apply_damage($Animates[HERO], 'attackby', $self);

    $Violent_Sleep_Of_Reason = 1;

    return MOVE_OKAY, $weap->[W_COST];
}

sub use_item {
    my ($loot, $i, $stash) = @_;
    if (!($loot->[$i][SPECIES] == GEM or $loot->[$i][SPECIES] == AMULET)) {
        log_code('0111');
        return;
    }
    if (defined $stash->[SHIELDUP]) {
        ($stash->[SHIELDUP], $loot->[$i]) = ($loot->[$i], $stash->[SHIELDUP]);
    } else {
        $stash->[SHIELDUP] = splice $loot->@*, $i, 1;
    }
    print display_shieldup();
}

sub veggie_name {
    my ($veg) = @_;
    my $s;
    if ($veg->[SPECIES] == GEM or $veg->[SPECIES] == AMULET) {
        $s = sprintf "(%d) %s", $veg->[STASH]->@[ GEM_VALUE, GEM_NAME ];
    } else {
        $s = $Descript{ $veg->[SPECIES] };
    }
    return $s;
}

sub with_adjacent {
    my ($col, $row, $callback) = @_;
    for my $adj (
        [ -1, -1 ], [ -1, 0 ],  [ -1, 1 ], [ 0, -1 ],
        [ 0,  1 ],  [ 1,  -1 ], [ 1,  0 ], [ 1, 1 ]
    ) {
        my ($ac, $ar) = ($col + $adj->[PCOL], $row + $adj->[PROW]);
        next if $ac < 0 or $ac >= MAP_COLS or $ar < 0 or $ar >= MAP_ROWS;
        $callback->([ $ac, $ar ]);
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
