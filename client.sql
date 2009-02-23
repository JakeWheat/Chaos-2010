/*

Copyright 2009 Jake Wheat

= Overview

windows manager widget
extra stuff - colours, sprites, wizard display info
              (additional info for each wizard)
board widget
info widget
spell book widget
new game widget
planned widgets

actions
key config
action valid view
turn phase
cursor/go
new game

see chaos.lhs for then main ui docs
*/
select new_module('client', 'chaos');

/*
================================================================================

= windows manager
Store window positions, size, maximised/minimised,
  open/close so this is restored when
  you restart the program or if it crashes
*/
select new_module('window_management', 'client');
/*
windows relvar
*/
create domain window_state as text
       check (value in ('maximised', 'minimised',
                        'hidden', 'normal'));
/*

Window with name name: top left corner of window is at position px, py
and the size of the window is sx, sy.  It is in state 'state'.

*/
create table windows (
  window_name text,
  px integer, --position
  py integer,
  sx integer, --size
  sy integer,
  state window_state
); --assert there is a row for every widget type.
select add_key('windows', 'window_name');
select set_relvar_type('windows', 'data');

/*

function to reset the windows to default, can be used if the windows
get too messed up or e.g. the window manager row is deleted

*/
create function action_reset_windows() returns void as $$ --tags: init
begin
  delete from windows;
  insert into windows (window_name, px, py, sx, sy, state) values
    ('window_manager', 0,28, 92,320, 'normal'),
    ('info', 0,371, 579,213, 'normal'),
    ('spell_book', 587,28, 268,556, 'normal'),
    ('new_game', 514, 27, 500, 500, 'hidden'),
    ('board', 99,28, 480,320, 'normal'),
    ('action_history', 843,28, 429,556, 'normal');
end;
$$ language plpgsql volatile strict;

create function action_hide_window(vname text) returns void as $$
begin
  if vname = 'window_manager' then
    raise exception 'cannot hide window manager';
  end if;
  update windows set state='hidden' where window_name = vname;
end;
$$ language plpgsql volatile strict;
select set_module_for_preceding_objects('window_management');

/*

When another window is closed that window is hidden.  when the window
manager is closed, the app exits

TODO: add window zoom and scroll positions to relvar
*/

create function action_refresh_widgets() returns void as $$
begin
--doesn't do owt at the moment, all in the haskell code,
-- just has this stub here to avoid special casing it in the
--haskell code
end;
$$ language plpgsql volatile strict;

/*
================================================================================

= extras
== colours
*/
create table colours (
       name text,
       red int,
       green int,
       blue int
);
select add_key('colours', 'name');
select set_relvar_type('colours', 'readonly');

copy colours (name,red,green,blue) from stdin;
grid	32767	32767	32767
background	0	0	32767
black	0	0	0
blue	0	0	65535
green	0	65535	0
red	65535	0	0
pink	65535	49407	49407
purple	65535	0	65535
cyan	0	65535	65535
yellow	65535	65535	0
orange	65535	41215	0
grey	32767	32767	32767
white	65535	65535	65535
\.


/*

================================================================================

== sprites

just the list of the names of the sprites and their animation speed.
todo: add the png data here

pngs for every sprite listed in this table must exist on the disk to
be loaded or the game will refuse to run

*/
select new_module('sprites', 'client');

create table sprites ( --tags:readonly
  sprite text, -- name of sprite, also part of the name of the png frames
  animation_speed int
--todo: add sprite data here
);
select add_key('sprites', 'sprite');
select set_relvar_type('sprites', 'readonly');
select set_module_for_preceding_objects('sprites');

copy sprites (sprite,animation_speed) from stdin;
bat	8
dead_bat	250
bear	23
dead_bear	250
centaur	23
dead_centaur	250
crocodile	34
dead_crocodile	250
dark_citadel	50
dire_wolf	12
dead_dire_wolf	250
eagle	14
dead_eagle	250
elf	26
dead_elf	250
faun	20
dead_faun	250
ghost	15
giant	23
dead_giant	250
giant_rat	13
dead_giant_rat	250
goblin	12
dead_goblin	250
golden_dragon	27
dead_golden_dragon	250
gooey_blob	40
gorilla	18
dead_gorilla	250
green_dragon	32
dead_green_dragon	250
gryphon	10
dead_gryphon	250
harpy	13
dead_harpy	250
horse	21
dead_horse	250
hydra	36
dead_hydra	250
king_cobra	30
dead_king_cobra	250
lion	38
dead_lion	250
magic_castle	50
magic_fire	12
magic_tree	250
manticore	13
dead_manticore	250
ogre	23
dead_ogre	250
orc	21
dead_orc	250
pegasus	16
dead_pegasus	250
red_dragon	34
dead_red_dragon	250
shadow_tree	30
skeleton	17
spectre	15
unicorn	16
dead_unicorn	250
vampire	40
wall	30
wizard0	250
wizard1	250
wizard2	250
wizard3	250
wizard4	250
wizard5	250
wizard6	250
wizard7	250
wizard_magic_armour	250
wizard_magic_bow	50
wizard_magic_knife	50
wizard_magic_shield	250
wizard_magic_sword	50
wizard_magic_wings	50
wraith	10
zombie	25
magic_bolt	250
lightning	250
law	250
large_law	250
chaos	250
large_chaos	250
vengeance	250
subversion	250
turmoil	250
disbelieve	250
justice	250
dark_power	250
decree	250
raise_dead	250
cursor	250
highlight_cast_target_spell	250
highlight_cast_activate_spell	250
highlight_mount	250
highlight_enter	250
highlight_dismount	250
highlight_exit	250
highlight_select_piece_at_position	250
highlight_walk	250
highlight_fly	250
highlight_attack	250
highlight_ranged_attack	250
\.

/*
================================================================================

== wizard display info

This table associates a wizards name (= the allegiance) from the
server with a colour for the wizard and his army and a wizard sprite
for display purposes.

The sprite in this table is what the wizard uses if he doesn't have
any upgrades.

Wizard named 'name' started with sprite default_sprite, his army is
coloured 'colour'.

*/
select new_module('wizard_display_info', 'client');

create table wizard_display_info (
  wizard_name text,
  default_sprite text, -- and matches /wizard.*/
  colour text
);
select add_key('wizard_display_info', 'wizard_name');
select add_key('wizard_display_info', 'default_sprite');
select add_key('wizard_display_info', 'colour');
select add_foreign_key('wizard_display_info', 'wizard_name', 'wizards');
select add_foreign_key('wizard_display_info', 'default_sprite',
                        'sprites', 'sprite');
select set_relvar_type('wizard_display_info','data');

create table init_wizard_display_info_argument (
  wizard_name text,
  sprite text, -- starts with wizard
  colour text --todo: make list of colours
);
select add_key('init_wizard_display_info_argument', 'wizard_name');
select add_key('init_wizard_display_info_argument', 'sprite');
select add_key('init_wizard_display_info_argument', 'colour');
select add_foreign_key('init_wizard_display_info_argument',
                       'wizard_name', 'wizards');
select add_foreign_key('init_wizard_display_info_argument',
                       'sprite', 'sprites');
select set_relvar_type('init_wizard_display_info_argument', 'stack');

create function init_wizard_display_info() returns void as $$ --tags: init
declare
  r record;
begin
    insert into wizard_display_info (wizard_name, default_sprite,  colour)
       select wizard_name,sprite,colour
       from init_wizard_display_info_argument;
end;
$$ language plpgsql volatile strict;


select set_module_for_preceding_objects('wizard_display_info');

/*
================================================================================

== action history with colours

*/
create view action_history_colour_mr as
select a.*,
       wn.colour as wizard_name_colour,
       cw.colour as current_wizard_colour,
       al.colour as allegiance_colour
  from action_history_mr a
  left outer join wizard_display_info wn
    on a.wizard_name = wn.wizard_name
  left outer join wizard_display_info cw
    on a.current_wizard = wn.wizard_name
  left outer join wizard_display_info al
    on a.allegiance = wn.wizard_name;

/*
================================================================================

= board widget

*/
select new_module('board_widget', 'client');
/*
== cursor position + ops

The cursor is at position x,y

The server code has no concept of the cursor.
In the end, this has just made the code more complicated for no reason.
*/
create table cursor_position (
  x int,
  y int
);
select add_constraint('cursor_position_coordinates_valid',
$$ not exists (select 1 from cursor_position
  cross join board_size
  where x >= width or y >= height)$$,
array['cursor_position', 'board_size']);
select constrain_to_zero_or_one_tuple('cursor_position');
select set_relvar_type('cursor_position', 'data');

/*
=== actions
cursor movement
*/

create function action_move_cursor(direction text) returns void as $$
begin
  if direction = 'up' then
    if (select y from cursor_position) > 0 then
      update cursor_position set y = y - 1;
    end if;
  elseif direction = 'down' then
    if (select y from cursor_position) <
       ((select height from board_size) - 1) then
      update cursor_position set y = y + 1;
    end if;
  elseif direction = 'left' then
    if (select x from cursor_position) > 0 then
      update cursor_position set x = x - 1;
    end if;
  elseif direction = 'right' then
    if (select x from cursor_position) <
       ((select width from board_size) - 1) then
      update cursor_position set x = x + 1;
    end if;
  elseif direction = 'up-left' then
    if (select y from cursor_position) > 0 then
      update cursor_position set y = y - 1;
    end if;
    if (select x from cursor_position) > 0 then
      update cursor_position set x = x - 1;
    end if;
  elseif direction = 'up-right' then
    if (select y from cursor_position) > 0 then
      update cursor_position set y = y - 1;
    end if;
    if (select x from cursor_position) <
       ((select width from board_size) - 1) then
      update cursor_position set x = x + 1;
    end if;
  elseif direction = 'down-left' then
    if (select y from cursor_position) <
       ((select height from board_size) - 1) then
      update cursor_position set y = y + 1;
    end if;
    if (select x from cursor_position) > 0 then
      update cursor_position set x = x - 1;
    end if;
  elseif direction = 'down-right' then
    if (select y from cursor_position) <
       ((select height from board_size) - 1) then
      update cursor_position set y = y + 1;
    end if;
    if (select x from cursor_position) <
       ((select width from board_size) - 1) then
      update cursor_position set x = x + 1;
    end if;
  else
    raise exception
      'asked to move cursor in direction % which isn''t valid',
      direction;
  end if;
end
$$ language plpgsql volatile strict;

/*
=== internals
When next phase is called, moved the cursor to that wizard
*/
create function action_move_cursor_to_current_wizard() returns void as $$
begin
  --don't move cursor during autonomous phase
  if get_turn_phase() != 'autonomous' then
    update cursor_position set (x,y) =
      ((select x from pieces
         inner join current_wizard_table
         on (current_wizard = allegiance)
         where ptype = 'wizard'),
       (select y from pieces
         inner join current_wizard_table
         on (current_wizard = allegiance)
         where ptype = 'wizard'));
  end if;
end;
$$ language plpgsql volatile strict;

create function init_cursor_position() returns void as $$
begin
  insert into cursor_position (x,y) values (0,0);
end;
$$ language plpgsql volatile strict;

/*

the plan is to have a board_sprites view for the board widget. This
contains all the sprites on the board (basically everything drawn on
the board: piece sprites, cursor, highlights, etc.)  all the board
needs is x,y,sprite and order. The order is used to make sure
overlapping sprites e.g. a piece, the cursor and a highlight, are
drawn in the right order

*/


/*
== piece sprites
Want to produce a list of x,y,sprite rows
for the pieces on top, the cursor,
and the highlights for the currently available actions

create a view to supply none as colour for corpses (corpses don't have
an allegiance)

*/
create view allegiance_colours as
  select wizard_name as allegiance, colour from wizard_display_info union
  select 'dead' as allegiance, 'none' as colour;

/*
wizard sprites: look in the action history to find the most recent upgrade
*/
create view wizard_sprites as
  select distinct on (wizard_name) o, wizard_name, sprite, w.colour from
  (select -1 as o, wizard_name, default_sprite as sprite
      from wizard_display_info
    union
    select id as o, wizard_name, 'wizard_' || spell_name
      from action_history_cast_succeeded
      natural inner join spells
      where spell_name != 'shadow_form'
      and spell_category = 'wizard') as a
  natural inner join wizard_display_info as w
  order by wizard_name, o desc;

/*

piece ptype-allegiance-tag is at x,y, allegiance colour is 'colour',
sprite is 'sprite', sprite priority is sp.

*/
create view piece_sprite as
  select x, y, ptype, sprite, colour, tag, allegiance
    from pieces inner join
    wizard_sprites on (allegiance = wizard_name)
      where ptype = 'wizard'
  union
  select x, y, ptype, 'dead_' || ptype as sprite, colour, tag, allegiance
    from dead_monster_pieces
    natural inner join allegiance_colours
  union
  select x, y, ptype, ptype as sprite, colour, tag, allegiance from
    (select * from pieces where ptype != 'wizard'
     except
     select * from dead_monster_pieces) as a
    natural inner join allegiance_colours;

/*
== highlights
*/

create view board_highlights as
-- include the squares for the selected spell
-- when still in the choose phase, so the user can
--see what squares are valid for their chosen spell
select x,y,'highlight_cast_target_spell' as sprite
  from current_wizard_spell_squares
  where get_turn_phase() = 'choose'
union
select x,y,'highlight_' || action  as sprite
  from valid_target_actions;

/*
== animation

we save a starting frame against each piece. Not really sure what the
best way to do this, some options are:

* have a trigger on the pieces table

* have an update function which is run every time the ui refreshes the
  piece data from the database, this inserts any missing starting frames
  in to a separate table at that time.

The advantage to this one is only the haskell code needs to understand
frame numbers

*/
create table piece_starting_frames (
  ptype text,
  allegiance text,
  tag int,
  start_frame int
);
select add_key('piece_starting_frames',
               array['ptype', 'allegiance', 'tag']);
select add_foreign_key('piece_starting_frames',
                       array['ptype', 'allegiance', 'tag'], 'pieces_mr');
select set_relvar_type('piece_starting_frames', 'data');


create function action_update_missing_startframes(fp int)
  returns void as $$
begin
  insert into piece_starting_frames (ptype,allegiance,tag,start_frame)
    select ptype,allegiance,tag, fp from pieces_mr
      where (ptype,allegiance,tag) not in
        (select ptype,allegiance,tag
        from piece_starting_frames);
end;
$$ language plpgsql volatile strict;

/*


== board sprites

put the piece sprites, the highlight and the cursor
together to give the full list of sprites

*/
create view board_sprites as
  select x,y,ptype,allegiance,tag,
    sprite,colour,sp,start_frame, animation_speed
    from piece_sprite
  natural inner join pieces_on_top
  natural inner join piece_starting_frames
  natural inner join sprites
union
select x,y, '', '', -1,'cursor', 'white', 6,0, animation_speed
from cursor_position
 inner join sprites on sprite='cursor'
union
select x,y, '', '', -1, sprite, 'white', 5,0, animation_speed
from board_highlights
natural inner join sprites
order by sp;

/*
================================================================================

= info widget

create a few views to help with the stuff shown
in the info widget

*/

create view piece_details as
  select * from pieces_mr
            full outer join
      (select 'wizard'::text as wtype,* from live_wizards) as a
            on (allegiance = wizard_name and ptype = wtype)
    natural inner join pieces_with_priorities
    natural inner join piece_sprite;

create view cursor_piece_details as
  select * from piece_details
      natural inner join cursor_position;

create view selected_piece_details as
  select * from piece_details
      natural inner join selected_piece
      natural full outer join remaining_walk_table;

select set_module_for_preceding_objects('board_widget');

/*
================================================================================

= spell book widget

order the spells:
wizard, attack, object, misc, monster
law spells, then neutral, then chaos,
highest to lowest base chance,
alpha by spell name

this is a proper mess

== sprites
*/
create table spell_sprites (
  spell_name text,
  sprite text
);
select add_key('spell_sprites', 'spell_name');
select add_foreign_key('spell_sprites', 'sprite', 'sprites');
select add_foreign_key('spell_sprites', 'spell_name', 'spells_mr');
select set_relvar_type('spell_sprites', 'readonly');

copy spell_sprites(spell_name, sprite) from stdin;
magic_wood	magic_tree
shadow_wood	shadow_tree
magic_fire	magic_fire
gooey_blob	gooey_blob
wall	wall
magic_castle	magic_castle
dark_citadel	dark_citadel
magic_bolt	magic_bolt
lightning	lightning
vengeance	vengeance
justice	justice
dark_power	dark_power
decree	decree
magic_armour	wizard_magic_armour
magic_shield	wizard_magic_shield
magic_knife	wizard_magic_knife
magic_sword	wizard_magic_sword
magic_bow	wizard_magic_bow
magic_wings	wizard_magic_wings
law	law
large_law	large_law
chaos	chaos
large_chaos	large_chaos
raise_dead	raise_dead
subversion	subversion
turmoil	turmoil
disbelieve	disbelieve
eagle	eagle
elf	elf
faun	faun
ghost	ghost
giant	giant
giant_rat	giant_rat
goblin	goblin
golden_dragon	golden_dragon
gorilla	gorilla
green_dragon	green_dragon
gryphon	gryphon
harpy	harpy
horse	horse
hydra	hydra
king_cobra	king_cobra
lion	lion
manticore	manticore
ogre	ogre
orc	orc
pegasus	pegasus
red_dragon	red_dragon
skeleton	skeleton
spectre	spectre
unicorn	unicorn
vampire	vampire
wraith	wraith
zombie	zombie
shadow_form	chaos
\.


select new_module('spell_book_widget', 'client');

/*
== show all setting
*/
select create_var('spell_book_show_all', 'boolean');
select set_relvar_type('spell_book_show_all_table', 'data');

create function action_spell_book_show_all_update(v boolean)
  returns void as $$
begin
  update spell_book_show_all_table set spell_book_show_all=v;
end;
$$ language plpgsql volatile strict;


/*
=== internals
==== ordering
order the spells by spell category
*/
create view section_order as
  select 1 as section_order, 'wizard' as spell_category
    union
  select 2 as section_order, 'attacking' as spell_category
    union
  select 3 as section_order, 'object' as spell_category
    union
  select 4 as section_order, 'miscellaneous' as spell_category
    union
  select 5 as section_order, 'monster' as spell_category;

create view spells_with_order as
  select *, case
          when alignment > 0 then 0
              when alignment = 0 then 1
        when alignment < 0 then 2
      end as alignment_order
  from spells natural inner join section_order;
/*
==== spell counts
*/
create view current_wizard_spell_counts as
  select spell_name, 0 as count from
    (select spell_name from spells except
     select spell_name from spell_books
       inner join current_wizard_table
       on (wizard_name = current_wizard)) as a
 union
  select spell_name, count(spell_name)
  from spell_books
  inner join current_wizard_table
    on (wizard_name = current_wizard)
  group by spell_name;

--create a string to represent the number of copies of each spell
create function count_icons(int) returns text as $$
  select repeat('#', $1) as result;
$$ language sql immutable strict;

--create a string to represent the alignment of each spell
create function align_icons(int) returns text as $$
  select case
    when $1 < 0 then  repeat('*', -$1)
    when $1 > 0 then  repeat('+', $1)
    else '-'
  end as result
$$ language sql immutable strict;
/*
==== colours
colour each spell according to the probability of casting success
*/

create function chance_colour(chance int) returns text as $$
begin
  return case
    when chance = 0 then 'grey'
    when chance between 1 and 20 then 'red'
    when chance between 21 and 40 then 'purple'
    when chance between 41 and 60 then 'green'
    when chance between 61 and 80 then 'cyan'
    when chance between 81 and 99 then 'yellow'
    when chance = 100 then 'white'
    else 'blue'
  end;
end;
$$ language plpgsql immutable strict;

create view spell_colours as
  select spell_name, chance_colour(chance) as colour
    from spell_cast_chance;

create function spell_colour(vspell text, vcount int) returns text as $$
declare
  colour text;
begin
  --if spell is current wizard's selected spell then highlight it
  --if spell count is 0 or we aren't in choose phase then colour is grey
  --else colour spell according to casting chance
  if (exists (select 1 from wizard_spell_choices
             inner join current_wizard_table
        on wizard_name = current_wizard
        where spell_name = vspell)) then
    colour := 'inverse-' || chance_colour(spell_cast_chance(vspell));
  elseif (vcount = 0 or get_turn_phase() != 'choose') then
    colour := 'grey';
  else
    colour := chance_colour(spell_cast_chance(vspell));
  end if;
  if colour is null then colour := 'blue'; end if;
  return colour;
end;
$$ language plpgsql stable strict;

-- format function for alignment
create function format_alignment(alignment int) returns text as $$
begin
  if (alignment < 0) then
    return 'chaos-' || cast(@ alignment as text);
  elseif (alignment > 0) then
    return 'law-' || cast(alignment as text);
  else
    return 'neutral';
  end if;
end;
$$ language plpgsql immutable strict;
/*
== spell choice controls
*/
create table spell_keys (
  spell_name text,
  key text
);
select add_key('spell_keys', 'spell_name');
select add_key('spell_keys', 'key');
select add_foreign_key('spell_keys', 'spell_name', 'spells_mr');
select set_relvar_type('spell_keys', 'readonly');

copy spell_keys (spell_name, key) from stdin;
magic_knife	1
magic_shield	2
magic_armour	3
magic_bow	4
magic_sword	5
shadow_form	6
magic_wings	7
decree	A
justice	B
lightning	C
magic_bolt	D
vengeance	E
dark_power	F
magic_wood	G
magic_castle	H
wall	I
gooey_blob	J
magic_fire	K
dark_citadel	L
shadow_wood	M
law	O
large_law	P
disbelieve	Q
subversion	R
turmoil	S
chaos	T
large_chaos	U
raise_dead	V
horse	a
king_cobra	b
eagle	c
elf	d
unicorn	e
gryphon	f
lion	g
pegasus	h
giant	i
golden_dragon	j
giant_rat	k
gorilla	l
goblin	m
orc	o
zombie	p
faun	q
ogre	r
skeleton	s
harpy	t
spectre	u
ghost	v
hydra	w
manticore	x
wraith	z
vampire	W
green_dragon	X
red_dragon	Z
\.

/*
== stuff
*/
create view spell_book_table as
  select spell_category, spell_name, count,
    spell_cast_chance(spell_name) as chance,
    alignment, format_alignment(alignment) as alignment_string,
    key, sprite, section_order, alignment_order, base_chance,
    count_icons(count::int), align_icons(alignment::int),
    spell_colour(spell_name, count::int) as colour
  from spells_with_order
  natural inner join current_wizard_spell_counts
  natural inner join spell_keys
  natural inner join spell_sprites
  cross join spell_book_show_all_table
  where not (spell_book_show_all = false and count = 0);

create view spell_details as
  select * from spells_mr
  full outer join spell_sprites using (spell_name)
  full outer join (
    select /*spell_category,*/ spell_name, count, chance,
    /*alignment,*/ alignment_string,
    key, /*sprite,*/ section_order, alignment_order, /*base_chance,*/
    count_icons, align_icons,
    colour
    from spell_book_table
    ) as balls using (spell_name);

create view current_wizard_selected_spell_details as
  select spell_name, spell_category, sprite, base_chance, description,
    num, range, count, chance, alignment_string
  from spell_details
  natural inner join wizard_spell_choices
  inner join current_wizard_table on (wizard_name = current_wizard);
select set_module_for_preceding_objects('spell_book_widget');

/*
================================================================================

= new game widget

Starting new game involves the following choices:
number of wizards (2-8)
computer wizards same ai same stats as player
for each wizard:
    name text - autogenerated, can be changed
    computer_controlled bool
    sprite and colour displayed but cannot currently be changed

to add
    AI level for each computer controlled wizard
    change playing area size, square or hexagon tiles
*/
select new_module('new_game_widget', 'client');

/*
== data
*/

create domain new_wizard_state as text
  check (value in ('human', 'computer', 'none'));

create table new_game_widget_state (
  line int,
  wizard_name text,
  sprite text,
  colour text,
  state new_wizard_state
);
select add_key('new_game_widget_state', 'line');
select add_key('new_game_widget_state', 'wizard_name');
select add_key('new_game_widget_state', 'sprite');
select add_key('new_game_widget_state', 'colour');
select add_foreign_key('new_game_widget_state', 'sprite', 'sprites');
select add_constraint('new_game_widget_state_line_valid',
' not exists(select 1 from new_game_widget_state
  where line >= 8)',
array['new_game_widget_state']);
select set_relvar_type('new_game_widget_state', 'data');

/*
== helpers
*/

create function extract_wizard_state(state text) returns boolean as $$
declare
  ret boolean;
begin
  if state = 'human' then
    ret = false;
  elseif state = 'computer' then
    ret = true;
  else
    raise exception
      'argument must be human or computer, called with %', state;
  end if;
  return ret;
end
$$ language plpgsql immutable strict;

create function action_reset_new_game_widget_state() returns void as $$
begin
    delete from new_game_widget_state;
    insert into new_game_widget_state
      (line, wizard_name, sprite, colour, state) values
      (0, 'Buddha', 'wizard0', 'blue', 'human'),
      (1, 'Kong Fuzi', 'wizard1', 'purple', 'computer'),
      (2, 'Laozi', 'wizard2', 'cyan', 'computer'),
      (3, 'Moshe', 'wizard3', 'yellow', 'computer'),
      (4, 'Muhammad', 'wizard4', 'green', 'computer'),
      (5, 'Shiva', 'wizard5', 'red', 'computer'),
      (6, 'Yeshua', 'wizard6', 'white', 'computer'),
      (7, 'Zarathushthra', 'wizard7', 'orange', 'computer');
end
$$ language plpgsql volatile strict;
/*
== actions
*/
create function action_client_new_game_using_new_game_widget_state()
  returns void as $$
declare
  r record;
  p int;
begin
  delete from action_client_new_game_argument;
  p := 0;
  for r in select * from new_game_widget_state
      where state != 'none' order by line loop
    insert into action_client_new_game_argument
        (place, wizard_name, sprite, colour, computer_controlled)
      values
        (p, r.wizard_name, r.sprite, r.colour,
         extract_wizard_state(r.state));
    p := p + 1;
  end loop;
  perform action_client_new_game();
end
$$ language plpgsql volatile strict;
select set_module_for_preceding_objects('new_game_widget');

/*

================================================================================

= info widget (split?)

turn phase spell, cursor info & highlight key, cursor & selected piece info

================================================================================

= planned widget notes:

== help widget
=== controls
=== tutorials/ examples
=== rules reference
== spell info, monster info - reference widget
== wizard army widget
== versioning access widget
== action history widget
== game manager widget
== power/ debugger widget

================================================================================

= actions
*/
select new_module('client_actions', 'client');

/*
== action valid views

we add this view to cover the actions which are defined
in the client to supplement the action valid views
for the server actions define in the server.
*/
create view client_valid_target_actions as
  select * from valid_target_actions
  where not exists (select 1 from game_completed_table);

create view client_valid_activate_actions as
select * from (
  select * from valid_activate_actions
union select 'move_cursor_up'
union select 'move_cursor_down'
union select 'move_cursor_left'
union select 'move_cursor_right'
union select 'move_cursor_up_left'
union select 'move_cursor_down_left'
union select 'move_cursor_up_right'
union select 'move_cursor_down_right'
union select 'print_widget_info'
union select 'refresh_windows'
union select 'spell_book_show_all_update_on'
union select 'spell_book_show_all_update_off'
union select 'client_next_phase'
union select 'go') as a
  where not exists (select 1 from game_completed_table);

/*
== key controls
create a table to map gtk key descriptions to the
names of the action functions which are called.

*/

select new_module('key_controls', 'client');

create table key_control_settings (
  key_code text,
  action_name text
);
select add_key('key_control_settings', array['key_code','action_name']);
select set_relvar_type('key_control_settings', 'readonly');

copy key_control_settings(key_code, action_name) from stdin;
Up	move_cursor_up
KP_Up	move_cursor_up
Left	move_cursor_left
KP_Left	move_cursor_left
Right	move_cursor_right
KP_Right	move_cursor_right
Down	move_cursor_down
KP_Down	move_cursor_down
KP_Home	move_cursor_up_left
KP_Page_Up	move_cursor_up_right
KP_Page_Down	move_cursor_down_right
KP_End	move_cursor_down_left
End	cancel
F11	print_widget_info
F12	refresh_widgets
0	choose_no_spell
Insert	spell_book_show_all_update_on
Delete	spell_book_show_all_update_off
space	client_next_phase
KP_Begin	go
Return	go
KP_5	go
y	set_imaginary
Y	set_imaginary
n	set_real
N	set_real
\.

/*
== key press actions
*/
create function create_client_action_wrapper(client_action_name text,
                                              action_call text)
  returns void as $$
begin
  execute $f$
create function action_$f$ || client_action_name || $f$() returns void as $a$
begin
  perform action_$f$ || action_call || $f$;
end;
$a$ language plpgsql volatile strict;$f$;
end;
$$ language plpgsql volatile strict;

/*
cursor movement action redirections, used to make sense but don't
anymore - todo: split the move_cursor function into separate ones.
*/

select create_client_action_wrapper('move_cursor_down',
       $$move_cursor('down')$$);
select create_client_action_wrapper('move_cursor_up',
       $$move_cursor('up')$$);
select create_client_action_wrapper('move_cursor_left',
       $$move_cursor('left')$$);
select create_client_action_wrapper('move_cursor_right',
       $$move_cursor('right')$$);
select create_client_action_wrapper('move_cursor_up_left',
       $$move_cursor('up-left')$$);
select create_client_action_wrapper('move_cursor_up_right',
       $$move_cursor('up-right')$$);
select create_client_action_wrapper('move_cursor_down_right',
       $$move_cursor('down-right')$$);
select create_client_action_wrapper('move_cursor_down_left',
       $$move_cursor('down-left')$$);
select create_client_action_wrapper('spell_book_show_all_update_on',
       $$spell_book_show_all_update(true)$$);
select create_client_action_wrapper('spell_book_show_all_update_off',
       $$spell_book_show_all_update(false)$$);

create function action_key_pressed(pkeycode text) returns void as $$
declare
  r record;
begin
/*
basic plan
have a table with key code, and action name
then a strategy of taking an action and
     a. deciding where to get the arguments
     b. deciding if it is allowed
*/
  --raise notice 'action_key_pressed %', pkeycode;
  for r in select * from key_control_settings where key_code = pkeycode loop
    if exists (select 1 from client_valid_activate_actions
        where action=r.action_name) then
      --raise notice 'running %', r.action_name;
      execute 'select action_' || r.action_name || '();';
    elseif exists(select 1 from client_valid_target_actions
            natural inner join cursor_position
            where action = r.action_name) then
      execute 'select action_' || r.action_name ||
              '(' || (select x from cursor_position) ||
              ', ' || (select y from cursor_position) || ');';
      --raise notice 'running %', r.action_name;
    else
      --just ignore instead of raising exception
      --raise notice 'no matches';
      null;
    end if;
  end loop;
end;
$$ language plpgsql volatile strict;

/*
=== spell choice
*/


  insert into key_control_settings(key_code, action_name)
    select key, 'choose_' || spell_name || '_spell'
    from spell_keys;


/*

================================================================================

== turn phases
*/

create function action_client_next_phase() returns void as $$
begin
  perform action_next_phase();
  perform action_move_cursor_to_current_wizard();
end;
$$ language plpgsql volatile strict;

/*
================================================================================

== cursor/go actions
*/

create function action_go() returns void as $$
declare
  r record;
  s text;
begin
  select into r x,y,action from client_valid_target_actions
    natural inner join cursor_position;
  if r is not null then
    s :=  'select action_' || r.action || '(' || r.x || ',' || r.y || ')';
    execute s;
  else
    select into r action
      from client_valid_activate_actions;
--      where action <> 'activate';
    if r is not null then
      s := 'select action_' || r.action || '()';
      execute s;
    end if;
  end if;
  return ;
end;
$$ language plpgsql volatile strict;


/*

================================================================================

== new game action
*/

select new_module('client_new_game', 'client');

create table action_client_new_game_argument (
  place int,
  wizard_name text,
  sprite text,
  colour text,
  computer_controlled boolean
);
select add_key('action_client_new_game_argument', 'place');
select add_key('action_client_new_game_argument', 'wizard_name');
select add_key('action_client_new_game_argument', 'sprite');
select add_key('action_client_new_game_argument', 'colour');
select add_foreign_key('action_client_new_game_argument',
                       'sprite', 'sprites');
select add_constraint('action_client_new_game_place_valid',
'(select count(*) from action_client_new_game_argument
  where place >=
  (select count(*) from action_client_new_game_argument)) = 0',
 array['action_client_new_game_argument']);
select set_relvar_type('action_client_new_game_argument', 'stack');

--this calls server new game
create function action_client_new_game() returns void as $$
begin
  --assert: argument has between 2 and 8 active wizards
  delete from action_new_game_argument;
  delete from init_wizard_display_info_argument;
  -- clear data tables
  delete from cursor_position;
  delete from wizard_display_info;
  -- don't reset windows, see below
  --call server new_game
  --populate argument first
  delete from action_new_game_argument;
  insert into action_new_game_argument
    (wizard_name, computer_controlled, place)
    select wizard_name, computer_controlled, place
      from action_client_new_game_argument;
  perform action_new_game();

  --wizard display_info
  delete from init_wizard_display_info_argument;
  insert into init_wizard_display_info_argument
      (wizard_name, sprite, colour)
    select wizard_name, sprite, colour
    from action_client_new_game_argument;
  perform init_wizard_display_info();

  --populate window data,
  -- preserve settings from previous game if there are some
  if not exists(select 1 from windows) then
    perform action_reset_windows();
  end if;

  if not exists(select 1 from spell_book_show_all_table) then
    insert into spell_book_show_all_table values (false);
  end if;

  perform init_cursor_position();
end
$$ language plpgsql volatile strict;
select set_module_for_preceding_objects('client_new_game');

select protect_readonly_relvars();
select set_all_attributes_to_not_null();
