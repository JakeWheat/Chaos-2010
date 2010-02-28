/*

Action validity
===============

All the updates in this database are via stored procs (um, functions
in postgres, called actions here), which are simple and modular. Each
'public' stored proc has precondition checks, which also double as a
guide to what the ui can do, so they are exact preconditions. So this
is seriously paranoid code - chaos is serious business.

All these views need a rethink, its a massive mess.

*/
select module('Chaos.Server.Actions.SquaresValid');

/*
pieces on top
-------------

The topmost piece on each square is the one you interact with most of
the time, e.g. when selecting, attacking, etc.

The exception to this rule is when you select a wizard that is in a
magic tree or castle or mounted on a monster.

The pieces_on_top view also determines what sprite is shown in a
square in the ui

*/

create view pieces_with_priorities as
  select ptype,allegiance,tag,x,y,
    case
      when allegiance='dead' then 3
      when ptype='wizard' then 2
      when ptype in (select ptype from monster_prototypes) then 1
      else 0
    end as sp
    from pieces;

--restrict this view taking only the top piece from each square to get
--the final result

create view pieces_on_top as
  select x,y,ptype,allegiance,tag,sp from
    (select row_number() over(partition by (x,y) order by sp) as rn,
            x, y, ptype, allegiance, tag, sp
      from pieces_with_priorities) as pwp where rn = 1;

--create a full view to help with updates

-- question: why does pieces_view natural inner join pieces_on_top
-- return too many rows?

create view pieces_on_top_view as
  select p.* from pieces_mr p
    inner join pieces_on_top
    using (ptype,allegiance,tag);

/*
selectable squares and pieces
-----------------------------

We can't use the pieces on top for the selection because of
the exceptions re castles, magic wood and mounted wizards,
so create a similar view so we can determine the piece that
gets selected by square, this part is just the pieces on top
combined with all the wizards even if they are not on top.

This is finished off using the pieces_to_move relvar.

*/
create view moving_pieces as
  select ptype, allegiance, tag,x,y from pieces_mr
    where speed is not null
      or attack_strength is not null
      or ranged_attack_strength is not null;

create view selectable_pieces_with_priorities as
  select ptype,allegiance,tag,x,y,
    case
      when ptype='wizard' then 0
      else 1
    end as sp
    from moving_pieces
    where (x,y) not in(select x,y from pieces
                       where ptype = 'gooey_blob');

/*

internals
---------
*/

create function distance(int, int, int, int) returns float(24) as $$
  select (point($1, $2) <-> point($3, $4))::float(24) as result;
$$ language sql immutable;

create view board_ranges as
--iterate x,y over each square on board
--  iterate d over 0 to 20
--    iterate tx,ty over each square on board
--      include x,y,d, tx, ty iff d(x,y,tx,ty) < d
--so: we include squares <= to the range, not just squares at that
--range

  select * from generate_series(0, 14) as x
                cross join generate_series(0, 9) as y
                cross join generate_series(1, 20) as range
                cross join generate_series(0, 14) as tx
                cross join generate_series(0, 9) as ty
  where
    --we never need the centre square to be included
    (x,y) != (tx,ty) and
     --round to closest int
    distance(x,y,tx,ty) - 0.5 <= range;

--this view contains all the squares with no pieces in them
create view empty_squares as
  select x,y from generate_series(0, 14) as x
                cross join generate_series(0, 9) as y
  except
  select x,y from pieces;

-- this view contains all the squares containing corpses and nothing else
create view corpse_only_squares as
  select x,y from pieces_on_top
    natural inner join dead_monster_pieces;

--empty or corpse only doubles as the list of squares moveable to
--either by walking or flying
create view empty_or_corpse_only_squares as
  select * from empty_squares
  union
  select * from corpse_only_squares;

-- this view contains all the squares which are exactly one square
-- away from a tree (doesn't include the tree squares themselves)
create view adjacent_to_tree_squares as
  select tx as x, ty as y
    from board_ranges
    natural inner join pieces
    where ptype in ('magic_tree', 'shadow_tree')
      and range = 1;

create view empty_and_not_adjacent_to_tree_squares as
  select * from empty_squares
  except
  select * from adjacent_to_tree_squares;

--this view contains squares which the 'top piece' is attackable
create view attackable_squares as
  select x,y from attackable_pieces
    natural inner join pieces_on_top;

--this view contains squares which the 'top piece' is a creature
--(i.e. a wizard or monster)
create view creature_on_top_squares as
  select x,y from creature_pieces
  natural inner join pieces_on_top;

--this view contains squares which the 'top piece' is a monster
create view monster_on_top_squares as
  select x,y from monster_pieces
  natural inner join pieces_on_top;

/*
create a view containing all the squares the selected piece
could move to if they had unlimited speed
*/
create view selected_piece_move_squares as
  select x,y from empty_or_corpse_only_squares
  union
  select x,y from pieces
  natural inner join
    (select ptype from enterable_piece_types
       where (select ptype='wizard' from selected_piece)
     union
     select ptype from ridable_prototypes
       where (select ptype='wizard' from selected_piece)) as a
  where allegiance = (select allegiance from selected_piece);

-- all the squares valid for walking to at this time
-- so we start with a selected piece, and at the bottom
-- we make sure it has squares left to walk
create view selected_piece_walk_squares as
  select x,y from
    selected_piece_move_squares
  intersect
--adjust for 1 square away from selected piece:
--get the ranges
  select tx as x, ty as y from board_ranges
    natural inner join selected_piece
    natural inner join pieces
--restrict to 1 range
--exclude flying creatures
    where range = 1
      and not (select flying or engaged
               from creature_pieces
               natural inner join selected_piece)
-- only if the selected piece has squares left to walk
    and get_remaining_walk() > 0;

create view squares_within_selected_piece_flight_range as
  select tx as x, ty as y from board_ranges
  natural inner join selected_piece
  natural inner join creature_pieces
  where flying and range <= speed;

--this view is the analogue of selected_piece_walk_squares
-- for flying creatures
create view selected_piece_fly_squares as
select x,y from selected_piece_move_squares
intersect
select x,y from squares_within_selected_piece_flight_range
-- only if the selected piece hasn't moved
  where (select move_phase from selected_piece) = 'motion';

create view selected_piecexy as
  select * from selected_piece
  natural inner join pieces;

create function is_equipped(text) returns boolean as $$

  select magic_sword or magic_knife or magic_bow
    from wizards where wizard_name = $1;

$$ language sql stable;

create view selected_piece_attackable_squares as
  select x,y from pieces_on_top t
  natural inner join pieces_mr p
  cross join selected_piece s
  where physical_defense is not null
    and p.allegiance <> s.allegiance
    and p.allegiance <> 'dead'
    -- wizards can't attack magic trees but monsters can
    and not (p.ptype='magic_tree' and s.ptype='wizard')
    -- logic isn't quite right - a wizard can only attack undead with
    -- the imagic weapon so e.g. they shouldn't be able to attack h2h
    -- if they only have a magic bow
    and (not coalesce(undead,false)
          or coalesce((select coalesce(undead,false) from pieces_mr
                        natural inner join selected_piece), false)
          or coalesce(s.ptype = 'wizard'
                      and is_equipped(s.allegiance), false));

create view selected_piece_walk_attack_squares as
  select x,y from selected_piece_attackable_squares
  intersect
  select tx,ty from board_ranges r
    natural inner join selected_piecexy
    where range = 1
      and move_phase in ('motion','attack');

create view selected_piece_fly_attack_squares as
  select x,y from selected_piece_attackable_squares
  natural inner join squares_within_selected_piece_flight_range
  where (select move_phase from selected_piece) = 'motion';

create view selected_piece_in_range_squares as
  select tx as x, ty as y from board_ranges b
  natural inner join ranged_weapon_pieces s
  natural inner join selected_piece
  where b.range <= s.range;

create view selected_piece_ranged_attackable_squares as
  select x,y from selected_piece_attackable_squares
  natural inner join selected_piece_in_range_squares;

/*
this view lists all the squares which have pieces which can be
selected. It is empty when:
  not in move phase
  there is a currently selected piece
  the current wizard has no pieces left to select
the pieces to move only has entries for
 the current wizard who's moving, else it's empty
 so only need to switch the contents dependant on
 whether there is a selected piece or not
*/

create view selectable_pieces as
  select * from
    (select row_number() over (partition by (x,y) order by sp) as rn, *
       from selectable_pieces_with_priorities
       --natural inner join pieces_to_move
       where not exists(select 1 from selected_piece)
             and (ptype,allegiance,tag) not in (select * from pieces_moved)
    ) as s where rn = 1;
/*
valid actions
=============

The end result: two relvars, one with x,y, to list all the valid
actions at any time.

*/


-------------------------------------
/*
ideas:

quite a complicated process, want to try to make it declarative rather
than procedural and see if can make quick

the result needs to be action_name,x,y
where the action name is one of
cast_target_spell
select_piece
walk
fly
attack
ranged_attack

in order to make it quick and also make it understandable, do a two
stage process.

In the first view, we collect together all the information from the
pieces - their position and other attributes

The in the second view, we combine the turn sequence information to
get the final list of valid target squares.

[update: exposing some intermediates which are used by the board
sprites and ai]

what do we need to collect in the first view?
first categorize each square into one or more of the following:
completely empty
corpse only
attackable piece on top
adjacent to tree
wizard
mount_enter

the we add all the extra information from the pieces information that
will be used by the second view

we can get the category of spell that can be cast on a square for
cast_target_spell select_piece: need all pieces not in pieces_moved
that are in the current wizard's army: so need to keep the
ptype,allegiance and tag

walk,fly: pretty straight forward

attack,ranged attack: need the allegiance

we need to be able to implement the
following special cases:

cannot select piece under blob: use attackable on top for selection

dismount, exit: the wizard isn't the piece on top when he is on a
mount or in a castle or tree - won't be in the attackable pieces
squares which is what we're using to feed to selectable pieces, so add
the wizard squares as extra category

mount, enter: need to add these squares in as extra category, so we
can add them to the walk/fly squares iff the selected piece is a
wizard

trees: monsters will attack any tree, wizards will move into an empty
magic tree and attack occupied ones as well as shadow_trees

undead: if a piece is undead, it can only be attacked by another
undead creature or a magic weapon

some spells can be cast on corpses as well as monsters/wizards, so we
include corpses in the attackable on top category, and filter corpses
where neccessary using the allegiance info.

  */

create view squares_valid_categories as
  with
    es as (select x,y from generate_series(0, 14) as x
                      cross join generate_series(0, 9) as y
           except select x,y from pieces)
   ,ta as (select p.x,p.y,p.ptype,p,allegiance,p.tag,v.undead,
             case when speed is null then false
                  else true
             end as creature
             ,case when undead is null then false
                  else true
             end as monster
             from attackable_pieces p
             inner join pieces_on_top_view v
               using (ptype,allegiance,tag))
   ,co as (select x,y
             from pieces_on_top
             where allegiance = 'dead')
   ,tree_pos as (select x,y from pieces
                   where ptype in ('magic_tree', 'shadow_tree'))
   ,tree_adj as (select x,y from tree_pos
                 union all select x-1,y-1 from tree_pos
                 union all select x-1,y from tree_pos
                 union all select x-1,y+1 from tree_pos
                 union all select x,y-1 from tree_pos
                 union all select x,y+1 from tree_pos
                 union all select x+1,y-1 from tree_pos
                 union all select x+1,y from tree_pos
                 union all select x+1,y+1 from tree_pos)
   ,wz as (select x,y,ptype,allegiance,tag from pieces where ptype = 'wizard')
   ,me as (select x,y,ptype,allegiance,tag from pieces_mr
           where ridable
             or ptype in ('magic_tree','magic_castle','dark_citadel'))
  select 'empty' as category,x,y,
         null::text as ptype,
         null::text as allegiance,
         null::int as tag,
         null::boolean as undead,
         null::boolean as creature,
         null::boolean as monster
      from es
  union select 'attackable',x,y,ptype,allegiance,tag,undead,creature,monster from ta
  union select 'corpse-only',x,y,null,null,null,null,null,null from co
  union select distinct 'tree-adjacent',x,y,null,null,null::int,null::boolean,null::boolean,null::boolean
          from tree_adj
          where x between 0 and 14 and y between 0 and 9
  union select 'wizard',x,y,ptype,allegiance,tag,null,null,null from wz
  union select 'mount-enter',x,y,ptype,allegiance,tag,null,null,null from me
          where (x,y) not in (select x,y from wz)
  ;

---------------------------

create view spell_valid_squares as
 -- convert the square categories to spell categories
 -- maybe this should be done in the square categories view?
 select 'empty' as valid_square_category, x,y
         from squares_valid_categories
           where category = 'empty'
 union select 'empty_or_corpse_only' as valid_square_category,x,y
         from squares_valid_categories
           where category in ('empty','corpse-only')
 union select 'attackable' as valid_square_category,x,y
         from squares_valid_categories
           where category ='attackable'
 union select 'creature_on_top' as valid_square_category,x,y
         from squares_valid_categories
           where category ='attackable' and creature
 union select 'monster_on_top' as valid_square_category,x,y
         from squares_valid_categories
           where category ='attackable' and monster
 union select 'corpse_only' as valid_square_category,x,y
         from squares_valid_categories
           where category ='corpse-only'
 union select 'empty_and_not_adjacent_to_tree' as valid_square_category, x,y
         from (select x,y from squares_valid_categories where category='empty'
               except select x,y from squares_valid_categories where category='tree-adjacent') as a;

create view current_wizard_spell_squares as
with
   -- put together a relation with x,y position for the current wizard
   -- and the range for his currently chosen spell
   -- so we only get a row iff the current wizard has a target spell
   -- chosen and it's the cast phase
   cwsr as
       (select x, y, range, valid_square_category
          from pieces
          inner join current_wizard_table
            on (allegiance = current_wizard)
          inner join wizard_spell_choices
            on (wizard_name = current_wizard)
          natural inner join target_spells
          where ptype = 'wizard'
                and get_turn_phase() = 'cast')
  select distinct svs.x,svs.y
    from (select tx as x, ty as y
                          from board_ranges
                          where (x,y,range) = (select x,y,range from cwsr)) as a
    natural inner join spell_valid_squares svs
    natural inner join (select valid_square_category from cwsr) as b;

create view valid_target_actions as
select * from (
--target spells
  select x,y,'cast_target_spell'::text as action
    from current_wizard_spell_squares
--selecting a piece
union
select x,y,action from (
select x,y, 'select_piece_at_position':: text as action
  from selectable_pieces
--walking
union
select x,y, 'walk'::text as action
  from selected_piece_walk_squares
--flying
union
select x,y, 'fly'::text as action
  from selected_piece_fly_squares
--attacking
union
select x,y, 'attack'::text as action
  from selected_piece_walk_attack_squares
--fly attacking
union
select x,y, 'attack'::text as action
  from selected_piece_fly_attack_squares
--shooting
union
select x,y, 'ranged_attack'::text as action
  from selected_piece_ranged_attackable_squares
)as s1
where get_turn_phase()='move'
) as s
where not exists (select 1 from game_completed_table);

---------------------------------------------

create view valid_activate_actions as
select * from (
--next_phase - always valid
select 'next_phase'::text as action
--choose spell - need one for each spell, add programmatically
--set imaginary
union
select 'set_imaginary'::text as action
  from monster_spells
  where get_current_wizard_spell() is not null
    and spell_name = get_current_wizard_spell()
--set real
union
select 'set_real'::text as action
  from monster_spells
  where get_current_wizard_spell() is not null
    and spell_name = get_current_wizard_spell()
--cast activate spell
union
select 'cast_activate_spell'::text as action
  where exists (select 1
         from current_wizard_spell
         natural inner join activate_spells
         where get_turn_phase() = 'cast')
      or (select spell_name ='magic_wood'
          from current_wizard_spell
          where get_turn_phase() = 'cast')
--skip spell ** why is this commented out? **
--union
--select 'skip_spell'::text as action
--  where get_turn_phase() = 'cast'
--unselect
union
select 'unselect_piece'::text as action
  from selected_piece
--next subphase
union
select 'cancel'::text as action
  from selected_piece
union
-- generate a separate choose action wrapper for each spell
--
-- without this, we can add a general choose spell action but then we
-- first check if the current player can choose a spell at this time,
-- and then check if they have the particular spell they are trying to
-- choose.
--
-- By creating these simple wrappers, we can check both at once, and
-- also the ui has one simple test to see if a spell choice action is
-- valid instead of two stages.
select 'choose_' || spell_name || '_spell'::text as action
  from spell_books where wizard_name = get_current_wizard()
  and get_turn_phase()='choose'
union
select 'choose_no_spell'::text as action
  from turn_phase_table where turn_phase ='choose'
union
select 'ai_continue'
  from wizards
  inner join current_wizard_table
    on wizard_name = current_wizard
    where computer_controlled
) as a
where not exists (select 1 from game_completed_table);

/*
provide shortcut functions to check if an action can be run using
these views

*/
create function check_can_run_action(action_name text) returns void as $$
begin
  if not exists (select 1 from valid_activate_actions
     where action = action_name) then
    raise exception 'cannot run % here', action_name;
  end if;
end;
$$ language plpgsql stable;

create function check_can_run_action(action_name text, px int, py int)
  returns void as $$
begin
  if not exists (select 1 from valid_target_actions
     where action = action_name and (x,y) = (px,py)) then
    raise exception 'cannot run % on %,% here', action_name, px, py;
  end if;
end;
$$ language plpgsql stable;

