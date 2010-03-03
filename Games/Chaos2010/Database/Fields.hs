{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
module Games.Chaos2010.Database.Fields where

import Database.HaskellDB.DBLayout

data IdTag
type Id = Proxy IdTag
instance ShowLabel Id where showLabel _ = "id"

xid :: Id
xid = proxy
data History_nameTag
type History_name = Proxy History_nameTag
history_name :: History_name
history_name = proxy
instance ShowLabel History_name
    where showLabel _ = "history_name"
data AllegianceTag
type Allegiance = Proxy AllegianceTag
allegiance :: Allegiance
allegiance = proxy
instance ShowLabel Allegiance
    where showLabel _ = "allegiance"
data XTag
type X = Proxy XTag
x :: X
x = proxy
instance ShowLabel X
    where showLabel _ = "x"
data YTag
type Y = Proxy YTag
y :: Y
y = proxy
instance ShowLabel Y
    where showLabel _ = "y"
data TxTag
type Tx = Proxy TxTag
tx :: Tx
tx = proxy
instance ShowLabel Tx
    where showLabel _ = "tx"
data TyTag
type Ty = Proxy TyTag
ty :: Ty
ty = proxy
instance ShowLabel Ty
    where showLabel _ = "ty"
data PtypeTag
type Ptype = Proxy PtypeTag
ptype :: Ptype
ptype = proxy
instance ShowLabel Ptype
    where showLabel _ = "ptype"
data TagTag
type Tag = Proxy TagTag
tag :: Tag
tag = proxy
instance ShowLabel Tag
    where showLabel _ = "tag"
data Spell_nameTag
type Spell_name = Proxy Spell_nameTag
spell_name :: Spell_name
spell_name = proxy
instance ShowLabel Spell_name
    where showLabel _ = "spell_name"
data Num_wizardsTag
type Num_wizards = Proxy Num_wizardsTag
num_wizards :: Num_wizards
num_wizards = proxy
instance ShowLabel Num_wizards
    where showLabel _ = "num_wizards"
data Turn_numberTag
type Turn_number = Proxy Turn_numberTag
turn_number :: Turn_number
turn_number = proxy
instance ShowLabel Turn_number
    where showLabel _ = "turn_number"
data Turn_phaseTag
type Turn_phase = Proxy Turn_phaseTag
turn_phase :: Turn_phase
turn_phase = proxy
instance ShowLabel Turn_phase
    where showLabel _ = "turn_phase"
data ColourTag
type Colour = Proxy ColourTag
colour :: Colour
colour = proxy
instance ShowLabel Colour
    where showLabel _ = "colour"
data ActionTag
type Action = Proxy ActionTag
action :: Action
action = proxy
instance ShowLabel Action
    where showLabel _ = "action"
data HelpTag
type Help = Proxy HelpTag
help :: Help
help = proxy
instance ShowLabel Help
    where showLabel _ = "help"
data Object_typeTag
type Object_type = Proxy Object_typeTag
object_type :: Object_type
object_type = proxy
instance ShowLabel Object_type
    where showLabel _ = "object_type"
data Object_nameTag
type Object_name = Proxy Object_nameTag
object_name :: Object_name
object_name = proxy
instance ShowLabel Object_name
    where showLabel _ = "object_name"
data Module_nameTag
type Module_name = Proxy Module_nameTag
module_name :: Module_name
module_name = proxy
instance ShowLabel Module_name
    where showLabel _ = "module_name"
data Physical_defenseTag
type Physical_defense = Proxy Physical_defenseTag
physical_defense :: Physical_defense
physical_defense = proxy
instance ShowLabel Physical_defense
    where showLabel _ = "physical_defense"
data Attack_strengthTag
type Attack_strength = Proxy Attack_strengthTag
attack_strength :: Attack_strength
attack_strength = proxy
instance ShowLabel Attack_strength
    where showLabel _ = "attack_strength"
data Attribute_nameTag
type Attribute_name = Proxy Attribute_nameTag
attribute_name :: Attribute_name
attribute_name = proxy
instance ShowLabel Attribute_name
    where showLabel _ = "attribute_name"
data Type_nameTag
type Type_name = Proxy Type_nameTag
type_name :: Type_name
type_name = proxy
instance ShowLabel Type_name
    where showLabel _ = "type_name"
data Relvar_nameTag
type Relvar_name = Proxy Relvar_nameTag
relvar_name :: Relvar_name
relvar_name = proxy
instance ShowLabel Relvar_name
    where showLabel _ = "relvar_name"
data Constraint_nameTag
type Constraint_name = Proxy Constraint_nameTag
constraint_name :: Constraint_name
constraint_name = proxy
instance ShowLabel Constraint_name
    where showLabel _ = "constraint_name"
data XtypeTag
type Xtype = Proxy XtypeTag
xtype :: Xtype
xtype = proxy
instance ShowLabel Xtype
    where showLabel _ = "xtype"
data SpriteTag
type Sprite = Proxy SpriteTag
sprite :: Sprite
sprite = proxy
instance ShowLabel Sprite
    where showLabel _ = "sprite"
data RangeTag
type Range = Proxy RangeTag
range :: Range
range = proxy
instance ShowLabel Range
    where showLabel _ = "range"
data WidthTag
type Width = Proxy WidthTag
width :: Width
width = proxy
instance ShowLabel Width
    where showLabel _ = "width"
data HeightTag
type Height = Proxy HeightTag
height :: Height
height = proxy
instance ShowLabel Height
    where showLabel _ = "height"
data SpTag
type Sp = Proxy SpTag
sp :: Sp
sp = proxy
instance ShowLabel Sp
    where showLabel _ = "sp"
data Start_tickTag
type Start_tick = Proxy Start_tickTag
start_tick :: Start_tick
start_tick = proxy
instance ShowLabel Start_tick
    where showLabel _ = "start_tick"
data Animation_speedTag
type Animation_speed = Proxy Animation_speedTag
animation_speed :: Animation_speed
animation_speed = proxy
instance ShowLabel Animation_speed
    where showLabel _ = "animation_speed"
data SelectedTag
type Selected = Proxy SelectedTag
selected :: Selected
selected = proxy
instance ShowLabel Selected
    where showLabel _ = "selected"
data Cast_alignmentTag
type Cast_alignment = Proxy Cast_alignmentTag
cast_alignment :: Cast_alignment
cast_alignment = proxy
instance ShowLabel Cast_alignment
    where showLabel _ = "cast_alignment"
data Cast_success_checkedTag
type Cast_success_checked = Proxy Cast_success_checkedTag
cast_success_checked :: Cast_success_checked
cast_success_checked = proxy
instance ShowLabel Cast_success_checked
    where showLabel _ = "cast_success_checked"
data PlaceTag
type Place = Proxy PlaceTag
place :: Place
place = proxy
instance ShowLabel Place
    where showLabel _ = "place"
data Wizard_nameTag
type Wizard_name = Proxy Wizard_nameTag
wizard_name :: Wizard_name
wizard_name = proxy
instance ShowLabel Wizard_name
    where showLabel _ = "wizard_name"
data Computer_controlledTag
type Computer_controlled = Proxy Computer_controlledTag
computer_controlled :: Computer_controlled
computer_controlled = proxy
instance ShowLabel Computer_controlled
    where showLabel _ = "computer_controlled"
data NameTag
type Name = Proxy NameTag
name :: Name
name = proxy
instance ShowLabel Name
    where showLabel _ = "name"
data RedTag
type Red = Proxy RedTag
red :: Red
red = proxy
instance ShowLabel Red
    where showLabel _ = "red"
data GreenTag
type Green = Proxy GreenTag
green :: Green
green = proxy
instance ShowLabel Green
    where showLabel _ = "green"
data BlueTag
type Blue = Proxy BlueTag
blue :: Blue
blue = proxy
instance ShowLabel Blue
    where showLabel _ = "blue"
data Creating_new_gameTag
type Creating_new_game = Proxy Creating_new_gameTag
creating_new_game :: Creating_new_game
creating_new_game = proxy
instance ShowLabel Creating_new_game
    where showLabel _ = "creating_new_game"
data FlyingTag
type Flying = Proxy FlyingTag
flying :: Flying
flying = proxy
instance ShowLabel Flying
    where showLabel _ = "flying"
data SpeedTag
type Speed = Proxy SpeedTag
speed :: Speed
speed = proxy
instance ShowLabel Speed
    where showLabel _ = "speed"
data AgilityTag
type Agility = Proxy AgilityTag
agility :: Agility
agility = proxy
instance ShowLabel Agility
    where showLabel _ = "agility"
data Magic_defenseTag
type Magic_defense = Proxy Magic_defenseTag
magic_defense :: Magic_defense
magic_defense = proxy
instance ShowLabel Magic_defense
    where showLabel _ = "magic_defense"
data Spell_categoryTag
type Spell_category = Proxy Spell_categoryTag
spell_category :: Spell_category
spell_category = proxy
instance ShowLabel Spell_category
    where showLabel _ = "spell_category"
data Base_chanceTag
type Base_chance = Proxy Base_chanceTag
base_chance :: Base_chance
base_chance = proxy
instance ShowLabel Base_chance
    where showLabel _ = "base_chance"
data DescriptionTag
type Description = Proxy DescriptionTag
description :: Description
description = proxy
instance ShowLabel Description
    where showLabel _ = "description"
data NumbTag
type Numb = Proxy NumbTag
numb :: Numb
numb = proxy
instance ShowLabel Numb
    where showLabel _ = "numb"
data CountTag
type Count = Proxy CountTag
count :: Count
count = proxy
instance ShowLabel Count
    where showLabel _ = "count"
data ChanceTag
type Chance = Proxy ChanceTag
chance :: Chance
chance = proxy
instance ShowLabel Chance
    where showLabel _ = "chance"
data Alignment_stringTag
type Alignment_string = Proxy Alignment_stringTag
alignment_string :: Alignment_string
alignment_string = proxy
instance ShowLabel Alignment_string
    where showLabel _ = "alignment_string"
data Current_wizardTag
type Current_wizard = Proxy Current_wizardTag
current_wizard :: Current_wizard
current_wizard = proxy
instance ShowLabel Current_wizard
    where showLabel _ = "current_wizard"
data ImaginaryTag
type Imaginary = Proxy ImaginaryTag
imaginary :: Imaginary
imaginary = proxy
instance ShowLabel Imaginary
    where showLabel _ = "imaginary"
data UndeadTag
type Undead = Proxy UndeadTag
undead :: Undead
undead = proxy
instance ShowLabel Undead
    where showLabel _ = "undead"
data RidableTag
type Ridable = Proxy RidableTag
ridable :: Ridable
ridable = proxy
instance ShowLabel Ridable
    where showLabel _ = "ridable"
data Ranged_weapon_typeTag
type Ranged_weapon_type = Proxy Ranged_weapon_typeTag
ranged_weapon_type :: Ranged_weapon_type
ranged_weapon_type = proxy
instance ShowLabel Ranged_weapon_type
    where showLabel _ = "ranged_weapon_type"
data Ranged_attack_strengthTag
type Ranged_attack_strength = Proxy Ranged_attack_strengthTag
ranged_attack_strength :: Ranged_attack_strength
ranged_attack_strength = proxy
instance ShowLabel Ranged_attack_strength
    where showLabel _ = "ranged_attack_strength"
data WtypeTag
type Wtype = Proxy WtypeTag
wtype :: Wtype
wtype = proxy
instance ShowLabel Wtype
    where showLabel _ = "wtype"
data Shadow_formTag
type Shadow_form = Proxy Shadow_formTag
shadow_form :: Shadow_form
shadow_form = proxy
instance ShowLabel Shadow_form
    where showLabel _ = "shadow_form"
data Magic_swordTag
type Magic_sword = Proxy Magic_swordTag
magic_sword :: Magic_sword
magic_sword = proxy
instance ShowLabel Magic_sword
    where showLabel _ = "magic_sword"
data Magic_knifeTag
type Magic_knife = Proxy Magic_knifeTag
magic_knife :: Magic_knife
magic_knife = proxy
instance ShowLabel Magic_knife
    where showLabel _ = "magic_knife"
data Magic_shieldTag
type Magic_shield = Proxy Magic_shieldTag
magic_shield :: Magic_shield
magic_shield = proxy
instance ShowLabel Magic_shield
    where showLabel _ = "magic_shield"
data Magic_wingsTag
type Magic_wings = Proxy Magic_wingsTag
magic_wings :: Magic_wings
magic_wings = proxy
instance ShowLabel Magic_wings
    where showLabel _ = "magic_wings"
data Magic_armourTag
type Magic_armour = Proxy Magic_armourTag
magic_armour :: Magic_armour
magic_armour = proxy
instance ShowLabel Magic_armour
    where showLabel _ = "magic_armour"
data Magic_bowTag
type Magic_bow = Proxy Magic_bowTag
magic_bow :: Magic_bow
magic_bow = proxy
instance ShowLabel Magic_bow
    where showLabel _ = "magic_bow"
data Original_placeTag
type Original_place = Proxy Original_placeTag
original_place :: Original_place
original_place = proxy
instance ShowLabel Original_place
    where showLabel _ = "original_place"
data ExpiredTag
type Expired = Proxy ExpiredTag
expired :: Expired
expired = proxy
instance ShowLabel Expired
    where showLabel _ = "expired"
data ExpressionTag
type Expression = Proxy ExpressionTag
expression :: Expression
expression = proxy
instance ShowLabel Expression
    where showLabel _ = "expression"
data Disable_spreadingTag
type Disable_spreading = Proxy Disable_spreadingTag
disable_spreading :: Disable_spreading
disable_spreading = proxy
instance ShowLabel Disable_spreading
    where showLabel _ = "disable_spreading"
data Dont_nest_ai_next_phaseTag
type Dont_nest_ai_next_phase = Proxy Dont_nest_ai_next_phaseTag
dont_nest_ai_next_phase :: Dont_nest_ai_next_phase
dont_nest_ai_next_phase = proxy
instance ShowLabel Dont_nest_ai_next_phase
    where showLabel _ = "dont_nest_ai_next_phase"
data Game_completedTag
type Game_completed = Proxy Game_completedTag
game_completed :: Game_completed
game_completed = proxy
instance ShowLabel Game_completed
    where showLabel _ = "game_completed"
data In_next_phase_hackTag
type In_next_phase_hack = Proxy In_next_phase_hackTag
in_next_phase_hack :: In_next_phase_hack
in_next_phase_hack = proxy
instance ShowLabel In_next_phase_hack
    where showLabel _ = "in_next_phase_hack"
data Key_codeTag
type Key_code = Proxy Key_codeTag
key_code :: Key_code
key_code = proxy
instance ShowLabel Key_code
    where showLabel _ = "key_code"
data Action_nameTag
type Action_name = Proxy Action_nameTag
action_name :: Action_name
action_name = proxy
instance ShowLabel Action_name
    where showLabel _ = "action_name"
data Module_orderTag
type Module_order = Proxy Module_orderTag
module_order :: Module_order
module_order = proxy
instance ShowLabel Module_order
    where showLabel _ = "module_order"
data AlignmentTag
type Alignment = Proxy AlignmentTag
alignment :: Alignment
alignment = proxy
instance ShowLabel Alignment
    where showLabel _ = "alignment"
data Valid_square_categoryTag
type Valid_square_category = Proxy Valid_square_categoryTag
valid_square_category :: Valid_square_category
valid_square_category = proxy
instance ShowLabel Valid_square_category
    where showLabel _ = "valid_square_category"
data LineTag
type Line = Proxy LineTag
line :: Line
line = proxy
instance ShowLabel Line
    where showLabel _ = "line"
data StateTag
type State = Proxy StateTag
state :: State
state = proxy
instance ShowLabel State
    where showLabel _ = "state"
data New_wizard_nameTag
type New_wizard_name = Proxy New_wizard_nameTag
new_wizard_name :: New_wizard_name
new_wizard_name = proxy
instance ShowLabel New_wizard_name
    where showLabel _ = "new_wizard_name"
data Object_orderTag
type Object_order = Proxy Object_orderTag
object_order :: Object_order
object_order = proxy
instance ShowLabel Object_order
    where showLabel _ = "object_order"
data Operator_nameTag
type Operator_name = Proxy Operator_nameTag
operator_name :: Operator_name
operator_name = proxy
instance ShowLabel Operator_name
    where showLabel _ = "operator_name"
data SourceTag
type Source = Proxy SourceTag
source :: Source
source = proxy
instance ShowLabel Source
    where showLabel _ = "source"
data PreferenceTag
type Preference = Proxy PreferenceTag
preference :: Preference
preference = proxy
instance ShowLabel Preference
    where showLabel _ = "preference"
data Remaining_walk_hackTag
type Remaining_walk_hack = Proxy Remaining_walk_hackTag
remaining_walk_hack :: Remaining_walk_hack
remaining_walk_hack = proxy
instance ShowLabel Remaining_walk_hack
    where showLabel _ = "remaining_walk_hack"
data Remaining_walkTag
type Remaining_walk = Proxy Remaining_walkTag
remaining_walk :: Remaining_walk
remaining_walk = proxy
instance ShowLabel Remaining_walk
    where showLabel _ = "remaining_walk"
data Scalar_nameTag
type Scalar_name = Proxy Scalar_nameTag
scalar_name :: Scalar_name
scalar_name = proxy
instance ShowLabel Scalar_name
    where showLabel _ = "scalar_name"
data Section_orderTag
type Section_order = Proxy Section_orderTag
section_order :: Section_order
section_order = proxy
instance ShowLabel Section_order
    where showLabel _ = "section_order"
data Move_phaseTag
type Move_phase = Proxy Move_phaseTag
move_phase :: Move_phase
move_phase = proxy
instance ShowLabel Move_phase
    where showLabel _ = "move_phase"
data EngagedTag
type Engaged = Proxy EngagedTag
engaged :: Engaged
engaged = proxy
instance ShowLabel Engaged
    where showLabel _ = "engaged"
data Spell_book_show_allTag
type Spell_book_show_all = Proxy Spell_book_show_allTag
spell_book_show_all :: Spell_book_show_all
spell_book_show_all = proxy
instance ShowLabel Spell_book_show_all
    where showLabel _ = "spell_book_show_all"
data KeyTag
type Key = Proxy KeyTag
key :: Key
key = proxy
instance ShowLabel Key
    where showLabel _ = "key"
data Alignment_orderTag
type Alignment_order = Proxy Alignment_orderTag
alignment_order :: Alignment_order
alignment_order = proxy
instance ShowLabel Alignment_order
    where showLabel _ = "alignment_order"
data Count_iconsTag
type Count_icons = Proxy Count_iconsTag
count_icons :: Count_icons
count_icons = proxy
instance ShowLabel Count_icons
    where showLabel _ = "count_icons"
data Align_iconsTag
type Align_icons = Proxy Align_iconsTag
align_icons :: Align_icons
align_icons = proxy
instance ShowLabel Align_icons
    where showLabel _ = "align_icons"
data Spell_choice_hackTag
type Spell_choice_hack = Proxy Spell_choice_hackTag
spell_choice_hack :: Spell_choice_hack
spell_choice_hack = proxy
instance ShowLabel Spell_choice_hack
    where showLabel _ = "spell_choice_hack"
data Row_numberTag
type Row_number = Proxy Row_numberTag
row_number :: Row_number
row_number = proxy
instance ShowLabel Row_number
    where showLabel _ = "row_number"
data Spell_parts_to_castTag
type Spell_parts_to_cast = Proxy Spell_parts_to_castTag
spell_parts_to_cast :: Spell_parts_to_cast
spell_parts_to_cast = proxy
instance ShowLabel Spell_parts_to_cast
    where showLabel _ = "spell_parts_to_cast"
data CategoryTag
type Category = Proxy CategoryTag
category :: Category
category = proxy
instance ShowLabel Category
    where showLabel _ = "category"
data CreatureTag
type Creature = Proxy CreatureTag
creature :: Creature
creature = proxy
instance ShowLabel Creature
    where showLabel _ = "creature"
data MonsterTag
type Monster = Proxy MonsterTag
monster :: Monster
monster = proxy
instance ShowLabel Monster
    where showLabel _ = "monster"
data OverrideTag
type Override = Proxy OverrideTag
override :: Override
override = proxy
instance ShowLabel Override
    where showLabel _ = "override"
data SettingTag
type Setting = Proxy SettingTag
setting :: Setting
setting = proxy
instance ShowLabel Setting
    where showLabel _ = "setting"
data Trigger_catalogTag
type Trigger_catalog = Proxy Trigger_catalogTag
trigger_catalog :: Trigger_catalog
trigger_catalog = proxy
instance ShowLabel Trigger_catalog
    where showLabel _ = "trigger_catalog"
data Trigger_schemaTag
type Trigger_schema = Proxy Trigger_schemaTag
trigger_schema :: Trigger_schema
trigger_schema = proxy
instance ShowLabel Trigger_schema
    where showLabel _ = "trigger_schema"
data Trigger_nameTag
type Trigger_name = Proxy Trigger_nameTag
trigger_name :: Trigger_name
trigger_name = proxy
instance ShowLabel Trigger_name
    where showLabel _ = "trigger_name"
data Event_manipulationTag
type Event_manipulation = Proxy Event_manipulationTag
event_manipulation :: Event_manipulation
event_manipulation = proxy
instance ShowLabel Event_manipulation
    where showLabel _ = "event_manipulation"
data Event_object_catalogTag
type Event_object_catalog = Proxy Event_object_catalogTag
event_object_catalog :: Event_object_catalog
event_object_catalog = proxy
instance ShowLabel Event_object_catalog
    where showLabel _ = "event_object_catalog"
data Event_object_schemaTag
type Event_object_schema = Proxy Event_object_schemaTag
event_object_schema :: Event_object_schema
event_object_schema = proxy
instance ShowLabel Event_object_schema
    where showLabel _ = "event_object_schema"
data Event_object_tableTag
type Event_object_table = Proxy Event_object_tableTag
event_object_table :: Event_object_table
event_object_table = proxy
instance ShowLabel Event_object_table
    where showLabel _ = "event_object_table"
data Action_orderTag
type Action_order = Proxy Action_orderTag
action_order :: Action_order
action_order = proxy
instance ShowLabel Action_order
    where showLabel _ = "action_order"
data Action_conditionTag
type Action_condition = Proxy Action_conditionTag
action_condition :: Action_condition
action_condition = proxy
instance ShowLabel Action_condition
    where showLabel _ = "action_condition"
data Action_statementTag
type Action_statement = Proxy Action_statementTag
action_statement :: Action_statement
action_statement = proxy
instance ShowLabel Action_statement
    where showLabel _ = "action_statement"
data Action_orientationTag
type Action_orientation = Proxy Action_orientationTag
action_orientation :: Action_orientation
action_orientation = proxy
instance ShowLabel Action_orientation
    where showLabel _ = "action_orientation"
data Condition_timingTag
type Condition_timing = Proxy Condition_timingTag
condition_timing :: Condition_timing
condition_timing = proxy
instance ShowLabel Condition_timing
    where showLabel _ = "condition_timing"
data Condition_reference_old_tableTag
type Condition_reference_old_table = Proxy Condition_reference_old_tableTag
condition_reference_old_table :: Condition_reference_old_table
condition_reference_old_table = proxy
instance ShowLabel Condition_reference_old_table
    where showLabel _ = "condition_reference_old_table"
data Condition_reference_new_tableTag
type Condition_reference_new_table = Proxy Condition_reference_new_tableTag
condition_reference_new_table :: Condition_reference_new_table
condition_reference_new_table = proxy
instance ShowLabel Condition_reference_new_table
    where showLabel _ = "condition_reference_new_table"
data Condition_reference_old_rowTag
type Condition_reference_old_row = Proxy Condition_reference_old_rowTag
condition_reference_old_row :: Condition_reference_old_row
condition_reference_old_row = proxy
instance ShowLabel Condition_reference_old_row
    where showLabel _ = "condition_reference_old_row"
data Condition_reference_new_rowTag
type Condition_reference_new_row = Proxy Condition_reference_new_rowTag
condition_reference_new_row :: Condition_reference_new_row
condition_reference_new_row = proxy
instance ShowLabel Condition_reference_new_row
    where showLabel _ = "condition_reference_new_row"
data CreatedTag
type Created = Proxy CreatedTag
created :: Created
created = proxy
instance ShowLabel Created
    where showLabel _ = "created"
data View_nameTag
type View_name = Proxy View_nameTag
view_name :: View_name
view_name = proxy
instance ShowLabel View_name
    where showLabel _ = "view_name"
data Table_catalogTag
type Table_catalog = Proxy Table_catalogTag
table_catalog :: Table_catalog
table_catalog = proxy
instance ShowLabel Table_catalog
    where showLabel _ = "table_catalog"
data DefinitionTag
type Definition = Proxy DefinitionTag
definition :: Definition
definition = proxy
instance ShowLabel Definition
    where showLabel _ = "definition"
data Table_schemaTag
type Table_schema = Proxy Table_schemaTag
table_schema :: Table_schema
table_schema = proxy
instance ShowLabel Table_schema
    where showLabel _ = "table_schema"
data Table_nameTag
type Table_name = Proxy Table_nameTag
table_name :: Table_name
table_name = proxy
instance ShowLabel Table_name
    where showLabel _ = "table_name"
data View_definitionTag
type View_definition = Proxy View_definitionTag
view_definition :: View_definition
view_definition = proxy
instance ShowLabel View_definition
    where showLabel _ = "view_definition"
data Check_optionTag
type Check_option = Proxy Check_optionTag
check_option :: Check_option
check_option = proxy
instance ShowLabel Check_option
    where showLabel _ = "check_option"
data Is_updatableTag
type Is_updatable = Proxy Is_updatableTag
is_updatable :: Is_updatable
is_updatable = proxy
instance ShowLabel Is_updatable
    where showLabel _ = "is_updatable"
data Is_insertable_intoTag
type Is_insertable_into = Proxy Is_insertable_intoTag
is_insertable_into :: Is_insertable_into
is_insertable_into = proxy
instance ShowLabel Is_insertable_into
    where showLabel _ = "is_insertable_into"
data Default_spriteTag
type Default_sprite = Proxy Default_spriteTag
default_sprite :: Default_sprite
default_sprite = proxy
instance ShowLabel Default_sprite
    where showLabel _ = "default_sprite"
data Wizard_countTag
type Wizard_count = Proxy Wizard_countTag
wizard_count :: Wizard_count
wizard_count = proxy
instance ShowLabel Wizard_count
    where showLabel _ = "wizard_count"
data World_alignmentTag
type World_alignment = Proxy World_alignmentTag
world_alignment :: World_alignment
world_alignment = proxy
instance ShowLabel World_alignment
    where showLabel _ = "world_alignment"