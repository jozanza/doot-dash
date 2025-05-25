use std::collections::{BTreeMap, BTreeSet};
use std::result;
use std::sync::LazyLock;

turbo::init!(
    struct GameState {
        dungeon: Dungeon,
        gameplay_settings: struct GameplaySettings {
            auto_confirm_start_encounter: bool,
            auto_confirm_movement: bool,
        }
    } = init()
);
turbo::go!({
    let mut state = GameState::load();
    main(&mut state);
    state.save();
});

//------------------------------------------------------------------------------
// Game Initialization
//------------------------------------------------------------------------------
fn init() -> GameState {
    GameState {
        dungeon: Dungeon::new(16, 16),
        gameplay_settings: GameplaySettings {
            auto_confirm_start_encounter: true,
            auto_confirm_movement: false,
        },
    }
}

//------------------------------------------------------------------------------
// Game Loop
//------------------------------------------------------------------------------
fn main(state: &mut GameState) {
    //--------------------------------------------------------------------------
    // Draw Grid & Borders
    //--------------------------------------------------------------------------
    let dungeon_bounds = Bounds::new(
        0,
        0,
        state.dungeon.width as u32 * 16,
        state.dungeon.height as u32 * 16,
    );
    sprite!(
        "grid_bg",
        xy = dungeon_bounds.xy(),
        wh = dungeon_bounds.wh(),
        opacity = 0.01,
        repeat = true,
    );
    let dungeon_border_bounds = dungeon_bounds.expand(1);
    rect!(
        color = 0,
        border_size = 1,
        border_color = 0xffffffff,
        xy = dungeon_border_bounds.xy(),
        wh = dungeon_border_bounds.wh(),
    );

    //--------------------------------------------------------------------------
    // Draw Debug Data
    //--------------------------------------------------------------------------
    let canvas_bounds = bounds::canvas();
    let debug_bounds = canvas_bounds
        .adjust_height_by_fraction(0.63)
        .anchor_bottom(&canvas_bounds);
    // .adjust_width(-512)
    // .anchor_right(&canvas_bounds);
    rect!(
        color = 0x000000ff,
        xy = debug_bounds.xy(),
        wh = debug_bounds.wh(),
        border_size = 1,
        border_color = 0xffffffff,
    );
    let debug = format!(
        "--------------------\nDUNGEON STATE:\n--------------------\n{:#?}\n\n--------------------\nSIMULATION:\n--------------------\n{:#?}",
        state.dungeon.state, state.dungeon.simulated
    )
    .replace("  ", " ");
    text!(
        &debug,
        font = "medium",
        xy = debug_bounds.translate(4, 4).xy()
    );

    //--------------------------------------------------------------------------
    // Draw Action Preview
    //--------------------------------------------------------------------------
    if let Some(action) = &state
        .dungeon
        .simulated
        .as_ref()
        .and_then(DungeonSimulation::get_dungeon_action)
    {
        match action {
            DungeonAction::MonsterUseSkill(MonsterUseSkillOptions {
                monster_id,
                skill_id,
                direction,
            }) => {
                if let Some(skill) = Skill::from_id(*skill_id) {
                    let positions = state.dungeon.get_skill_target_positions(
                        *monster_id,
                        *skill_id,
                        *direction,
                    );
                    let p = positions.clone().concat();
                    for positions in positions {
                        for pos in positions {
                            let (x, y) = pos.grid_xy();
                            if let TargetingRule::User = skill.targeting {
                                rect!(
                                    color = 0,
                                    x = x - 2.,
                                    y = y - 2.,
                                    wh = (20, 20),
                                    opacity = 0.2,
                                    border_size = 2,
                                    border_color = 0x00ff00ff,
                                );
                            } else {
                                rect!(
                                    color = 0xff0000ff, // red preview zone
                                    x = x,
                                    y = y,
                                    wh = (16, 16),
                                    opacity = 0.2
                                );
                            }
                        }
                    }

                    // Optionally draw skill name
                    let skill_debug_bounds = debug_bounds
                        .adjust_width_by_fraction(0.5)
                        .anchor_right(&debug_bounds);
                    let pos = format!(
                        "{:?}",
                        p.chunks(4)
                            .map(|xs| xs.iter().map(Position::xy).collect::<Vec<_>>())
                            .collect::<Vec<_>>()
                    )
                    .replace("], ", "],\n")
                    .replace("[", "")
                    .replace("]", "");
                    text!(
                        "--------------------\nSkill:\n--------------------\n{:#?}\n--------------------\nPositions:\n--------------------\n{}", skill, pos;
                        fixed = true,
                        xy = skill_debug_bounds.translate(4, 4).xy(),
                        // x = 772,
                        // y = 4,
                        font = "medium",
                        color = 0xffffffff
                    );
                }
            }
            _ => {
                //
            }
        }
        //
    }

    //--------------------------------------------------------------------------
    // Draw Floors
    //--------------------------------------------------------------------------
    for floor in &state.dungeon.floors {
        let (x, y) = floor.position.grid_xy();
        let color = match floor.kind {
            FloorKind::Lava => 0xff4500ff,            // orange-red
            FloorKind::Ice => 0xadd8e6ff,             // light blue
            FloorKind::Water => 0x1e90ffff,           // dodger blue
            FloorKind::ConveyorBelt(_) => 0x808080ff, // grey
            FloorKind::SpikeTrap { armed } => {
                if armed {
                    0x2f4f4fff // dark slate gray
                } else {
                    0x4f6f6fff // slate gray
                }
            }
        };
        rect!(color = color, x = x, y = y, wh = (16, 16));
    }

    //--------------------------------------------------------------------------
    // Draw Impediments
    //--------------------------------------------------------------------------
    for imp in &state.dungeon.impediments {
        let (x, y) = imp.position.grid_xy();
        let color = match imp.kind {
            ImpedimentKind::Wall => 0xddddddff,
            ImpedimentKind::Boulder => 0xa0522dff, // sienna
        };
        rect!(color = color, x = x, y = y, wh = (16, 16));
    }

    //--------------------------------------------------------------------------
    // Draw Monsters
    //--------------------------------------------------------------------------
    for monster in &state.dungeon.monsters {
        if monster.hp.val == 0 {
            continue;
        }
        let (x, y) = monster.position.grid_xy();
        let color = if monster.ctrl.is_player() {
            0x0000ffff
        } else {
            0xff00ffff
        };
        ellipse!(color = color, x = x, y = y, wh = (16, 16));
        for effect in state.dungeon.get_action_effects() {
            match effect {
                &DungeonActionEffect::MonsterMove {
                    monster_id,
                    from,
                    to,
                } if monster_id == monster.id => {
                    if let Some(direction) = from.direction_of(to) {
                        let (x, y) = monster.position.dir(direction).grid_xy();
                        let dir_string = format!("{:?}", direction).to_lowercase();
                        let arrow_sprite_name = format!("{}_arrow", dir_string);
                        sprite!(&arrow_sprite_name, x = x, y = y);
                    }
                }
                &DungeonActionEffect::MonsterApplyDamage { monster_id, amount }
                    if monster_id == monster.id =>
                {
                    text!("-{}", amount; x = x + 8., y = y - 8., color = 0xff9999ff);
                }
                &DungeonActionEffect::MonsterHeal { monster_id, amount }
                    if monster_id == monster.id =>
                {
                    text!("+{}", amount; x = x + 8., y = y, color = 0x99ff99ff);
                }
                _ => {}
            }
        }
        let direction = monster.direction;
        let (x, y) = monster.position.grid_xy();
        let dir_string = format!("{:?}", direction).to_lowercase();
        let arrow_sprite_name = format!("{}_arrow", dir_string);
        sprite!(&arrow_sprite_name, x = x, y = y, opacity = 0.25);
    }

    //--------------------------------------------------------------------------
    // Draw Monster HUD
    //--------------------------------------------------------------------------
    if let Some(monster) = state.dungeon.monsters.get(0) {
        let monster_hud_bounds = dungeon_border_bounds
            .right_of_self()
            .width(canvas_bounds.w() - dungeon_border_bounds.w());
        rect!(
            color = 0,
            border_size = 1,
            border_color = 0xffffffff,
            xy = monster_hud_bounds.xy(),
            wh = monster_hud_bounds.wh()
        );
        let monster_hud_inner_bounds = monster_hud_bounds.inset(4);
        let skills = monster
            .get_all_skill_data()
            .iter()
            .enumerate()
            .map(|(i, (skill, status))| {
                let prefix = if state
                    .dungeon
                    .simulated_use_skill_options()
                    .map_or(false, |opts| opts.skill_id == skill.id)
                {
                    "->"
                } else {
                    "- "
                };
                format!(
                    "  {} {}{}",
                    prefix,
                    skill.name,
                    if status.is_ready() {
                        String::new()
                    } else {
                        format!(" (Ready in {} turns)", status.cooldown)
                    }
                )
            })
            // .skip(1)
            .collect::<Vec<_>>()
            .join("\n");
        let text = format!(
            "--------------------\nMONSTER:\n--------------------\n- ID:     {}\n- CTRL:   {:?}\n- HP:     {:?}\n- ENERGY: {:?}\n- SKILLS:\n{}",
            monster.id,
            monster.ctrl,
            (monster.hp.val, monster.hp.max),
            (monster.energy.val, monster.energy.max),
            skills,
        );
        text!(&text, xy = monster_hud_inner_bounds.xy(),);
    }

    //--------------------------------------------------------------------------
    // Handle Player Input
    //--------------------------------------------------------------------------

    let hint_bounds = dungeon_border_bounds
        .below_self()
        .height(28)
        .width(canvas_bounds.w())
        .inset(8);
    // rect!(
    //     color = 0xff0000ff,
    //     xy = hint_bounds.xy(),
    //     wh = hint_bounds.wh()
    // );

    let gp = gamepad(0);

    // Generic confirm/cancel
    if let Some(simulated) = &state.dungeon.simulated {
        if simulated.is_ok() {
            // text_box!(
            //     "Press SPACE to confirm. Press RETURN to cancel.",
            //     font = "medium",
            //     color = 0xffffffff,
            //     xy = hint_bounds.xy(),
            //     wh = hint_bounds.wh()
            // );
            if gp.start.just_pressed() {
                state.dungeon.send(DungeonCommand::Confirm);
            }
            if gp.select.just_pressed() {
                state.dungeon.send(DungeonCommand::Cancel);
            }
        } else {
            if gp.start.just_pressed() {
                state.dungeon.send(DungeonCommand::Cancel);
            }
            if gp.select.just_pressed() {
                state.dungeon.send(DungeonCommand::Cancel);
            }
        }
    }

    // State-specific actions
    match &state.dungeon.state {
        DungeonState::JustCreated => {
            text_box!(
                "Press SPACE to start encounter.",
                xy = hint_bounds.xy(),
                wh = hint_bounds.wh()
            );
            if gp.start.just_pressed() {
                let auto_confirm = state.gameplay_settings.auto_confirm_start_encounter;
                let action = DungeonAction::start_encounter();
                state.dungeon.simulate(auto_confirm, action);
            }
        }
        DungeonState::AwaitingAction { monster_id } => {
            // Active monster
            let current_monster = state.dungeon.monsters.iter().find(|m| m.is_alive());
            if let Some(monster) = current_monster.filter(|m| m.ctrl.is_cpu()).cloned() {
                text_box!(
                    "Press SPACE to trigger CPU action.",
                    xy = hint_bounds.xy(),
                    wh = hint_bounds.wh()
                );
                if gp.start.just_pressed() {
                    let auto_confirm = false;
                    let action = DungeonActionProxy::from_monster_behaviors(monster.id);
                    state.dungeon.proxy(auto_confirm, action);
                }
            } else if let Some(monster) = current_monster.cloned() {
                let monster_id = monster.id;
                if let Some(skill) = state
                    .dungeon
                    .simulated_use_skill_options()
                    .filter(|o| o.monster_id == monster_id)
                    .and_then(|o| Skill::from_id(o.skill_id))
                {
                    text_box!(
                        "Press SPACE to confirm. Press RETURN to cancel.",
                        xy = hint_bounds.xy(),
                        wh = hint_bounds.wh()
                    );
                    // Change attack direction
                    let skill_direction_input = [
                        (gp.up.just_pressed(), Direction::Up),
                        (gp.down.just_pressed(), Direction::Down),
                        (gp.left.just_pressed(), Direction::Left),
                        (gp.right.just_pressed(), Direction::Right),
                    ];
                    if let Some(direction) =
                        skill_direction_input
                            .iter()
                            .find_map(|a| if a.0 { Some(a.1) } else { None })
                    {
                        let action =
                            DungeonAction::monster_use_skill(monster_id, skill.id, direction);
                        state.dungeon.simulate(false, action);
                    }
                } else {
                    text_box!(
                        "Press WASD/Arrow Keys to Move. Press ZXCV to select another skill. Press RETURN to end turn.",
                        xy = hint_bounds.xy(),
                        wh = hint_bounds.wh()
                    );
                    // Monster movement
                    let movement_input = [
                        (gp.up.just_pressed(), Direction::Up),
                        (gp.down.just_pressed(), Direction::Down),
                        (gp.left.just_pressed(), Direction::Left),
                        (gp.right.just_pressed(), Direction::Right),
                    ];
                    if let Some(direction) =
                        movement_input
                            .iter()
                            .find_map(|a| if a.0 { Some(a.1) } else { None })
                    {
                        if let Some((skill, skill_status)) =
                            state.dungeon.get_monster_skill_data_by_index(monster_id, 0)
                        {
                            if skill_status.is_ready() {
                                let auto_confirm = state.gameplay_settings.auto_confirm_movement;
                                let action = DungeonAction::monster_use_skill(
                                    monster_id, skill.id, direction,
                                );
                                state.dungeon.simulate(auto_confirm, action);
                            }
                        }
                    }
                }
                // Monster Skill Usage
                let btns = [
                    gp.a.just_pressed(),
                    gp.b.just_pressed(),
                    gp.x.just_pressed(),
                    gp.y.just_pressed(),
                ];
                if let Some((skill, _)) = btns.iter().position(|a| *a).and_then(|skill_index| {
                    state
                        .dungeon
                        .get_monster_skill_data_by_index(monster_id, skill_index + 1)
                }) {
                    let auto_confirm = false;
                    let direction =
                        if let Some(options) = state.dungeon.simulated_use_skill_options() {
                            options.direction
                        } else {
                            monster.direction
                        };
                    let action = DungeonAction::monster_use_skill(monster_id, skill.id, direction);
                    state.dungeon.simulate(auto_confirm, action);
                }
                // End Turn
                if gp.select.just_pressed() && state.dungeon.simulated.is_none() {
                    let auto_confirm = false;
                    let action = DungeonAction::end_turn();
                    state.dungeon.simulate(auto_confirm, action);
                }
            }
        }
        DungeonState::EncounterComplete(results) => {
            // Reset game
            if gp.start.pressed() && gp.select.pressed() {
                *state = init();
            }
        }
    }

    // Bottom HUD
    if false {
        let hud_height = 32;
        let portrait_width = 32;
        let root = canvas_bounds
            .height(hud_height)
            .anchor_bottom(&canvas_bounds);
        rect!(
            fixed = true,
            color = 0,
            border_size = 1,
            border_color = 0xffffffff,
            xy = root.xy(),
            wh = root.wh()
        );
        let portait_section = root.width(portrait_width);
        rect!(
            fixed = true,
            color = 0,
            border_size = 1,
            border_color = 0xffffffff,
            xy = portait_section.xy(),
            wh = portait_section.wh()
        );
        let actions_section = root
            .adjust_width(portait_section.w() as i32 * -1)
            .anchor_right(&root);
        rect!(
            fixed = true,
            color = 0xffffffff,
            opacity = 0.1,
            border_size = 1,
            border_color = 0xffffffff,
            xy = actions_section.xy(),
            wh = actions_section.wh()
        );
    }
}

//------------------------------------------------------------------------------
// Dungeon Actions
//------------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum DungeonAction {
    StartEncounter(StartEncounterOptions),
    MonsterUseSkill(MonsterUseSkillOptions),
    EndTurn(EndTurnOptions),
}
impl DungeonAction {
    pub fn start_encounter() -> Self {
        Self::StartEncounter(())
    }
    pub fn monster_use_skill(
        monster_id: MonsterId,
        skill_id: SkillId,
        direction: Direction,
    ) -> Self {
        Self::MonsterUseSkill(MonsterUseSkillOptions {
            monster_id,
            skill_id,
            direction,
        })
    }
    pub fn end_turn() -> Self {
        Self::EndTurn(())
    }
}

type StartEncounterOptions = ();

type EndTurnOptions = ();

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
struct MonsterUseSkillOptions {
    monster_id: MonsterId,
    skill_id: SkillId,
    direction: Direction,
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum DungeonActionEffect {
    SpawnPlayerMonster {
        player_id: PlayerId,
    },
    SpawnCPUMonsters {
        amount: u8,
    },
    StartTurn,
    MonsterMove {
        monster_id: MonsterId,
        from: Position,
        to: Position,
    },
    MonsterChangeDirection {
        monster_id: MonsterId,
        direction: Direction,
    },
    MonsterApplyDamage {
        monster_id: MonsterId,
        amount: u8,
    },
    MonsterHeal {
        monster_id: MonsterId,
        amount: u8,
    },
    MonsterEnergyIncrease {
        monster_id: MonsterId,
        amount: u8,
    },
    MonsterEnergyDecrease {
        monster_id: MonsterId,
        amount: u8,
    },
    SpawnCPUMonster {
        monster_id: MonsterId,
    },
    RemoveMonster {
        monster_id: MonsterId,
    },
    ImpedimentMove {
        impediment_id: ImpedimentId,
        from: Position,
        to: Position,
    },
    DecrementActiveMonsterCooldowns,
    ExhaustMonsterSkillCooldown {
        monster_id: MonsterId,
        skill_id: SkillId,
    },
    UpdateTraps,
    IncrementMonsterTurnCount {
        monster_id: MonsterId,
    },
    RotateActiveMonster,
    MonsterBehaviorUsed {
        monster_id: MonsterId,
        behavior_id: BehaviorId,
    },
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum DungeonActionError {
    InvalidAction,
    NoMonstersInDungeon,
    MonsterAlreadyAdded {
        monster_id: MonsterId,
    },
    SkillNotFound {
        skill_id: SkillId,
    },
    SkillStatusNotFound {
        skill_id: SkillId,
    },
    SkillNotReady {
        skill_id: SkillId,
        remaining_cooldown: u8,
    },
    NotEnoughEnergy {
        available: u8,
        required: u8,
    },
    PositionBlockedByMonster {
        monster_id: MonsterId,
    },
    PositionBlockedByImpediment {
        impediment_id: ImpedimentId,
    },
    CannotMoveOutOfBounds,
    MonsterCannotSwim {
        floor_id: FloorId,
    },
    MonsterIsDead {
        monster_id: MonsterId,
    },
    InteractionPreventedByMonster {
        monster_id: MonsterId,
    },
    InteractionPreventedByImpediment {
        impediment_id: ImpedimentId,
    },
    InteractionPreventedByFloor {
        floor_id: FloorId,
    },
    InteractionPreventedByOutOfBounds,
}

type DungeonActionResult = Result<Vec<DungeonActionEffect>, DungeonActionError>;

//------------------------------------------------------------------------------
// Dungeon Proxies
//------------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum DungeonActionProxy {
    FromMonsterBehaviors(FromMonsterBehaviorsOptions),
}
impl DungeonActionProxy {
    pub fn from_monster_behaviors(monster_id: MonsterId) -> Self {
        Self::FromMonsterBehaviors(FromMonsterBehaviorsOptions { monster_id })
    }
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
struct FromMonsterBehaviorsOptions {
    monster_id: MonsterId,
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum DungeonActionProxyError {
    CannotDetermineActionFromNonExistentMonster { monster_id: MonsterId },
    CannotDetermineActionFromDeadMonster { monster_id: MonsterId },
}

type DungeonActionProxyResult = Result<DungeonActionSimulation, DungeonActionProxyError>;

//------------------------------------------------------------------------------
// Dungeon Commands
//------------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum DungeonCommand {
    Simulate {
        action: DungeonAction,
        auto_confirm: bool,
    },
    Proxy {
        proxy: DungeonActionProxy,
        auto_confirm: bool,
    },
    Confirm,
    Cancel,
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum DungeonSimulation {
    Action(DungeonActionSimulation),
    Proxy(DungeonActionProxySimulation),
}
impl DungeonSimulation {
    pub fn new_action(action: DungeonAction, result: DungeonActionResult) -> Self {
        Self::Action(DungeonActionSimulation::new(action, result))
    }
    pub fn new_proxy(proxy: DungeonActionProxy, result: DungeonActionProxyResult) -> Self {
        Self::Proxy(DungeonActionProxySimulation::new(proxy, result))
    }
    pub fn get_dungeon_action(&self) -> Option<&DungeonAction> {
        match self {
            Self::Action(sim) => Some(&sim.action),
            Self::Proxy(sim) => match &sim.result {
                Err(_) => None,
                Ok(sim) => Some(&sim.action),
            },
        }
    }
    pub fn get_dungeon_action_effects<'a>(&'a self) -> &'a [DungeonActionEffect] {
        match self {
            Self::Action(sim) => match &sim.result {
                Ok(effects) => &effects.as_slice(),
                _ => &[],
            },
            Self::Proxy(ref sim) => match &sim.result {
                Err(_) => &[],
                Ok(sim) => match &sim.result {
                    Ok(effects) => &effects.as_slice(),
                    _ => &[],
                },
            },
        }
    }
    pub fn is_ok(&self) -> bool {
        match self {
            Self::Action(sim) => sim.result.is_ok(),
            Self::Proxy(sim) => match &sim.result {
                Err(_) => false,
                Ok(sim) => sim.result.is_ok(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
struct DungeonActionSimulation {
    action: DungeonAction,
    result: DungeonActionResult,
}
impl DungeonActionSimulation {
    pub fn new(action: DungeonAction, result: DungeonActionResult) -> Self {
        Self { action, result }
    }
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
struct DungeonActionProxySimulation {
    proxy: DungeonActionProxy,
    result: Result<DungeonActionSimulation, DungeonActionProxyError>,
}
impl DungeonActionProxySimulation {
    pub fn new(proxy: DungeonActionProxy, result: DungeonActionProxyResult) -> Self {
        Self { proxy, result }
    }
}

//------------------------------------------------------------------------------
// Dungeon Rules
//------------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
struct DungeonRules {
    win: Vec<ConditionGroup<WinCondition>>,
    lose: Vec<ConditionGroup<LoseCondition>>,
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum WinCondition {
    EliminateAllEnemies,
    ReachTile(Position),
    SurviveTurns(u32),
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum LoseCondition {
    AllPlayersDead,
    Timeout(u32),
    ProtectUnitDies(MonsterId),
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum ConditionGroup<T> {
    All(Vec<T>),
    Any(Vec<T>),
}

//------------------------------------------------------------------------------
// Dungeon State
//------------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum DungeonState {
    JustCreated,
    AwaitingAction { monster_id: MonsterId },
    EncounterComplete(EncounterResults),
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum EncounterResults {
    Win(ConditionGroup<WinCondition>),
    Lose(ConditionGroup<LoseCondition>),
}

//------------------------------------------------------------------------------
// Dungeon
//------------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
struct Dungeon {
    pub width: u8,
    pub height: u8,
    pub state: DungeonState,
    pub rules: DungeonRules,
    pub simulated: Option<DungeonSimulation>,
    pub monsters: Vec<Monster>,
    pub impediments: Vec<Impediment>,
    pub floors: Vec<Floor>,
}
impl Dungeon {
    pub fn new(width: u8, height: u8) -> Self {
        let mut impediments = vec![];
        let mut floors = vec![];
        for y in 0..height {
            for x in 0..width {
                let position = Position::new(x, y);
                if rand() % 10 == 0 {
                    let imp = Impediment::new_wall().with_position(position);
                    impediments.push(imp);
                } else if rand() % 10 == 0 {
                    let floor = Floor::new_lava().with_position(position);
                    floors.push(floor);
                } else if rand() % 20 == 0 {
                    let floor = Floor::new_spike_trap().with_position(position);
                    floors.push(floor);
                } else {
                    continue;
                };
            }
        }
        Self {
            width,
            height,
            state: DungeonState::JustCreated,
            rules: DungeonRules {
                // win: vec![ConditionGroup::Any(vec![WinCondition::SurviveTurns(1)])],
                win: vec![],
                lose: vec![],
            },
            simulated: None,
            monsters: vec![],
            impediments,
            floors,
        }
    }
}

//------------------------------------------------------------------------------
// Dungeon Rules Methods
//------------------------------------------------------------------------------

impl Dungeon {
    fn check_win(&self) -> Option<ConditionGroup<WinCondition>> {
        self.rules.win.iter().find_map(|group| match group {
            ConditionGroup::All(conds) => {
                let mut passed = vec![];
                for cond in conds {
                    if let Some(c) = self.evaluate_win(cond) {
                        passed.push(c);
                    } else {
                        return None;
                    }
                }
                Some(ConditionGroup::All(passed))
            }
            ConditionGroup::Any(conds) => {
                for cond in conds {
                    if let Some(c) = self.evaluate_win(cond) {
                        return Some(ConditionGroup::Any(vec![c]));
                    }
                }
                None
            }
        })
    }
    fn check_lose(&self) -> Option<ConditionGroup<LoseCondition>> {
        self.rules.lose.iter().find_map(|group| match group {
            ConditionGroup::All(conds) => {
                let mut failed = vec![];
                for cond in conds {
                    if let Some(c) = self.evaluate_lose(cond) {
                        failed.push(c);
                    } else {
                        return None;
                    }
                }
                Some(ConditionGroup::All(failed))
            }
            ConditionGroup::Any(conds) => {
                for cond in conds {
                    if let Some(c) = self.evaluate_lose(cond) {
                        return Some(ConditionGroup::Any(vec![c]));
                    }
                }
                None
            }
        })
    }
    fn evaluate_win(&self, condition: &WinCondition) -> Option<WinCondition> {
        match condition {
            WinCondition::EliminateAllEnemies => {
                if self
                    .monsters
                    .iter()
                    .all(|m| m.ctrl.is_player() || m.hp.val == 0)
                {
                    Some(condition.clone())
                } else {
                    None
                }
            }
            WinCondition::ReachTile(pos) => {
                if self
                    .monsters
                    .iter()
                    .any(|m| m.ctrl.is_player() && m.position == *pos)
                {
                    Some(condition.clone())
                } else {
                    None
                }
            }
            WinCondition::SurviveTurns(t) => {
                if self
                    .monsters
                    .iter()
                    .any(|m| m.ctrl.is_player() && m.turns_taken == *t)
                {
                    Some(condition.clone())
                } else {
                    None
                }
            }
        }
    }

    fn evaluate_lose(&self, condition: &LoseCondition) -> Option<LoseCondition> {
        match condition {
            LoseCondition::AllPlayersDead => {
                if self
                    .monsters
                    .iter()
                    .all(|m| !m.ctrl.is_player() || m.hp.val == 0)
                {
                    Some(condition.clone())
                } else {
                    None
                }
            }
            LoseCondition::Timeout(t) => {
                if self
                    .monsters
                    .iter()
                    .any(|m| m.ctrl.is_player() && m.turns_taken >= *t)
                {
                    Some(condition.clone())
                } else {
                    None
                }
            }
            LoseCondition::ProtectUnitDies(monster_id) => {
                if self
                    .monsters
                    .iter()
                    .any(|m| m.id == *monster_id && m.hp.val == 0)
                {
                    Some(condition.clone())
                } else {
                    None
                }
            }
        }
    }
}

//------------------------------------------------------------------------------
// Dungeon Command Methods
//------------------------------------------------------------------------------

impl Dungeon {
    pub fn simulate(&mut self, auto_confirm: bool, action: DungeonAction) {
        self.send(DungeonCommand::Simulate {
            action,
            auto_confirm,
        })
    }

    pub fn proxy(&mut self, auto_confirm: bool, proxy: DungeonActionProxy) {
        self.send(DungeonCommand::Proxy {
            proxy,
            auto_confirm,
        })
    }

    pub fn send(&mut self, command: DungeonCommand) {
        log!("Command send {:?}", command);
        match command {
            DungeonCommand::Simulate {
                action,
                auto_confirm,
            } => {
                let result = self.preview_action(&action);
                if result.is_ok() && auto_confirm {
                    let effects = result.unwrap();
                    self.apply_effects(&effects);
                    self.simulated = None;
                } else {
                    self.simulated = Some(DungeonSimulation::new_action(action, result));
                }
            }
            DungeonCommand::Proxy {
                proxy,
                auto_confirm,
            } => match &proxy {
                DungeonActionProxy::FromMonsterBehaviors(options) => {
                    let &FromMonsterBehaviorsOptions { monster_id } = options;
                    match self.get_action_and_result_from_monster_behaviors(&options) {
                        // Failed to proxy
                        Err(err) => {
                            self.simulated = Some(DungeonSimulation::new_proxy(proxy, Err(err)));
                        }
                        // Succesfully proxied
                        Ok(result) => match result {
                            // Action failed
                            (action, Err(err), _) => {
                                self.simulated = Some(DungeonSimulation::new_proxy(
                                    proxy,
                                    Ok(DungeonActionSimulation::new(action, Err(err))),
                                ));
                            }
                            // Action succeeded (no behavior)
                            (action, Ok(effects), None) => {
                                if auto_confirm {
                                    self.apply_effects(&effects);
                                    self.simulated = None;
                                } else {
                                    self.simulated = Some(DungeonSimulation::new_proxy(
                                        proxy,
                                        Ok(DungeonActionSimulation::new(action, Ok(effects))),
                                    ));
                                }
                            }
                            // Action succeeded (used behavior)
                            (action, Ok(mut effects), Some(behavior_id)) => {
                                effects.push(DungeonActionEffect::MonsterBehaviorUsed {
                                    monster_id,
                                    behavior_id,
                                });
                                if auto_confirm {
                                    self.apply_effects(&effects);
                                    self.simulated = None;
                                } else {
                                    self.simulated = Some(DungeonSimulation::new_proxy(
                                        proxy,
                                        Ok(DungeonActionSimulation::new(action, Ok(effects))),
                                    ));
                                }
                            }
                        },
                    }
                }
            },
            DungeonCommand::Confirm => match self.simulated.clone() {
                None => log!("Nothing to confirm"),
                Some(DungeonSimulation::Action(sim)) => {
                    if let Ok(effects) = &sim.result {
                        self.apply_effects(effects);
                    }
                    self.simulated = None;
                }
                Some(DungeonSimulation::Proxy(sim)) => {
                    if let Ok(sim) = &sim.result {
                        if let Ok(effects) = &sim.result {
                            self.apply_effects(effects);
                        }
                    }
                    self.simulated = None;
                }
            },
            DungeonCommand::Cancel => {
                self.simulated = None;
            }
        }
    }
}

//------------------------------------------------------------------------------
// Dungeon Util Methods
//------------------------------------------------------------------------------

impl Dungeon {
    pub fn is_in_bounds(&self, position: Position) -> bool {
        position.x < self.width && position.y < self.height
    }

    /// Simulates movement on ice tiles by traversing in the given direction
    /// until encountering a blocking position, based on custom logic provided
    /// in `is_blocked`. Returns the final position where movement stops.
    ///
    /// `thing` is a reference to the entity moving (e.g., monster or impediment).
    /// `is_blocked` defines whether a position blocks movement for that entity.
    pub fn traverse_ice<T>(
        start: Position,
        direction: Direction,
        thing: &T,
        is_blocked: impl Fn(&T, Position) -> bool,
    ) -> Position {
        let mut current = start;
        loop {
            let next = current.dir(direction);
            if is_blocked(thing, next) {
                break current;
            }
            current = next;
        }
    }

    /// Traverses conveyor belt tiles starting from the given position.
    /// Follows the belt's direction until a tile is blocked or no longer a conveyor belt.
    /// Returns a list of positions the entity would traverse.
    ///
    /// `is_blocked` is a closure that takes the moving entity and a position,
    /// and returns true if that position is blocked for the entity.
    pub fn traverse_conveyor_belt<T>(
        &self,
        start: Position,
        entity: &T,
        is_blocked: impl Fn(&T, Position) -> bool,
    ) -> Vec<Position> {
        let mut path = vec![];
        let mut current = start;
        let mut visited = BTreeSet::new();

        while let Some(Floor {
            kind: FloorKind::ConveyorBelt(dir),
            ..
        }) = self.get_floor_at(current)
        {
            let next = current.dir(*dir);

            // Prevent infinite loops
            if !visited.insert(next) {
                break;
            }

            // Stop if blocked
            if is_blocked(entity, next) {
                break;
            }

            path.push(next);
            current = next;
        }

        path
    }

    /// Returns a closure that returns true if the given position blocks a monster.
    pub fn monster_is_blocked_fn(&self) -> impl Fn(&Monster, Position) -> bool + '_ {
        move |monster, pos| {
            !self.is_in_bounds(pos)
                || self
                    .get_monster_at(pos)
                    .map_or(false, |m| m.is_alive() && m.id != monster.id)
                || self.has_impediment_at(pos)
                || self
                    .get_floor_at(pos)
                    .map_or(false, |f| f.kind == FloorKind::Water)
        }
    }

    /// Returns a closure that returns true if the given position blocks an impediment.
    pub fn impediment_is_blocked_fn(&self) -> impl Fn(&Impediment, Position) -> bool + '_ {
        move |impediment, pos| {
            !self.is_in_bounds(pos)
                || self.get_monster_at(pos).map_or(false, |m| m.is_alive())
                || self.has_impediment_at(pos)
                || self
                    .get_floor_at(pos)
                    .map_or(false, |f| f.kind == FloorKind::SpikeTrap { armed: true })
        }
    }
}

//------------------------------------------------------------------------------
// Dungeon Action Preview Methods
//------------------------------------------------------------------------------

impl Dungeon {
    fn preview_action(&self, action: &DungeonAction) -> DungeonActionResult {
        match action {
            DungeonAction::StartEncounter(options) => self.preview_action_start_encounter(options),
            DungeonAction::MonsterUseSkill(options) => {
                self.preview_action_monster_use_skill(options)
            }
            DungeonAction::EndTurn(options) => self.preview_action_end_turn(options),
        }
    }

    fn preview_action_end_turn(&self, _options: &EndTurnOptions) -> DungeonActionResult {
        match self.state {
            DungeonState::AwaitingAction { monster_id } => Ok(vec![
                DungeonActionEffect::UpdateTraps,
                DungeonActionEffect::IncrementMonsterTurnCount { monster_id },
                DungeonActionEffect::RotateActiveMonster,
                DungeonActionEffect::DecrementActiveMonsterCooldowns,
                DungeonActionEffect::StartTurn,
            ]),
            _ => Err(DungeonActionError::InvalidAction),
        }
    }

    fn preview_action_start_encounter(
        &self,
        _options: &StartEncounterOptions,
    ) -> DungeonActionResult {
        match self.state {
            DungeonState::JustCreated => Ok(vec![
                DungeonActionEffect::SpawnPlayerMonster { player_id: 0 },
                DungeonActionEffect::SpawnCPUMonsters { amount: 4 },
                DungeonActionEffect::DecrementActiveMonsterCooldowns,
                DungeonActionEffect::StartTurn,
            ]),
            _ => Err(DungeonActionError::InvalidAction),
        }
    }

    fn get_action_and_result_from_monster_behaviors(
        &self,
        options: &FromMonsterBehaviorsOptions,
    ) -> Result<(DungeonAction, DungeonActionResult, Option<BehaviorId>), DungeonActionProxyError>
    {
        let &FromMonsterBehaviorsOptions { monster_id } = options;
        let monster = self.monsters.iter().find(|m| m.id == monster_id).ok_or(
            DungeonActionProxyError::CannotDetermineActionFromNonExistentMonster { monster_id },
        )?;
        if !monster.is_alive() {
            return Err(
                DungeonActionProxyError::CannotDetermineActionFromDeadMonster { monster_id },
            );
        }

        for behavior in &monster.behaviors {
            match &behavior.kind {
                BehaviorKind::HealIfLowHP {
                    skill_id,
                    threshold,
                } => {
                    let current_ratio = monster.hp.val as f32 / monster.hp.max as f32;
                    if current_ratio >= *threshold {
                        continue;
                    }

                    let direction = monster.direction;
                    let action = DungeonAction::monster_use_skill(monster.id, *skill_id, direction);
                    let result = self.preview_action(&action);
                    if result.is_ok() {
                        return Ok((action, result, Some(behavior.behavior_id)));
                    }
                }
                BehaviorKind::MoveInRandomDirection => {
                    // Assume skill 0 is movement
                    // TODO: filter and select first ready skill with movement effects
                    if let Some((skill, status)) = monster.get_skill_data_by_index(0) {
                        if status.is_ready() {
                            let directions = &mut [
                                Direction::Up,
                                Direction::Down,
                                Direction::Left,
                                Direction::Right,
                            ];
                            shuffle(directions);
                            for &mut direction in directions {
                                let action = DungeonAction::monster_use_skill(
                                    monster_id, skill.id, direction,
                                );
                                let result = self.preview_action(&action);
                                if result.is_ok() {
                                    return Ok((action, result, Some(behavior.behavior_id)));
                                }
                            }
                        }
                    }
                }
                BehaviorKind::DoMaxDamageToNearestPlayer => {
                    let mut best_action = None;
                    let mut max_damage = 0;

                    for (skill, status) in monster.get_all_skill_data() {
                        if !status.is_ready() {
                            continue;
                        }
                        let directions = if skill.requires_direction() {
                            vec![
                                Direction::Up,
                                Direction::Down,
                                Direction::Left,
                                Direction::Right,
                            ]
                        } else {
                            vec![monster.direction]
                        };

                        for direction in directions {
                            let action =
                                DungeonAction::monster_use_skill(monster.id, skill.id, direction);
                            if let Ok(effects) = self.preview_action(&action) {
                                let damage: u32 = effects
                                    .iter()
                                    .filter_map(|e| match e {
                                        DungeonActionEffect::MonsterApplyDamage {
                                            monster_id,
                                            amount,
                                        } => self
                                            .get_monster_by_id(*monster_id)
                                            .filter(|m| m.is_alive())
                                            .map(|_| *amount as u32),
                                        _ => None,
                                    })
                                    .sum();

                                if damage > max_damage {
                                    best_action = Some((action, effects));
                                    max_damage = damage;
                                }
                            }
                        }
                    }

                    if let Some((action, effects)) = best_action {
                        return Ok((action, Ok(effects), Some(behavior.behavior_id)));
                    }
                }
            }
        }

        let action = DungeonAction::end_turn();
        Ok((action.clone(), self.preview_action(&action), None))
    }

    fn preview_action_monster_use_skill(
        &self,
        options: &MonsterUseSkillOptions,
    ) -> DungeonActionResult {
        // Ensure it is the monster's turn
        match self.state {
            DungeonState::AwaitingAction { monster_id } => {
                if monster_id != options.monster_id {
                    return Err(DungeonActionError::InvalidAction);
                }
            }
            _ => {}
        }
        let &MonsterUseSkillOptions {
            monster_id,
            skill_id,
            direction,
        } = options;
        let monster = self
            .monsters
            .iter()
            .find(|m| m.id == monster_id)
            .ok_or(DungeonActionError::NoMonstersInDungeon)?;

        if !monster.is_alive() {
            return Err(DungeonActionError::MonsterIsDead { monster_id });
        }

        let Some((skill, skill_status)) = monster.get_skill_data_by_id(skill_id) else {
            return Err(DungeonActionError::SkillStatusNotFound { skill_id: skill_id });
        };

        if !skill_status.is_ready() {
            return Err(DungeonActionError::SkillNotReady {
                skill_id,
                remaining_cooldown: skill_status.cooldown,
            });
        }

        if monster.energy.val < skill.energy_cost {
            return Err(DungeonActionError::NotEnoughEnergy {
                available: monster.energy.val,
                required: skill.energy_cost,
            });
        }

        let positions = self
            .get_skill_target_positions(monster_id, skill_id, direction)
            .iter()
            .cloned()
            .flatten()
            .collect::<BTreeSet<_>>();

        let mut effects = vec![];

        for skill_effect in &skill.effects {
            for target_pos in &positions {
                if let Some(target) = self.get_monster_at(*target_pos) {
                    let is_user = target.id == monster_id;
                    if !is_user && skill.targeting == TargetingRule::User {
                        continue;
                    }
                    let is_ally = target.ctrl == monster.ctrl;
                    if !is_ally && skill.targeting == TargetingRule::LivingAllies {
                        continue;
                    }
                    if is_ally && skill.targeting == TargetingRule::LivingEnemies {
                        continue;
                    }
                    match skill_effect {
                        &SkillEffect::Move { distance } => {
                            let from = monster.position;
                            let to = from.dir_with_distance(direction, distance);

                            // Check that the destination is within bounds
                            if !self.is_in_bounds(to) {
                                return Err(DungeonActionError::CannotMoveOutOfBounds);
                            }

                            // Check for collisions with other monsters
                            if let Some(monster) = self.get_monster_at(to) {
                                if monster.is_alive() {
                                    return Err(DungeonActionError::PositionBlockedByMonster {
                                        monster_id: monster.id,
                                    });
                                }
                            }

                            // Check for impediment collision
                            if let Some(impediment) = self.get_impediment_at(to) {
                                match impediment.kind {
                                    ImpedimentKind::Boulder => {
                                        // Boulders on lava or water are stuck there
                                        if let Some(floor) = self.get_floor_at(to) {
                                            match floor.kind {
                                                FloorKind::Lava | FloorKind::Water => {
                                                    return Err(
                                                        DungeonActionError::InteractionPreventedByFloor {
                                                            floor_id: floor.id,
                                                        },
                                                    );
                                                }
                                                _ => {}
                                            }
                                        }

                                        let boulder_to = to.dir(direction);

                                        // Check that boulder destination is in bounds
                                        if !self.is_in_bounds(boulder_to) {
                                            return Err(
                                                DungeonActionError::InteractionPreventedByOutOfBounds,
                                            );
                                        }

                                        // Ensure destination is not blocked by a living monster
                                        if let Some(monster) = self.get_monster_at(boulder_to) {
                                            if monster.is_alive() {
                                                return Err(
                                                    DungeonActionError::InteractionPreventedByMonster {
                                                        monster_id: monster.id,
                                                    },
                                                );
                                            }
                                        }

                                        // Ensure destination is not blocked by an impediment
                                        if let Some(impediment) = self.get_impediment_at(boulder_to)
                                        {
                                            return Err(
                                                DungeonActionError::PositionBlockedByImpediment {
                                                    impediment_id: impediment.id,
                                                },
                                            );
                                        }

                                        // Handle impediment floor interactions
                                        if let Some(floor) = self.get_floor_at(boulder_to) {
                                            match floor.kind {
                                                // Can't push onto an armed spike trap
                                                FloorKind::SpikeTrap { armed } => {
                                                    if armed {
                                                        return Err(
                                                            DungeonActionError::InteractionPreventedByFloor {
                                                                floor_id: floor.id,
                                                            },
                                                        );
                                                    }
                                                }
                                                FloorKind::Ice => {
                                                    let final_pos = Self::traverse_ice(
                                                        boulder_to,
                                                        direction,
                                                        impediment,
                                                        Dungeon::impediment_is_blocked_fn(self),
                                                    );
                                                    let mut current_pos = to;
                                                    while current_pos != final_pos {
                                                        let next_pos = current_pos.dir(direction);
                                                        effects.extend([
                                                            DungeonActionEffect::MonsterChangeDirection {
                                                                monster_id,
                                                                direction,
                                                            },
                                                            DungeonActionEffect::ImpedimentMove {
                                                                impediment_id: impediment.id,
                                                                from: to,
                                                                to: next_pos,
                                                            },
                                                        ]);
                                                        current_pos = next_pos;
                                                    }
                                                    continue;
                                                }
                                                FloorKind::ConveyorBelt(_) => {
                                                    effects.extend([
                                                        DungeonActionEffect::MonsterChangeDirection {
                                                            monster_id,
                                                            direction,
                                                        },
                                                        DungeonActionEffect::ImpedimentMove {
                                                            impediment_id: impediment.id,
                                                            from: to,
                                                            to: boulder_to,
                                                        },
                                                    ]);

                                                    let is_blocked =
                                                        self.impediment_is_blocked_fn();
                                                    let positions = self.traverse_conveyor_belt(
                                                        boulder_to, impediment, is_blocked,
                                                    );
                                                    for i in 0..positions.len().saturating_sub(1) {
                                                        let from = positions[i];
                                                        let to = positions[i + 1];
                                                        effects.push(
                                                            DungeonActionEffect::ImpedimentMove {
                                                                impediment_id: impediment.id,
                                                                from,
                                                                to,
                                                            },
                                                        );
                                                    }
                                                    continue;
                                                }
                                                _ => {}
                                            }
                                        }

                                        effects.extend([
                                            DungeonActionEffect::MonsterChangeDirection {
                                                monster_id,
                                                direction,
                                            },
                                            DungeonActionEffect::ImpedimentMove {
                                                impediment_id: impediment.id,
                                                from: to,
                                                to: boulder_to,
                                            },
                                        ]);

                                        continue;
                                    }
                                    _ => {
                                        return Err(
                                            DungeonActionError::PositionBlockedByImpediment {
                                                impediment_id: impediment.id,
                                            },
                                        );
                                    }
                                }
                            }

                            // Check for floor intersection
                            match self.get_floor_at(to) {
                                // No floor
                                // TODO: give dungeon a default floor
                                None => effects.extend([
                                    DungeonActionEffect::MonsterChangeDirection {
                                        monster_id,
                                        direction,
                                    },
                                    DungeonActionEffect::MonsterMove {
                                        monster_id,
                                        from,
                                        to,
                                    },
                                ]),
                                // Floor detected
                                Some(&Floor {
                                    id: floor_id,
                                    position: _,
                                    ref kind,
                                }) => match kind {
                                    &FloorKind::Lava => effects.extend([
                                        DungeonActionEffect::MonsterChangeDirection {
                                            monster_id,
                                            direction,
                                        },
                                        DungeonActionEffect::MonsterMove {
                                            monster_id,
                                            from,
                                            to,
                                        },
                                        DungeonActionEffect::MonsterApplyDamage {
                                            monster_id,
                                            amount: Floor::LAVA_FLOOR_DAMAGE,
                                        },
                                    ]),
                                    &FloorKind::Ice => {
                                        let is_blocked = self.monster_is_blocked_fn();
                                        let ice_end =
                                            Self::traverse_ice(to, direction, monster, is_blocked);
                                        let mut current_pos = to;
                                        while current_pos != ice_end {
                                            let next_pos = current_pos.dir(direction);
                                            effects.push(DungeonActionEffect::MonsterMove {
                                                monster_id,
                                                from: current_pos,
                                                to: next_pos,
                                            });
                                            current_pos = next_pos;
                                        }
                                    }
                                    &FloorKind::ConveyorBelt(_) => {
                                        let is_blocked = self.monster_is_blocked_fn();
                                        let positions =
                                            self.traverse_conveyor_belt(to, monster, is_blocked);
                                        for i in 0..positions.len().saturating_sub(1) {
                                            let from = positions[i];
                                            let to = positions[i + 1];
                                            effects.push(DungeonActionEffect::MonsterMove {
                                                monster_id,
                                                from,
                                                to,
                                            });
                                        }
                                    }
                                    &FloorKind::SpikeTrap { armed } => {
                                        effects.extend([
                                            DungeonActionEffect::MonsterChangeDirection {
                                                monster_id,
                                                direction,
                                            },
                                            DungeonActionEffect::MonsterMove {
                                                monster_id,
                                                from,
                                                to,
                                            },
                                        ]);
                                        if armed {
                                            effects.push(DungeonActionEffect::MonsterApplyDamage {
                                                monster_id,
                                                amount: Floor::SPIKE_TRAP_DAMAGE,
                                            });
                                        }
                                    }
                                    &FloorKind::Water => {
                                        return Err(DungeonActionError::MonsterCannotSwim {
                                            floor_id,
                                        });
                                    }
                                },
                            }
                        }
                        SkillEffect::Damage { amount } => {
                            // Do not allow self-damage or friendly-fire
                            if is_user || is_ally {
                                continue;
                            }
                            effects.push(DungeonActionEffect::MonsterApplyDamage {
                                monster_id: target.id,
                                amount: *amount,
                            });
                        }
                        SkillEffect::Heal { amount } => {
                            // Do not heal enemies
                            if !is_ally && !is_user {
                                continue;
                            }
                            effects.push(DungeonActionEffect::MonsterHeal {
                                monster_id: target.id,
                                amount: *amount,
                            });
                        }
                        SkillEffect::EnergyIncrease { amount } => {
                            // Do not increase enemy energy
                            if !is_ally && !is_user {
                                continue;
                            }
                            effects.push(DungeonActionEffect::MonsterEnergyIncrease {
                                monster_id: target.id,
                                amount: *amount,
                            });
                        }
                        SkillEffect::EnergyDecrease { amount } => {
                            // Do not decrease ally energy
                            if is_user || is_ally {
                                continue;
                            }
                            effects.push(DungeonActionEffect::MonsterEnergyDecrease {
                                monster_id: target.id,
                                amount: *amount,
                            });
                        }
                    }
                }
            }
        }

        // Deduct energy_cost of using skill
        if skill.energy_cost > 0 {
            effects.push(DungeonActionEffect::MonsterEnergyDecrease {
                monster_id: monster_id,
                amount: skill.energy_cost,
            });
        }

        // Exhause the skill's cooldown
        effects.push(DungeonActionEffect::ExhaustMonsterSkillCooldown {
            monster_id: monster_id,
            skill_id: skill_id,
        });

        Ok(effects)
    }
}

//------------------------------------------------------------------------------
// Dungeon Action Effects Methods
//------------------------------------------------------------------------------
impl Dungeon {
    fn get_action_effects<'a>(&'a self) -> &'a [DungeonActionEffect] {
        if let Some(sim) = &self.simulated {
            sim.get_dungeon_action_effects()
        } else {
            &[]
        }
    }
    fn apply_effects(&mut self, effects: &[DungeonActionEffect]) {
        for effect in effects {
            match effect {
                &DungeonActionEffect::SpawnPlayerMonster { player_id } => {
                    let monster = Monster::new_player(player_id);
                    self.monsters.push(monster);
                }
                &DungeonActionEffect::SpawnCPUMonsters { amount } => {
                    for _ in 0..amount {
                        let mut monster = Monster::new_cpu();
                        let x = rand() % self.width as u32;
                        monster.position.x = x as u8;
                        let y = rand() % self.width as u32;
                        monster.position.y = y as u8;
                        self.monsters.push(monster);
                    }
                }
                &DungeonActionEffect::StartTurn => {
                    if let Some(monster) = self.monsters.iter().find(|m| m.is_alive()) {
                        let monster_id = monster.id;
                        // Check for floor damage at the monster's position
                        if let Some(floor) = self.get_floor_at(monster.position) {
                            let damage = match floor.kind {
                                FloorKind::Lava => Some(Floor::LAVA_FLOOR_DAMAGE),
                                FloorKind::SpikeTrap { armed: true } => {
                                    Some(Floor::SPIKE_TRAP_DAMAGE)
                                }
                                _ => None,
                            };
                            if let Some(amount) = damage {
                                self.apply_effects(&[DungeonActionEffect::MonsterApplyDamage {
                                    monster_id,
                                    amount,
                                }]);
                                // Re-check monster's alive status before setting state
                                if !self
                                    .get_monster_by_id(monster_id)
                                    .map_or(false, Monster::is_alive)
                                {
                                    // Opt out of remaining effects
                                    break;
                                }
                            }
                        }

                        // Wait for their next action
                        self.state = DungeonState::AwaitingAction { monster_id };
                    }
                }
                &DungeonActionEffect::MonsterMove {
                    monster_id,
                    from: _,
                    to,
                } => {
                    if let Some(monster) = self.get_monster_mut_by_id(monster_id) {
                        if !monster.is_alive() {
                            continue;
                        }
                        monster.position = to;
                    }
                }
                &DungeonActionEffect::MonsterChangeDirection {
                    monster_id,
                    direction,
                } => {
                    if let Some(monster) = self.get_monster_mut_by_id(monster_id) {
                        if !monster.is_alive() {
                            continue;
                        }
                        monster.direction = direction;
                    }
                }
                &DungeonActionEffect::MonsterApplyDamage { monster_id, amount } => {
                    if let Some(monster) = self.get_monster_mut_by_id(monster_id) {
                        if !monster.is_alive() {
                            continue;
                        }
                        monster.hp.val = monster.hp.val.saturating_sub(amount);
                    }
                }
                &DungeonActionEffect::ExhaustMonsterSkillCooldown {
                    monster_id,
                    skill_id,
                } => {
                    if let Some(monster) = self.get_monster_mut_by_id(monster_id) {
                        if !monster.is_alive() {
                            continue;
                        }
                        if let Some((skill, skill_status)) = monster
                            .skills
                            .iter_mut()
                            .find(|skill_status| skill_status.skill_id == skill_id)
                            .and_then(|skill_status| {
                                Some((skill_status.get_skill()?, skill_status))
                            })
                        {
                            skill_status.cooldown = skill.cooldown;
                        }
                    }
                }
                &DungeonActionEffect::DecrementActiveMonsterCooldowns => {
                    if let Some(monster) = self.monsters.get_mut(0) {
                        if !monster.is_alive() {
                            continue;
                        }
                        for status in &mut monster.skills {
                            status.cooldown = status.cooldown.saturating_sub(1);
                        }
                    }
                }
                DungeonActionEffect::UpdateTraps => {
                    for floor in &mut self.floors {
                        if let FloorKind::SpikeTrap { armed } = &mut floor.kind {
                            *armed = !*armed;
                        }
                    }
                }
                &DungeonActionEffect::IncrementMonsterTurnCount { monster_id } => {
                    if let Some(monster) = self.get_monster_mut_by_id(monster_id) {
                        if !monster.is_alive() {
                            continue;
                        }
                        monster.turns_taken = monster.turns_taken.saturating_add(1);
                    }
                }
                DungeonActionEffect::RotateActiveMonster => {
                    let len = self.monsters.len();
                    for _ in 0..len {
                        self.monsters.rotate_left(1);
                        // Ensures we skip dead monsters
                        if self.monsters[0].is_alive() {
                            break;
                        }
                    }
                }
                &DungeonActionEffect::MonsterEnergyIncrease { monster_id, amount } => {
                    if let Some(monster) = self.get_monster_mut_by_id(monster_id) {
                        if !monster.is_alive() {
                            continue;
                        }
                        monster.energy.val = monster
                            .energy
                            .val
                            .saturating_add(amount)
                            .min(monster.energy.max);
                    }
                }
                &DungeonActionEffect::MonsterBehaviorUsed {
                    monster_id,
                    behavior_id,
                } => {
                    if let Some(monster) = self.get_monster_mut_by_id(monster_id) {
                        let turn = monster.turns_taken;
                        if let Some(behavior) = monster
                            .behaviors
                            .iter_mut()
                            .find(|b| b.behavior_id == behavior_id)
                        {
                            behavior.last_turn_used = Some(turn);
                        }
                    }
                }
                effect => {
                    log!("Unimplemented {:?}", effect);
                }
            }
        }

        if let DungeonState::AwaitingAction { monster_id } = self.state {
            if self
                .get_monster_by_id(monster_id)
                .map_or(true, |m| !m.is_alive())
            {
                self.apply_effects(&[
                    DungeonActionEffect::RotateActiveMonster,
                    DungeonActionEffect::StartTurn,
                ]);
            }
        }

        // Check for win
        if let Some(cond) = self.check_win() {
            self.state = DungeonState::EncounterComplete(EncounterResults::Win(cond));
        }
        // Check for loss
        if let Some(cond) = self.check_lose() {
            self.state = DungeonState::EncounterComplete(EncounterResults::Lose(cond));
        }
    }
}

//------------------------------------------------------------------------------
// Dungeon Monster Methods
//------------------------------------------------------------------------------

impl Dungeon {
    pub fn has_monster_at(&self, position: Position) -> bool {
        self.monsters.iter().any(|m| m.position == position)
    }

    pub fn get_monster_at(&self, position: Position) -> Option<&Monster> {
        self.monsters.iter().find(|m| m.position == position)
    }

    pub fn get_monster_by_id(&self, monster_id: MonsterId) -> Option<&Monster> {
        self.monsters.iter().find(|m| m.id == monster_id)
    }

    pub fn get_monster_mut_by_id(&mut self, monster_id: MonsterId) -> Option<&mut Monster> {
        self.monsters.iter_mut().find(|m| m.id == monster_id)
    }
}

//------------------------------------------------------------------------------
// Dungeon Skill Methods
//------------------------------------------------------------------------------

impl Dungeon {
    pub fn simulated_use_skill_options(&self) -> Option<&MonsterUseSkillOptions> {
        if let Some(DungeonAction::MonsterUseSkill(ref options)) = self
            .simulated
            .as_ref()
            .and_then(DungeonSimulation::get_dungeon_action)
        {
            return Some(options);
        }
        None
    }

    pub fn get_monster_skill_data_by_index(
        &self,
        monster_id: MonsterId,
        index: usize,
    ) -> Option<(&Skill, &SkillStatus)> {
        let monster = self.get_monster_by_id(monster_id)?;
        monster.get_skill_data_by_index(index)
    }

    pub fn get_skill_target_positions(
        &self,
        monster_id: MonsterId,
        skill_id: SkillId,
        direction: Direction,
    ) -> [Vec<Position>; 2] {
        let monster = match self.get_monster_by_id(monster_id) {
            Some(m) => m,
            None => return [vec![], vec![]],
        };

        let skill = match Skill::from_id(skill_id) {
            Some(s) => s,
            None => return [vec![], vec![]],
        };

        let origin = monster.position;

        // Step 1: base positions from targeting rule
        let mut base_positions: Vec<Position> = match skill.targeting {
            TargetingRule::User => vec![origin],

            TargetingRule::Line { range } => {
                let mut positions = BTreeSet::new();
                let mut pos = origin;
                for _ in 0..range {
                    pos = pos.dir(direction);
                    positions.insert(pos);
                }
                Vec::from_iter(positions)
            }

            TargetingRule::Circle { radius } => {
                let mut positions = BTreeSet::new();
                let r = radius as i8;
                for dx in -r..=r {
                    for dy in -r..=r {
                        if dx * dx + dy * dy <= r * r {
                            positions.insert(origin.with_offset(dx, dy));
                        }
                    }
                }
                Vec::from_iter(positions)
            }

            TargetingRule::LivingAllies => self
                .monsters
                .iter()
                .filter(|m| {
                    m.is_alive()
                        && match (m.ctrl.clone(), monster.ctrl.clone()) {
                            (MonsterController::Player(a), MonsterController::Player(b)) => a == b,
                            (MonsterController::CPU, MonsterController::CPU) => true,
                            _ => false,
                        }
                })
                .map(|m| m.position)
                .collect(),

            TargetingRule::LivingEnemies => self
                .monsters
                .iter()
                .filter(|m| {
                    m.is_alive()
                        && match (m.ctrl.clone(), monster.ctrl.clone()) {
                            (MonsterController::Player(a), MonsterController::Player(b)) => a != b,
                            (MonsterController::CPU, MonsterController::Player(_)) => true,
                            (MonsterController::Player(_), MonsterController::CPU) => true,
                            _ => false,
                        }
                })
                .map(|m| m.position)
                .collect(),
        };
        base_positions.sort_unstable();

        // Step 2: apply AoE expansion using Manhattan distance
        let mut aoe_positions = BTreeSet::new();
        if skill.aoe > 0 {
            for pos in &base_positions {
                if self.monsters.iter().any(|m| m.position == *pos) {
                    for position in manhattan_area(*pos, skill.aoe) {
                        aoe_positions.insert(position);
                    }
                }
            }
        }
        let mut aoe_positions = Vec::from_iter(aoe_positions);
        aoe_positions.sort_unstable();

        [base_positions, aoe_positions]
    }
}

//------------------------------------------------------------------------------
// Dungeon Impediment Methods
//------------------------------------------------------------------------------

impl Dungeon {
    pub fn has_impediment_at(&self, position: Position) -> bool {
        self.impediments.iter().any(|i| i.position == position)
    }
    pub fn get_impediment_at(&self, position: Position) -> Option<&Impediment> {
        self.impediments.iter().find(|i| i.position == position)
    }
}

//------------------------------------------------------------------------------
// Dungeon Floor Methods
//------------------------------------------------------------------------------

impl Dungeon {
    pub fn get_floor_at(&self, pos: Position) -> Option<&Floor> {
        self.floors.iter().find(|f| f.position == pos)
    }
}

//------------------------------------------------------------------------------
// stat
//------------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, BorshSerialize, BorshDeserialize)]
struct Stat {
    val: u8,
    max: u8,
}
impl Stat {
    pub fn new(max: u8) -> Self {
        Self { val: max, max }
    }
}

//------------------------------------------------------------------------------
// monster controller
//------------------------------------------------------------------------------

type PlayerId = u32;

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum MonsterController {
    CPU,
    Player(PlayerId),
}
impl MonsterController {
    pub fn cpu() -> Self {
        Self::CPU
    }
    pub fn player(id: PlayerId) -> Self {
        Self::Player(id)
    }
    pub fn is_cpu(&self) -> bool {
        matches!(self, Self::CPU)
    }
    pub fn is_player(&self) -> bool {
        matches!(self, Self::Player(_))
    }
}

//------------------------------------------------------------------------------
// monster
//------------------------------------------------------------------------------

type MonsterId = u32;

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
struct Monster {
    pub id: MonsterId,
    pub ctrl: MonsterController,
    pub turns_taken: u32,
    pub direction: Direction,
    pub position: Position,
    pub hp: Stat,
    pub energy: Stat,
    pub skills: Vec<SkillStatus>,
    pub behaviors: Vec<BehaviorStatus>,
}
impl Monster {
    pub fn new(ctrl: MonsterController) -> Self {
        Self {
            id: rand(),
            ctrl,
            turns_taken: 0,
            direction: Direction::Down,
            position: Position::new(0, 0),
            hp: Stat::new(6),
            energy: Stat::new(3),
            skills: vec![],
            behaviors: vec![],
        }
    }
    pub fn new_cpu() -> Self {
        let mut monster = Self::new(MonsterController::CPU);
        monster.add_skill(0);
        monster.add_skill(1);
        monster.add_skill(2);
        monster.add_skill(3);
        monster.add_skill(4);

        monster.behaviors = vec![
            BehaviorStatus {
                behavior_id: 0,
                last_turn_used: None,
                kind: BehaviorKind::DoMaxDamageToNearestPlayer,
            },
            BehaviorStatus {
                behavior_id: 1,
                last_turn_used: None,
                kind: BehaviorKind::HealIfLowHP {
                    skill_id: 2,
                    threshold: 0.5,
                },
            },
            BehaviorStatus {
                behavior_id: 2,
                last_turn_used: None,
                kind: BehaviorKind::MoveInRandomDirection,
            },
        ];

        monster
    }
    pub fn new_player(player_id: PlayerId) -> Self {
        let mut monster = Self::new(MonsterController::Player(player_id));
        monster.add_skill(0); // ensure monster has basic movement at index 0
        monster.add_skill(1);
        monster.add_skill(2);
        monster.add_skill(3);
        monster.add_skill(4);
        monster
    }
    pub fn is_alive(&self) -> bool {
        self.hp.val > 0
    }
    pub fn add_skill(&mut self, skill_id: SkillId) {
        self.skills.push(SkillStatus::from_id(skill_id))
    }
    pub fn has_skill(&self, skill_id: SkillId) -> bool {
        self.skills.iter().any(|s| s.skill_id == skill_id)
    }
    pub fn get_skill_data_by_index<'a>(
        &'a self,
        index: usize,
    ) -> Option<(&'a Skill, &'a SkillStatus)> {
        let status = self.skills.get(index)?;
        let skill = Skill::from_id(status.skill_id)?;
        Some((skill, status))
    }
    pub fn get_all_skill_data<'a>(&'a self) -> Vec<(&'a Skill, &'a SkillStatus)> {
        let mut skill_data = vec![];
        for i in 0..self.skills.len() {
            if let Some(data) = self.get_skill_data_by_index(i) {
                skill_data.push(data);
            }
        }
        skill_data
    }
    pub fn get_skill_data_by_id<'a>(
        &'a self,
        skill_id: SkillId,
    ) -> Option<(&'a Skill, &'a SkillStatus)> {
        let status = self.skills.iter().find(|s| s.skill_id == skill_id)?;
        let skill = Skill::from_id(status.skill_id)?;
        Some((skill, status))
    }
}

//------------------------------------------------------------------------------
// Behavior Status
//------------------------------------------------------------------------------

type BehaviorId = u32;

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
pub struct BehaviorStatus {
    pub behavior_id: BehaviorId,
    pub last_turn_used: Option<u32>,
    pub kind: BehaviorKind,
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
pub enum BehaviorKind {
    /// Move in a random direction if possible
    MoveInRandomDirection,

    /// Use a healing skill when current HP is below the threshold percentage
    HealIfLowHP {
        skill_id: SkillId,
        threshold: f32, // value between 0.0 and 1.0
    },

    /// Try to use the skill that deals the most damage to the nearest player
    DoMaxDamageToNearestPlayer,
}

//------------------------------------------------------------------------------
// Skill Status
//------------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
struct SkillStatus {
    pub skill_id: SkillId,
    pub cooldown: u8,
    // pub disabled: bool,
}
impl SkillStatus {
    pub fn from_id(skill_id: SkillId) -> Self {
        let mut status = Self {
            skill_id,
            cooldown: 0,
        };
        status.cooldown = status.get_skill().map_or(0, |s| s.cooldown);
        status
    }
    pub fn get_skill<'a>(&self) -> Option<&'a Skill> {
        Skill::from_id(self.skill_id)
    }
    pub fn is_ready(&self) -> bool {
        self.cooldown == 0
    }
}

//------------------------------------------------------------------------------
// impediment
//------------------------------------------------------------------------------

type ImpedimentId = u32;

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
struct Impediment {
    pub id: ImpedimentId,
    pub position: Position,
    pub kind: ImpedimentKind,
}
impl Impediment {
    pub fn new_wall() -> Self {
        Self {
            id: rand(),
            position: Position::new(0, 0),
            kind: ImpedimentKind::Wall,
        }
    }
    pub fn new_boulder() -> Self {
        Self {
            id: rand(),
            position: Position::new(0, 0),
            kind: ImpedimentKind::Boulder,
        }
    }
    pub fn with_position(self, position: Position) -> Self {
        Self { position, ..self }
    }
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum ImpedimentKind {
    Wall,
    Boulder,
    // Add more as needed
}

//------------------------------------------------------------------------------
// skill
//------------------------------------------------------------------------------

type SkillId = u32;

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
struct Skill {
    pub id: SkillId,
    pub name: String,
    pub energy_cost: u8,
    pub cooldown: u8,
    pub effects: Vec<SkillEffect>,
    pub targeting: TargetingRule,
    pub aoe: u8,
}
impl Skill {
    fn from_id<'a>(skill_id: SkillId) -> Option<&'a Self> {
        SKILLS.get(&skill_id)
    }
    fn requires_direction(&self) -> bool {
        matches!(self.targeting, TargetingRule::Line { .. })
    }
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum SkillEffect {
    Move { distance: u8 },
    // Knockback (for targeting others?)
    Damage { amount: u8 },
    Heal { amount: u8 },
    EnergyIncrease { amount: u8 },
    EnergyDecrease { amount: u8 },
    // etc
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum TargetingRule {
    User,
    LivingAllies,
    LivingEnemies,
    Line { range: u8 },
    Circle { radius: u8 },
}

static SKILLS: LazyLock<BTreeMap<SkillId, Skill>> = LazyLock::new(|| {
    let mut map = BTreeMap::new();
    let mut id = 0;

    // Basic move - 0
    let skill = Skill {
        id,
        name: "Move".into(),
        energy_cost: 0,
        cooldown: 1,
        targeting: TargetingRule::User,
        aoe: 0,
        effects: vec![SkillEffect::Move { distance: 1 }],
    };
    map.insert(skill.id, skill);

    // Tackle - 1
    id += 1;
    let skill = Skill {
        id,
        name: "Tackle".into(),
        energy_cost: 0,
        cooldown: 1,
        targeting: TargetingRule::Line { range: 1 },
        aoe: 0,
        effects: vec![SkillEffect::Damage { amount: 2 }],
    };
    map.insert(skill.id, skill);

    // Meditate - 2
    id += 1;
    let skill = Skill {
        id,
        name: "Meditate".into(),
        energy_cost: 0,
        cooldown: 10,
        targeting: TargetingRule::User,
        aoe: 1,
        effects: vec![
            SkillEffect::Heal { amount: 2 },
            SkillEffect::EnergyIncrease { amount: 1 },
        ],
    };
    map.insert(skill.id, skill);

    // Blast Wave - 3
    id += 1;
    let skill = Skill {
        id,
        name: "Blast Wave".into(),
        energy_cost: 3,
        cooldown: 5,
        targeting: TargetingRule::Line { range: 3 },
        aoe: 1,
        effects: vec![SkillEffect::Damage { amount: 3 }],
    };
    map.insert(skill.id, skill);

    // Aura Destroyer - 4
    id += 1;
    let skill = Skill {
        id,
        name: "Aura Destroyer".into(),
        energy_cost: 3,
        cooldown: 10,
        targeting: TargetingRule::Circle { radius: 4 },
        aoe: 0,
        effects: vec![
            SkillEffect::Damage { amount: 2 },
            SkillEffect::Heal { amount: 1 },
        ],
    };
    map.insert(skill.id, skill);

    // Overload - 5
    id += 1;
    let skill = Skill {
        id,
        name: "Overload".into(),
        energy_cost: 1,
        cooldown: 5,
        targeting: TargetingRule::LivingAllies,
        aoe: 0,
        effects: vec![SkillEffect::EnergyIncrease { amount: 2 }],
    };
    map.insert(skill.id, skill);

    map
});

//------------------------------------------------------------------------------
// Floor
//------------------------------------------------------------------------------

type FloorId = u32;

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
struct Floor {
    pub id: FloorId,
    pub position: Position,
    pub kind: FloorKind,
}
impl Floor {
    pub const LAVA_FLOOR_DAMAGE: u8 = 2;
    pub const SPIKE_TRAP_DAMAGE: u8 = 2;
    pub fn new_lava() -> Self {
        Self {
            id: rand(),
            position: Position::new(0, 0),
            kind: FloorKind::Lava,
        }
    }
    pub fn new_spike_trap() -> Self {
        Self {
            id: rand(),
            position: Position::new(0, 0),
            kind: FloorKind::SpikeTrap { armed: false },
        }
    }
    pub fn with_position(self, position: Position) -> Self {
        Self { position, ..self }
    }
}

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
enum FloorKind {
    Lava,
    Ice,
    Water,
    ConveyorBelt(Direction),
    SpikeTrap { armed: bool },
    // ...
}

//------------------------------------------------------------------------------
// direction
//------------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, BorshSerialize, BorshDeserialize)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

//------------------------------------------------------------------------------
// position
//------------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, BorshSerialize, BorshDeserialize)]
struct Position {
    x: u8,
    y: u8,
}
impl Position {
    pub fn new(x: u8, y: u8) -> Self {
        Self { x, y }
    }
    pub fn new_offset(base: Self, dx: i8, dy: i8) -> Self {
        Self {
            x: base.x.saturating_add_signed(dx),
            y: base.y.saturating_add_signed(dy),
        }
    }
    pub fn with_offset(self, dx: i8, dy: i8) -> Self {
        Self {
            x: self.x.saturating_add_signed(dx),
            y: self.y.saturating_add_signed(dy),
        }
    }
    pub fn xy(&self) -> (u8, u8) {
        (self.x, self.y)
    }
    pub fn grid_xy(&self) -> (f32, f32) {
        (self.x as f32 * 16., self.y as f32 * 16.)
    }
    pub fn up_with_distance(self, distance: u8) -> Self {
        Self {
            x: self.x,
            y: self.y.saturating_sub(distance),
        }
    }
    pub fn down_with_distance(self, distance: u8) -> Self {
        Self {
            x: self.x,
            y: self.y.saturating_add(distance),
        }
    }
    pub fn left_with_distance(self, distance: u8) -> Self {
        Self {
            x: self.x.saturating_sub(distance),
            y: self.y,
        }
    }
    pub fn right_with_distance(self, distance: u8) -> Self {
        Self {
            x: self.x.saturating_add(distance),
            y: self.y,
        }
    }
    pub fn dir_with_distance(self, direction: Direction, distance: u8) -> Self {
        match direction {
            Direction::Up => self.up_with_distance(distance),
            Direction::Down => self.down_with_distance(distance),
            Direction::Left => self.left_with_distance(distance),
            Direction::Right => self.right_with_distance(distance),
        }
    }
    pub fn up(self) -> Self {
        self.up_with_distance(1)
    }
    pub fn down(self) -> Self {
        self.down_with_distance(1)
    }
    pub fn left(self) -> Self {
        self.left_with_distance(1)
    }
    pub fn right(self) -> Self {
        self.right_with_distance(1)
    }
    pub fn dir(self, direction: Direction) -> Self {
        match direction {
            Direction::Up => self.up(),
            Direction::Down => self.down(),
            Direction::Left => self.left(),
            Direction::Right => self.right(),
        }
    }
    pub fn direction_of(&self, to: Self) -> Option<Direction> {
        let dx = to.x as i16 - self.x as i16;
        let dy = to.y as i16 - self.y as i16;
        match (dx, dy) {
            (1, 0) => Some(Direction::Right),
            (-1, 0) => Some(Direction::Left),
            (0, 1) => Some(Direction::Down),
            (0, -1) => Some(Direction::Up),
            _ => None, // Diagonal or no movement
        }
    }
}

//------------------------------------------------------------------------------
// utils
//------------------------------------------------------------------------------

fn manhattan_area(center: Position, radius: u8) -> Vec<Position> {
    let r = radius as i8;
    let mut area = vec![];
    for dx in -r..=r {
        for dy in -r..=r {
            if dx.abs() + dy.abs() <= r {
                area.push(center.with_offset(dx, dy));
            }
        }
    }
    area
}

fn shuffle<T>(items: &mut [T]) {
    let len = items.len();
    for i in (1..len).rev() {
        let j = (rand() as usize) % (i + 1);
        items.swap(i, j);
    }
}

fn select_random<'a, T>(items: &'a [T]) -> Option<&'a T> {
    if items.is_empty() {
        None
    } else {
        let index = (rand() as usize) % items.len();
        items.get(index)
    }
}
