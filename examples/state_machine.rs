//! You can see image of the state machine at /img/state_machine.gif

use std::{
    fmt::{Display, Formatter},
    io::Write,
    ops::ControlFlow,
};

use futures::future;

use dptree::{Filter, TerminalCont};

#[derive(Debug)]
pub enum CommandState {
    Active,
    Paused,
    Inactive,
    Exit,
}

impl Display for CommandState {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            CommandState::Active => f.write_str("Active"),
            CommandState::Paused => f.write_str("Paused"),
            CommandState::Inactive => f.write_str("Inactive"),
            CommandState::Exit => f.write_str("Exit"),
        }
    }
}

#[derive(Debug)]
pub enum Event {
    Begin,
    Pause,
    Resume,
    End,
    Exit,
}

impl Event {
    fn parse(input: &str) -> Option<Self> {
        match input {
            "begin" => Some(Event::Begin),
            "pause" => Some(Event::Pause),
            "resume" => Some(Event::Resume),
            "end" => Some(Event::End),
            "exit" => Some(Event::Exit),
            _ => None,
        }
    }
}

type Transition = Filter<'static, TransitionIn, TransitionOut, TerminalCont>;
type TransitionIn = (Event, CommandState);
type TransitionOut = CommandState;

mod transitions {
    use super::*;

    pub fn begin() -> Transition {
        dptree::filter(|(event, _)| future::ready(matches!(event, Event::Begin)))
            .endpoint(|_| async { CommandState::Active })
    }

    pub fn pause() -> Transition {
        dptree::filter(|(event, _)| future::ready(matches!(event, Event::Pause)))
            .endpoint(|_| async { CommandState::Paused })
    }

    pub fn end() -> Transition {
        dptree::filter(|(event, _)| future::ready(matches!(event, Event::End)))
            .endpoint(|_| async { CommandState::Inactive })
    }

    pub fn resume() -> Transition {
        dptree::filter(|(event, _)| future::ready(matches!(event, Event::Resume)))
            .endpoint(|_| async { CommandState::Active })
    }

    pub fn exit() -> Transition {
        dptree::filter(|(event, _)| future::ready(matches!(event, Event::Exit)))
            .endpoint(|_| async { CommandState::Exit })
    }
}

type Handler = Filter<
    'static,
    TransitionIn,
    TransitionOut,
    dptree::Handler<'static, TransitionIn, TransitionOut>,
>;

fn active_handler() -> Handler {
    dptree::filter::<_, _, _, _, TerminalCont>(|(_, state)| {
        future::ready(matches!(state, CommandState::Active))
    })
    .branch(transitions::pause())
    .branch(transitions::end())
}

fn paused_handler() -> Handler {
    dptree::filter::<_, _, _, _, TerminalCont>(|(_, state)| {
        future::ready(matches!(state, CommandState::Paused))
    })
    .branch(transitions::resume())
    .branch(transitions::end())
}

fn inactive_handler() -> Handler {
    dptree::filter::<_, _, _, _, TerminalCont>(|(_, state)| {
        future::ready(matches!(state, CommandState::Inactive))
    })
    .branch(transitions::begin())
    .branch(transitions::exit())
}

fn exit_handler() -> Handler {
    dptree::filter::<_, _, _, _, TerminalCont>(|(_, state)| {
        future::ready(matches!(state, CommandState::Exit))
    })
    .branch(transitions::exit())
}

#[tokio::main]
async fn main() {
    let mut state = CommandState::Inactive;

    let dispatcher = dptree::entry::<_, _, TerminalCont>()
        .branch(active_handler())
        .branch(paused_handler())
        .branch(inactive_handler())
        .branch(exit_handler());

    loop {
        println!("|| Current state is {}", state);
        print!(">> ");
        std::io::stdout().flush().unwrap();

        let mut cmd = String::new();
        std::io::stdin().read_line(&mut cmd).unwrap();

        let str = cmd.trim();
        let event = Event::parse(str);

        let new_state = match event {
            Some(event) => match dispatcher.clone().dispatch((event, state)).await {
                ControlFlow::Break(state) => state,
                ControlFlow::Continue((_, the_state)) => {
                    println!("There is no transition for the event");
                    state = the_state;
                    continue;
                }
            },
            _ => {
                println!("Unknown event");
                continue;
            }
        };
        state = new_state;
    }
}
