//! You can see image of the state machine at /img/state_machine.gif

use dispatch_tree::handler::{Filter, Leaf, Node};
use dispatch_tree::Handler;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::sync::Arc;

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

mod transitions {
    use super::*;

    pub fn begin() -> impl Handler<(Event, CommandState), Res = CommandState> {
        Filter::new(
            |(event, _): &(Event, CommandState)| matches!(event, Event::Begin),
            Leaf::from(|| async { CommandState::Active }),
        )
    }

    pub fn pause() -> impl Handler<(Event, CommandState), Res = CommandState> {
        Filter::new(
            |(event, _): &(Event, CommandState)| matches!(event, Event::Pause),
            Leaf::from(|| async { CommandState::Paused }),
        )
    }

    pub fn end() -> impl Handler<(Event, CommandState), Res = CommandState> {
        Filter::new(
            |(event, _): &(Event, CommandState)| matches!(event, Event::End),
            Leaf::from(|| async { CommandState::Inactive }),
        )
    }

    pub fn resume() -> impl Handler<(Event, CommandState), Res = CommandState> {
        Filter::new(
            |(event, _): &(Event, CommandState)| matches!(event, Event::Resume),
            Leaf::from(|| async { CommandState::Active }),
        )
    }

    pub fn exit() -> impl Handler<(Event, CommandState), Res = CommandState> {
        Filter::new(
            |(event, _): &(Event, CommandState)| matches!(event, Event::Exit),
            Leaf::from(|| async { CommandState::Exit }),
        )
    }
}

fn init_active_handler() -> impl Handler<(Event, CommandState), Res = CommandState> {
    Filter::new(
        |(_, state): &(Event, CommandState)| matches!(state, CommandState::Active),
        Node::new(Arc::new(vec![
            Box::new(transitions::pause()),
            Box::new(transitions::end()),
        ])),
    )
}

fn init_paused_handler() -> impl Handler<(Event, CommandState), Res = CommandState> {
    Filter::new(
        |(_, state): &(Event, CommandState)| matches!(state, CommandState::Paused),
        Node::new(Arc::new(vec![
            Box::new(transitions::resume()),
            Box::new(transitions::end()),
        ])),
    )
}

fn init_inactive_handler() -> impl Handler<(Event, CommandState), Res = CommandState> {
    Filter::new(
        |(_, state): &(Event, CommandState)| matches!(state, CommandState::Inactive),
        Node::new(Arc::new(vec![
            Box::new(transitions::begin()),
            Box::new(transitions::exit()),
        ])),
    )
}

fn init_exit_handler() -> impl Handler<(Event, CommandState), Res = CommandState> {
    Filter::new(
        |(_, state): &(Event, CommandState)| matches!(state, CommandState::Exit),
        Node::new(Arc::new(vec![])),
    )
}

#[tokio::main]
async fn main() {
    let mut state = CommandState::Inactive;

    let dispatcher = Node::<(Event, CommandState), CommandState>::new(Arc::new(vec![
        Box::new(init_active_handler()),
        Box::new(init_paused_handler()),
        Box::new(init_inactive_handler()),
        Box::new(init_exit_handler()),
    ]));

    loop {
        println!("|| Current state is {}", state);
        print!(">> ");
        std::io::stdout().flush().unwrap();

        let mut cmd = String::new();
        std::io::stdin().read_line(&mut cmd).unwrap();

        let str = cmd.trim();
        let event = Event::parse(str);

        let new_state = match event {
            Some(event) => match dispatcher.handle((event, state)).await {
                Ok(state) => state,
                Err((_, the_state)) => {
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
