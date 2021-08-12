//! You can see image of the state machine at /img/state_machine.gif

extern crate dispatch_tree as dptree;

use dispatch_tree::handler::Node;
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
        dptree::filter(|(event, _)| matches!(event, Event::Begin))
            .and_then_leaf(|| async { CommandState::Active })
    }

    pub fn pause() -> impl Handler<(Event, CommandState), Res = CommandState> {
        dptree::filter(|(event, _)| matches!(event, Event::Pause))
            .and_then_leaf(|| async { CommandState::Paused })
    }

    pub fn end() -> impl Handler<(Event, CommandState), Res = CommandState> {
        dptree::filter(|(event, _)| matches!(event, Event::End))
            .and_then_leaf(|| async { CommandState::Inactive })
    }

    pub fn resume() -> impl Handler<(Event, CommandState), Res = CommandState> {
        dptree::filter(|(event, _)| matches!(event, Event::Resume))
            .and_then_leaf(|| async { CommandState::Active })
    }

    pub fn exit() -> impl Handler<(Event, CommandState), Res = CommandState> {
        dptree::filter(|(event, _)| matches!(event, Event::Exit))
            .and_then_leaf(|| async { CommandState::Exit })
    }
}

#[rustfmt::skip]
fn init_active_handler() -> impl Handler<(Event, CommandState), Res = CommandState> {
    dptree::filter(|(_, state)| matches!(state, CommandState::Active))
        .and_then(Node::new(
            Arc::new(vec![
                Box::new(transitions::pause()),
                Box::new(transitions::end()),
            ]),
        ))
}

#[rustfmt::skip]
fn init_paused_handler() -> impl Handler<(Event, CommandState), Res = CommandState> {
    dptree::filter(|(_, state)| matches!(state, CommandState::Paused))
        .and_then(Node::new(
            Arc::new(vec![
                Box::new(transitions::resume()),
                Box::new(transitions::end()),
            ]),
        ))
}

#[rustfmt::skip]
fn init_inactive_handler() -> impl Handler<(Event, CommandState), Res = CommandState> {
    dptree::filter(|(_, state)| matches!(state, CommandState::Inactive))
        .and_then(Node::new(
            Arc::new(vec![
                Box::new(transitions::begin()),
                Box::new(transitions::exit()),
            ]),
        ))
}

fn init_exit_handler() -> impl Handler<(Event, CommandState), Res = CommandState> {
    dptree::filter(|(_, state)| matches!(state, CommandState::Exit))
        .and_then(Node::new(Arc::new(vec![])))
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
