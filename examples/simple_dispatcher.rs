extern crate dispatch_tree as dptree;

use dispatch_tree::parser::Parseable;
use dispatch_tree::Handler;
use std::io::Write;
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::Arc;

#[derive(Debug)]
struct SetValueEvent(i32);

#[derive(Debug)]
enum Event {
    Ping,
    SetValue(SetValueEvent),
    PrintValue,
}

impl Event {
    fn parse(input: &[&str]) -> Option<Self> {
        match input {
            ["ping"] => Some(Event::Ping),
            ["set_value", value] => Some(Event::SetValue(SetValueEvent(value.parse().unwrap()))),
            ["print"] => Some(Event::PrintValue),
            _ => None,
        }
    }
}

impl Parseable<SetValueEvent> for Event {
    type Rest = ();

    fn parse(self) -> Result<(SetValueEvent, Self::Rest), Self> {
        match self {
            Event::SetValue(e) => Ok((e, ())),
            _ => Err(self),
        }
    }

    fn recombine(data: (SetValueEvent, Self::Rest)) -> Self {
        Event::SetValue(data.0)
    }
}

fn ping_handler() -> impl Handler<Event, Res = String> {
    dptree::filter(dptree::matches!(Event::Ping)).leaf(|| async { "Pong".to_string() })
}

#[rustfmt::skip]
fn set_value_handler(store: Arc<AtomicI32>) -> impl Handler<Event, Res = String> {
    dptree::parser::<Event, SetValueEvent>()
        .leaf(
            move |event: SetValueEvent| {
                let store = store.clone();
                async move {
                    let value = event.0;
                    store.store(value, Ordering::SeqCst);
                    format!("{} stored", value)
                }
            },
        )
}

#[rustfmt::skip]
fn print_value_handler(store: Arc<AtomicI32>) -> impl Handler<Event, Res = String> {
    dptree::filter(dptree::matches!(Event::PrintValue))
        .leaf(move || {
            let store = store.clone();
            async move {
                let value = store.load(Ordering::SeqCst);
                format!("{}", value)
            }
        })
}

#[tokio::main]
async fn main() {
    let store = Arc::new(AtomicI32::new(0));

    let dispatcher = dptree::node::<Event, String>()
        .and(ping_handler())
        .and(set_value_handler(store.clone()))
        .and(print_value_handler(store.clone()))
        .build();

    loop {
        print!(">> ");
        std::io::stdout().flush().unwrap();

        let mut cmd = String::new();
        std::io::stdin().read_line(&mut cmd).unwrap();

        let strs = cmd.trim().split(" ").collect::<Vec<_>>();
        let event = Event::parse(strs.as_slice());

        let out = match event {
            Some(event) => dispatcher.handle(event).await.unwrap(),
            _ => "Unknown command".to_string(),
        };
        println!("{}", out);
    }
}
