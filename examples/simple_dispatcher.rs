use dispatch_tree::handler::{Node, Filter, Parser, Leaf, EventOwned};
use std::sync::Arc;
use dispatch_tree::Handler;
use dispatch_tree::parser::{Parseable, RecombineFrom};
use std::sync::atomic::{AtomicI32, Ordering};
use std::io::Write;

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
            _ => Err(self)
        }
    }
}

impl RecombineFrom<SetValueEvent> for Event {
    type Rest = ();

    fn recombine(data: (SetValueEvent, Self::Rest)) -> Self {
        Event::SetValue(data.0)
    }
}

fn init_ping_handler() -> impl Handler<Event, Res = String> {
    Filter::new(
        |event: &Event| matches!(event, Event::Ping),
        Leaf::from(|_: EventOwned<Event>| async { "Pong".to_string() })
    )
}

fn init_set_value_handler(store: Arc<AtomicI32>) -> impl Handler<Event, Res = String> {
    Parser::new(Leaf::from(move |event: EventOwned<SetValueEvent>| {
        let store = store.clone();
        async move {
            let value = (event.0).0;
            store.store(value, Ordering::SeqCst);
            format!("{} stored", value)
        }
    }))
}

fn init_print_value_handler(store: Arc<AtomicI32>) -> impl Handler<Event, Res = String> {
    Filter::new(
        |event: &Event| matches!(event, Event::PrintValue),
        Leaf::from(move |_: EventOwned<Event>| {
            let store = store.clone();
            async move {
                let value = store.load(Ordering::SeqCst);
                format!("{}", value)
            }
        })
    )
}

#[tokio::main]
async fn main() {
    let store = Arc::new(AtomicI32::new(0));

    let dispatcher = Node::<Event, String>::new(Arc::new(
        vec![
            Box::new(init_ping_handler()),
            Box::new(init_set_value_handler(store.clone())),
            Box::new(init_print_value_handler(store.clone())),
        ]
    ));

    loop {
        print!(">> ");
        std::io::stdout().flush().unwrap();

        let mut cmd = String::new();
        std::io::stdin().read_line(&mut cmd).unwrap();

        let strs = cmd.trim().split(" ").collect::<Vec<_>>();
        let event = Event::parse(strs.as_slice());

        let out = match event {
            Some(event) => dispatcher.handle(event).await.unwrap(),
            _ =>  "Unknown command".to_string()
        };
        println!("{}", out);
    }
}
