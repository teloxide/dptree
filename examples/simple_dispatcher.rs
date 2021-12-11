// In this example, we crate a simple dispatcher with 3 possible event:
// 1. `ping`. This command simply returns "pong" answer.
// 2. `print_value`. This command prints the value stored in the program.
// 3. `set_value`. This command set the value that is stored in the program.
//
// Usage:
// ```
// >> ping
// Pong
// >> print_value
// 0
// >> set_value 123
// 123 stored
// >> print_value
// 123
// ```

use std::{
    io::Write,
    ops::ControlFlow,
    sync::{
        atomic::{AtomicI32, Ordering},
        Arc,
    },
};

use dptree::{di::DependencyMap, prelude::*};

#[tokio::main]
async fn main() {
    let store = Arc::new(AtomicI32::new(0));

    let dispatcher = dptree::entry()
        .branch(ping_handler())
        .branch(set_value_handler())
        .branch(print_value_handler());

    repl(dispatcher, store).await
}

async fn repl(dispatcher: Handler<'static, Store, String>, store: Arc<AtomicI32>) -> ! {
    loop {
        print!(">> ");
        std::io::stdout().flush().unwrap();

        let mut cmd = String::new();
        std::io::stdin().read_line(&mut cmd).unwrap();

        let strs = cmd.trim().split(' ').collect::<Vec<_>>();
        let event = Event::parse(strs.as_slice());

        let out = match event {
            Some(event) => {
                let mut container = DependencyMap::new();
                container.insert(event);
                container.insert(store.clone());

                match dispatcher.dispatch(container).await {
                    ControlFlow::Continue(event) => panic!("Unhandled event {:?}", event),
                    ControlFlow::Break(result) => result,
                }
            }
            _ => "Unknown command".to_string(),
        };

        println!("{}", out);
    }
}

#[derive(Copy, Clone, Debug)]
enum Event {
    Ping,
    SetValue(i32),
    PrintValue,
}

impl Event {
    fn parse(input: &[&str]) -> Option<Self> {
        match input {
            ["ping"] => Some(Event::Ping),
            ["set_value", value] => Some(Event::SetValue(value.parse().ok()?)),
            ["print"] => Some(Event::PrintValue),
            _ => None,
        }
    }
}

type Store = DependencyMap;
type CommandHandler = Endpoint<'static, Store, String>;

fn ping_handler() -> CommandHandler {
    dptree::filter(|event: Arc<Event>| async move { matches!(*event, Event::Ping) })
        .endpoint(|| async { "Pong".to_string() })
}

fn set_value_handler() -> CommandHandler {
    dptree::map(|event: Arc<Event>| async move {
        match *event {
            Event::SetValue(value) => Some(value),
            _ => None,
        }
    })
    .endpoint(move |value: Arc<i32>, store: Arc<AtomicI32>| async move {
        store.store(*value, Ordering::SeqCst);
        format!("{} stored", value)
    })
}

fn print_value_handler() -> CommandHandler {
    dptree::filter(|event: Arc<Event>| async move { matches!(event.as_ref(), Event::PrintValue) })
        .endpoint(move |store: Arc<AtomicI32>| {
            async move {
                let value = store.load(Ordering::SeqCst);
                // Return value.
                format!("{}", value)
            }
        })
}
