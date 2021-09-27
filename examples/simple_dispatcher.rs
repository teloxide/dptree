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

use dptree::{Filter, TerminalCont};

use std::{
    io::Write,
    ops::ControlFlow,
    sync::{
        atomic::{AtomicI32, Ordering},
        Arc,
    },
};

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

fn ping_handler() -> Filter<'static, Event, String, TerminalCont> {
    dptree::filter(|&event| async move { matches!(event, Event::Ping) })
        .endpoint(|_| async { "Pong".to_string() })
}

// fn set_value_handler(store: Arc<AtomicI32>) -> Handler<'static, Event,
// String> {     dptree::parser(|&event| async move {
//         match event {
//             Event::SetValue(value) => Some(value),
//             _ => None,
//         }
//     })
//     .endpoint(move |value: i32| {
//         let store = store.clone();

//         async move {
//             store.store(value, Ordering::SeqCst);
//             format!("{} stored", value)
//         }
//     })
// }

fn print_value_handler(store: Arc<AtomicI32>) -> Filter<'static, Event, String, TerminalCont> {
    dptree::filter(|&event| async move { matches!(event, Event::PrintValue) }).endpoint(move |_| {
        let store = store.clone();

        async move {
            let value = store.load(Ordering::SeqCst);
            // Return value.
            format!("{}", value)
        }
    })
}

#[tokio::main]
async fn main() {
    // Create program store.
    let store = Arc::new(AtomicI32::new(0));

    // When we write all of our constructors - there are a question: how can we
    // combine them? For that purpose we use `Dispatcher` handler. It does a
    // simple job: passed input event to all handlers that it have and wait
    // until the event is processed. If no one endpoint process the event,
    // `Dispatcher` will return an error.
    let dispatcher = dptree::entry::<_, _, Filter<_, _, TerminalCont>>()
        .chain(ping_handler())
        //        .pipe_to(&set_value_handler(store.clone()))
        .chain(print_value_handler(store.clone()));

    // Simple REPL for the constructed dispatcher.
    loop {
        print!(">> ");
        std::io::stdout().flush().unwrap();

        let mut cmd = String::new();
        std::io::stdin().read_line(&mut cmd).unwrap();

        let strs = cmd.trim().split(" ").collect::<Vec<_>>();
        let event = Event::parse(strs.as_slice());

        let out = match event {
            Some(event) => match dispatcher.clone().dispatch(event).await {
                ControlFlow::Continue(event) => panic!("Unhandled event {:?}", event),
                ControlFlow::Break(result) => result,
            },
            _ => "Unknown command".to_string(),
        };
        println!("{}", out);
    }
}
