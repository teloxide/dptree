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

use dptree::Handler;
use std::io::Write;
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::Arc;

// First, we declare the type for incoming events.
#[derive(Debug)]
enum Event {
    Ping,
    SetValue(i32),
    PrintValue,
}

impl Event {
    // User will input text from the console - so we declare method to parse user input to our type.
    fn parse(input: &[&str]) -> Option<Self> {
        match input {
            ["ping"] => Some(Event::Ping),
            ["set_value", value] => Some(Event::SetValue(value.parse().ok()?)),
            ["print"] => Some(Event::PrintValue),
            _ => None,
        }
    }
    // This function will be need later. Here we check that event is `set_value` and if so,
    // we return the value that the user wants to set the program's value to.
    fn parse_to_set_value_event(&self) -> Option<i32> {
        match self {
            Event::SetValue(value) => Some(*value),
            _ => None,
        }
    }
}

// Second, we declare handlers constructors.
// This function will construct a handler that handle `ping` event.
#[rustfmt::skip]
fn ping_handler() -> impl Handler<Event, Res = String> {
    // Let's take a closer look.
    // We create here 2 handlers which are chained. This is called the `Chain Responsibility Pattern`.
    // First handler is `dptree::filter` that constructs `Filter` handler. It allows
    // filtering input event by some condition `Fn(&Event) -> bool`. In that case we want
    // pass only `ping` events, so we use `dptree::matches!` macro that is lazy variant of
    // `std::matches!` macro.
    dptree::filter(dptree::matches!(Event::Ping))
        // After a filter, we give only events that satisfies the condition. In our case it is
        // only `ping` events. We must handle that event - so we use `EndPoint` handler that allow
        // to handle all incoming events. In the handler we just returns `"Pong"` string, because we
        // know that earlier `Filter` accepts only `ping` event.
        .end_point(|| async { "Pong".to_string() })
}

// This function will construct a handler that handle `set_value` event.
#[rustfmt::skip]
fn set_value_handler(store: Arc<AtomicI32>) -> impl Handler<Event, Res = String> {
    // In this case in the endpoint we _must_ know to which value user want set program value. So
    // in this case we cannot use `Filter` as above because it does not provide information of the
    // internal representation of the event. So we use another handler - `Parser`. `Parser` allow
    // us to parse one event type to another. In our case we want to handle only `set_value` events,
    // so we use our `Event::parse_to_set_value_event` to do this.
    dptree::parser(Event::parse_to_set_value_event)
        // Next, handle the `set_value` event.
        .end_point(
            move |value: i32| {
                // Clone store to use in `async` block.
                let store = store.clone();
                async move {
                    // Store user input to store.
                    store.store(value, Ordering::SeqCst);
                    // Return info that value are stored.
                    format!("{} stored", value)
                }
            },
        )
}

// This function will construct a handler that handle `print_value` event.
#[rustfmt::skip]
fn print_value_handler(store: Arc<AtomicI32>) -> impl Handler<Event, Res = String> {
    // Filter only `Event::PrintValue` events.
    dptree::filter(dptree::matches!(Event::PrintValue))
        .end_point(move || {
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

    // When we write all of our constructors - there are a question: how can we combine them?
    // For that purpose we use `Dispatcher` handler. It does a simple job: passed input event to all
    // handlers that it have and wait until the event is processed. If no one endpoint process
    // the event, `Dispatcher` will return an error.
    let dispatcher = dptree::dispatch::<Event, String>()
        // Add all our handlers.
        .to(ping_handler())
        .to(set_value_handler(store.clone()))
        .to(print_value_handler(store.clone()))
        .build();

    // Simple REPL for the constructed dispatcher.
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
