use dptree::{deps, di::DependencyMap, prelude::*};
use regex::Regex;

type Request = Arc<&'static str>;

#[tokio::main]
async fn main() {
    let smile_handler = dptree::filter(|req: Request| async move { req.starts_with("/smile") })
        .endpoint(|| async { "🙃".to_owned() });

    let add_handler = dptree::filter_map(|req: Request| async move {
        let re = Regex::new(r"/add (?P<a>\d) (?P<b>\d)").unwrap();
        match re.captures(&req) {
            Some(caps) => Some((caps["a"].parse::<i32>().ok()?, caps["b"].parse::<i32>().ok()?)),
            _ => None,
        }
    })
    .endpoint(|numbers: Arc<(i32, i32)>| async move {
        let (a, b) = *numbers;
        format!("{} + {} is {}", a, b, a + b)
    });

    let not_found_handler = dptree::endpoint(|| async { "404 Not Found".to_owned() });

    let web_server =
        dptree::entry().branch(smile_handler).branch(add_handler).branch(not_found_handler);

    assert_eq!(web_server.dispatch(deps! { "/smile" }).await, ControlFlow::Break("🙃".to_owned()));
    assert_eq!(
        web_server.dispatch(deps! { "/add 3 5" }).await,
        ControlFlow::Break("3 + 5 is 8".to_owned())
    );
    assert_eq!(
        web_server.dispatch(deps! { "/lol" }).await,
        ControlFlow::Break("404 Not Found".to_owned())
    );
}
