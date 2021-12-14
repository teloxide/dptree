use dptree::{deps, di::DependencyMap, prelude::*};

type Request = Arc<&'static str>;
type Response = String;
type WebHandler = Endpoint<'static, DependencyMap, Response>;

#[tokio::main]
async fn main() {
    let web_server =
        dptree::entry().branch(smiles_handler()).branch(sqrt_handler()).branch(not_found_handler());

    let smile = web_server.dispatch(deps! { "/smile" }).await;
    let sqrt_16 = web_server.dispatch(deps! { "/sqrt 16" }).await;
    let not_found = web_server.dispatch(deps! { "/lol" }).await;

    assert_eq!(smile, ControlFlow::Break("🙃".to_owned()));
    assert_eq!(sqrt_16, ControlFlow::Break("4".to_owned()));
    assert_eq!(not_found, ControlFlow::Break("404 Not Found".to_owned()));
}

fn smiles_handler() -> WebHandler {
    dptree::filter(|req: Request| async move { req.starts_with("/smile") })
        .endpoint(|| async { "🙃".to_owned() })
}

fn sqrt_handler() -> WebHandler {
    dptree::filter_map(|req: Request| async move {
        if req.starts_with("/sqrt") {
            let (_, n) = req.split_once(" ")?;
            n.parse::<f64>().ok()
        } else {
            None
        }
    })
    .endpoint(|n: Arc<f64>| async move { format!("{}", n.sqrt()) })
}

fn not_found_handler() -> WebHandler {
    dptree::endpoint(|| async { "404 Not Found".to_owned() })
}
