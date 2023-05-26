use std::panic::Location;

use dptree::{Handler, HandlerDescription};

struct CollectLocations {
    locations: Vec<(&'static str, &'static Location<'static>)>,
}

#[track_caller]
fn collector(name: &'static str) -> CollectLocations {
    CollectLocations { locations: vec![(name, Location::caller())] }
}

impl HandlerDescription for CollectLocations {
    #[track_caller]
    fn entry() -> Self {
        collector("entry")
    }

    #[track_caller]
    fn user_defined() -> Self {
        collector("user_defined")
    }

    #[track_caller]
    fn merge_chain(&self, other: &Self) -> Self {
        let locations = self
            .locations
            .iter()
            .chain(&other.locations)
            .copied()
            .chain([("chain", Location::caller())])
            .collect();

        Self { locations }
    }

    #[track_caller]
    fn merge_branch(&self, other: &Self) -> Self {
        let locations = self
            .locations
            .iter()
            .chain(&other.locations)
            .copied()
            .chain([("branch", Location::caller())])
            .collect();

        Self { locations }
    }

    #[track_caller]
    fn map() -> Self {
        collector("map")
    }

    #[track_caller]
    fn map_async() -> Self {
        collector("map_async")
    }

    #[track_caller]
    fn filter() -> Self {
        collector("filter")
    }

    #[track_caller]
    fn filter_async() -> Self {
        collector("filter_async")
    }

    #[track_caller]
    fn filter_map() -> Self {
        collector("filter_map")
    }

    #[track_caller]
    fn filter_map_async() -> Self {
        collector("filter_map_async")
    }

    #[track_caller]
    fn endpoint() -> Self {
        collector("endpoint")
    }
}

fn get_locations<'a, 'b>(
    handler: &'a Handler<'b, impl Send + Sync + 'b, CollectLocations>,
) -> &'a [(&'static str, &'static Location<'static>)] {
    &handler.description().locations
}

fn main() {
    #[rustfmt::skip]
    let some_tree: Handler< _, _> = dptree::entry()
        .branch(
            dptree::filter(|| true)
                .endpoint(|| async {})
        )
        .branch(
            dptree::filter_async(|| async { true })
                .chain(dptree::filter_map(|| Some(1) ))
                .endpoint(|| async {})
        );

    get_locations(&some_tree).iter().for_each(|(name, loc)| println!("{name: <12} @ {loc}"));
}
