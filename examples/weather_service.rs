use dptree::{container::TypeMapDi, di_fn::IntoDiFn, prelude::*};
use sqlx::SqlitePool;
use std::{io::Write, ops::ControlFlow};

type City = String;
type Kelvins = f32;
type Pascals = f32;

#[derive(Debug, PartialEq, Clone, sqlx::FromRow)]
struct Weather {
    city: City,
    temperature: Kelvins,
    pressure: Pascals,
}

#[derive(Debug, PartialEq, Clone, enum_as_inner::EnumAsInner)]
enum Event {
    SetWeather(Weather),
    GetWeather(City),
    DeleteWeather(City),
}

struct WeatherStorage {
    connection: Arc<SqlitePool>,
}

impl WeatherStorage {
    async fn init_db(&self) {
        sqlx::query(
            "CREATE TABLE weather ( city CHAR(80) PRIMARY KEY, temperature FLOAT, pressure FLOAT \
             );",
        )
        .execute(&*self.connection)
        .await
        .unwrap();
    }
    async fn set(&self, weather: &Weather) -> Result<(), sqlx::Error> {
        sqlx::query(
            "INSERT OR REPLACE INTO weather (city, temperature, pressure) VALUES (?, ?, ?)",
        )
        .bind(&weather.city)
        .bind(weather.temperature)
        .bind(weather.pressure)
        .execute(&*self.connection)
        .await?;
        Ok(())
    }
}
async fn set_weather(weather: Arc<Weather>, storage: Arc<WeatherStorage>) -> Result<(), Error> {
    storage.set(&weather).await?;
    Ok(())
}

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("Temperature is not in kelvins: {0}")]
    WrongTemperature(f32),

    #[error("Pressure is not in pascals: {0}")]
    WrongPressure(f32),

    #[error("City {0} does not exists.")]
    CityDoesNotExists(City),

    #[error(transparent)]
    Sqlite(#[from] sqlx::Error),
}

fn create_dispatcher() -> Handler<'static, TypeMapDi, Result<(), Error>> {
    //fn assert<F: IntoDiFn<TypeMapDi, Result<(), Error>, (Fut, Weather,
    // WeatherStorage)>, Fut>(f: F) {} assert(set_weather);
    // Entry returns empty handler, that will pass input event into next branches.
    dptree::entry()
        // `branch` method create a branch in the tree of dispatching, which is
        // tree-of-responsibility pattern.
        .branch(
            // `parser` method create a middleware that parses one type into another.
            // Here we filter input only `Event::SetWeather` events and return its `Weather` value.
            dptree::parser(|event: &Event| event.clone().into_set_weather().ok())
                // If parsing successful, endpoint will be called.
                .endpoint(set_weather),
        )
}

#[tokio::main]
async fn main() {}
