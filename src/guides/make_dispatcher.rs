//! Let's create a dispatcher with an explanation of each step.
//!
//! # Do I need a dispatcher?
//!
//! First, let's figure out what a dispatcher is and when it's needed. A
//! dispatcher is a structure that allows you to distribute events to different
//! handlers under certain conditions. This is a common programming task. For
//! example, a dispatcher is required to handle a request by REST api. The
//! conditions here are the endpoints of the requests. A similar way used in
//! `actix-web`, `warp` and other web frameworks.
//!
//! So, if you want a way to distribute events by handlers, you need a
//! dispatcher.
//!
//! # Let's create a dispatcher
//!
//! We will create a dispatcher for a simple CRUD-similar weather-storing
//! service.
//!
//! The first thing we need is to split our program into `events` and
//! `handlers`. The `events` in our case will be requests to set or get weather,
//! while handlers will be functions that handles these events.
//!
//! Let's create our `event` type:
//! ```
//! type City = String;
//! type Kelvins = f32;
//! type Pascals = f32;
//!
//! #[derive(Debug, PartialEq, Clone)]
//! struct Weather {
//!     city: City,
//!     temperature: Kelvins,
//!     pressure: Pascals,
//! }
//!
//! #[derive(Debug, PartialEq, Clone)]
//! enum Event {
//!     SetWeather(Weather),
//!     GetWeather(City),
//!     DeleteWeather(City),
//! }
//! ```
//!
//! `Event::SetWeather` is respond for changing a weather in the city, or insert
//! city if it does not exists.
//!
//! `Event::GetWeather` is respond for getting a weather for the city.
//!
//! `Event::DeleteWeather` is respond for deleting a weather.
//!
//! We need to storage of our weather. In our example we will [`sqlx`] crate for
//! `Sqlite` database.
//!
//! Let's write weather storage. We will not dwell on this for a long time,
//! since it is beyond the scope of this guide.
//!
//! ```
//! # type City = String; type Kelvins = f32; type Pascals = f32;
//!
//! use sqlx::sqlite::SqlitePool;
//!
//! #[derive(Debug, PartialEq, Clone, sqlx::FromRow)]
//! struct Weather {
//!     city: City,
//!     temperature: Kelvins,
//!     pressure: Pascals,
//! }
//!
//! struct WeatherStorage {
//!     connection: SqlitePool,
//! }
//! impl WeatherStorage {
//!     async fn init_db(&self) {
//!         sqlx::query(
//!             "CREATE TABLE weather ( city CHAR(80) PRIMARY KEY, temperature FLOAT, pressure \
//!                  FLOAT );",
//!         )
//!         .execute(&self.connection)
//!         .await
//!         .unwrap();
//!     }
//!     async fn get(&self, city: City) -> Result<Weather, sqlx::Error> {
//!         sqlx::query_as("SELECT city, temperature, pressure FROM weather WHERE city = ?")
//!             .bind(city)
//!             .fetch_one(&self.connection)
//!             .await
//!     }
//!     async fn set(&self, weather: &Weather) -> Result<(), sqlx::Error> {
//!         sqlx::query(
//!             "INSERT OR REPLACE INTO weather (city, temperature, pressure) VALUES (?, ?, ?)",
//!         )
//!         .bind(&weather.city)
//!         .bind(weather.temperature)
//!         .bind(weather.pressure)
//!         .execute(&self.connection)
//!         .await?;
//!         Ok(())
//!     }
//!     async fn delete(&self, city: City) -> Result<(), sqlx::Error> {
//!         sqlx::query("DELETE FROM weather WHERE city = ?")
//!             .bind(city)
//!             .execute(&self.connection)
//!             .await?;
//!         Ok(())
//!     }
//! }
//! ```
//!
//! Now we can start to define our handlers. We want to define one handler per
//! event, to satisfy [SRP (single-responsibility principle)].
//!
//! First, let's create endpoints. Endpoint is a function that handles one type
//! of incoming events. We will create three endpoints, one per possible event:
//! ```
//! use dptree::prelude::*;
//! # type City = String; type Kelvins = f32; type Pascals = f32;
//! # #[derive(Debug, PartialEq, Clone, sqlx::FromRow)] struct Weather { city: City, temperature: Kelvins, pressure: Pascals, }
//! # struct WeatherStorage { }
//! # impl WeatherStorage {
//! #  async fn get(&self, city: City) -> Result<Weather, sqlx::Error> { unimplemented!() }
//! #  async fn set(&self, weather: &Weather) -> Result<(), sqlx::Error> { unimplemented!() }
//! #  async fn delete(&self, city: City) -> Result<(), sqlx::Error> { unimplemented!() }
//! # }
//!
//! // In endpoint arguments we describe what we need to handle the event.
//! // In this case we need know weather to set, and storage where weather is store.
//! // All endpoint arguments must be wrapped around `Arc`.
//! async fn set_weather(weather: Arc<Weather>, storage: Arc<WeatherStorage>) -> Result<Response, Error> {
//!     // Validate input parameters.
//!     validate_temperature(weather.temperature)?;
//!     validate_pressure(weather.pressure)?;
//!     // If parameters are valid, set the weather.
//!     storage.set(&weather).await?;
//!     Ok(Response::Ok)
//! }
//!
//! // In this case we need know requested city.
//! async fn get_weather(city: Arc<City>, storage: Arc<WeatherStorage>) -> Result<Response, Error> {
//!     let city = (*city).clone();
//!     let weather = storage.get(city.clone()).await.map_err(|err| {
//!         match err {
//!             sqlx::Error::RowNotFound => Error::CityDoesNotExists(city),
//!             err => err.into()
//!         }
//!     })?;
//!     Ok(Response::Weather(weather))
//! }
//!
//! // In this case we need know requested city.
//! async fn delete_weather(city: Arc<City>, storage: Arc<WeatherStorage>) -> Result<Response, Error> {
//!     let city = (*city).clone();
//!     storage.delete(city).await?;
//!     Ok(Response::Ok)
//! }
//!
//! fn validate_temperature(temperature: f32) -> Result<(), Error> {
//!     if temperature < 0.0 { Err(Error::WrongTemperature(temperature)) }
//!     else { Ok(()) }
//! }
//!
//! fn validate_pressure(pressure: f32) -> Result<(), Error> {
//!     if pressure < 0.0 { Err(Error::WrongPressure(pressure)) }
//!     else { Ok(()) }
//! }
//!
//! #[derive(Debug, PartialEq)]
//! enum Response {
//!     Ok,
//!     Weather(Weather),
//! }
//!
//! #[derive(Debug, thiserror::Error)]
//! enum Error {
//!     #[error("Temperature is not in kelvins: {0}")]
//!     WrongTemperature(f32),
//!
//!     #[error("Pressure is not in pascals: {0}")]
//!     WrongPressure(f32),
//!
//!     #[error("City {0} does not exists.")]
//!     CityDoesNotExists(City),
//!
//!     #[error(transparent)]
//!     Sqlite(#[from] sqlx::Error)
//! }
//! ```
//!
//! When we implement all endpoints we can create a dispatcher to dispatch
//! `Event` into endpoints:
//!
//! ```
//! use dptree::prelude::*;
//! use enum_as_inner::EnumAsInner;
//! # type City = String; type Kelvins = f32; type Pascals = f32;
//! # struct Error; struct Response; #[derive(Debug, PartialEq, Clone)]struct Weather; struct WeatherStorage;
//! # async fn set_weather(weather: Arc<Weather>, storage: Arc<WeatherStorage>) -> Result<Response, Error> { unimplemented!() }
//! # async fn get_weather(city: Arc<City>, storage: Arc<WeatherStorage>) -> Result<Response, Error> { unimplemented!() }
//! # async fn delete_weather(city: Arc<City>, storage: Arc<WeatherStorage>) -> Result<Response, Error> { unimplemented!() }
//! // We use enum_as_inner::EnumAsInner to define methods converting Event into
//! // inner values. For more information see https://crates.io/crates/enum-as-inner.
//! #[derive(Debug, PartialEq, Clone, EnumAsInner)]
//! enum Event {
//!     SetWeather(Weather),
//!     GetWeather(City),
//!     DeleteWeather(City),
//! }
//!
//! // Entry returns empty handler, that will pass input event into next branches.
//! let dispatcher = dptree::entry()
//!     // `branch` method create a branch in the tree of dispatching, which is
//!     // tree-of-responsibility pattern.
//!     .branch(
//!         // `parser` method create a middleware that parses one type into another.
//!         // Here we filter input only `Event::SetWeather` events and return its `Weather` value.
//!         dptree::parser(|event: &Event| event.clone().into_set_weather().ok())
//!             // If parsing successful, endpoint will be called.
//!             .endpoint(set_weather)
//!     )
//!     // Same as above, but with `Event::GetWeather`.
//!     .branch(
//!         dptree::parser(|event: &Event| event.clone().into_get_weather().ok())
//!             .endpoint(get_weather)
//!     )
//!     // Same as above, but with `Event::DeleteWeather`.
//!     .branch(
//!         dptree::parser(|event: &Event| event.clone().into_delete_weather().ok())
//!             .endpoint(delete_weather)
//!     );
//!
//! # dispatcher.dispatch(dptree::container::TypeMapDi::new()); // to infer input type
//! ```
//!
//! Now we can try to dispatch some event. In `dptree` dispatching happens with
//! containers. Container is an abstraction that holds value of the specified
//! type. For more information see [dptree::container] module. We will use
//! [`TypeMapDi`] container that implement DI pattern:
//!
//! ```ignore
//! use dptree::container::TypeMapDi;
//! use sqlx::sqlite::SqlitePool;
//!
//! # struct WeatherStorage { connection: SqlitePool }
//!
//! let mut container = TypeMapDi::new();
//! let connection = SqlitePool::connect("sqlite::memory:");
//! container.insert(WeatherStorage { connection });
//!
//! container.insert(/* event */);
//! dispatcher.dispatch(container).await;
//! ```
//!
//! Full code with dispatcher calling:
//!
//! ```
//! use sqlx::SqlitePool;
//! use dptree::prelude::*;
//! use dptree::container::TypeMapDi;
//!
//! type City = String;
//! type Kelvins = f32;
//! type Pascals = f32;
//!
//! #[derive(Debug, PartialEq, Clone, sqlx::FromRow)]
//! struct Weather {
//!     city: City,
//!     temperature: Kelvins,
//!     pressure: Pascals,
//! }
//!
//! #[derive(Debug, PartialEq, Clone, enum_as_inner::EnumAsInner)]
//! enum Event {
//!     SetWeather(Weather),
//!     GetWeather(City),
//!     DeleteWeather(City),
//! }
//!
//! //-------------------------------WeatherStorage---------------------
//!
//! #[derive(Clone)]
//! struct WeatherStorage {
//!     connection: SqlitePool,
//! }
//!
//! impl WeatherStorage {
//!     async fn init_db(&self) {
//!         sqlx::query("CREATE TABLE weather ( city CHAR(80) PRIMARY KEY, temperature FLOAT, pressure FLOAT );")
//!             .execute(&self.connection)
//!             .await
//!             .unwrap();
//!     }
//!     async fn get(&self, city: City) -> Result<Weather, sqlx::Error> {
//!         sqlx::query_as(
//!             "SELECT city, temperature, pressure FROM weather WHERE city = ?",
//!         )
//!             .bind(city)
//!             .fetch_one(&self.connection)
//!             .await
//!     }
//!     async fn set(&self, weather: &Weather) -> Result<(), sqlx::Error> {
//!         sqlx::query(
//!             "INSERT OR REPLACE INTO weather (city, temperature, pressure) VALUES (?, ?, ?)",
//!         )
//!             .bind(&weather.city)
//!             .bind(weather.temperature)
//!             .bind(weather.pressure)
//!             .execute(&self.connection)
//!             .await?;
//!         Ok(())
//!     }
//!     async fn delete(&self, city: City) -> Result<(), sqlx::Error> {
//!         sqlx::query(
//!             "DELETE FROM weather WHERE city = ?",
//!         )
//!             .bind(city)
//!             .execute(&self.connection)
//!             .await?;
//!         Ok(())
//!     }
//! }
//!
//! // ---------------------Endpoints------------------------------
//!
//! // In endpoint arguments we describe what we need to handle the event.
//! // In this case we need know weather to set, and storage where weather is store.
//! // All endpoint arguments must be wrapped around `Arc`.
//! async fn set_weather(weather: Arc<Weather>, storage: Arc<WeatherStorage>) -> Result<Response, Error> {
//!     // Validate input parameters.
//!     validate_temperature(weather.temperature)?;
//!     validate_pressure(weather.pressure)?;
//!     // If parameters are valid, set the weather.
//!     storage.set(&weather).await?;
//!     Ok(Response::Ok)
//! }
//!
//! // In this case we need to know requested city.
//! async fn get_weather(city: Arc<City>, storage: Arc<WeatherStorage>) -> Result<Response, Error> {
//!     let city = (*city).clone();
//!     let weather = storage.get(city.clone()).await.map_err(|err| {
//!         match err {
//!             sqlx::Error::RowNotFound => Error::CityDoesNotExists(city),
//!             err => err.into()
//!         }
//!     })?;
//!     Ok(Response::Weather(weather))
//! }
//!
//! // In this case we need know requested city.
//! async fn delete_weather(city: Arc<City>, storage: Arc<WeatherStorage>) -> Result<Response, Error> {
//!     let city = (*city).clone();
//!     storage.delete(city).await?;
//!     Ok(Response::Ok)
//! }
//!
//! fn validate_temperature(temperature: f32) -> Result<(), Error> {
//!     if temperature < 0.0 { Err(Error::WrongTemperature(temperature)) }
//!     else { Ok(()) }
//! }
//!
//! fn validate_pressure(pressure: f32) -> Result<(), Error> {
//!     if pressure < 0.0 { Err(Error::WrongPressure(pressure)) }
//!     else { Ok(()) }
//! }
//!
//! #[derive(Debug, PartialEq)]
//! enum Response {
//!     Ok,
//!     Weather(Weather),
//! }
//!
//! #[derive(Debug, thiserror::Error)]
//! enum Error {
//!     #[error("Temperature is not in kelvins: {0}")]
//!     WrongTemperature(f32),
//!
//!     #[error("Pressure is not in pascals: {0}")]
//!     WrongPressure(f32),
//!
//!     #[error("City {0} does not exists.")]
//!     CityDoesNotExists(City),
//!
//!     #[error(transparent)]
//!     Sqlite(#[from] sqlx::Error)
//! }
//!
//! fn create_dispatcher() -> Handler<'static, TypeMapDi, Result<Response, Error>> {
//! // Entry returns empty handler, that will pass input event into next branches.
//! dptree::entry()
//!     // `branch` method create a branch in the tree of dispatching, which is
//!     // tree-of-responsibility pattern.
//!     .branch(
//!         // `parser` method create a middleware that parses one type into another.
//!         // Here we filter input only `Event::SetWeather` events and return its `Weather` value.
//!         dptree::parser(|event: &Event| event.clone().into_set_weather().ok())
//!             // If parsing successful, endpoint will be called.
//!             .endpoint(set_weather)
//!     )
//!     // Same as above, but with `Event::GetWeather`.
//!     .branch(
//!         dptree::parser(|event: &Event| event.clone().into_get_weather().ok())
//!             .endpoint(get_weather)
//!     )
//!     // Same as above, but with `Event::DeleteWeather`.
//!     .branch(
//!         dptree::parser(|event: &Event| event.clone().into_delete_weather().ok())
//!             .endpoint(delete_weather)
//!     )
//! }
//!
//! // -------------------------------Dispatcher---------------------------
//! # #[tokio::main] async fn main() {
//!
//! let dispatcher = create_dispatcher();
//!
//! // ------------------------Examples of run---------------
//! use dptree::container::TypeMapDi;
//! use std::ops::ControlFlow;
//!
//! let connection = SqlitePool::connect("sqlite::memory:").await.unwrap();
//! let store = WeatherStorage { connection };
//! store.init_db().await;
//!
//! let create_cont = |event: Event| {
//!     let mut container = TypeMapDi::new();
//!     container.insert(store.clone());
//!     container.insert(event);
//!     container
//! };
//!
//! let london_weather = Weather { city: "London".into(), temperature: 280.0, pressure: 1000.0 };
//! let set_weather_cont = || create_cont(Event::SetWeather(london_weather.clone()));
//! let get_weather_cont = || create_cont(Event::GetWeather("London".into()));
//! let get_weather_wrong_city_cont = || create_cont(Event::GetWeather("Berlin".into()));
//! let delete_weather_cont = || create_cont(Event::DeleteWeather("London".into()));
//!
//! assert_eq!(unwrap(dispatcher.dispatch(set_weather_cont()).await), Ok(Response::Ok));
//! assert_eq!(unwrap(dispatcher.dispatch(get_weather_cont()).await), Ok(Response::Weather(london_weather)));
//! assert_eq!(unwrap(dispatcher.dispatch(get_weather_wrong_city_cont()).await),
//! Err(Error::CityDoesNotExists("Berlin".into())));
//! assert_eq!(unwrap(dispatcher.dispatch(delete_weather_cont()).await), Ok(Response::Ok));
//! assert_eq!(unwrap(dispatcher.dispatch(get_weather_cont()).await), Err(Error::CityDoesNotExists("London".into())));
//!
//! // ControlFlow::unwrap is unstable
//! fn unwrap(cf: ControlFlow<Result<Response, Error>, TypeMapDi>) -> Result<Response, Error> {
//!     match cf {
//!         ControlFlow::Break(b) => b, _ => unreachable!(),
//!     }
//! }
//!
//! # // sqlx::Error does not implement PartialEq
//! # impl PartialEq for Error { fn eq(&self, other: &Self) -> bool {
//! #     match (self, other) {
//! #         (Error::CityDoesNotExists(city1), Error::CityDoesNotExists(city2)) => city1 == city2,
//! #         _ => false,
//! #     }
//! # }}
//!
//! # }
//! ```
//!
//! [`rusqlite`]: https://docs.rs/rusqlite/0.26.1/rusqlite/
//! [SRP (single-responsibility principle)]: https://en.wikipedia.org/wiki/Single-responsibility_principle
//! [dptree::container]: dptree::container
//! [`TypeMapDi`]: dptree::container::TypeMapDi

fn _empty() {}
