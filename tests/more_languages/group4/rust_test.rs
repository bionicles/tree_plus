// rust_test.rs
enum Days {
    Sun,
    Mon,
    Tue,
    Wed,
    Thu,
    Fri,
    Sat,
}

struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn get_origin() -> Point {
        Point { x: 0, y: 0 }
    }
}

struct Person {
    name: String,
}

impl Person {
    fn greet(&self) {
        println!("Hello, {}", self.name);
    }
}

fn add_two_longs(x1: i64, x2: i64) -> i64 {
    x1 + x2
}

fn add_two_longs_longer(
    x1: i64,
    x2: i64,
) -> i64 {
    x1 + x2
}

fn multiply_by_two(num: f64) -> f64 {
    num * 2.0
}

fn get_first_character(s: &str) -> Option<char> {
    s.chars().next()
}

trait Drawable {
    fn draw(&self);
}

impl Drawable for Point {
    fn draw(&self) {
        println!("Drawing point at ({}, {})", self.x, self.y);
    }
}

fn main() {
    let person = Person { name: "World".into() };
    person.greet();
    let point = Point { x: 1, y: 2 };
    point.draw();
}

pub struct VisibleStruct {
    pub field: i32,
}

mod my_module {
    // Module contents...
}

macro_rules! say_hello {
    () => {
        println!("Hello, world!");
    };
}

pub mod lib {
    pub mod interfaces;
    mod engine;
}

// Define a flow function
pub fn flow<S1, S2, S3, S4, E, T, L>(
    source: S1, 
    extractor: E, 
    inbox: S2, 
    transformer: T, 
    outbox: S3, 
    loader: L, 
    sink: &mut S4,
) -> Result<(), Box<dyn Error>>
where
    S1: Extractable,
    S2: Extractable + Loadable,
    S3: Extractable + Loadable,
    S4: Loadable,
    E: Extractor<S1, S2>,
    T: Transformer<S2, S3>,
    L: Loader<S3, S4>,
{
    let logger = setup_logger("system.rs message");
    log_message(&logger, Level::Info, "Source: {:#?}", source);
    let inbox = extractor(source)?;
    log_message(&logger, Level::Info, "Inbox {:#?}", inbox);
    let outbox = transformer(inbox)?;
    log_message(&logger, Level::Info, "Outbox {:#?}", outbox);
    loader(outbox, sink);
    log_message(&logger, Level::Info, "Sink {:#?}", sink);
}