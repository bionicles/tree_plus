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
