// rust_test.rs
enum Color {
    Red,
    Blue,
    Green,
}

struct Point {
    x: i32,
    y: i32,
}

trait Drawable {
    fn draw(&self);
}

impl Drawable for Point {
    fn draw(&self) {
        println!("Drawing point at ({}, {})", self.x, self.y);
    }
}

// TODO: This todo tests parse_todo
fn main() {
    let point = Point { x: 1, y: 2 };
    point.draw();
}
