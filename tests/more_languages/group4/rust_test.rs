fn at_beginning<'a>(&'a str) {
    todo!()
}

// rust_test.rs
#[derive(Default)]
pub enum Days<E: EdgeCase> {
    #[default]
    Sun,
    Mon,
    #[error("edge case {idx}, expected at least {} and at most {}", .limits.lo, .limits.hi)]
    Tue,
    Wed,
    Thu(i16, bool),
    Fri { day: u8 },
    Sat {
        urday: String,
        edge_case: E,
    },
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

const fn multiply_by_two(num: f64) -> f64 {
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

fn with_generic<D: Drawable>(d: D) {
}
fn with_generic<D>(d: D)
where 
    D: Drawable
{
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
    pub struct AlsoVisibleStruct<T>(T, T);
}

macro_rules! say_hello {
    () => {
        println!("Hello, world!");
    };
}


#[macro_export]
macro_rules! hello_tree_plus {
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
    source: S1, // edge
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

trait Container {
    fn items(&self) -> impl Iterator<Item = Widget>;
}

trait HttpService {
    async fn fetch(&self, url: Url) -> HtmlBody;
//  ^^^^^^^^ desugars to:
//  fn fetch(&self, url: Url) -> impl Future<Output = HtmlBody>;
}

// Define a generic struct with two generic types
struct Pair<T, U> {
    first: T,
    second: U,
}

// Define a generic trait with one generic type
trait Transformer<T> {
    fn transform(&self, input: T) -> T;
}

// Implement the Transformer trait for the Pair struct
// This implementation will transform a Pair of the same types (T, T)
impl<T: std::ops::Add<Output = T> + Copy> Transformer<T> for Pair<T, T> {
    fn transform(&self, input: T) -> T {
        self.first + self.second + input
    }
}

// Main function for demonstration
fn main() {
    let pair = Pair { first: 10, second: 20 };
    println!("Result: {}", pair.transform(5));
}

async fn handle_get(State(pool): State<PgPool>) -> Result<Html<String>, (StatusCode, String)> 
where
    Bion: Cool
{
    println!("Bion is cool!");
}

// reproduces a catastrophic backtracking error
#[macro_export]
macro_rules! unit {
        impl crate::lib::Lensable<(), $unit_dtype> for $unit_name {
            fn insert(
                &mut self,
                key: (),
                value: $unit_dtype,
            ) -> Result<Option<$unit_dtype>, ETLError> {
                if key == () {
                    let old_value = self.0 as $unit_dtype;
                    self.0 = value as $unit_dtype;
                    Ok(Some(old_value))
                } else {
                    Err(ETLError::KeyError)
                }
            }
            // other methods omitted as they did not trigger the issue
        }
        // other impls omitted, as they did not trigger the issue
    }
};

/// example of extractors with destructuring
///
/// # Errors
/// - if rendering a route fails
pub async fn handle_get_axum_route(
    Session { maybe_claims }: Session,
    Path(RouteParams {
        alpha,
        bravo,
        charlie,
        edge_case
    }): Path<RouteParams>,
) -> ServerResult<Response> {
    todo!()
}

/// example with square bracket arguments
fn encode_pipeline(cmds: &[Cmd], atomic: bool) -> Vec<u8> {
    let mut rv = vec![];
    write_pipeline(&mut rv, cmds, atomic);
    rv
}

pub async fn handle_post_yeet(
    State(auth_backend): State<AuthBackend>,
    Session { maybe_claims }: Session,
    Form(yeet_form): Form<YeetForm>,
) -> Result<Response, AuthError> {
    println!("yeet yeet!")
}

/// GET /handle_get_thingy -> `/handle_get_thingy/{THINGY_NAME}`
///
/// # Errors
/// - if tree_plus regex doesn't match on the .. in destructuring
#[instrument(skip_all)]
pub async fn handle_get_thingy(
    session: Session,
    State(ApiBackend {
        page_cache,
        auth_backend,
        library_sql,
        some_data_cache,
        metadata_cache,
        thingy_client,
        ..
    }): State<ApiBackend>,
) -> ServerResult<Response> {
    todo!("test thy shit");
}