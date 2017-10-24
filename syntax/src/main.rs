#![feature(try_from)]

#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate derivative;

use std::convert::TryFrom;
use std::fmt::{self, Debug};

pub trait EnvironmentTrait: Clone + PartialEq + Eq + fmt::Debug {}
pub trait TagsTrait: Default + Clone + PartialEq + Eq + fmt::Debug {}

#[derive(Default, Clone, PartialEq, Eq, Debug)]
struct NoTags;

impl TagsTrait for NoTags {}

bitflags! {
    #[derive(Default)]
    struct Tag: usize {
        const FOO = 1 << 0;
        const BAR = 1 << 1;
    }
}

impl TagsTrait for Tag {}

impl<'a> TryFrom<&'a str> for Tag {
    type Error = String;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        match &value.to_lowercase()[..] {
            "foo" => Ok(Tag::FOO),
            "bar" => Ok(Tag::BAR),
            tag => Err(format!("Unrecognized tag {:?}", tag))
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound="T: Debug, U: Debug"))]
pub struct Suite<T, U> {
    name: String,
    environment: T,
    tags: Option<U>,
    context: Context<T, U>,
}

#[derive(Derivative)]
#[derivative(Debug(bound="T: Debug, U: Debug"))]
pub struct Context<T, U> {
    name: Option<String>,
    tags: Option<U>,
    blocks: Vec<Block<T, U>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound="T: Debug, U: Debug"))]
pub struct Example<T, U> {
    name: String,
    tags: Option<U>,
    #[derivative(Debug="ignore")]
    function: Box<Fn(&T) -> bool>,
}

#[derive(Derivative)]
#[derivative(Debug(bound="T: Debug, U: Debug"))]
pub enum Block<T, U> {
    Context(Context<T, U>),
    Example(Example<T, U>),
}

#[derive(Derivative)]
#[derivative(Debug(bound="T: Debug, U: Debug"))]
enum AssemblyItem<T, U> {
    Suite(Suite<T, U>),
    Context(Context<T, U>),
}

type Assembly<T, U> = std::thread::LocalKey<RefCell<Vec<AssemblyItem<T, U>>>>;

use std::cell::RefCell;

fn suite_internal<S, T, U, F>(
    name: S,
    environment: T,
    tags: U,
    assembly: &'static Assembly<T, U>,
    f: F
) -> Suite<T, U>
where
    S: Into<String>,
    T: EnvironmentTrait,
    U: TagsTrait,
    F: 'static + Fn() -> ()
{
    // Check for validity of assembly
    assembly.with(|cell| {
        let assembly = cell.borrow();
        if !assembly.is_empty() {
            panic!("Expected empty assembly!");
        }
    });

    // Push suite on assembly:
    assembly.with(|cell| {
        let mut assembly = cell.borrow_mut();
        let suite = Suite {
            name: name.into(),
            environment: environment,
            tags: Some(tags),
            context: Context {
                name: None,
                tags: None,
                blocks: vec![],
            },
        };
        assembly.push(AssemblyItem::Suite(suite));
    });

    // Call provided closure:
    f();

    // Pop suite from assembly:
    let suite = assembly.with(|cell| {
        let mut assembly = cell.borrow_mut();
        let assembly_item = assembly.pop();
        if let Some(AssemblyItem::Suite(suite)) = assembly_item {
            suite
        } else {
            panic!("Expected suite on assembly!");
        }
    });

    println!("{:?}", suite);

    // return assembled suite:
    suite
}

fn context_internal<S, T, U, F>(
    name: S,
    tags: U,
    assembly: &'static Assembly<T, U>,
    f: F
)
where
    S: Into<String>,
    T: EnvironmentTrait,
    U: TagsTrait,
    F: 'static + Fn() -> ()
{
    // Check for validity of assembly
    assembly.with(|cell| {
        let assembly = cell.borrow();
        match assembly.last() {
            Some(&AssemblyItem::Suite(_)) => (),
            Some(&AssemblyItem::Context(_)) => (),
            _ => {
                panic!("Expected suite or context on assembly!");
            }
        }
    });

    // Push suite on assembly:
    assembly.with(|cell| {
        let mut assembly = cell.borrow_mut();
        let context = Context {
            tags: Some(tags),
            name: Some(name.into()),
            blocks: vec![],
        };
        assembly.push(AssemblyItem::Context(context));
    });

    // Call provided closure:
    f();

    // Pop context from assembly:
    let context = assembly.with(|cell| {
        let mut assembly = cell.borrow_mut();
        let assembly_item = assembly.pop();
        if let Some(AssemblyItem::Context(context)) = assembly_item {
            context
        } else {
            panic!("Expected context on assembly!");
        }
    });

    // Pop parent from assembly:
    let mut parent = assembly.with(|cell| {
        let mut assembly = cell.borrow_mut();
        assembly.pop().unwrap()
    });

    // Push parent back on assembly:
    match parent {
        AssemblyItem::Suite(ref mut parent) => {
            parent.context = context;
        },
        AssemblyItem::Context(ref mut parent) => {
            parent.blocks.push(Block::Context(context));
        },
    }

    // Push suite on assembly:
    assembly.with(|cell| {
        let mut assembly = cell.borrow_mut();
        assembly.push(parent);
    });
}

fn example_internal<S, T, U, F>(
    name: S,
    tags: U,
    assembly: &'static Assembly<T, U>,
    f: F
)
where
    S: Into<String>,
    T: EnvironmentTrait,
    U: TagsTrait,
    F: 'static + Fn(&T) -> bool
{
    // Check for validity of assembly
    assembly.with(|cell| {
        let assembly = cell.borrow();
        match assembly.last() {
            Some(&AssemblyItem::Context(_)) => (),
            _ => {
                panic!("Expected context on assembly!");
            }
        }
    });

    // Add example to parent context:
    assembly.with(|cell| {
        let mut assembly = cell.borrow_mut();
        let assembly_item = assembly.last_mut().unwrap();
        if let &mut AssemblyItem::Context(ref mut context) = assembly_item {
            let example = Example {
                name: name.into(),
                tags: Some(tags),
                function: Box::new(f)
            };
            context.blocks.push(Block::Example(example));
        } else {
            panic!("Expected context on assembly!");
        }
    });
}

#[macro_export]
macro_rules! scenario {
    ($name:expr, env: $env:ident, || $closure:block) => ({
        scenario!($name, tags: NoTags, env: $env => $closure);
    });
    ($name:expr, tags: $tags:ident, env: $env:ident, || $closure:block) => ({
        #![allow(dead_code)]

        thread_local! {
            static ASSEMBLY: RefCell<Vec<AssemblyItem<$env, $tags>>> = RefCell::new(vec![]);
        }

        fn suite_default<S, F>(name: S, env: $env, f: F)
        where
            S: Into<String>,
            F: 'static + Fn() -> ()
        {
            suite(name, env, $tags::default(), f);
        }

        fn suite<S, F>(name: S, env: $env, tags: $tags, f: F)
        where
            S: Into<String>,
            F: 'static + Fn() -> ()
        {
            suite_internal(name, env, tags, &ASSEMBLY, f);
        }

        fn context_default<S, F>(name: S, f: F)
        where
            S: Into<String>,
            F: 'static + Fn() -> ()
        {
            context(name, $tags::default(), f);
        }

        fn context<S, F>(name: S, tags: $tags, f: F)
        where
            S: Into<String>,
            F: 'static + Fn() -> ()
        {
            context_internal(name, tags, &ASSEMBLY, f);
        }

        fn example_default<S, F>(name: S, f: F)
        where
            S: Into<String>,
            F: 'static + Fn(&$env) -> bool
        {
            example(name, $tags::default(), f);
        }

        fn example<S, F>(name: S, tags: $tags, f: F)
        where
            S: Into<String>,
            F: 'static + Fn(&$env) -> bool
        {
            example_internal(name, tags, &ASSEMBLY, f);
        }

        $closure;
    })
}

#[macro_export]
macro_rules! suite {
    ($name:expr, env: $env:expr, || $closure:block) => {
        suite_default($name, $env, || $closure);
    };
    ($name:expr, tags: $tags:expr, env: $env:expr, || $closure:block) => {
        suite($name, $env, $tags, || $closure);
    };
}

#[macro_export]
macro_rules! context {
    ($name:expr, || $closure:block) => {
        context_default($name, || $closure);
    };
    ($name:expr, tags: $tags:expr, || $closure:block) => {
        context($name, $tags, || $closure);
    };
}

#[macro_export]
macro_rules! example {
    ($name:expr, |$env:ident| $closure:block) => {
        example_default($name, |$env| $closure);
    };
    ($name:expr, tags: $tags:expr, |$env:ident| $closure:block) => {
        example($name, $tags, |$env| $closure);
    };
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Environment(usize);

impl EnvironmentTrait for Environment {}

fn main() {
    scenario!("test", tags: Tag, env: Environment, || {
        suite!("suite", tags: Tag::FOO, env: Environment(42), || {
            context!("context", || {
                example!("example", |env| {
                    env.0 == 42
                });
                example!("example", |env| {
                    env.0 == 42
                });
                context!("context", || {
                    example!("example", |env| {
                        env.0 == 42
                    });
                    example!("example", |env| {
                        env.0 == 42
                    });
                });
            });
        });
    });
}
