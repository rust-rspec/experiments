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
        const FOO  = 1 << 0;
        const BAR  = 1 << 1;
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

#[doc(hidden)]
#[macro_export]
macro_rules! scenario_impl {
    ($stack:ident (given $($tail:tt)*)) => {
        scenario_impl! { $stack (suite $($tail)*) }
    };
    ($stack:ident (describe $($tail:tt)*)) => {
        scenario_impl! { $stack (suite $($tail)*) }
    };
    ($stack:ident (suite $name:expr, env: $($tail:tt)*)) => {
        scenario_impl! { $stack (suite $name, tags: Tag::default(), env: $($tail)*) }
    };
    ($stack:ident (suite $name:expr, tags: $tags:expr, env: $env:expr => { $($head:tt)* } $($tail:tt)*)) => {
        println!("suite {:?}, tags: {:?}, env: {:?} {{ ... }}", $name, $tags, $env);

        // let node = $crate::virtual_dom::VTag::new(stringify!($starttag));
        // $stack.push(node);
        suite_impl! { $stack ($($head)*) }

        scenario_impl! { $stack ($($tail)*) }

        $stack.push(42);
    };
    ($stack:ident ()) => {

    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! suite_impl {
    ($stack:ident (then $($tail:tt)*)) => {
        context_impl! { $stack (example $($tail)*) }
    };
    ($stack:ident (it $($tail:tt)*)) => {
        context_impl! { $stack (example $($tail)*) }
    };
    ($stack:ident (example $($tail:tt)*)) => {
        context_impl! { $stack (example $($tail)*) }
    };
    ($stack:ident (when $($tail:tt)*)) => {
        suite_impl! { $stack (context $($tail)*) }
    };
    ($stack:ident (specify $($tail:tt)*)) => {
        suite_impl! { $stack (context $($tail)*) }
    };
    ($stack:ident (context $name:expr => { $($head:tt)* } $($tail:tt)*)) => {
        suite_impl! { $stack (context $name, tags: Tag::default() => { $($head)* } $($tail)*) }
    };
    ($stack:ident (context $name:expr, tags: $tags:expr => { $($head:tt)* } $($tail:tt)*)) => {
        println!("context {:?}, tags: {:?} {{ ... }}", $name, $tags);

        context_impl! { $stack ($($head)*) }

        // let node = $crate::virtual_dom::VTag::new(stringify!($starttag));
        // $stack.push(node);
        suite_impl! { $stack ($($tail)*) }

        $stack.push(42);
    };
    ($stack:ident ()) => {

    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! context_impl {
    ($stack:ident (when $($tail:tt)*)) => {
        suite_impl! { $stack (context $($tail)*) }
    };
    ($stack:ident (specify $($tail:tt)*)) => {
        suite_impl! { $stack (context $($tail)*) }
    };
    ($stack:ident (context $($tail:tt)*)) => {
        suite_impl! { $stack (context $($tail)*) }
    };
    ($stack:ident (then $($tail:tt)*)) => {
        context_impl! { $stack (example $($tail)*) }
    };
    ($stack:ident (it $($tail:tt)*)) => {
        context_impl! { $stack (example $($tail)*) }
    };
    // ($stack:ident (example $name:expr => |$arg:ident| { $($head:tt)* } $($tail:tt)*)) => {
    ($stack:ident (example $name:expr => |$arg:ident| $head:block $($tail:tt)*)) => {
        // context_impl! { $stack (example $name, tags: Tag::default() => |$arg| { $($head)* } $($tail)*) }
        context_impl! { $stack (example $name, tags: Tag::default() => |$arg| $head $($tail)*) }
    };
    // ($stack:ident (example $name:expr, tags: $tags:expr => |$arg:ident| { $($head:tt)* } $($tail:tt)*)) => {
    ($stack:ident (example $name:expr, tags: $tags:expr => |$arg:ident| $head:block $($tail:tt)*)) => {
        println!("example {:?}, tags: {:?} {{ ... }}", $name, $tags);

        // example_impl! { $stack (example |$arg| { $($head)* }) }
        example_impl! { $stack (example |$arg| $head) }

        // let node = $crate::virtual_dom::VTag::new(stringify!($starttag));
        // $stack.push(node);
        context_impl! { $stack ($($tail)*) }

        $stack.push(42);
    };
    ($stack:ident ()) => {

    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! example_impl {
    // ($stack:ident (example |$head:ident| $($tail:tt)*)) => {
    ($stack:ident (example |$head:ident| $tail:block)) => {
        // |$head| { $($tail)* }
        // let _ = |$head| $tail;
    };
}

#[macro_export]
macro_rules! scenario {
    ($name:expr, env: $($tail:tt)*) => ({
        scenario!($name, tags: NoTags, env: $($tail:tt)*);
    });
    ($name:expr, tags: $tags:ident, env: $env:ident => { $($tail:tt)* }) => ({
        println!("scenario {:?}, tags: {:?}, env: {:?} {{ ... }}", $name, stringify!($tags), stringify!($env));

        let mut stack = Vec::new();
        scenario_impl! { stack ($($tail)*) }
    });
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Environment(usize);

impl EnvironmentTrait for Environment {}

fn main() {
    scenario!("test", tags: Tag, env: Environment => {
        suite "suite", tags: Tag::FOO, env: Environment(42) => {
            context "context" => {
                example "example" => |env| {
                    env.0 == 42
                }
                example "example" => |env| {
                    // env.0 == 42
                }
                context "context" => {
                    example "example" => |env| {
                        // env.0 == 42
                    }
                    example "example" => |env| {
                        // env.0 == 42
                    }
                }
            }
        }
        suite "suite", env: Environment(42) => {
            context "context" => {
                example "example" => |env| {
                    let foo = 42;
                }
            }
        }
    });
}
