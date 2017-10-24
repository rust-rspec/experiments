#![feature(try_from)]

#[macro_use]
extern crate bitflags;

use std::convert::TryFrom;
use std::fmt::Debug;

pub trait TagsTrait: PartialEq + Eq + Debug {}

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

fn main() {
    let strings = vec!["foo", "bar", "baz", "blee"];
    for string in strings.into_iter() {
        let tag = Tag::try_from(string);
        println!("tag: {:?}", tag);
    }
}
