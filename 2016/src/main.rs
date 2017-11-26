extern crate aoc2016;

use std::string::String;
use std::io::prelude::*;
use std::fs::File;

use aoc2016::*;

struct Day {
    name: String,
    solve: fn(&String) -> String,
}

fn main() {
    let days = [
        Day {
            name: String::from("Day 1"),
            solve: day1::solve,
        },
    ];

    for &Day { ref name, solve } in days.iter() {
        let input_file = "./input/".to_string() + &name.replace("Day ", "day") + ".txt";
        let mut input = String::new();
        let mut file = File::open(input_file).expect("Failed to open file");
        file.read_to_string(&mut input).expect(
            "Failed reading file",
        );
        println!("{}: {}", name, solve(&input));
    }
}
