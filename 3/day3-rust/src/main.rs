use std::env;
use std::fs::{File};
use std::io::{prelude::*, BufReader};

#[derive(Debug)]
enum Value {
    Symbol(String),
    Number(i32),
    Empty,
}

#[derive(Debug)]
struct Coord {
    x: i32,
    y: i32,
    value: Value,
}

fn parse_line(line: String, lineNo: i32) -> Vec<Coord> {
    let mut coords: Vec<Coord> = Vec::new();
    let mut x = 0;
    let y = lineNo;
    for c in line.chars() {
        let value = if c == '.' {
            Value::Empty
        } else if c.is_digit(10) {
            Value::Number(c.to_digit(10).unwrap() as i32)
        } else {
            Value::Symbol(c.to_string())
        };
        coords.push(Coord { x, y, value });
        x += 1;
    }

    // Combine adjacent numbers
    let mut i = 0;
    while i < coords.len() {
        if let Value::Number(n) = coords[i].value {
            if i > 0 && let Value::Number(m) = coords[i-1].value {
                coords[i-1].value = Value::Number(m * 10 + n);
                coords.remove(i);
                continue;
            }
        }
        i += 1;
    }

    coords
}


fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    let filename = &args[1];
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    let mut field: Vec<Vec<Coord>> = Vec::new();

    for (i, line) in reader.lines().enumerate() {
        let line = line?;
        println!("{}", line);
        field.push(parse_line(line, i as i32));
    }
    Ok(())
}
