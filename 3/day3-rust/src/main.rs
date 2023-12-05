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
    accounted_for: bool,
}

const adjecent_coords: [(i32, i32); 8] = [
    (-1, -1), (0, -1), (1, -1),
    (-1,  0),          (1,  0),
    (-1,  1), (0,  1), (1,  1),
];

fn parse_line(line: String, line_no: i32) -> Vec<Coord> {
    let mut coords: Vec<Coord> = Vec::new();
    let mut x = 0;
    let y = line_no;
    for c in line.chars() {
        let value = if c == '.' {
            Value::Empty
        } else if c.is_digit(10) {
            Value::Number(c.to_digit(10).unwrap() as i32)
        } else {
            Value::Symbol(c.to_string())
        };
        coords.push(Coord {x, y, value, accounted_for: false });
        x += 1;
    }

    // Concatenate adjacent digits
    let mut tracking_number = false;
    let mut tracking_count = String::new();
    let mut i = 0;
    while i < coords.len() {
        match coords[i].value {
            Value::Number(n) => {
                if tracking_number {
                    tracking_count += &n.to_string();
                    coords[i].value = Value::Empty;
                } else {
                    tracking_number = true;
                    tracking_count = n.to_string();
                }
            },
            _ => {
                if tracking_number {
                    let len = tracking_count.len();
                    let num = tracking_count.parse::<i32>().unwrap();
                    for j in i - len..i {
                        coords[j].value = Value::Number(num);
                    }
                    tracking_number = false;
                    tracking_count.clear();
                }
            }
        }
        i += 1;
    }

    // Handle end of line
    if tracking_number {
        let len = tracking_count.len();
        let num = tracking_count.parse::<i32>().unwrap();
        for j in i - len..i {
            coords[j].value = Value::Number(num);
        }
    }

    coords
}
fn account_number(field: &mut Vec<Vec<Coord>>, x: i32, y: i32) {
    let i = y ;
    let mut j = x  - 1;
    // Account adjacent numbers on the left
    while j >= 0 {
        match field[i as usize][j as usize].value {
            Value::Number(_) => {
                if !field[i as usize][j as usize].accounted_for {
                    field[i as usize][j as usize].accounted_for = true;
                }
            },
            _ => {break;}
        }
        j -= 1;
    }
    j = x + 1;
    // Account adjacent numbers on the right
    while j < field[i as usize].len() as i32 {
        match field[i as usize][j as usize].value {
            Value::Number(_) => {
                if !field[i as usize][j as usize].accounted_for {
                    field[i as usize][j as usize].accounted_for = true;
                }
            },
            _ => {break;}
        }
        j += 1;
    }
}


fn account_numbers(field: &mut Vec<Vec<Coord>>) -> i32  {
    let mut i = 0; // y
    let mut sum = 0;
    while i < field.len() {
        let mut j = 0; // x
        while j < field[0].len() {
            println!("i:{} j:{}", i, j);
            match field[i][j].value {
                Value::Symbol(_) => {
                    for (dx, dy) in adjecent_coords {
                        let x = (j as i32) + dx;
                        let y = (i as i32) + dy;
                        println!("x:{} y:{}", x, y);
                        if x >= 0 && x < field[0].len() as i32 && y >= 0 && y < field.len() as i32 {
                            match field[y as usize][x as usize].value {
                                Value::Number(n) => {
                                    if !field[y as usize][x as usize].accounted_for {
                                        field[y as usize][x as usize].accounted_for = true;
                                        account_number(field, x, y);
                                        sum += n;
                                    }
                                },
                                _ => {}
                            }
                        }
                    }
                }
                _  => {}
            }
            j += 1;
        }
        i += 1;
    }
    sum
}

fn account_numbers2(field: &mut Vec<Vec<Coord>>) -> i32  {
    let mut i = 0; // y
    let mut sum = 0;
    while i < field.len() {
        let mut j = 0; // x
        while j < field[0].len() {
            match &field[i][j].value {
                Value::Symbol(c) => {
                    if c.eq("*") {
                        let mut adjacent_numbers = 0;
                        let mut local_product = 1;
                        for (dx, dy) in adjecent_coords {
                            let x = (j as i32) + dx;
                            let y = (i as i32) + dy;
                            println!("x:{} y:{}", x, y);
                            if x >= 0 && x < field[0].len() as i32 && y >= 0 && y < field.len() as i32 {
                                match field[y as usize][x as usize].value {
                                    Value::Number(n) => {
                                        if !field[y as usize][x as usize].accounted_for {
                                            field[y as usize][x as usize].accounted_for = true;
                                            account_number(field, x, y);
                                            local_product *= n;
                                            adjacent_numbers += 1;
                                        }
                                    },
                                    _ => {}
                                }
                            }
                        }
                        if adjacent_numbers == 2 {
                            sum += local_product;
                        }
                    }
                }
                _  => {}
            }
            j += 1;
        }
        i += 1;
    }
    sum
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
    //let sum = account_numbers(&mut field); // part 1
    let sum = account_numbers2(&mut field); // part 2

    println!("{:?}", field);
    println!("Sum: {}", sum);



    Ok(())
}
