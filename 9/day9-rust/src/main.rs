use std::fs::File;
use std::io::{BufRead, BufReader};


fn expand_lines_right(lines: &mut Vec<Vec<i64>>) -> Vec<Vec<i64>> {
    for i in (0..lines.len()).rev() {
        if i == lines.len() - 1 {
            lines[i].push(0);
            continue;
        }
        let line_length = lines[i].len();
        let prev_line_last = lines[i + 1][line_length - 1];
        let current_line_last = lines[i][line_length - 1];
        lines[i].push(prev_line_last + current_line_last);
    }
    lines.to_vec()
}

fn expand_lines_left(lines: &mut Vec<Vec<i64>>) -> Vec<Vec<i64>> {
    for i in (0..lines.len()).rev() {
        if i == lines.len() - 1 {
            lines[i].insert(0, 0);
            continue;
        }
        let prev_line_first = lines[i + 1][0];
        let current_line_first = lines[i][0];
        lines[i].insert(0, current_line_first - prev_line_first);
    }
    lines.to_vec()
}

fn forward_step(line: &Vec<i64>) -> Vec<i64> {
    let mut new_line: Vec<i64> = Vec::new();
    for i in 0..(line.len() - 1) {
        new_line.push(line[i +1] - line[i])
    }
    new_line
}

// Used in part2
fn add_new_line(lines: &mut Vec<Vec<i64>>) {
    let mut new_line: Vec<i64> = Vec::new();
    for i in 0..lines[0].len()-1 {
        let new_value = lines[0][i] + lines[0][i+1];
        new_line.push(new_value);
    }
    lines.push(new_line);

    // Extrapolate fist value of the new line
}

fn process_line_part2(line: String) -> i64 {
    let mut lines: Vec<Vec<i64>> = Vec::new();

    // Parse line to
    let mut first_line: Vec<i64> = Vec::new();
    for c in line.split(" ") {
        let num = c.parse::<i64>().unwrap();
        first_line.push(num);
    }
    lines.push(first_line);

    // Forward step until we find a line with all zeros
    let mut index = 0;
    loop {
        let new_line = forward_step(&lines[index]);
        index += 1;
        if new_line.iter().all(|&x| x == 0) {
            lines.push(new_line);
            break;
        }
        lines.push(new_line);
    }
    // Expand each line to the left
    let mut new_lines = expand_lines_left(&mut lines);

    // Extrapolate
    //add_new_line(&mut new_lines);
    println!("{:?}", new_lines);

    return new_lines[0][0]

}

fn process_line_part1(line: String) -> i64 {
    let mut lines: Vec<Vec<i64>> = Vec::new();

    // Parse line to
    let mut first_line: Vec<i64> = Vec::new();
    for c in line.split(" ") {
        let num = c.parse::<i64>().unwrap();
        first_line.push(num);
    }
    lines.push(first_line);

    // Forward step until we find a line with all zeros
    let mut index = 0;
    loop {
        let new_line = forward_step(&lines[index]);
        index += 1;
        if new_line.iter().all(|&x| x == 0) {
            lines.push(new_line);
            break;
        }
        lines.push(new_line);
    }

    // Expand each line
    let mut new_lines = expand_lines_right(&mut lines);

    return new_lines[0][new_lines[0].len() - 1]
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let filename = &args[1];
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    let mut sum = 0;
    for (index, line) in reader.lines().enumerate() {
        println!("Processing line {}", index);
        let line = line.unwrap();
        //sum += process_line_part1(line);// Part 1
        sum += process_line_part2(line);// Part 2
    }

    println!("Sum: {}", sum);

    Ok(())

}
