use std::fs::File;
use std::io::{BufRead, BufReader};

fn time_permutation(time: i32) -> Vec<i32> {
    let mut times: Vec<i32> = Vec::new();
    for hold_time in 0..time {
        let speed = hold_time * 1; // 1 m/s
        let run_time = time - hold_time;
        let distance = speed * run_time;
        times.push(distance);
    }
    times
}

fn time_permutation_u64(time: u64) -> Vec<u64> {
    let mut times: Vec<u64> = Vec::new();
    for hold_time in 0..time {
        let speed = hold_time * 1; // 1 m/s
        let run_time = time - hold_time;
        let distance = speed * run_time;
        times.push(distance);
    }
    times
}

fn scores(times: &Vec<i32>, distances: &Vec<i32>) -> i32 {
     let mut wins_amount: Vec<i32> = Vec::new();
     for (index, time) in times.iter().enumerate() {
        let target_distance = distances[index];
        let permutations: Vec<i32> = time_permutation(*time);
        let win_indexes: Vec<usize> = permutations.iter().enumerate().filter(|&(_, x)| *x > target_distance).map(|(i, _)| i).collect();
        println!("Permutations: {:?}", permutations);
        println!("Winning indexes: {:?}", win_indexes);
        wins_amount.push(win_indexes.len().try_into().unwrap());
    }
    println!("Wins amount: {:?}", wins_amount);
    return wins_amount.into_iter().reduce(|a, b| a * b).unwrap();

}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    let filename = &args[1];
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    let mut timesStr: String = "".to_string();
    let mut distancesStr: String = "".to_string();

    for (index, line) in reader.lines().enumerate() {
        match index {
            0 => timesStr = line.unwrap(),
            1 => distancesStr = line.unwrap(),
            _ => break,
        }
    }

    // Part 1
    let times: Vec<i32> = timesStr.split(" ").filter(|&x| x != "" && x != "Time:").map(|x| x.parse::<i32>().unwrap()).collect();
    let distances: Vec<i32> = distancesStr.split(" ").filter(|&x| x != "" && x != "Distance:").map(|x| x.parse::<i32>().unwrap()).collect();
    println!("Times: {:?}", times);
    println!("Distances: {:?}", distances);

    let score = scores(&times, &distances);
    println!("Score: {}", score);

    // Part 2
    let timeStrArray: Vec<&str> = timesStr.split(" ").filter(|&x| x != "" && x != "Time:").collect();
    let distanceStrArray: Vec<&str> = distancesStr.split(" ").filter(|&x| x != "" && x != "Distance:").collect();
    let mut time: String = "".to_string();
    let mut distance: String = "".to_string();
    for x in timeStrArray {
        time.push_str(x)
    }
    for x in distanceStrArray {
        distance.push_str(x)
    }

    println!("{:?}", time);
    println!("{:?}", distance);
    let permutations = time_permutation_u64(time.parse::<u64>().unwrap());
    let no_wins: Vec<u64> = permutations.into_iter().filter(|&x| x > distance.parse::<u64>().unwrap()).collect();
    println!("Wins {}", no_wins.len());
    Ok(())

}
