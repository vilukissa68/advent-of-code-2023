use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::io::{Seek, SeekFrom, Read};

#[derive(Debug, Clone)]
struct Node {
    name : String,
    left : Option<Box<Node>>,
    right : Option<Box<Node>>,
}


#[derive(Debug, Clone)]
struct GraphLoop {
    length : i32,
    end_point_remainders : Vec<i32>,
}

fn treverse_graph(nodes : &HashMap<String, Node>, insturctions: String) -> i32 {
    let mut step = 0;
    let mut current_node = "AAA".to_string();
    println!("Traversing graph, with instructions: {}", insturctions);
    loop {
        // Clamp step
        let index = if step > 0 { step % insturctions.len() } else { 0 };
        if insturctions.chars().nth(index).unwrap() == 'L' {
            current_node = nodes.get(&current_node).unwrap().left.as_ref().unwrap().name.clone();
        } else {
            current_node = nodes.get(&current_node).unwrap().right.as_ref().unwrap().name.clone();
        }

        println!("Step: {} Node: {}", step, current_node);
        step += 1;
        if current_node == "ZZZ" {
            break;
        }
    }
    step as i32
}

fn treverse_graph_part2(nodes : &HashMap<String, Node>, starting_node: String, insturctions: String) -> GraphLoop {
    let mut step = 0;
    let mut current_node = starting_node.clone();
    let mut end_point_remainders: Vec<i32> = Vec::new();
    let mut loop_length: i32 = 0;

    // Trevese graph
    loop {
        // Clamp step
        let index = if step > 0 { step % insturctions.len() } else { 0 };
        // Move on all nodes
        if insturctions.chars().nth(index).unwrap() == 'L' {
            current_node = nodes.get(&current_node).unwrap().left.as_ref().unwrap().name.clone();
        } else {
            current_node = nodes.get(&current_node).unwrap().right.as_ref().unwrap().name.clone();
        }
        step += 1;
        println!("Step: {} Node: {}", step, current_node);
        println!("Loop length: {}", loop_length);
        println!("End point remainders: {:?}", end_point_remainders);
        println!("Starting node: {}", starting_node);

        // Check if node has found Z
        if current_node.chars().nth(2).unwrap() == 'Z' && loop_length > 0 {
            end_point_remainders.push(step as i32 % loop_length);
        }
        if current_node == starting_node && loop_length > 0 {
            break;
        }
        if current_node == starting_node {
            loop_length = step as i32;
        }
    }
    GraphLoop { length : loop_length, end_point_remainders : end_point_remainders }

}

fn process_part2(filename : String) -> std::io::Result<()> {
    let file = File::open(filename)?;
    let mut reader = BufReader::new(file);
    let mut instructions = String::new();

    let mut nodes : HashMap<String, Node> = HashMap::new();

    // Run first pass to find all the nodes
    for (index, line) in reader.by_ref().lines().enumerate() {
        let line = line.unwrap();

        // Empty line
        if index == 0 {
            // Part instructions
            instructions = line;
            continue;
        }
        if index == 1 {
            continue;
        }

        let split_line: Vec<&str>= line.split(" = ").collect(); // Divide to node name | left and rigth
        nodes.insert(split_line[0].to_string(), Node { name : split_line[0].to_string(), left : None, right : None });

    }

    reader.seek(SeekFrom::Start(0))?; // Reset the reader

    // Second pass to build the graph
    for (index, line) in reader.lines().enumerate() {
        let line = line.unwrap();

        // Empty line
        if index == 0 {
            // Part instructions
            continue;
        }
        if index == 1 {
            continue;
        }

        let split_line: Vec<&str>= line.split(" = ").collect(); // Divide to node name | left and rigth
        let node_names: Vec<&str>= split_line[1].split(", ").collect(); // Divide to node name | left and rigth
        let left_name = node_names[0].chars().skip(1).take(3).collect::<String>();
        let right_name = node_names[1].chars().take(3).collect::<String>();
        println!("{} {} {}", split_line[0], left_name, right_name);

        // Update graph
        nodes.get_mut(split_line[0]).unwrap().left = nodes.get_mut(&left_name).map(|x| Some(Box::new(x.clone()))).unwrap_or(None);
        nodes.get_mut(split_line[0]).unwrap().right = nodes.get_mut(&right_name).map(|x| Some(Box::new(x.clone()))).unwrap_or(None);
    }
    // Find all starting nodes
    let mut starting_nodes: Vec<String> = Vec::new();
    // Find all nodes ending with A
    for (key, _) in &nodes {
        if key.chars().nth(2).unwrap() == 'A' {
            starting_nodes.push(key.clone());
        }
    }

    // Build loop structs
    let mut loops: Vec<GraphLoop> = Vec::new();
    for node in starting_nodes {
        loops.push(treverse_graph_part2(&nodes, node, instructions.clone()));
    }
    println!("Loops: {:?}", loops);


    Ok(())
}

fn process_part1(filename : String) -> std::io::Result<()> {
    let file = File::open(filename)?;
    let mut reader = BufReader::new(file);
    let mut instructions = String::new();

    let mut nodes : HashMap<String, Node> = HashMap::new();

    // Run first pass to find all the nodes
    for (index, line) in reader.by_ref().lines().enumerate() {
        let line = line.unwrap();

        // Empty line
        if index == 0 {
            // Part instructions
            instructions = line;
            continue;
        }
        if index == 1 {
            continue;
        }

        let split_line: Vec<&str>= line.split(" = ").collect(); // Divide to node name | left and rigth
        nodes.insert(split_line[0].to_string(), Node { name : split_line[0].to_string(), left : None, right : None });

    }

    reader.seek(SeekFrom::Start(0))?; // Reset the reader

    // Second pass to build the graph
    for (index, line) in reader.lines().enumerate() {
        let line = line.unwrap();

        // Empty line
        if index == 0 {
            // Part instructions
            continue;
        }
        if index == 1 {
            continue;
        }

        let split_line: Vec<&str>= line.split(" = ").collect(); // Divide to node name | left and rigth
        let node_names: Vec<&str>= split_line[1].split(", ").collect(); // Divide to node name | left and rigth
        let left_name = node_names[0].chars().skip(1).take(3).collect::<String>();
        let right_name = node_names[1].chars().take(3).collect::<String>();
        println!("{} {} {}", split_line[0], left_name, right_name);

        // Update graph
        nodes.get_mut(split_line[0]).unwrap().left = nodes.get_mut(&left_name).map(|x| Some(Box::new(x.clone()))).unwrap_or(None);
        nodes.get_mut(split_line[0]).unwrap().right = nodes.get_mut(&right_name).map(|x| Some(Box::new(x.clone()))).unwrap_or(None);
    }
    let steps = treverse_graph(&nodes, instructions); // Part 1
    println!("Steps: {}", steps);

    Ok(())

}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let filename = &args[1];

    //process_part1(filename.to_string())?;
    process_part2(filename.to_string())?;

    Ok(())

}
