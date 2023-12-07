use std::env;
use std::fs::{File};
use std::io::{prelude::*, BufReader};

#[derive(Debug)]
struct RangeMap {
    destination_start: u64,
    destination_end: u64,
    source_start: u64,
    source_end: u64,
    range_length: u64,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
struct SeedRange {
    start: u64,
    end: u64,
}

enum MapType {
    SeedToSoil,
    SoilToFertilizer,
    FertilizerToWater,
    WaterToLight,
    LightToTemperature,
    TemperatureToHumidity,
    HumidityToLocation,
    End,
}

fn convert_x_with_map(x: u64, map: &RangeMap) -> u64 {
    let mut new_x = x;
    if x >= map.source_start && x <= map.source_start + map.range_length - 1 {
        new_x = map.destination_start + (x - map.source_start);
    }
    new_x
}

// Multithread this?
fn convert_x_with_maps(x: u64, maps: &Vec<RangeMap>) -> u64 {
    let mut new_x = x;
    for map in maps {
        if x >= map.source_start && x <= map.source_start + map.range_length - 1 {
            new_x = map.destination_start + (x - map.source_start);
            break;
        }
    }
    println!("{} -> {}", x, new_x);
    new_x
}

fn convert_seed_range_with_maps(seed_ranges: &mut Vec<SeedRange>, maps: &Vec<RangeMap>) -> Vec<SeedRange> {
    let mut new_seed_ranges: Vec<SeedRange> = Vec::new();
    for seed_range in seed_ranges.iter_mut() {
        for map in maps {
            // Seed range is completely inside of map. This also means we can break out of the loop
            if seed_range.start >= map.source_start && seed_range.end <= map.source_end {
                new_seed_ranges.push(SeedRange {
                    start: map.destination_start + (seed_range.start - map.source_start),
                    end: map.destination_start + (seed_range.end - map.source_start),
                });
                //println!("Both inside {:?} -> {:?}", seed_range, new_seed_ranges);
                seed_range.start = seed_range.end;
                seed_range.end = seed_range.end;
                break;
            }

            // Start of seed range is inside of map but end is outside of map
            if seed_range.start >= map.source_start && seed_range.start <= map.source_end && seed_range.end > map.source_end {
                new_seed_ranges.push(SeedRange {
                    start: map.destination_start + (seed_range.start - map.source_start),
                    end: map.destination_end,
                });
                //println!("Start inside {:?} -> {:?}", seed_range, new_seed_ranges);
                // Remove the part of the seed range that was inside the map
                seed_range.start = map.source_end + 1;
                continue;
            }

            // End of seed range is inside of map but start is outside of map
            if seed_range.end >= map.source_start && seed_range.end <= map.source_end && seed_range.start < map.source_start {
                new_seed_ranges.push(SeedRange {
                    start: map.destination_start,
                    end: map.destination_start + (seed_range.end - map.source_start),
                });
                //println!("End inside {:?} -> {:?}", seed_range, new_seed_ranges);
                // Remove the part of the seed range that was inside the map
                seed_range.end = map.source_start - 1;
                continue;
            }
            // Seed range is completely outside use original seed range
            // if (seed_range.start < map.source_start && seed_range.end < map.source_start) || (seed_range.start > map.source_end && seed_range.end > map.source_end) {
            //     new_seed_ranges.push(SeedRange {
            //         start: seed_range.start,
            //         end: seed_range.end,
            //     });
            //     println!("Both outside {:?} -> {:?}", seed_range, new_seed_ranges);
            //     continue;
            // }
        }
        // After iterating through all maps, if the seed range is still not empty, add it to the new seed ranges
        if seed_range.start < seed_range.end {
            new_seed_ranges.push(SeedRange {
                start: seed_range.start,
                end: seed_range.end,
            });
            //println!("Leftover {:?} -> {:?}", seed_range, new_seed_ranges);
        }
    }
    println!("{:?} -> {:?}", seed_ranges, new_seed_ranges);
    new_seed_ranges
}

fn parse_seeds(line: String) -> Vec<u64> {
    let mut seeds: Vec<u64> = Vec::new();
    let splitted: Vec<&str> = line.split(" ").collect();
    for s in 1..splitted.len() {
        seeds.push(splitted[s].parse::<u64>().unwrap());
    }
    seeds
}

fn parse_seeds2(line: String) -> Vec<SeedRange> {
    let mut seeds: Vec<SeedRange> = Vec::new();
    let splitted: Vec<&str> = line.split(" ").collect();
    let mut i = 1;
    while i < splitted.len() {
        let start = splitted[i].parse::<u64>().unwrap();
        let range = splitted[i+1].parse::<u64>().unwrap();
        seeds.push(SeedRange {
            start: start,
            end: start + range - 1,
        });
        i += 2;
    }
    seeds
}

fn parse_map(line: String) -> RangeMap {
    let splitted: Vec<&str> = line.split(" ").collect();
    let destination_start = splitted[0].parse::<u64>().unwrap();
    let source_start = splitted[1].parse::<u64>().unwrap();
    let range_length = splitted[2].parse::<u64>().unwrap();
    let destination_end = destination_start + range_length - 1;
    let source_end = source_start + range_length - 1;
    RangeMap {
        destination_start,
        destination_end,
        source_start,
        source_end,
        range_length,
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    let filename = &args[1];
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    //let mut seeds: Vec<u64> = Vec::new(); // part 1
    let mut seeds: Vec<SeedRange> = Vec::new(); // part 2
    let mut seed_to_soil_maps: Vec<RangeMap> = Vec::new();
    let mut soil_to_fertilizer_maps: Vec<RangeMap> = Vec::new();
    let mut fertilizer_to_water_maps: Vec<RangeMap> = Vec::new();
    let mut water_to_light_maps: Vec<RangeMap> = Vec::new();
    let mut light_to_temperature_maps: Vec<RangeMap> = Vec::new();
    let mut temperature_to_humidity_maps: Vec<RangeMap> = Vec::new();
    let mut humidity_to_location_maps: Vec<RangeMap> = Vec::new();

    let mut current_map_type = MapType::SeedToSoil;

    for (i, line) in reader.lines().enumerate() {
        let line = match line {
            Ok(line) => line,
            Err(e) => panic!("Error reading line: {}", e),
        };

        // First line is always seeds
        if i == 0 {
            //seeds = parse_seeds(line); // part 1
            seeds = parse_seeds2(line); // part 2
            println!("{:?}", seeds);
            continue;
        }

        // Skip header lines
        if line.contains(":") || i == 1 {
            continue;
        }

        // On empty line, move to next map
        if line == "" {
            current_map_type = match current_map_type {
                MapType::SeedToSoil => MapType::SoilToFertilizer,
                MapType::SoilToFertilizer => MapType::FertilizerToWater,
                MapType::FertilizerToWater => MapType::WaterToLight,
                MapType::WaterToLight => MapType::LightToTemperature,
                MapType::LightToTemperature => MapType::TemperatureToHumidity,
                MapType::TemperatureToHumidity => MapType::HumidityToLocation,
                MapType::HumidityToLocation => MapType::End,
                MapType::End => MapType::End,
            };
            continue;
        }

        // Otherwise, parse map
        match current_map_type {
            MapType::SeedToSoil => {seed_to_soil_maps.push(parse_map(line));},
            MapType::SoilToFertilizer => {soil_to_fertilizer_maps.push(parse_map(line));},
            MapType::FertilizerToWater => {fertilizer_to_water_maps.push(parse_map(line));},
            MapType::WaterToLight => {water_to_light_maps.push(parse_map(line));},
            MapType::LightToTemperature => {light_to_temperature_maps.push(parse_map(line));},
            MapType::TemperatureToHumidity => {temperature_to_humidity_maps.push(parse_map(line));},
            MapType::HumidityToLocation => {humidity_to_location_maps.push(parse_map(line));},
            MapType::End => {break;},
        }
    }

    // println!("Seeds: {:?}", seeds);
    // println!("Seed to soil maps: {:?}", seed_to_soil_maps);
    // println!("Soil to fertilizer maps: {:?}", soil_to_fertilizer_maps);
    // println!("Fertilizer to water maps: {:?}", fertilizer_to_water_maps);
    // println!("Water to light maps: {:?}", water_to_light_maps);
    // println!("Light to temperature maps: {:?}", light_to_temperature_maps);
    // println!("Temperature to humidity maps: {:?}", temperature_to_humidity_maps);
    // println!("Humidity to location maps: {:?}", humidity_to_location_maps);

    let mut locations: Vec<u64> = Vec::new();

    // Part 1
    /*for seed in seeds {
        let mut x = seed;
        println!("Seed: {}", x);
        x = convert_x_with_maps(x, &seed_to_soil_maps);
        x = convert_x_with_maps(x, &soil_to_fertilizer_maps);
        x = convert_x_with_maps(x, &fertilizer_to_water_maps);
        x = convert_x_with_maps(x, &water_to_light_maps);
        x = convert_x_with_maps(x, &light_to_temperature_maps);
        x = convert_x_with_maps(x, &temperature_to_humidity_maps);
        x = convert_x_with_maps(x, &humidity_to_location_maps);
        locations.push(x);
        println!("______________________");
    }

    println!("Lowest location: {}", locations.iter().min().unwrap());*/

    // Part 2
    println!("_____________________");
    println!("Starting Seeds: {:?}", seeds);
    let mut xs = convert_seed_range_with_maps(&mut seeds, &seed_to_soil_maps);
    xs = convert_seed_range_with_maps(&mut xs, &soil_to_fertilizer_maps);
    xs = convert_seed_range_with_maps(&mut xs, &fertilizer_to_water_maps);
    xs = convert_seed_range_with_maps(&mut xs, &water_to_light_maps);
    xs = convert_seed_range_with_maps(&mut xs, &light_to_temperature_maps);
    xs = convert_seed_range_with_maps(&mut xs, &temperature_to_humidity_maps);
    xs = convert_seed_range_with_maps(&mut xs, &humidity_to_location_maps);
    //println!("Ending Seeds: {:?}", xs);
    println!("Number of locations in the end: {}", xs.len());

    // Take lowest starting location
    let mut xs: Vec<u64> = xs.iter().map(|x| x.start).collect();
    println!("Lowest location: {:?}", xs.iter().min().unwrap());

    Ok(())
}
