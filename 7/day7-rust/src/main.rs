use std::{fs::File, io::{BufRead, BufReader}};

#[derive(Debug, Eq, PartialEq, Copy, Clone, Ord, PartialOrd)]
enum Card {
    Joker = 1,
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7,
    Eight = 8,
    Nine = 9,
    Ten = 10,
    Jack = 11,
    Queen = 12,
    King = 13,
    Ace = 14,
}

impl Card {
    fn value(&self) -> u8 {
        match self {
            Card::Joker => 1,
            Card::Two => 2,
            Card::Three => 3,
            Card::Four => 4,
            Card::Five => 5,
            Card::Six => 6,
            Card::Seven => 7,
            Card::Eight => 8,
            Card::Nine => 9,
            Card::Ten => 10,
            Card::Jack => 11,
            Card::Queen => 12,
            Card::King => 13,
            Card::Ace => 14,
        }
    }
    fn parse(s: &str) -> Option<Card> {
        match s {
            "2" => Some(Card::Two),
            "3" => Some(Card::Three),
            "4" => Some(Card::Four),
            "5" => Some(Card::Five),
            "6" => Some(Card::Six),
            "7" => Some(Card::Seven),
            "8" => Some(Card::Eight),
            "9" => Some(Card::Nine),
            "T" => Some(Card::Ten),
            "J" => Some(Card::Jack),
            "Q" => Some(Card::Queen),
            "K" => Some(Card::King),
            "A" => Some(Card::Ace),
            _ => None,
        }
    }
    fn parse_with_joker(s: &str) -> Option<Card> {
        match s {
            "2" => Some(Card::Two),
            "3" => Some(Card::Three),
            "4" => Some(Card::Four),
            "5" => Some(Card::Five),
            "6" => Some(Card::Six),
            "7" => Some(Card::Seven),
            "8" => Some(Card::Eight),
            "9" => Some(Card::Nine),
            "T" => Some(Card::Ten),
            "J" => Some(Card::Joker),
            "Q" => Some(Card::Queen),
            "K" => Some(Card::King),
            "A" => Some(Card::Ace),
            _ => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Ord, PartialOrd)]
enum HandType {
    High = 1,
    Pair = 2,
    TwoPair = 3,
    ThreeOfAKind = 4,
    FullHouse = 5,
    FourOfAKind = 6,
    FiveOfAKind = 7,
}

#[derive(Debug, Eq, PartialEq)]
struct Hand {
    cards: Vec<Card>,
    cards_joker_applied: Vec<Card>,
    hand_type: HandType,
    type_value: Card,
    bid: u64,
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let hand_1 = self.hand_type;
        let hand_2 = other.hand_type;

        if hand_1 > hand_2 {
            return std::cmp::Ordering::Greater;
        } else if hand_1 < hand_2 {
            return std::cmp::Ordering::Less;
        } else {
            for i in 0..5 {
                if self.cards[i].value() > other.cards[i].value() {
                    return std::cmp::Ordering::Greater
                } else if self.cards[i].value() < other.cards[i].value() {
                    return std::cmp::Ordering::Less
                }
            }
            return std::cmp::Ordering::Equal;

        }
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}


fn highest_card(cards: &Vec<Card>) -> Card {
    let mut highest_card = cards[0];

    for card in cards {
        if card.value() > highest_card.value() {
            highest_card = *card;
        }
    }

    return highest_card;
}

fn is_pair(cards: &Vec<Card>) -> Option<Card> {

    for card in cards {
        if cards.iter().filter(|&c| *c == *card).count() == 2 {
            return Some(*card);
        }
    }

    return None;
}

fn is_two_pair(cards: &Vec<Card>) -> Option<Card> {

    let mut first_pair = None;
    let mut second_pair = None;

    for card in cards {
        if cards.iter().filter(|&c| *c == *card).count() == 2 {
            if first_pair.is_none() {
                first_pair = Some(*card);
            } else if second_pair.is_none() && *card != first_pair.unwrap() {
                second_pair = Some(*card);
            }
        }
    }

    if first_pair.is_some() && second_pair.is_some() {
        if first_pair.unwrap().value() > second_pair.unwrap().value() {
            return Some(first_pair.unwrap());
        } else {
            return Some(second_pair.unwrap());
        }
    }

    return None;
}

fn is_full_house(cards: &Vec<Card>) -> Option<Card> {
    if cards.len() != 5 {
        return None;
    }

    let mut three_of_a_kind = None;
    let mut two_of_a_kind = None;

    for card in cards {
        if cards.iter().filter(|&c| *c == *card).count() == 3 {
            three_of_a_kind = Some(*card);
        } else if cards.iter().filter(|&c| *c == *card).count() == 2 {
            two_of_a_kind = Some(*card);
        }
    }

    if three_of_a_kind.is_some() && two_of_a_kind.is_some() {
        return Some(three_of_a_kind.unwrap());
    }

    return None;
}

fn is_three_of_a_kind(cards: &Vec<Card>) -> Option<Card> {

    for card in cards {
        if cards.iter().filter(|&c| *c == *card).count() == 3 {
            return Some(*card);
        }
    }

    return None;
}

fn is_four_of_a_kind(cards: &Vec<Card>) -> Option<Card> {

    for card in cards {
        if cards.iter().filter(|&c| *c == *card).count() == 4 {
            return Some(*card)
        }
    }
    return None;
}

fn is_five_of_a_kind(cards: &Vec<Card>) -> Option<Card> {
    let mut count = 0;
    let tracked_card = cards[0];

    if cards.len() != 5 { return None;}

    for card in cards {
        if *card == tracked_card {
            count += 1;
        }
    }

    if count == 5 {
        return Some(tracked_card);
    }

    return None;
}

// Part 2
fn eval_jokers(cards: &Vec<Card>) -> Vec<Card> {
    let number_of_jokers = cards.iter().filter(|&c| *c == Card::Joker).count();
    match number_of_jokers {
        0 => cards.clone(),
        1 => {
            let other_cards: Vec<Card> = cards.iter().filter(|&c| *c != Card::Joker).cloned().collect();
            let hand_type = if is_four_of_a_kind(&other_cards).is_some() {
                HandType::FourOfAKind
            } else if is_three_of_a_kind(&other_cards).is_some() {
                HandType::ThreeOfAKind
            } else if is_two_pair(&other_cards).is_some() {
                HandType::TwoPair
            } else if is_pair(&other_cards).is_some() {
                HandType::Pair
            } else {
                HandType::High
            };

            let type_value = match hand_type {
                HandType::FourOfAKind => is_four_of_a_kind(&other_cards).unwrap(),
                HandType::ThreeOfAKind => is_three_of_a_kind(&other_cards).unwrap(),
                HandType::TwoPair => is_two_pair(&other_cards).unwrap(),
                HandType::Pair => is_pair(&other_cards).unwrap(),
                HandType::High => highest_card(&other_cards),
                _ => panic!("Invalid hand type"),
            };
            return vec![other_cards[0], other_cards[1], other_cards[2], other_cards[3], type_value];
       },
        2 => {
            let other_cards: Vec<Card> = cards.iter().filter(|&c| *c != Card::Joker).cloned().collect();
            let hand_type = if is_three_of_a_kind(&other_cards).is_some() {
                HandType::ThreeOfAKind
            } else if is_pair(&other_cards).is_some() {
                HandType::Pair
            } else {
                HandType::High
            };

            let type_value = match hand_type {
                HandType::ThreeOfAKind => is_three_of_a_kind(&other_cards).unwrap(),
                HandType::Pair => is_pair(&other_cards).unwrap(),
                HandType::High => highest_card(&other_cards),
                _ => panic!("Invalid hand type"),
            };
            return vec![other_cards[0], other_cards[1], other_cards[2], type_value, type_value];
        },
        3 => {
            let other_cards: Vec<Card> = cards.iter().filter(|&c| *c != Card::Joker).cloned().collect();
            if other_cards[0] == other_cards[1] {
                return vec![other_cards[0]; 5];
            }
            if other_cards[0] > other_cards[1] {
                return vec![other_cards[0], other_cards[0], other_cards[0], other_cards[0], other_cards[1]];
            } else {
                return vec![other_cards[0], other_cards[1], other_cards[1], other_cards[1], other_cards[1]];
            }
        },
        4 => {
            let card_not_joker = cards.iter().filter(|&c| *c != Card::Joker).nth(0).unwrap();
            return vec![*card_not_joker; 5];
        },
        5 => {
            return vec![Card::Ace; 5];
        },
        _ => panic!("Invalid number of jokers"),
    }
}

fn process_part1(filename: String) -> std::io::Result<()> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    let mut times_str: String = "".to_string();
    let mut distances_str: String = "".to_string();
    let mut hands: Vec<Hand> = Vec::new();

    for (index, line) in reader.lines().enumerate() {
        let mut cards: Vec<Card> = Vec::new();
        let line = line.unwrap();
        let split_line: Vec<&str> = line.split(" ").collect();
        let hand_str = split_line[0];
        let bid = split_line[1].parse::<u64>().unwrap();

        for c in hand_str.chars() {
            let card = Card::parse(&c.to_string()).unwrap();
            cards.push(card);
        }

        // Find hand type and associated value
        let hand_type = if is_five_of_a_kind(&cards).is_some() {
            HandType::FiveOfAKind
        } else if is_four_of_a_kind(&cards).is_some() {
            HandType::FourOfAKind
        } else if is_full_house(&cards).is_some() {
            HandType::FullHouse
        } else if is_three_of_a_kind(&cards).is_some() {
            HandType::ThreeOfAKind
        } else if is_two_pair(&cards).is_some() {
            HandType::TwoPair
        } else if is_pair(&cards).is_some() {
            HandType::Pair
        } else {
            HandType::High
        };

        let type_value = match hand_type {
            HandType::FiveOfAKind => is_five_of_a_kind(&cards).unwrap(),
            HandType::FourOfAKind => is_four_of_a_kind(&cards).unwrap(),
            HandType::FullHouse => is_full_house(&cards).unwrap(),
            HandType::ThreeOfAKind => is_three_of_a_kind(&cards).unwrap(),
            HandType::TwoPair => is_two_pair(&cards).unwrap(),
            HandType::Pair => is_pair(&cards).unwrap(),
            HandType::High => highest_card(&cards),
            _ => panic!("Invalid hand type"),
        };

        // Part 1 doesn't use jokers

        let cards_joker_applied = cards.clone();
        let hand: Hand = Hand { cards, cards_joker_applied, hand_type, type_value, bid };
        hands.push(hand);
    }

    hands.sort();

    // Check bids
    let mut total_winnings: u64 = 0;
    for (index, hand) in hands.iter().enumerate() {
        total_winnings += (index as u64 + 1) * hand.bid

    }

    //println!("{:?}", hands);
    println!("Total winnigs: {}", total_winnings);
    Ok(())

}

fn process_part2(filename: String) -> std::io::Result<()> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    let mut times_str: String = "".to_string();
    let mut distances_str: String = "".to_string();
    let mut hands: Vec<Hand> = Vec::new();

    for (index, line) in reader.lines().enumerate() {
        let mut cards: Vec<Card> = Vec::new();
        let line = line.unwrap();
        let split_line: Vec<&str> = line.split(" ").collect();
        let hand_str = split_line[0];
        let bid = split_line[1].parse::<u64>().unwrap();

        for c in hand_str.chars() {
            let card = Card::parse_with_joker(&c.to_string()).unwrap();
            cards.push(card);
        }
        let joker_applied_cards = eval_jokers(&cards);

        // Find hand type and associated value
        let hand_type = if is_five_of_a_kind(&joker_applied_cards).is_some() {
            HandType::FiveOfAKind
        } else if is_four_of_a_kind(&joker_applied_cards).is_some() {
            HandType::FourOfAKind
        } else if is_full_house(&joker_applied_cards).is_some() {
            HandType::FullHouse
        } else if is_three_of_a_kind(&joker_applied_cards).is_some() {
            HandType::ThreeOfAKind
        } else if is_two_pair(&joker_applied_cards).is_some() {
            HandType::TwoPair
        } else if is_pair(&joker_applied_cards).is_some() {
            HandType::Pair
        } else {
            HandType::High
        };

        let type_value = match hand_type {
            HandType::FiveOfAKind => is_five_of_a_kind(&joker_applied_cards).unwrap(),
            HandType::FourOfAKind => is_four_of_a_kind(&joker_applied_cards).unwrap(),
            HandType::FullHouse => is_full_house(&joker_applied_cards).unwrap(),
            HandType::ThreeOfAKind => is_three_of_a_kind(&joker_applied_cards).unwrap(),
            HandType::TwoPair => is_two_pair(&joker_applied_cards).unwrap(),
            HandType::Pair => is_pair(&joker_applied_cards).unwrap(),
            HandType::High => highest_card(&joker_applied_cards),
            _ => panic!("Invalid hand type"),
        };

        // Part 1 doesn't use jokers
        let hand: Hand = Hand { cards, cards_joker_applied: joker_applied_cards, hand_type, type_value, bid };
        hands.push(hand);
    }

    hands.sort();

    // Check bids
    let mut total_winnings: u64 = 0;
    for (index, hand) in hands.iter().enumerate() {
        total_winnings += (index as u64 + 1) * hand.bid

    }

    println!("{:?}", hands);
    println!("Total winnigs: {}", total_winnings);
    Ok(())

}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let filename = &args[1];
    // Part 1
    //process_part1(filename.to_string())?;

    // Part 2
    process_part2(filename.to_string())?;
    Ok(())
}
