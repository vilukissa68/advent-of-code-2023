use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
enum Card {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,  // 11
    Queen, // 12
    King,  // 13
    Ace,
}

impl Card {
    fn value(&self) -> u8 {
        match self {
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
}

#[derive(Debug)]
enum HandType {
    High,
    Pair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}



#[derive(Debug)]
struct Hand {
    cards: Vec<Card>,
    type: HandType,
    typeValue: Card,

}


fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    let filename = &args[1];
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    let mut timesStr: String = "".to_string();
    let mut distancesStr: String = "".to_string();

    let mut hands: Vec<Hand> = Vec::new();
    let mut bids: Vec<u32> = Vec::new();

    for (index, line) in reader.lines().enumerate() {
        let mut hand: Hand = Hand { cards: Vec::new() };

        let line = line.unwrap();
        let splitLine: Vec<&str> = line.split(" ").collect();
        let handStr = splitLine[0];
        let bid = splitLine[1].parse::<u32>().unwrap();

        for c in handStr.chars() {
            let card = Card::parse(&c.to_string()).unwrap();
            hand.cards.push(card);
        }

        hands.push(hand);
        bids.push(bid);
    }
    println!("{:?}", hands);

    Ok(())
}
