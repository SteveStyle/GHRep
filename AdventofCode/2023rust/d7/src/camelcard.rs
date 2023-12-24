use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug)]
enum HandType {
    HighCard = 1,
    Pair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Copy, Clone)]
enum Card {
    Two = 2,
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
    Ace,   // 14
}

impl Card {
    fn from_char(s: char) -> Card {
        match s {
            '2' => Card::Two,
            '3' => Card::Three,
            '4' => Card::Four,
            '5' => Card::Five,
            '6' => Card::Six,
            '7' => Card::Seven,
            '8' => Card::Eight,
            '9' => Card::Nine,
            'T' => Card::Ten,
            'J' => Card::Jack,
            'Q' => Card::Queen,
            'K' => Card::King,
            'A' => Card::Ace,
            _ => panic!("Invalid card string: {}", s),
        }
    }
}

impl Display for Card {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            Card::Two => "2",
            Card::Three => "3",
            Card::Four => "4",
            Card::Five => "5",
            Card::Six => "6",
            Card::Seven => "7",
            Card::Eight => "8",
            Card::Nine => "9",
            Card::Ten => "T",
            Card::Jack => "J",
            Card::Queen => "Q",
            Card::King => "K",
            Card::Ace => "A",
        };
        write!(f, "{}", s)
    }
}
#[derive(Debug, Hash, Clone)]
struct Hand {
    cards: Vec<Card>,
    bet: u32,
}

impl Hand {
    fn from_string(s: &str) -> Hand {
        let mut parts = s.split_whitespace();
        let mut cards = Vec::new();
        for card in parts.next().unwrap().chars() {
            cards.push(Card::from_char(card));
        }

        let bet = parts.next().unwrap().parse::<u32>().unwrap();
        Hand { cards, bet }
    }

    fn get_type(&self) -> HandType {
        let mut counts = HashMap::new();
        for card in &self.cards {
            let count = counts.entry(*card).or_insert(0);
            *count += 1;
        }
        let mut counts: Vec<(Card, u32)> = counts.into_iter().collect();
        counts.sort_by(|a, b| b.1.cmp(&a.1));
        let (card, count) = counts[0];
        match count {
            5 => HandType::FiveOfAKind,
            4 => HandType::FourOfAKind,
            3 => {
                if counts.len() == 2 {
                    HandType::FullHouse
                } else {
                    HandType::ThreeOfAKind
                }
            }
            2 => {
                if counts.len() == 3 {
                    HandType::TwoPair
                } else {
                    HandType::Pair
                }
            }
            1 => HandType::HighCard,
            _ => panic!("Invalid count: {}", count),
        }
    }
}

impl Display for Hand {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut s = String::new();
        for card in &self.cards {
            s.push_str(&format!("{} ", card));
        }
        write!(f, "{} {}", s, self.bet)
    }
}
impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}
impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Eq for Hand {}
impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_type = self.get_type();
        let other_type = other.get_type();
        if self_type != other_type {
            return self_type.cmp(&other_type);
        }
        match self.cards[0].cmp(&other.cards[0]) {
            Ordering::Equal => match self.cards[1].cmp(&other.cards[1]) {
                Ordering::Equal => match self.cards[2].cmp(&other.cards[2]) {
                    Ordering::Equal => match self.cards[3].cmp(&other.cards[3]) {
                        Ordering::Equal => self.cards[4].cmp(&other.cards[4]),
                        x => x,
                    },
                    x => x,
                },
                x => x,
            },
            x => x,
        }
    }
}

pub struct Game {
    hands: Vec<Hand>,
}

impl Game {
    pub fn from_string(s: &str) -> Game {
        let mut hands = Vec::new();
        for line in s.lines() {
            let hand = Hand::from_string(line);

            hands.push(hand);
        }
        hands.sort_by(|a, b| a.cmp(b));
        println!(
            "Hands: {}",
            Game {
                hands: hands.clone()
            }
        );
        Game { hands }
    }

    pub fn count_product(&self) -> u32 {
        let mut total = 0;
        for (rank, hand) in self.hands.iter().enumerate() {
            let rank = rank + 1;
            println!("Rank: {} Hand: {}", rank, hand);
            total += rank as u32 * hand.bet;
        }
        total
    }
}
impl Display for Game {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut s = String::new();
        for hand in &self.hands {
            s.push_str(&format!("{}\n", hand));
        }
        write!(f, "{}", s)
    }
}
const EXAMPLE_INPUT: &str = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483";
