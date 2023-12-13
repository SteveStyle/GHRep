pub(crate) struct Cards {
    cards: Vec<Card>,
}

#[derive(Clone)]
struct Card {
    card_number: u32,
    winning_numbers: Vec<u32>,
    numbers: Vec<u32>,
    score: u32,
    instances: u32,
}

impl Card {
    fn number_of_winning_numbers(&self) -> u32 {
        let mut count = 0;
        for number in &self.numbers {
            if self.winning_numbers.contains(number) {
                count += 1;
            }
        }
        count
    }
    fn score(&self) -> u32 {
        let count = self.number_of_winning_numbers();
        if count == 0 {
            0
        } else {
            2u32.pow(count - 1)
        }
    }
    fn update_instances(&mut self, instances: u32) {
        self.instances += instances;
    }
    fn set_score(&mut self) {
        self.score = self.number_of_winning_numbers();
    }
}

impl Cards {
    pub(crate) fn from_string(input: &str) -> Cards {
        let mut cards = Vec::new();
        for line in input.lines() {
            let mut parts = line.split(":");
            let card_number = parts.next().and_then(|s| {
                s.split_whitespace()
                    .nth(1)
                    .and_then(|s| s.parse::<u32>().ok())
            });
            match card_number {
                Some(card_number) => {
                    let mut winning_numbers = Vec::new();
                    let mut numbers = Vec::new();
                    let mut number_parts = parts.next().unwrap().trim().split("|");
                    let winning_number_part = number_parts.next().unwrap();
                    for number in winning_number_part.split(" ") {
                        match number.parse::<u32>() {
                            Ok(n) => winning_numbers.push(n),
                            Err(_) => {}
                        }
                    }
                    let number_part = number_parts.next().unwrap();
                    for number in number_part.split(" ") {
                        match number.parse::<u32>() {
                            Ok(n) => numbers.push(n),
                            Err(_) => {}
                        }
                    }
                    cards.push(Card {
                        card_number,
                        winning_numbers,
                        numbers,
                        score: 0,
                        instances: 1,
                    });
                }
                None => {
                    println!("No card number found: {}", line)
                }
            }
        }

        Cards { cards }
    }

    pub(crate) fn total_score(&self) -> u32 {
        let mut total_score = 0;
        for card in &self.cards {
            total_score += card.score();
        }
        total_score
    }

    pub(crate) fn total_score2(&mut self) -> u32 {
        let mut total_score = 0;
        for i in 0..self.cards.len() {
            total_score += 1;
            self.cards[i].set_score();
            for j in 0..i {
                let other_card_score = self.cards[j].score;
                let other_card_instances = self.cards[j].instances;
                let other_card_number = self.cards[j].card_number;
                if other_card_number + other_card_score >= self.cards[i].card_number {
                    self.cards[i].update_instances(other_card_instances);
                    total_score += other_card_instances;
                }
            }
        }

        for card in &self.cards {
            println!(
                "Card {} has {} instances and a score of {}.",
                card.card_number, card.instances, card.score
            )
        }

        total_score
    }
}

const EXAMPLE_INPUT: &str = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";
