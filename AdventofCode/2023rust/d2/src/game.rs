// define a TryColour enum with codes for red, green and blue
#[derive(Copy, Clone, Debug, PartialEq)]
enum TryColour {
    Red = 0,
    Green,
    Blue,
}
impl TryColour {
    fn from_str(s: &str) -> Option<TryColour> {
        match s {
            "red" => Some(TryColour::Red),
            "green" => Some(TryColour::Green),
            "blue" => Some(TryColour::Blue),
            _ => None,
        }
    }
}

const MAXCOLOURCOUNT: [u32; 3] = [12, 13, 14];
// define a Try struct with a colour and a count
struct Try {
    redcount: u32,
    greencount: u32,
    bluecount: u32,
}
impl Try {
    fn from_str(s: &str) -> Option<Try> {
        let mut trial = Try {
            redcount: 0,
            greencount: 0,
            bluecount: 0,
        };
        for try_part in s.split(",") {
            let mut parts = try_part.split_whitespace();
            match parts.next() {
                Some(count_str) => match parts.next() {
                    Some(colour_str) => match count_str.parse::<u32>() {
                        Ok(count) => match TryColour::from_str(colour_str) {
                            Some(TryColour::Red) => trial.redcount += count,
                            Some(TryColour::Green) => trial.greencount += count,
                            Some(TryColour::Blue) => trial.bluecount += count,
                            None => {}
                        },
                        Err(_) => {}
                    },
                    None => {}
                },
                None => {}
            }
        }
        Some(trial)
    }
    fn is_valid(&self) -> bool {
        self.redcount <= MAXCOLOURCOUNT[TryColour::Red as usize]
            && self.greencount <= MAXCOLOURCOUNT[TryColour::Green as usize]
            && self.bluecount <= MAXCOLOURCOUNT[TryColour::Blue as usize]
    }
    fn count(&self) -> u32 {
        self.redcount + self.greencount + self.bluecount
    }
}

// define a Game struct with a game id and a vector of tries
pub(crate) struct Game {
    game_id: u32,
    tries: Vec<Try>,
}
impl Game {
    pub fn from_str(s: &str) -> Option<Game> {
        let mut parts = s.split(":");
        match parts.next() {
            Some(game_id_str) => match parts.next() {
                Some(tries_str) => match game_id_str.trim().strip_prefix("Game ") {
                    Some(game_id_str) => match game_id_str.parse::<u32>() {
                        Ok(game_id) => {
                            let tries = tries_str
                                .split(";")
                                .map(|s| s.trim())
                                .filter_map(|s| Try::from_str(s))
                                .collect::<Vec<Try>>();
                            Some(Game { game_id, tries })
                        }
                        Err(_) => None,
                    },
                    None => None,
                },
                None => None,
            },
            None => None,
        }
    }
    fn is_valid(&self) -> bool {
        self.tries.iter().all(|t| t.is_valid())
    }
    fn count(&self) -> u32 {
        self.tries.iter().map(|t| t.count()).sum()
    }
    pub(crate) fn game_value(&self) -> u32 {
        if self.is_valid() {
            self.game_id
        } else {
            0
        }
    }
    // implement a function to return the miniumum redcount, greencount and bluecount
    pub fn min_counts(&self) -> (u32, u32, u32) {
        let mut min_redcount = MAXCOLOURCOUNT[TryColour::Red as usize];
        let mut min_greencount = MAXCOLOURCOUNT[TryColour::Green as usize];
        let mut min_bluecount = MAXCOLOURCOUNT[TryColour::Blue as usize];
        for try_part in &self.tries {
            if try_part.redcount < min_redcount {
                min_redcount = try_part.redcount;
            }
            if try_part.greencount < min_greencount {
                min_greencount = try_part.greencount;
            }
            if try_part.bluecount < min_bluecount {
                min_bluecount = try_part.bluecount;
            }
        }
        (min_redcount, min_greencount, min_bluecount)
    }

    pub fn max_counts(&self) -> (u32, u32, u32) {
        let mut max_redcount = 0;
        let mut max_greencount = 0;
        let mut max_bluecount = 0;
        for try_part in &self.tries {
            if try_part.redcount > max_redcount {
                max_redcount = try_part.redcount;
            }
            if try_part.greencount > max_greencount {
                max_greencount = try_part.greencount;
            }
            if try_part.bluecount > max_bluecount {
                max_bluecount = try_part.bluecount;
            }
        }
        (max_redcount, max_greencount, max_bluecount)
    }
}
