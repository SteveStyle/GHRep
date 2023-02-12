use std::{
    collections::HashSet,
    fmt::{Display, Formatter},
    ops::Neg,
    thread::current,
};

use board::{CellValue, WordPositions};
use pos::Position;
use rand::seq::index;
use tiles::{Letter, LetterSet, Tile, TileBag, TileList, ALPHABET};

use crate::word_list::is_word;
//use word_list::{is_word, LETTER_PREFIXES, LETTER_SUFFIXES};

pub mod board;
pub mod tiles;
pub mod word_list;

pub mod pos;
pub mod utils;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PlayerType {
    Human,
    Computer,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Player {
    pub player_type: PlayerType,
    pub rack: tiles::TileBag,
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Direction {
    Horizontal,
    Vertical,
}

impl Neg for Direction {
    type Output = Direction;
    fn neg(self) -> Self::Output {
        match self {
            Direction::Horizontal => Direction::Vertical,
            Direction::Vertical => Direction::Horizontal,
        }
    }
}

impl Direction {
    fn get_step(&self) -> Position {
        match self {
            Direction::Horizontal => Position::new(1, 0),
            Direction::Vertical => Position::new(0, 1),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MovePositionType {
    Open {
        tile: Option<Tile>,
    },
    Connecting {
        tile: Option<Tile>,
        letter_set: LetterSet,
        start_pos: Option<Position>,
        end_pos: Option<Position>,
        connecting_word: Option<String>,
    },
    Filled {
        letter: Letter,
    },
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MovePositionMap {
    positions: Vec<Position>,
    position_types: Vec<MovePositionType>,
    previous_cell_pos: Option<Position>,
    next_cell_pos: Option<Position>,
    min_tiles: u8,
    max_tiles: u8,
    word_multiplier: u8,
}

impl MovePositionMap {
    pub fn add(&mut self, position: Position, position_type: MovePositionType) {
        self.positions.push(position);
        self.position_types.push(position_type);
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GameMove {
    pub player: usize,
    pub direction: Direction,
    pub first_move: bool,
    pub starting_position: Position,
    pub length: u8,
    pub move_position_map: Option<MovePositionMap>,
    pub word: Option<String>,
    pub score: u16,
}

impl GameMove {
    pub fn new(
        player: usize,
        direction: Direction,
        first_move: bool,
        position: Position,
        length: u8,
    ) -> Self {
        Self {
            player,
            direction,
            first_move,
            starting_position: position,
            length,
            move_position_map: None,
            word: None,
            score: 0,
        }
    }
    pub fn generate_position_map(&mut self, board: &board::Board) -> Result<(), MoveError> {
        let mut move_position_map = MovePositionMap {
            positions: Vec::new(),
            position_types: Vec::new(),
            previous_cell_pos: None,
            next_cell_pos: None,
            min_tiles: 0,
            max_tiles: 7,
            word_multiplier: 1,
        };
        let mut enabler_found = false;
        let mut current_pos = self.starting_position;
        while let Some(previous_pos) = current_pos.try_step_backward(&self.direction) {
            if board.get_cell_pos(previous_pos).is_filled() {
                current_pos = previous_pos;
            } else {
                move_position_map.previous_cell_pos = Some(previous_pos);
                break;
            }
        }
        let mut tiles_placed = 0u8;
        /*  Loop through all the cells involved in the move and add them
           to the move_position_map.
           End the loop when:
               - we reach the edge of the board or
               - we reach an empty cell but have already placed all the tiles
        */
        loop {
            let cell = board.get_cell_pos(current_pos);
            let is_enabler = board.is_enabler(current_pos);
            let cell_value = &cell.value;
            let word_multiplier = cell.cell_type.word_multiplier();
            let allowed_letters = cell_value.allowed_letters(self.direction);
            match cell_value {
                CellValue::Empty {
                    horizontal_letters: _,
                    vertical_letters: _,
                } => {
                    if tiles_placed == self.length {
                        // We have placed all the tiles, so we finish the loop
                        move_position_map.next_cell_pos = Some(current_pos);
                        break;
                    }
                    // otherwise we still have tiles to place
                    if allowed_letters.is_empty() {
                        move_position_map.max_tiles = tiles_placed;
                        break;
                    }
                    if !allowed_letters.is_full() {
                        move_position_map.add(
                            current_pos,
                            MovePositionType::Connecting {
                                tile: None,
                                letter_set: allowed_letters.clone(),
                                start_pos: None,
                                end_pos: None,
                                connecting_word: None,
                            },
                        );
                    } else {
                        move_position_map.add(current_pos, MovePositionType::Open { tile: None });
                    }
                    move_position_map.word_multiplier *= word_multiplier;
                    tiles_placed += 1;
                }
                CellValue::Filled {
                    letter,
                    is_blank: _,
                } => move_position_map.add(
                    current_pos,
                    MovePositionType::Filled {
                        letter: letter.clone(),
                    },
                ),
            }
            if !enabler_found && is_enabler {
                move_position_map.min_tiles = tiles_placed;
                enabler_found = true;
            }

            if let Some(next_pos) = current_pos.try_step_forward(&self.direction) {
                current_pos = next_pos;
            } else {
                // We have reached the edge of the board
                break;
            }
        }
        self.move_position_map = Some(move_position_map);
        Ok(())
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Game {
    //  fixed part of the game
    scrabble_variant: &'static board::ScrabbleVariant,
    //  mutable part of the game
    pub players: [Player; 2],
    board: board::Board,
    pub bag: tiles::TileBag,
    pub next_player: usize, // index into `players`
    pub first_move: bool,
    pub is_over: bool,
    pub scores: [u32; 2], // score for each player
    //  history of moves
    pub moves: Vec<GameMove>,
    //local_word_list: HashSet<String>,
}

impl Display for Game {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Board:")?;
        writeln!(f, "{}", self.board)?;
        writeln!(f, "Bag: {}", self.bag)?;
        writeln!(
            f,
            "Racks: {} {}",
            self.players[0].rack, self.players[1].rack
        )?;
        writeln!(f, "Scores: {:?}", self.scores)?;
        Ok(())
    }
}

// implement an error type for my apply move function
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MoveError {
    InvalidMove,
    InvalidWord(String),
    InvalidPosition,
    InvalidDirection,
    TilesDonNotFit,
    TilesDoNotConnect,
    LetterNotAllowedInPosition,
    TilesNotInRack(Tile),
    BlankTileNotActingAsLetter,
    NoTilesPassed,
    NotEnoughSpaceForTiles(u8),
    BlockingEmptyCellFound,
}

impl Game {
    pub fn new(scrabble_variant: &'static board::ScrabbleVariant) -> Self {
        let mut bag = tiles::TileBag::new(scrabble_variant);

        let mut players = [
            Player {
                player_type: PlayerType::Human,
                rack: TileBag::new_empty(),
                name: "Player 1".to_string(),
            },
            Player {
                player_type: PlayerType::Computer,
                rack: TileBag::new_empty(),
                name: "Player 2".to_string(),
            },
        ];
        players[0].rack.fill_rack(&mut bag);
        players[1].rack.fill_rack(&mut bag);
        let board = board::Board::new(scrabble_variant);
        let next_player = 0;
        let moves = Vec::new();
        //let local_word_list = word_list::generate_anagrams(&players[1].rack);
        Self {
            scrabble_variant,
            players,
            board,
            bag,
            next_player,
            first_move: true,
            is_over: false,
            moves,
            scores: [0, 0],
            //  local_word_list,
        }
    }

    // returns the positions of each tile and the positions of filled cells

    pub fn validate_position(&self, game_move: &mut GameMove) -> Result<(), MoveError> {
        game_move.generate_position_map(&self.board)
    }

    pub fn validate_move(
        &self,
        game_move: &mut GameMove,
        tiles: &TileList,
    ) -> Result<(), MoveError> {
        assert_eq!(game_move.player, self.next_player);
        let player = &self.players[self.next_player];
        // check that the tiles are in the rack
        player.rack.confirm_contains_tile_list(&tiles);

        let cross_direction = -game_move.direction;

        let pm = game_move.move_position_map.as_mut().unwrap();
        let positions_length = pm.positions.len();

        // apply tiles to the position map
        let mut word = String::new();
        let mut word_score: u8 = 0;
        let mut connecting_word_scores = 0u8;
        let mut tile_idx = 0;
        for position_map_idx in 0..positions_length {
            let p = &pm.positions[position_map_idx];
            let pt = &mut pm.position_types[position_map_idx];
            match pt {
                MovePositionType::Open { tile: cell_tile } => {
                    let tile = tiles.0[tile_idx];
                    tile_idx += 1;
                    let letter = tile.letter().unwrap();
                    word.push(letter.as_char());
                    *cell_tile = Some(tile);
                    let cell = self.board.get_cell_pos(*p);
                    let letter_multiplier = cell.cell_type.letter_multiplier();
                    word_score +=
                        self.scrabble_variant.letter_values[letter.as_usize()] * letter_multiplier;
                    //word_multiplier *= cell.cell_type.word_multiplier();
                }
                MovePositionType::Connecting {
                    tile: cell_tile,
                    letter_set,
                    start_pos,
                    end_pos,
                    connecting_word,
                } => {
                    let tile = tiles.0[tile_idx];
                    tile_idx += 1;
                    let letter = tile.letter().unwrap();
                    if !letter_set.contains(letter) {
                        return Err(MoveError::LetterNotAllowedInPosition);
                    }
                    word.push(letter.as_char());
                    *cell_tile = Some(tile);
                    let cell = self.board.get_cell_pos(*p);
                    let letter_multiplier = cell.cell_type.letter_multiplier();
                    word_score +=
                        self.scrabble_variant.letter_values[letter.as_usize()] * letter_multiplier;

                    let mut cross_score = 0;
                    let mut cross_start_pos = *p;
                    let mut previous_letters: Vec<Letter> = vec![];
                    while let Some(cross_previous_pos) =
                        cross_start_pos.try_step_backward(&cross_direction)
                    {
                        if let Some(&letter) =
                            self.board.get_cell_pos(cross_previous_pos).as_filled()
                        {
                            previous_letters.push(letter);
                            cross_start_pos = cross_previous_pos;
                            cross_score += self.scrabble_variant.letter_values[letter.as_usize()];
                        } else {
                            break;
                        }
                    }

                    let mut cross_word = String::new();
                    for letter in previous_letters.iter().rev() {
                        cross_word.push(letter.as_char());
                    }
                    cross_word.push(letter.as_char());
                    cross_score +=
                        self.scrabble_variant.letter_values[letter.as_usize()] * letter_multiplier;

                    let mut cross_end_pos = *p;
                    while let Some(cross_next_pos) =
                        cross_end_pos.try_step_forward(&cross_direction)
                    {
                        if let Some(&letter) = self.board.get_cell_pos(cross_next_pos).as_filled() {
                            cross_word.push(letter.as_char());
                            cross_end_pos = cross_next_pos;
                            cross_score += self.scrabble_variant.letter_values[letter.as_usize()];
                        } else {
                            break;
                        }
                    }

                    *start_pos = Some(cross_start_pos);
                    *end_pos = Some(cross_end_pos);
                    *connecting_word = Some(cross_word);
                    connecting_word_scores += cross_score * cell.cell_type.word_multiplier();
                }
                MovePositionType::Filled { letter } => {
                    word.push(letter.clone().as_char());
                }
            }
        }
        // check that the word is in the word list
        if !is_word(&word) {
            return Err(MoveError::InvalidWord(word));
        }
        game_move.score = word_score as u16 * pm.word_multiplier as u16
            + connecting_word_scores as u16
            + if game_move.length == 7 {
                self.scrabble_variant.bingo_bonus as u16
            } else {
                0
            };
        game_move.word = Some(word);
        Ok(())
    }

    pub fn apply_move(&mut self, game_move: &GameMove) -> Result<u16, MoveError> {
        let word = game_move.word.as_ref().unwrap();
        let score = game_move.score;
        let direction = game_move.direction;
        let cross_direction = -direction;
        let pm = game_move.move_position_map.as_ref().unwrap();
        let positions_length = pm.positions.len();
        let previous_cell_pos = pm.previous_cell_pos.unwrap();
        let next_cell_pos = pm.next_cell_pos.unwrap();

        let player_rack = &mut self.players[self.next_player].rack;

        let mut previous_letter_set = LetterSet::new_empty();
        for letter in tiles::ALPHABET.iter() {
            if is_word(&format!("{}{}", letter.as_char(), word)) {
                previous_letter_set.add(*letter);
            }
        }
        self.board
            .get_cell_pos_mut(previous_cell_pos)
            .value
            .set_letter_set(cross_direction, previous_letter_set);

        let mut next_letter_set = LetterSet::new_empty();
        for letter in ALPHABET.iter() {
            if is_word(&format!("{}{}", word, letter.as_char())) {
                next_letter_set.add(*letter);
            }
        }
        self.board
            .get_cell_pos_mut(next_cell_pos)
            .value
            .set_letter_set(cross_direction, next_letter_set);

        for idx in 0..game_move.length {
            let p = pm.positions[idx as usize];
            let pt = &pm.position_types[idx as usize];
            let cell = self.board.get_cell_pos_mut(p);
            match pt {
                MovePositionType::Open {
                    tile: Some(played_tile),
                } => {
                    cell.set_tile(*played_tile);
                    player_rack.remove_tile(*played_tile);

                    self.board.update_word_gaps(
                        WordPositions {
                            start_pos: p,
                            end_pos: p,
                        },
                        cross_direction,
                    );
                }
                MovePositionType::Connecting {
                    tile: Some(played_tile),
                    letter_set,
                    start_pos,
                    end_pos,
                    connecting_word,
                } => {
                    cell.set_tile(*played_tile);
                    player_rack.remove_tile(*played_tile);

                    self.board.update_word_gaps(
                        WordPositions {
                            start_pos: start_pos.unwrap(),
                            end_pos: end_pos.unwrap(),
                        },
                        cross_direction,
                    );
                }
                _ => {}
            }
        }

        player_rack.fill_rack(&mut self.bag);
        self.scores[self.next_player] += score as u32;

        if player_rack.is_empty() {
            self.is_over = true;
        }
        self.next_player = (self.next_player + 1) % self.players.len();

        Ok(score)
    }

    pub fn human_move(&mut self, tiles: &str) -> Result<u16, MoveError> {
        let tiles = tiles.trim().to_ascii_uppercase();
        let tiles = tiles.as_str().into();
        self.players[self.next_player]
            .rack
            .confirm_contains_tile_list(&tiles)?;

        let length = tiles.len() as u8;
        let mut game_move = GameMove {
            starting_position: Position::new(7, 7),
            player: 0,
            first_move: true,
            direction: Direction::Horizontal,
            length,
            move_position_map: None,
            word: None,
            score: 0,
        };

        //let mut game_move = game_move.clone();
        self.validate_position(&mut game_move)?;

        self.validate_move(&mut game_move, &tiles)?;
        let score = self.apply_move(&game_move)?;
        Ok(score)
    }

    // recursive function to find the best move for a given position
    fn computer_move_position(
        &mut self,
        best_move: &mut GameMove, // the best move found so far, the contents will be updated if a better move is found
        pos_ref_move: &GameMove,  // contains the starting position and min/max tiles
        current_tile_list: TileList,
        current_rack: TileBag,
    ) {
        let mut position_map = best_move.move_position_map.as_ref().unwrap();
        let min_tiles = position_map.min_tiles;
        let max_tiles = position_map.max_tiles;

        if (current_tile_list.0.len() as u8) >= min_tiles {
            //let try_move = &mut best_move.clone();
            let try_move = &mut GameMove {
                starting_position: best_move.starting_position,
                player: best_move.player,
                first_move: best_move.first_move,
                direction: best_move.direction,
                length: current_tile_list.0.len() as u8,
                move_position_map: None,
                word: None,
                score: 0,
            };
            if self.validate_position(try_move).is_ok() {
                if self.validate_move(try_move, &current_tile_list).is_ok() {
                    if try_move.score > best_move.score {
                        *best_move = try_move.clone();
                    }
                }
            }
        }

        if (current_tile_list.0.len() as u8) < max_tiles && !current_rack.is_empty() {
            let has_blanks = current_rack.blanks > 0;
            for letter in ALPHABET {
                if current_rack.count_letter(*letter) > 0 {
                    let mut new_tile_list = current_tile_list.clone();
                    let tile = Tile::Letter(*letter);
                    new_tile_list.0.push(tile);
                    let mut new_rack = current_rack.clone();
                    new_rack.remove_letter(*letter);
                    self.computer_move_position(best_move, new_tile_list, new_rack);
                }
                if has_blanks {
                    let mut new_tile_list = current_tile_list.clone();
                    let tile = Tile::Blank {
                        acting_as_letter: Some(*letter),
                    };
                    new_tile_list.0.push(tile);
                    let mut new_rack = current_rack.clone();
                    new_rack.remove_blank();
                    self.computer_move_position(best_move, new_tile_list, new_rack);
                }
            }
        }
    }

    pub fn computer_move(&mut self) -> Result<u16, MoveError> {
        let player = &self.players[self.next_player];

        let direction = Direction::Horizontal;
        let start_pos = Position::new(7, 7);

        let best_move =
            &mut GameMove::new(self.next_player, direction, self.first_move, start_pos, 7);
        self.validate_position(best_move)?;

        self.computer_move_position(
            best_move,
            Vec::new().into(),
            self.players[self.next_player].rack.clone(),
        );

        let score = self.apply_move(best_move)?;

        Ok(score)
    }
}

//test game
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_game() {
        let game = Game::new(&board::SCRABBLE_VARIANT_WORDFEUD);
        println! {"{}", game};
    }
}
