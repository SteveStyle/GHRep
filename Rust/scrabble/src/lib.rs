use std::{
    collections::HashSet,
    fmt::{Display, Formatter},
    ops::Neg,
    sync::atomic::AtomicBool,
    thread::current,
};

use board::{Board, CellValue, MoveCell, WordPositions};
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
    pub passes: u8,
    pub exchanges: u8,
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

pub struct MovePositionMap {
    positions: Vec<Position>,
    position_types: Vec<MoveCell>,
    previous_cell_pos: Option<Position>,
    next_cell_pos: Option<Position>,
    min_tiles: u8,
    max_tiles: u8,
    word_multiplier: u8,
}

impl MovePositionMap {
    pub fn add(&mut self, position: Position, position_type: MoveCell) {
        self.positions.push(position);
        self.position_types.push(position_type);
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GameMove {
    //    pub player: usize,
    pub starting_position: Position,
    pub direction: Direction,
    pub tiles: TileList,
    pub score: u16,
}

impl GameMove {
    pub fn new(
        starting_position: Position,
        direction: Direction,
        tiles: TileList,
        score: u16,
    ) -> Self {
        Self {
            starting_position,
            direction,
            tiles,
            score,
        }
    }
    pub fn get_main_word_start_pos(&self, board: &board::Board) -> Position {
        let mut current_position = self.starting_position;
        while let Some(next_position) = current_position.try_step_backward(self.direction) {
            if board.get_cell_pos(next_position).is_empty() {
                break;
            }
            current_position = next_position;
        }
        current_position
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Game {
    //  fixed part of the game
    scrabble_variant: &'static board::ScrabbleVariant,
    //  mutable part of the game
    pub players: [Player; 2],
    pub board: board::Board,
    pub bag: tiles::TileBag,
    pub current_player: usize, // index into `players`
    pub first_move: bool,
    pub is_over: bool,
    pub last_player_to_play: Option<usize>,
    pub winner: Option<usize>,
    pub non_scoring_plays: u8,
    pub scores: [u16; 2], // score for each player
    //  history of moves
    pub moves: Vec<GameMove>,
    //local_word_list: HashSet<String>,
}

impl Display for Game {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "\x1B[2J\x1B[1;1H")?;
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
    NotEnoughTilesInBag,
}

impl Game {
    pub fn new(scrabble_variant: &'static board::ScrabbleVariant) -> Self {
        let mut bag = tiles::TileBag::new(scrabble_variant);

        let mut players = [
            Player {
                player_type: PlayerType::Human,
                rack: TileBag::new_empty(),
                name: "Player 1".to_string(),
                passes: 0,
                exchanges: 0,
            },
            Player {
                player_type: PlayerType::Computer,
                rack: TileBag::new_empty(),
                name: "Player 2".to_string(),
                passes: 0,
                exchanges: 0,
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
            current_player: next_player,
            first_move: true,
            is_over: false,
            last_player_to_play: None,
            winner: None,
            non_scoring_plays: 0,
            scores: [0, 0],
            moves,
            //  local_word_list,
        }
    }

    // returns the positions of each tile and the positions of filled cells
    pub fn validate_position(
        &self,
        starting_position: Position,
        direction: Direction,
    ) -> Result<(u8, u8), MoveError> {
        // if valid returns the min and max number of tiles that can be placed
        let mut tiles_placed = 0u8;
        let mut min_tiles = 0u8;
        //let mut max_tiles = 0u8;
        /*  Loop through all the cells involved in the move
           End the loop when:
               - we reach the edge of the board or
               - we reach an empty cell but have already placed all the tiles
        */
        for (current_pos, move_cell) in self.board.move_iterator(starting_position, direction) {
            match move_cell {
                MoveCell::Open => {
                    // we can place a tile here
                    tiles_placed += 1;
                }
                MoveCell::Connecting { letter_set } => {
                    if letter_set.allows_rack(&self.players[self.current_player].rack) {
                        // we can place a tile here
                        tiles_placed += 1;
                        if min_tiles == 0 {
                            min_tiles = tiles_placed;
                        }
                    } else {
                        // we are blocked
                        break;
                    }
                }
                MoveCell::Filled { .. } => {
                    if min_tiles == 0 {
                        if tiles_placed == 0 {
                            min_tiles = 1;
                        } else {
                            min_tiles = tiles_placed;
                        }
                    }
                }
            }
            if min_tiles == 0 && current_pos == (Position { x: 7, y: 7 }) {
                min_tiles = tiles_placed;
            }
        }
        if min_tiles == 0 {
            return Err(MoveError::TilesDoNotConnect);
        }
        Ok((min_tiles, tiles_placed))
    }

    pub fn validate_move(
        &self,
        starting_position: Position,
        direction: Direction,
        tiles: &TileList,
    ) -> Result<u16, MoveError> {
        // returns the score of the move
        // check that the tiles are in the rack
        self.players[self.current_player]
            .rack
            .confirm_contains_tile_list(tiles)?;

        let cross_direction = -direction;

        // apply tiles to the position map
        let mut word = String::new();
        let mut main_word_score: u16 = 0;
        let mut connecting_word_scores = 0u16;
        let mut word_multiplier = 1u8;
        let mut tile_idx = 0;
        let number_of_tiles = tiles.0.len();

        // move iterators goes back to the start of contiguous filled cells
        for (current_pos, move_cell) in self.board.move_iterator(starting_position, direction) {
            match move_cell {
                MoveCell::Open | MoveCell::Connecting { .. } => {
                    // if we are out of tiles then finish
                    if tile_idx >= number_of_tiles {
                        break;
                    }
                    let tile = tiles.0[tile_idx];
                    tile_idx += 1;
                    let letter = tile.letter().unwrap();
                    word.push(letter.as_char());
                    let cell = self.board.get_cell_pos(current_pos);
                    let letter_multiplier = cell.cell_type.letter_multiplier();
                    main_word_score +=
                        (tile.score(self.scrabble_variant) * letter_multiplier) as u16;
                    word_multiplier *= cell.cell_type.word_multiplier();

                    if let MoveCell::Connecting { letter_set } = move_cell {
                        if !letter_set.contains(letter) {
                            return Err(MoveError::LetterNotAllowedInPosition);
                        }
                        connecting_word_scores += self.board.score_cross_word(
                            current_pos,
                            cross_direction,
                            tile,
                            letter,
                        )?;
                    }
                }

                MoveCell::Filled { letter, score } => {
                    word.push(letter.clone().as_char());
                    main_word_score += score;
                }
            }
        }
        // check that the word is in the word list
        if !is_word(&word) {
            return Err(MoveError::InvalidWord(word));
        }
        let score = main_word_score * word_multiplier as u16
            + connecting_word_scores
            + if tile_idx == 7 {
                self.scrabble_variant.bingo_bonus as u16
            } else {
                0
            };
        Ok(score)
    }

    // returns true if the game is over
    pub fn apply_move(
        &mut self,
        starting_position: Position,
        direction: Direction,
        tile_list: &TileList,
        score: u16,
    ) -> Result<bool, MoveError> {
        let player_rack = &mut self.players[self.current_player].rack;
        let cross_direction = -direction;

        let mut tile_vec = tile_list.0.clone();
        tile_vec.reverse();

        let mut current_pos = starting_position;
        loop {
            let cell = self.board.get_cell_pos_mut(current_pos);
            match cell.value {
                CellValue::Empty { .. } => {
                    if let Some(played_tile) = tile_vec.pop() {
                        cell.set_tile(played_tile);
                        player_rack.remove_tile(played_tile);

                        self.board.update_word_gaps(current_pos, cross_direction);
                    } else {
                        break;
                    }
                }
                _ => {}
            }
            if let Some(pos) = current_pos.try_step_forward(direction) {
                current_pos = pos;
            } else {
                break;
            }
        }

        self.board.update_word_gaps(starting_position, direction);

        player_rack.fill_rack(&mut self.bag);
        self.scores[self.current_player] += score as u16;

        if player_rack.is_empty() {
            self.end_game();
        } else {
            self.reset_current_player_stats();
            self.current_player = (self.current_player + 1) % self.players.len();
            self.first_move = false;
        }

        Ok(self.is_over)
    }

    fn exchange_tiles(&mut self, tiles: &TileList) -> Result<(), MoveError> {
        if self.bag.count() < 7 {
            return Err(MoveError::NotEnoughTilesInBag);
        }

        let player_rack = &mut self.players[self.current_player].rack;
        player_rack.confirm_contains_tile_list(tiles)?;
        player_rack.remove_tile_list(tiles);
        self.bag.add_tile_list(tiles);
        player_rack.fill_rack(&mut self.bag);
        self.players[self.current_player].exchanges += 1;
        self.non_scoring_plays += 1;

        if self.non_scoring_plays >= 6 {
            self.end_game();
        } else {
            self.current_player = (self.current_player + 1) % self.players.len();
        }
        Ok(())
    }

    fn pass(&mut self) -> Result<(), MoveError> {
        self.players[self.current_player].passes += 1;
        self.non_scoring_plays += 1;
        if self.non_scoring_plays >= 6 {
            self.end_game();
        } else {
            self.current_player = (self.current_player + 1) % self.players.len();
        }
        Ok(())
    }

    pub fn reset_current_player_stats(&mut self) {
        for player in self.players.iter_mut() {
            player.exchanges = 0;
            player.passes = 0;
        }
    }

    pub fn human_move(&mut self, tiles: &str, move_type: MoveType) -> Result<u16, MoveError> {
        let tiles = tiles.trim().to_ascii_uppercase();
        let tiles = tiles.as_str().into();
        self.players[self.current_player]
            .rack
            .confirm_contains_tile_list(&tiles)?;

        match move_type {
            MoveType::Exchange => {
                self.exchange_tiles(&tiles)?;
                Ok(0)
            }
            MoveType::Pass => {
                self.pass()?;
                Ok(0)
            }
            MoveType::Play => {
                let (min_tiles, max_tiles) =
                    self.validate_position(Position { x: 7, y: 7 }, Direction::Horizontal)?;

                if tiles.len() < min_tiles as usize || tiles.len() > max_tiles as usize {
                    return Err(MoveError::TilesDonNotFit);
                }
                let score =
                    self.validate_move(Position { x: 7, y: 7 }, Direction::Horizontal, &tiles)?;
                let game_over = self.apply_move(
                    Position { x: 7, y: 7 },
                    Direction::Horizontal,
                    &tiles,
                    score,
                )?;
                self.last_player_to_play = Some(self.current_player);
                self.reset_current_player_stats();
                Ok(score)
            }
        }
    }

    // recursive function to find the best move for a given position
    fn computer_move_position(
        &mut self,
        best_move: &mut GameMove, // the best move found so far, the contents will be updated if a better move is found
        starting_position: Position,
        direction: Direction,
        min_tiles: u8,
        max_tiles: u8,
        current_tile_list: TileList,
        current_rack: TileBag,
    ) {
        if current_tile_list.len() as u8 >= max_tiles {
            return;
        }
        if (current_tile_list.len() as u8) >= min_tiles {
            //let try_move = &mut best_move.clone();
            if let Ok(score) = self.validate_move(starting_position, direction, &current_tile_list)
            {
                if score > best_move.score {
                    *best_move = GameMove::new(
                        starting_position,
                        direction,
                        current_tile_list.clone(),
                        score,
                    );
                }
            }
        }

        if (current_tile_list.len() as u8) < max_tiles && !current_rack.is_empty() {
            let has_blanks = current_rack.blanks > 0;
            for &letter in ALPHABET {
                if current_rack.contains(letter) {
                    let mut new_tile_list = current_tile_list.clone();
                    let tile = Tile::Letter(letter);
                    new_tile_list.0.push(tile);
                    let mut new_rack = current_rack.clone();
                    new_rack.remove_letter(letter);
                    self.computer_move_position(
                        best_move,
                        starting_position,
                        direction,
                        min_tiles,
                        max_tiles,
                        new_tile_list,
                        new_rack,
                    );
                }
                if current_rack.blanks > 0 {
                    let mut new_tile_list = current_tile_list.clone();
                    let tile = Tile::Blank {
                        acting_as_letter: Some(letter),
                    };
                    new_tile_list.0.push(tile);
                    let mut new_rack = current_rack.clone();
                    new_rack.remove_blank();
                    self.computer_move_position(
                        best_move,
                        starting_position,
                        direction,
                        min_tiles,
                        max_tiles,
                        new_tile_list,
                        new_rack,
                    );
                }
            }
        }
    }

    pub fn computer_move(&mut self) -> Result<u16, MoveError> {
        let start_pos = Position::new(7, 7);
        let direction = Direction::Horizontal;
        let best_move = &mut GameMove::new(start_pos, direction, TileList::new(), 0);

        for &direction in [Direction::Vertical, Direction::Horizontal].iter() {
            for y in 0..15 {
                for x in 0..15 {
                    let start_pos = Position::new(x, y);
                    if let Ok((min_tiles, max_tiles)) = self.validate_position(start_pos, direction)
                    {
                        self.computer_move_position(
                            best_move,
                            start_pos,
                            direction,
                            min_tiles,
                            max_tiles,
                            TileList::new(),
                            self.players[self.current_player].rack.clone(),
                        );
                    }
                }
            }
        }

        if best_move.score == 0 {
            self.computer_no_move()?;
        } else {
            let game_over = self.apply_move(
                best_move.starting_position,
                best_move.direction,
                &best_move.tiles,
                best_move.score,
            )?;
        }

        Ok(best_move.score)
    }

    fn computer_no_move(&mut self) -> Result<(), MoveError> {
        // if best move has score 0, then we haven't found a move, so we either pass or exchange tiles
        // if we were the last player to play, we pass
        // we can assume that someone can play, it is not possible that no-one can get a playable rack given we can exchange tiles

        if self.bag.count() >= 7 {
            let tiles = self.players[self.current_player].rack.into();
            self.exchange_tiles(&tiles)?;
            return Ok(());
        } else {
            self.pass()?;
            return Ok(());
        }
    }

    fn end_game(&mut self) {
        self.is_over = true;
        // if the current player has no tiles left, they get the sum of all the tiles in the other players racks
        // otherwise, each player has the sum of their rack deducted from their score
        let mut racks_total = 0;
        for player in self.players.iter() {
            racks_total += player.rack.sum_tile_values(self.scrabble_variant);
        }
        let current_player = &self.players[self.current_player];
        if current_player.rack.is_empty() {
            self.scores[self.current_player] += racks_total;
        }
        for (i, player) in self.players.iter().enumerate() {
            if i != self.current_player {
                self.scores[i] -= player.rack.sum_tile_values(self.scrabble_variant);
            }
        }

        let mut max_score = 0;
        let mut winner = 0;
        for (i, player) in self.players.iter().enumerate() {
            let player_score = self.scores[i];
            if player_score > max_score {
                max_score = player_score;
                winner = i;
            }
        }
        self.is_over = true;
        self.winner = Some(winner);
    }
}

pub enum MoveType {
    Play,
    Pass,
    Exchange,
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
