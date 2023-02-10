use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use crate::pos::Position;
use crate::tiles::Letter;
use crate::tiles::ALPHABET;
use crate::Direction;

use crate::tiles::LetterSet;
use crate::tiles::Tile;
use crate::word_list::is_word;

pub(crate) const BOARD_SIZE: u8 = 15;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum CellValue {
    // The possible letters when playing horizontally or vertically, so that this cell can be filled and make a valid word
    // in the other direction
    Empty {
        horizontal_letters: LetterSet,
        vertical_letters: LetterSet,
    },
    Filled {
        letter: Letter,
        is_blank: bool,
    },
}

impl Display for CellValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty {
                horizontal_letters,
                vertical_letters,
            } => write!(
                f,
                "Empty {{ horizontal_letters: {}, vertical_letters: {} }}",
                horizontal_letters, vertical_letters
            ),
            Self::Filled { letter, is_blank } => write!(f, "Filled {{ letter: {} }}", letter),
        }
    }
}

impl CellValue {
    pub(crate) fn as_filled(&self) -> Option<&Letter> {
        if let Self::Filled { letter, is_blank } = self {
            Some(letter)
        } else {
            None
        }
    }

    pub fn is_vertically_connected(&self) -> bool {
        match self {
            Self::Empty {
                horizontal_letters,
                vertical_letters,
            } => !vertical_letters.is_empty(),
            Self::Filled { .. } => true,
        }
    }

    pub fn is_horizontally_connected(&self) -> bool {
        match self {
            Self::Empty {
                horizontal_letters,
                vertical_letters,
            } => !horizontal_letters.is_empty(),
            Self::Filled { .. } => true,
        }
    }

    pub fn is_empty_connected(&self) -> bool {
        match self {
            Self::Empty {
                horizontal_letters,
                vertical_letters,
            } => !horizontal_letters.is_empty() || !vertical_letters.is_empty(),
            Self::Filled { .. } => false,
        }
    }

    pub fn is_connected(&self) -> bool {
        self.is_vertically_connected() || self.is_horizontally_connected()
    }

    pub fn allowed_letters(&self, direction: Direction) -> LetterSet {
        match self {
            Self::Empty {
                horizontal_letters,
                vertical_letters,
            } => match direction {
                Direction::Horizontal => *horizontal_letters,
                Direction::Vertical => *vertical_letters,
            },
            Self::Filled { letter, is_blank } => LetterSet::from(*letter),
        }
    }

    /// Returns `true` if the cell value is [`Filled`].
    ///
    /// [`Filled`]: CellValue::Filled
    #[must_use]
    pub(crate) fn is_filled(&self) -> bool {
        matches!(self, Self::Filled { .. })
    }

    pub(crate) fn try_into_filled(self) -> Result<Letter, Self> {
        if let Self::Filled { letter, is_blank } = self {
            Ok(letter)
        } else {
            Err(self)
        }
    }

    /// Returns `true` if the cell value is [`Empty`].
    ///
    /// [`Empty`]: CellValue::Empty
    #[must_use]
    pub(crate) fn is_empty(&self) -> bool {
        matches!(self, Self::Empty { .. })
    }

    pub(crate) fn try_into_empty(self) -> Result<(LetterSet, LetterSet), Self> {
        if let Self::Empty {
            horizontal_letters,
            vertical_letters,
        } = self
        {
            Ok((horizontal_letters, vertical_letters))
        } else {
            Err(self)
        }
    }

    pub(crate) fn as_empty(&self) -> Option<(&LetterSet, &LetterSet)> {
        if let Self::Empty {
            horizontal_letters,
            vertical_letters,
        } = self
        {
            Some((horizontal_letters, vertical_letters))
        } else {
            None
        }
    }

    pub(crate) fn set_letter_set(&mut self, direction: Direction, letter_set: LetterSet) {
        match self {
            Self::Empty {
                horizontal_letters,
                vertical_letters,
            } => match direction {
                Direction::Horizontal => *horizontal_letters = letter_set,
                Direction::Vertical => *vertical_letters = letter_set,
            },
            Self::Filled { .. } => {}
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CellType {
    Blank,
    DoubleLetter,
    TripleLetter,
    DoubleWord,
    TripleWord,
}

impl From<char> for CellType {
    fn from(c: char) -> Self {
        match c {
            '2' => Self::DoubleLetter,
            '3' => Self::TripleLetter,
            'd' => Self::DoubleWord,
            't' => Self::TripleWord,
            _ => Self::Blank,
        }
    }
}

impl CellType {
    pub(crate) fn as_char(&self) -> char {
        match self {
            Self::Blank => ' ',
            Self::DoubleLetter => '2',
            Self::TripleLetter => '3',
            Self::DoubleWord => 'd',
            Self::TripleWord => 't',
        }
    }

    pub(crate) fn word_multiplier(&self) -> u8 {
        match self {
            Self::DoubleWord => 2,
            Self::TripleWord => 3,
            _ => 1,
        }
    }

    pub(crate) fn letter_multiplier(&self) -> u8 {
        match self {
            Self::DoubleLetter => 2,
            Self::TripleLetter => 3,
            _ => 1,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Cell {
    pub(crate) value: CellValue,
    pub(crate) cell_type: CellType,
}

impl Display for Cell {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.value {
            CellValue::Empty {
                horizontal_letters,
                vertical_letters,
            } => write!(f, "{} ", self.cell_type.as_char()),
            CellValue::Filled { letter, is_blank } => write!(f, "{}", letter),
        }
    }
}

impl Cell {
    pub(crate) fn new(cell_type: CellType) -> Self {
        Self {
            value: CellValue::Empty {
                horizontal_letters: LetterSet::new_full(),
                vertical_letters: LetterSet::new_full(),
            },
            cell_type,
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.value.is_empty()
    }

    pub(crate) fn is_filled(&self) -> bool {
        self.value.is_filled()
    }

    pub(crate) fn as_filled(&self) -> Option<&Letter> {
        self.value.as_filled()
    }

    pub(crate) fn as_empty(&self) -> Option<(&LetterSet, &LetterSet)> {
        self.value.as_empty()
    }

    pub(crate) fn set_value(&mut self, value: CellValue) {
        self.value = value;
    }

    pub(crate) fn set_letter(&mut self, letter: Letter) {
        self.value = CellValue::Filled {
            letter,
            is_blank: false,
        };
    }

    pub(crate) fn set_tile(&mut self, tile: Tile) {
        self.value = CellValue::Filled {
            letter: tile.letter().unwrap(),
            is_blank: tile.is_blank(),
        };
    }

    pub(crate) fn set_empty(&mut self) {
        self.value = CellValue::Empty {
            horizontal_letters: LetterSet::new_full(),
            vertical_letters: LetterSet::new_full(),
        };
    }

    pub(crate) fn set_horizontal_letters(&mut self, letters: LetterSet) {
        if let CellValue::Empty {
            horizontal_letters,
            vertical_letters,
        } = &mut self.value
        {
            *horizontal_letters = letters;
        }
    }

    pub(crate) fn set_vertical_letters(&mut self, letters: LetterSet) {
        if let CellValue::Empty {
            horizontal_letters,
            vertical_letters,
        } = &mut self.value
        {
            *vertical_letters = letters;
        }
    }

    pub(crate) fn set_horizontal_letter(&mut self, letter: Letter) {
        if let CellValue::Empty {
            horizontal_letters,
            vertical_letters,
        } = &mut self.value
        {
            horizontal_letters.add(letter);
        }
    }

    pub(crate) fn set_vertical_letter(&mut self, letter: Letter) {
        if let CellValue::Empty {
            horizontal_letters,
            vertical_letters,
        } = &mut self.value
        {
            vertical_letters.add(letter);
        }
    }

    pub(crate) fn remove_horizontal_letter(&mut self, letter: Letter) {
        if let CellValue::Empty {
            horizontal_letters,
            vertical_letters,
        } = &mut self.value
        {
            horizontal_letters.remove(letter);
        }
    }

    pub(crate) fn remove_vertical_letter(&mut self, letter: Letter) {
        if let CellValue::Empty {
            horizontal_letters,
            vertical_letters,
        } = &mut self.value
        {
            vertical_letters.remove(letter);
        }
    }
}

pub(crate) type BoardLayout = [(u8, u8, CellType); 18];
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScrabbleVariant {
    pub(crate) letter_values: [u8; 26],
    pub(crate) letter_distribution: [u8; 26],
    pub(crate) blanks: u8,
    pub(crate) bingo_bonus: u8,
    pub(crate) board_layout: BoardLayout,
    pub width: u8,
    pub height: u8,
}

pub(crate) const SCRABBLE_VARIANT_OFFICIAL: ScrabbleVariant = ScrabbleVariant {
    letter_values: [
        /*
        A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q , R, S, T, U, V, W, X, Y, Z, */
        1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10,
    ],
    letter_distribution: [
        /*
        A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q , R, S, T, U, V, W, X, Y, Z, */
        9, 2, 2, 4, 12, 2, 3, 2, 9, 1, 1, 4, 2, 6, 8, 2, 1, 6, 4, 6, 4, 2, 2, 1, 2, 1,
    ],
    blanks: 2,
    bingo_bonus: 50,
    board_layout: [
        (0, 0, CellType::TripleWord),
        (3, 0, CellType::DoubleLetter),
        (7, 0, CellType::TripleWord),
        (1, 1, CellType::DoubleWord),
        (5, 1, CellType::TripleLetter),
        (2, 2, CellType::DoubleWord),
        (6, 2, CellType::DoubleLetter),
        (0, 3, CellType::DoubleLetter),
        (3, 3, CellType::DoubleWord),
        (7, 3, CellType::DoubleLetter),
        (4, 4, CellType::DoubleWord),
        (1, 5, CellType::TripleLetter),
        (5, 5, CellType::TripleLetter),
        (2, 6, CellType::DoubleLetter),
        (6, 6, CellType::DoubleLetter),
        (0, 7, CellType::TripleWord),
        (3, 7, CellType::DoubleLetter),
        (7, 7, CellType::DoubleWord),
    ],
    width: 15,
    height: 15,
};

pub const SCRABBLE_VARIANT_WORDFEUD: ScrabbleVariant = ScrabbleVariant {
    letter_values: [
        /*
        A, B, C, D, E, F, G, H, I, J , K, L, M, N, O, P, Q , R, S, T, U, V, W, X, Y,
        Z */
        1, 4, 4, 2, 1, 4, 3, 4, 1, 10, 5, 1, 3, 1, 1, 4, 10, 1, 1, 1, 2, 4, 4, 8, 4,
        10,
        //1, 3, 3, 2, 1, 4, 2, 4, 1, 8 , 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10,
    ],
    letter_distribution: [
        /*
        A , B, C, D, E , F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,  */
        10, 2, 2, 5, 12, 2, 3, 3, 9, 1, 1, 4, 2, 6, 7, 2, 1, 6, 5, 7, 4, 2, 2, 1, 2, 1,
    ],
    blanks: 2,
    bingo_bonus: 40,
    board_layout: [
        (0, 0, CellType::TripleLetter),
        (4, 0, CellType::TripleWord),
        (7, 0, CellType::DoubleLetter),
        (1, 1, CellType::DoubleLetter),
        (5, 1, CellType::TripleLetter),
        (2, 2, CellType::DoubleWord),
        (6, 2, CellType::DoubleLetter),
        (3, 3, CellType::TripleLetter),
        (7, 3, CellType::DoubleWord),
        (0, 4, CellType::TripleWord),
        (4, 4, CellType::DoubleWord),
        (6, 4, CellType::DoubleLetter),
        (1, 5, CellType::TripleLetter),
        (5, 5, CellType::TripleLetter),
        (2, 6, CellType::DoubleLetter),
        (4, 6, CellType::DoubleLetter),
        (0, 7, CellType::DoubleLetter),
        (3, 7, CellType::DoubleWord),
    ],
    width: 15,
    height: 15,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Board {
    pub(crate) variant: &'static ScrabbleVariant,
    pub(crate) cells: [Cell; 225],
}

const VERTICAL_BORDER: char = '|';
const HORIZONTAL_BORDER: char = '─';
// line border is a string consisting of 2*BOARD_SIZE + 1 horizontal borders
const LINE_BORDER: &str = "───────────────────────────────\n";

// TO DO: use unicode box drawing characters
// https://en.wikipedia.org/wiki/Box-drawing_character
// https://www.fileformat.info/info/unicode/block/box_drawing/list.htm

// impl Display as a grid with borders around each cell
impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        for y in 0..15 {
            s.push_str(&LINE_BORDER);
            for x in 0..15 {
                s.push(VERTICAL_BORDER);
                s.push_str(&format!("{}", self.get_cell(x, y)));
            }
            s.push(VERTICAL_BORDER);
            s.push('\n');
        }
        s.push_str(&LINE_BORDER);
        write!(f, "{}", s)
    }
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct WordPositions {
    pub start_pos: Position,
    pub end_pos: Position,
}

impl Board {
    pub(crate) fn get_cell(&self, x: u8, y: u8) -> &Cell {
        &self.cells[(y * 15 + x) as usize]
    }
    pub(crate) fn get_cell_mut<'a>(&'a mut self, x: u8, y: u8) -> &'a mut Cell {
        &mut self.cells[(y * 15 + x) as usize]
    }
    pub(crate) fn get_cell_pos(&self, pos: Position) -> &Cell {
        &self.cells[(pos.y * 15 + pos.x) as usize]
    }
    pub(crate) fn get_cell_pos_mut<'a>(&'a mut self, pos: Position) -> &'a mut Cell {
        &mut self.cells[(pos.y * 15 + pos.x) as usize]
    }
    pub(crate) fn new(variant: &'static ScrabbleVariant) -> Board {
        let mut board = Board {
            variant,
            cells: [Cell {
                value: CellValue::Empty {
                    horizontal_letters: LetterSet::new_full(),
                    vertical_letters: LetterSet::new_full(),
                },
                cell_type: CellType::Blank,
            }; 225],
        };
        for (x, y, cell_type) in variant.board_layout.iter() {
            board.get_cell_mut(*x, *y).cell_type = *cell_type;
            board.get_cell_mut(14 - *x, *y).cell_type = *cell_type;
            board.get_cell_mut(*x, 14 - *y).cell_type = *cell_type;
            board.get_cell_mut(14 - *x, 14 - *y).cell_type = *cell_type;
        }
        board
    }

    pub fn is_enabler(&self, pos: Position) -> bool {
        if pos == (Position { x: 7, y: 7 }) {
            return true;
        }
        let cell = self.get_cell_pos(pos);
        cell.value.is_horizontally_connected() || cell.value.is_vertically_connected()
    }

    pub fn read_word(&self, wp: WordPositions, direction: Direction) -> String {
        // assume positions are aligned along direction
        // assume cells in range are filled with letters

        let mut pos = wp.start_pos;
        let mut word = String::from(self.get_cell_pos(pos).as_filled().unwrap().as_char());
        while let Some(pos) = pos.try_step_forward(&direction) {
            word.push(self.get_cell_pos(pos).as_filled().unwrap().as_char());
            if pos == wp.end_pos {
                break;
            }
        }
        word
    }

    pub fn update_word_gaps(&mut self, central_word: WordPositions, direction: Direction) {
        // assume central word is surrounded by empty cells or the board edge
        let mut previous_word: String = String::new();
        let mut next_word: String = String::new();
        let central_word_string = self.read_word(central_word, direction);

        let mut pos = central_word.start_pos;
        if let Some(pos) = pos.try_step_backward(&direction) {
            // there is an empty cell, not the board edge
            let previous_gap = pos;
            if let Some(pos) = pos.try_step_backward(&direction) {
                if self.get_cell_pos(pos).value.is_filled() {
                    // there is a previous word, not another empty cell or the board edge
                    let mut previous_word_start = pos;
                    let previous_word_end = pos;
                    while let Some(pos) = pos.try_step_backward(&direction) {
                        if self.get_cell_pos(pos).value.is_filled() {
                            previous_word_start = pos;
                        } else {
                            break;
                        }
                    }
                    previous_word = self.read_word(
                        WordPositions {
                            start_pos: previous_word_start,
                            end_pos: previous_word_end,
                        },
                        direction,
                    );
                }
            }
            let mut ls = LetterSet::new_empty();
            for l in ALPHABET {
                if is_word(format!("{}{}{}", previous_word, l, central_word_string).as_str()) {
                    ls.add(*l);
                }
            }
            self.get_cell_pos_mut(previous_gap)
                .value
                .set_letter_set(-direction, ls);
        }

        let mut pos = central_word.end_pos;
        if let Some(pos) = pos.try_step_forward(&direction) {
            // there is an empty cell, not the board edge
            let next_gap = pos;
            if let Some(pos) = pos.try_step_forward(&direction) {
                if self.get_cell_pos(pos).value.is_filled() {
                    // there is a next word, not another empty cell or the board edge
                    let next_word_start = pos;
                    let mut next_word_end = pos;
                    while let Some(pos) = pos.try_step_forward(&direction) {
                        if self.get_cell_pos(pos).value.is_filled() {
                            next_word_end = pos;
                        } else {
                            break;
                        }
                    }
                    next_word = self.read_word(
                        WordPositions {
                            start_pos: next_word_start,
                            end_pos: next_word_end,
                        },
                        direction,
                    );
                }
            }
            let mut ls = LetterSet::new_empty();
            for l in ALPHABET {
                if is_word(format!("{}{}{}", central_word_string, l, next_word).as_str()) {
                    ls.add(*l);
                }
            }
            self.get_cell_pos_mut(next_gap)
                .value
                .set_letter_set(-direction, ls);
        }
    }
}

//test board
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_board() {
        let board = Board::new(&SCRABBLE_VARIANT_WORDFEUD);
        println!("{}", board);
    }

    #[test]
    fn test_chars() {
        for n in 0x2500..0x257F {
            if let Some(c) = std::char::from_u32(n) {
                println!("{}: {}", n, c);
            } else {
                println!("{}: {}", n, "not a char");
            }
        }
    }
}
