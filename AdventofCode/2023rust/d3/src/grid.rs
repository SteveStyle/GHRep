use std::{fmt::Display, os::windows::process};

#[derive(Clone, Copy, Debug, PartialEq)]
enum CellType {
    Empty,
    Digit,
    Symbol,
}

impl CellType {
    fn from_char(c: char) -> CellType {
        match c {
            '0'..='9' => CellType::Digit,
            '.' => CellType::Empty,
            _ => CellType::Symbol,
        }
    }
    fn is_empty(&self) -> bool {
        match self {
            CellType::Empty => true,
            _ => false,
        }
    }
    fn is_digit(&self) -> bool {
        match self {
            CellType::Digit => true,
            _ => false,
        }
    }
    fn is_symbol(&self) -> bool {
        match self {
            CellType::Symbol => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct PartNum {
    num: u32,
    row: usize,
    col_start: usize,
    col_end: usize,
}

impl PartNum {
    fn new(num: u32, row: usize, col_start: usize, col_end: usize) -> PartNum {
        PartNum {
            num,
            row,
            col_start,
            col_end,
        }
    }
    fn next_to_cell(&self, row: i32, col: i32) -> bool {
        if row < 0 || col < 0 {
            return false;
        }
        if row < self.row as i32 - 1 || row > self.row as i32 + 1 {
            return false;
        }
        if col < self.col_start as i32 - 1 || col > self.col_end as i32 + 1 {
            return false;
        }
        true
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Grid {
    rows: Vec<String>,
    no_rows: usize,
    no_cols: usize,
    part_nums: Vec<PartNum>,
}

impl Grid {
    pub(crate) fn from_string(input: &str) -> Grid {
        let rows: Vec<String> = input.lines().map(|s| s.to_string()).collect();
        let no_rows = rows.len();
        let no_cols = rows[0].len();
        let mut part_nums = Vec::new();
        for row in 0..no_rows {
            let mut col_start = 0;
            let mut col_end = 0;
            let mut num = 0;
            let mut processing_num = false;
            for col in 0..no_cols {
                let c = rows[row].chars().nth(col).unwrap();
                let cell_type = CellType::from_char(c);
                if cell_type.is_digit() {
                    if !processing_num {
                        col_start = col;
                        num = 0;
                        processing_num = true;
                    }
                    col_end = col;
                    num = num * 10 + c.to_digit(10).unwrap();
                } else {
                    if processing_num {
                        part_nums.push(PartNum::new(num, row, col_start, col_end));
                        processing_num = false;
                    }
                }
            }
            if processing_num {
                part_nums.push(PartNum::new(num, row, col_start, col_end));
            }
        }
        Grid {
            rows,
            no_rows,
            no_cols,
            part_nums,
        }
    }
    fn cell_type(&self, row: i32, col: i32) -> CellType {
        if row < 0 || col < 0 {
            return CellType::Empty;
        }
        let row = row as usize;
        let col = col as usize;
        if row >= self.no_rows || col >= self.no_cols {
            return CellType::Empty;
        }
        CellType::from_char(self.rows[row].chars().nth(col).unwrap())
    }
    fn next_to_symbol(&self, part_num: PartNum) -> bool {
        if self
            .cell_type(part_num.row as i32, part_num.col_start as i32 - 1)
            .is_symbol()
            || self
                .cell_type(part_num.row as i32, part_num.col_end as i32 + 1)
                .is_symbol()
        {
            return true;
        }
        for col in part_num.col_start as i32 - 1..=part_num.col_end as i32 + 1 {
            if self
                .cell_type(part_num.row as i32 - 1, col as i32)
                .is_symbol()
                || self
                    .cell_type(part_num.row as i32 + 1, col as i32)
                    .is_symbol()
            {
                return true;
            }
        }
        return false;
    }

    pub(crate) fn sum_of_part_nums_next_to_symbol(&self) -> u32 {
        let mut sum = 0;
        for part_num in &self.part_nums {
            if self.next_to_symbol(*part_num) {
                sum += part_num.num;
            }
        }
        sum
    }
    pub(crate) fn sum_of_products_of_gears(&self) -> u32 {
        let mut sum = 0;
        for row in 0..self.no_rows {
            for col in 0..self.no_cols {
                let c = self.rows[row].chars().nth(col).unwrap();
                let cell_type = CellType::from_char(c);
                if cell_type.is_symbol() {
                    let mut product = 1;
                    let mut count: u8 = 0;
                    for part_num in &self.part_nums {
                        if part_num.next_to_cell(row as i32, col as i32) {
                            count += 1;
                            product *= part_num.num;
                        }
                    }
                    if count == 2 {
                        sum += product;
                    }
                }
            }
        }
        sum
    }
}

mod tests {
    use super::*;

    const EXAMPLE_INPUT: &str = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..";

    #[test]
    fn test_new_grid() {
        let grid = Grid::from_string(EXAMPLE_INPUT);
        println!("{:?}", grid);
        assert_eq!(grid.no_rows, 10);
        assert_eq!(grid.no_cols, 10);
        assert_eq!(grid.part_nums.len(), 10);
        assert_eq!(grid.part_nums[0].num, 467);
        assert_eq!(grid.part_nums[0].row, 0);
        assert_eq!(grid.part_nums[0].col_start, 0);
        assert_eq!(grid.part_nums[0].col_end, 2);
        assert_eq!(grid.part_nums[1].num, 114);
        assert_eq!(grid.part_nums[1].row, 0);
        assert_eq!(grid.part_nums[1].col_start, 5);
        assert_eq!(grid.part_nums[1].col_end, 7);
        assert_eq!(grid.part_nums[2].num, 35);
        assert_eq!(grid.part_nums[2].row, 2);
        assert_eq!(grid.part_nums[2].col_start, 2);
        assert_eq!(grid.part_nums[2].col_end, 3);
        assert_eq!(grid.part_nums[3].num, 633);
        assert_eq!(grid.part_nums[3].row, 2);
        assert_eq!(grid.part_nums[3].col_start, 6);
        assert_eq!(grid.part_nums[3].col_end, 8);
        assert_eq!(grid.part_nums[4].num, 617);
        assert_eq!(grid.part_nums[4].row, 4);
        assert_eq!(grid.part_nums[4].col_start, 0);
        assert_eq!(grid.part_nums[4].col_end, 2);
    }
}
