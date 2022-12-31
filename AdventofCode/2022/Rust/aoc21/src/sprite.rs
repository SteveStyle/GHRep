pub(crate) mod sprite {
    use std::ops::{Add, Sub};
    use std::fmt::{Display, self, Debug};




    #[derive(Debug, Clone, Copy)]
    struct Pos {
        x: i32,
        y: i32,
    }
    
    impl Add for Pos {
        type Output = Pos;
    
        fn add(self, rhs: Self) -> Self::Output {
            Pos { x: self.x + rhs.x, y: self.y + rhs.y }
        }
    }
    
    impl Sub for Pos {
        type Output = Pos;
    
        fn sub(self, rhs: Self) -> Self::Output {
            Pos { x: self.x - rhs.x, y: self.y - rhs.y }
        }
    }

    impl PartialEq for Pos {
        fn eq(&self, other: &Self) -> bool {
            self.x == other.x && self.y == other.y
        }
    }
    
    impl Eq for Pos { }

    #[derive(Debug, Clone)]
    struct Grid  {
        width: usize,
        height: usize,
        data: Vec<Vec<char>>,
    }

    impl Display for Grid {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut result = writeln!(f, "width: {}, height: {}",self.width, self.height);
        for y in 0..self.height {
            for x in 0..self.width {        
                result = write!(f, "{}", self.data[x][self.height - y -1]);
            }
            result = writeln!(f,"");
        }
        return result;
/*        let mut data = self.data.clone();
        let mut new_data : Vec<Vec<char>> = vec![];
        for i in 0..data.len() {
            let v = data[i].clone();
            v.reverse();
            new_data.push(v);
        }
*/
    }
    }
    

    impl Grid {
        fn new(width : usize, new_rows: Vec<&str>) -> Grid {
            let mut grid = Grid { width, height: 0, data: vec![vec![]; width] };
            grid.add_rows(new_rows);
            grid
        }
        fn add_row(&mut self, new_row: &str) {
            for (idx, c) in new_row.clone().chars().enumerate() {
                self.data[idx].push(c);
            }
            self.height += 1;
        }
        fn add_rows(&mut self, new_rows: Vec<&str>) {
            for new_row in new_rows {
                self.add_row(&new_row);
            }
        }
        fn set_pos(&mut self,p:Pos) {
            self.data[p.x as usize][p.y as usize] = '#';
        }
        fn is_set(&self, p:Pos) -> bool {
            self.data[p.x as usize][p.y as usize] != '.'
        }
        fn overlaps(&self, offset: Pos, shape: &Grid) -> bool {
            for x in 0..shape.width {
                for y in 0..shape.height {
                    let p = Pos{x : x as i32, y: y as i32};
                    if self.is_set( offset + p) && shape.is_set(p) { return true; }
                }
            }
            return false;
        }
        fn set_shape(&mut self, offset: Pos, shape: &Grid) -> i32 {
            // returns the highest rock position
            let mut rock_height = 0;
            for x in 0..shape.width {
                for y in 0..shape.height {
                    let p = Pos{x : x as i32, y: y as i32};
                    if shape.is_set(p) {
                        self.set_pos( offset + p);
                        if (offset+p).y > rock_height { rock_height = (offset+p).y; }
                    }
                }
            }
            //println!("rock height: {}",rock_height);
            rock_height
        }
    }

    #[derive(Debug, Clone)]
    pub struct Sprite {
        gas_direction : Vec<char>,
        next_gas_move : usize,
        pos : Pos,
        next_move_fall : bool,
        env : Grid,
        rock_height : i32,
        shapes : Vec<Grid>,
        shape : usize,
        no_rocks : usize,
    }

    impl Sprite {
        fn new_shapes() -> Vec<Grid> {
            let mut result = vec![];
            result.push( Grid::new( 4, vec!["####"]));
            result.push( Grid::new( 3, vec![".#.","###",".#."]));
            result.push( Grid::new( 3, vec!["###","..#","..#"]));
            result.push( Grid::new( 1, vec!["#","#","#","#"]));
            result.push( Grid::new( 2, vec!["##","##"]));
            result
        }
        pub fn new(contents: &str) -> Sprite {
            let env = Grid::new(9,vec!["+-------+","|.......|","|.......|","|.......|","|.......|"]);
            let shapes = Self::new_shapes();
            Sprite { 
                gas_direction: contents.chars().collect(), 
                next_gas_move: 0, 
                pos: Pos { x: 3, y: 4 }, 
                next_move_fall : false,
                env, 
                rock_height: 0,
                shapes,
                shape: 0,
                no_rocks : 0,
             }
        }
        fn reset_shape(&mut self) {
            self.shape = (self.shape + 1)%5;
            self.set_env_height(self.rock_height + 8);
            self.pos = Pos { x: 3, y: self.rock_height + 4 }
        }
        fn set_env_height( &mut self, new_height : i32 ) {
            while self.env.height < new_height as usize { self.env.add_row("|.......|"); }
        }
        fn move_rock_once(&mut self) -> bool {
            // returns true if the rock moved, false if it came to rest
            let this_move_fall = self.next_move_fall;
            self.next_move_fall = !self.next_move_fall;
            
            let displacement : Pos =
                if this_move_fall {
                    Pos{x:0,y:-1}
                } else {
                    let gas_direction  = self.gas_direction[self.next_gas_move];
                    self.next_gas_move = (self.next_gas_move + 1)%self.gas_direction.len();
                    //print!("{}",gas_direction);
                    match gas_direction {
                        '<' => Pos{x:-1,y:0},
                        '>' => Pos{x:1,y:0},
                        _ => panic!["gas direction not < or >, {}", gas_direction],
                    }
                };
            let shape = self.shapes[self.shape].clone();
            if self.env.overlaps(displacement + self.pos, &shape) {
                if this_move_fall {
                    // we are falling and have hit something below, so we come to rest without moving
                    let top_of_rock = self.env.set_shape( self.pos, &shape);
                    if top_of_rock > self.rock_height {self.rock_height = top_of_rock;}
                    self.reset_shape();
                    self.no_rocks += 1;
                    return false;
                } else {
                    // we are moving sideways but hit the wall, so we just don't move.  Next move we will fall.
                    return true;
                }
            } else {
                // we have no problem moving, in whatever direction it was
                self.pos = displacement + self.pos;
                return true;
            }
            true
        }
        fn move_rock(&mut self) {
            while self.move_rock_once() { }
            //println!("{}",self.env);
        }
        pub fn move_rocks(&mut self, no_rocks : usize) -> i32 {
            for i in 0..no_rocks { self.move_rock(); }
            return self.rock_height;
        }
        pub fn find_cycle(&mut self, limit_rocks : usize, limit_repetitions : usize) -> u128 {
            const target_rocks : u128 = 1000000000000;
            let mut rock_height = self.move_rocks(2000) as u128;
            let mut no_rocks = self.no_rocks as u128;
            let mut delta_rocks = 0;
            let mut delta_rock_height = 0;
            let mut state = (self.shape, self.next_gas_move);
            let mut repetitions = 0;
//            let mut state = (self.no_rocks, self.next_gas_move, self.rock_height);
            let mut v = vec![(self.no_rocks, self.rock_height)];
            println!("initial state: shape {}, next gas move {}",self.shape, self.next_gas_move);
            println!("initial state: no rocks {}, rock height {}",self.no_rocks, self.rock_height);
            for i in 0..limit_rocks {
                self.move_rock();
                if state == (self.shape, self.next_gas_move) {
                    println!("state: no rocks {}, rock height {}",self.no_rocks, self.rock_height);
                    delta_rocks = self.no_rocks as u128 - no_rocks;
                    delta_rock_height = self.rock_height as u128 - rock_height;
                    println!("delta: no rocks {}, rock height {}",delta_rocks, delta_rock_height);
                    no_rocks = self.no_rocks as u128;
                    rock_height = self.rock_height as u128;
                    v.push((self.no_rocks, self.rock_height));
                    repetitions += 1;
                    if repetitions >= limit_repetitions {break;}
                }
            }
            let reduced_target = ((target_rocks - no_rocks)%(delta_rocks) ) + no_rocks;
            let no_repetitions = (target_rocks - reduced_target)/delta_rocks as u128;
            let mut new_sprite = Sprite::new("");
            new_sprite.gas_direction = self.gas_direction.clone();
            println!("target_rocks: {}\nreduced target: {}\nno_repetitions: {}",target_rocks, reduced_target, no_repetitions);
            return (new_sprite.move_rocks(reduced_target as usize) as u128 + no_repetitions * delta_rock_height );

        }
    }
    
}