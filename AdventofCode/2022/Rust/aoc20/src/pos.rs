pub mod pos;

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