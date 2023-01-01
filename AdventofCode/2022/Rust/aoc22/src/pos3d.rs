pub mod pos3d {

use crate::utils::*;
use std::ops::Add;
use std::ops::Sub;

#[derive(Debug, Clone, Copy)]
    pub struct Pos3 {
        pub x: isize,
        pub y: isize,
        pub z: isize,
    }

    pub const UX : Pos3 = Pos3{x:1,y:0,z:0};
    pub const UY : Pos3 = Pos3{x:0,y:1,z:0};
    pub const UZ : Pos3 = Pos3{x:0,y:0,z:1};
    
    impl Add for Pos3 {
        type Output = Pos3;
    
        fn add(self, rhs: Self) -> Self::Output {
            Pos3 { x: self.x + rhs.x, y: self.y + rhs.y, z: self.z + rhs.z }
        }
    }
    
    impl Sub for Pos3 {
        type Output = Pos3;
    
        fn sub(self, rhs: Self) -> Self::Output {
            Pos3 { x: self.x - rhs.x, y: self.y - rhs.y, z: self.z - rhs.z }
        }
    }

    impl PartialEq for Pos3 {
        fn eq(&self, other: &Self) -> bool {
            self.x == other.x && self.y == other.y && self.z == other.z
        }
    }
    
    impl Eq for Pos3 { }

}