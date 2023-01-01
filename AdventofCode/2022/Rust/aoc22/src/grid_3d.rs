pub mod grid_3d {



use crate::pos3d::pos3d::*;
use crate::utils::utils::get_numbers;

pub struct Grid3d {
    max_x : usize,
    max_y : usize,
    max_z : usize,
    data: Vec<Vec<Vec<bool>>>,
    open_cells: Vec<Vec<Vec<bool>>>,
    pub surface_area : u128,
    pub surface_area2 : u128,
}

impl Grid3d {
    fn is_set( &self, p : &Pos3  ) -> bool {
        self.data[p.x as usize][p.y as usize][p.z as usize]
    }
    fn set( &mut self, p : &Pos3, value : bool ) {
        self.data[p.x as usize][p.y as usize][p.z as usize] = value;
    }

    fn get_neighbours(&self, p : &Pos3) -> Vec<Pos3> {
        let mut result = vec![];
        let p = p.clone();
        if p.x >0 { result.push(p - UX); }
        if p.x < self.max_x as isize {result.push(p + UX);}
        if p.y >0 { result.push(p - UY); }
        if p.y < self.max_y as isize {result.push(p + UY);}
        if p.z > 0 { result.push(p - UZ) }
        if p.z < self.max_z as isize {result.push(p + UZ);}
        
        result
    }

    fn count_neighbours(&self, p : &Pos3) -> u128 {
        let mut result = 0;
        let p = p.clone();
        for n in &self.get_neighbours(&p) { if self.is_set(n) { result += 1}}

        result        
    }

    pub fn new(contents : &str) -> Grid3d {
        let mut pos_list = vec![];
        for line in contents.lines() {
            let coordinates = get_numbers(line);
            assert_eq!(coordinates.len(),3,"Found {} numbers reading line.", coordinates.len());
            let p = Pos3{x: coordinates[0], y: coordinates[1], z: coordinates[2]};
            pos_list.push(p);
        }
        let max_x = pos_list.iter().map(|p| p.x).max().unwrap() as usize;
        let max_y = pos_list.iter().map(|p| p.y).max().unwrap() as usize;
        let max_z = pos_list.iter().map(|p| p.z).max().unwrap() as usize;
        
        let mut grid = Grid3d{ data: vec![vec![vec![false;max_z +2];max_y +2];max_x +2], max_x, max_y, max_z, surface_area: 0, surface_area2: 0, open_cells: vec![vec![vec![false;max_z +2];max_y +2];max_x +2] };
        for p in &pos_list { grid.set(&p, true); }
        grid.surface_area = pos_list.iter().map(|p| 6 - grid.count_neighbours(p)).sum();
        grid.set_open_cells(&Pos3{x:0,y:0,z:0});
        grid.set_open_cells(&Pos3{x:max_x as isize,y:max_y as isize,z:max_z as isize});
        grid.surface_area2 = pos_list.iter().map(|p| grid.count_open_neighbours(p)).sum();

        grid
    }

    pub fn set_open_cells(&mut self, starting_cell : &Pos3) {
        self.open_cells[starting_cell.x as usize][starting_cell.y  as usize][starting_cell.z as usize] = true;
        for n in &self.get_neighbours(starting_cell) {
            if !self.is_set(n) && !self.open_cells[n.x as usize][n.y as usize][n.z as usize] {
                self.set_open_cells(n);
            }
        }
    }

    pub fn count_open_neighbours(&self, p : &Pos3) -> u128 {
        //count closed neighbours and deduct from 6
        let mut closed : u128 = 0;
        for n in &self.get_neighbours(p) {
            if !self.open_cells[n.x as usize][n.y as usize][n.z as usize] {closed += 1;}
        }
        return 6 - closed;
    }

}

}