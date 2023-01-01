use crate::utils::get_numbers;

pub type Num = isize;

#[derive(Debug,Clone)]
pub struct LLEntry {
    predecessor : usize,
    successor   : usize,
    value       : Num,
}

#[derive(Debug,Clone)]
pub struct NumberList {
    v: Vec<LLEntry>,
}


impl NumberList {

    pub fn find_move(&mut self, n_idx: usize) {
        let LLEntry{ predecessor: old_predecessor, successor: old_successor, value : n } = self.v[n_idx];

        let len = self.v.len() as Num;
        let mut moves = n%(len -1);
        if moves == 0 { return; }
        if moves < 0 { moves += len -1; }

        let mut target_predecessor = n_idx;
        for _i in 0..moves { target_predecessor = self.v[target_predecessor].successor; }
        let target_successor = self.v[target_predecessor].successor;

        // remove the element from the list
        self.v[old_predecessor].successor = old_successor;
        self.v[old_successor].predecessor = old_predecessor;

        // add to the list
        self.v[target_predecessor].successor = n_idx;
        self.v[n_idx].predecessor = target_predecessor;
        self.v[n_idx].successor = target_successor;
        self.v[target_successor].predecessor = n_idx;
    }

    pub fn find_move_all(&mut self) {
        for i in 0..self.v.len() {
            self.find_move(i);
        }
    }

    pub fn get(&self, n : usize) -> Num {
        return self.values_from_0()[n%self.v.len()];
    }
    pub fn new(contents: &str, part: isize ) -> NumberList {
        let mut numbers = get_numbers::<Num>(contents);
        let len = numbers.len();
        let mut v: Vec<LLEntry> = vec![];

        if part == 2 {
            // multiply each element of numbers by 811589153

            for i in &mut numbers {
                *i *= 811589153;
            }
        }

        v.push(LLEntry { predecessor: len -1, successor: 1, value: numbers[0] });
        for i in 1..len-1 {
            v.push(LLEntry { predecessor: i-1, successor: i+1, value: numbers[i] })
        }
        v.push(LLEntry { predecessor: len-2, successor: 0, value: numbers[len-1] });

        let result = NumberList { v };
        return result;
    }
    pub fn values_from_0(&self) -> Vec<Num> {
        let mut idx = self.v.iter().position(|&LLEntry{ predecessor: _, successor: _, value: x}| x== 0).unwrap();
        let mut result = vec![];
        for _i in 0..self.v.len() {
            result.push(self.v[idx].value);
            idx = self.v[idx].successor;
        }
        return result;
    }
    pub fn len(&self) -> usize {
        let v = self.values_from_0();
        for i in 1..v.len() {
            if v[i] == 0 { return i; }
        }        
        return v.len();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_find_move_all() {
        let mut nl = crate::number_list::NumberList::new("1, 2, -3, 3, -2, 0, 4", 1);
        println!("before: {:?}",nl.values_from_0());
        nl.find_move_all();
        assert_eq!(nl.values_from_0(), vec![0, 3, -2, 1, 2, -3, 4]);
        println!("after: {:?}",nl.values_from_0());
        assert_eq!(nl.get(1000),4);
        assert_eq!(nl.get(2000),-3);
        assert_eq!(nl.get(3000),2);
    }

    #[test]
    fn test_find_move_all2() {
        let mut nl = crate::number_list::NumberList::new("1, 2, -3, 3, -2, 0, 4", 2);
        println!("before: {:?}",nl.values_from_0());
        for _ in 0..10 {
            nl.find_move_all();
        }
        assert_eq!(nl.values_from_0(), vec![0, -2434767459, 1623178306, 3246356612, -1623178306, 2434767459, 811589153]);
        println!("after: {:?}",nl.values_from_0());
        assert_eq!(nl.get(1000),811589153);
        assert_eq!(nl.get(2000),2434767459);
        assert_eq!(nl.get(3000),-1623178306);
    }
}