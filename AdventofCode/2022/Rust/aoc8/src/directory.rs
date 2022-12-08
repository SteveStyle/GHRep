pub mod directory {

pub enum DirectoryItem {
    File{ name: String, size: usize},
    Directory{ name: String, dir_items: Vec<DirectoryItem>},
}

impl Clone for DirectoryItem {
    fn clone(&self) -> DirectoryItem {
        match self {
            DirectoryItem::File{ name, size } => DirectoryItem::File{
                name: name.clone(),
                size: *size,
            },
            DirectoryItem::Directory{ name, dir_items } => DirectoryItem::Directory{
                name: name.clone(),
                dir_items: dir_items.iter().map(|x| x.clone()).collect(),
            },
        }
    }
}

impl DirectoryItem {

    pub fn add(&mut self, d: &DirectoryItem) -> &DirectoryItem {
        match self {
            DirectoryItem::Directory{ name: _, dir_items } => { 
                dir_items.push(d.clone()); 
                return dir_items.last().unwrap();
            },
            DirectoryItem::File{ name:_, size:_ } => panic!("DirectoryItem.add() can only be called on a DirectoryItem::Directory.  Attempt to call on DirectoryItem::File"),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            DirectoryItem::File{ name: _, size } => *size,
            DirectoryItem::Directory{ name: _, dir_items } => dir_items.iter().map( |x| x.size() ).sum(),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            DirectoryItem::Directory { name, dir_items: _ } => name,
            DirectoryItem::File { name, size: _ } => name,
        }
    }

    pub fn get_directory_sizes(&self,threshold :usize) -> ( Vec<(usize, String)>, usize ) {
        let mut result : Vec<(usize, String)> = vec![];
        for item in self.clone() {
            let item_size = item.size();
            if item_size <= threshold {
                if let DirectoryItem::Directory { name, dir_items: _ } = item {
                    result.push( (item_size, name) );
                }
            }
        }
        result.sort();
        result.reverse();
        let total = result.iter().map( |(size, _name)| size ).sum();
        return ( result, total );
    }

}



impl IntoIterator for DirectoryItem {
    type Item = DirectoryItem;
    type IntoIter = DirectoryItemIter;

    fn into_iter(self) -> Self::IntoIter {
        DirectoryItemIter { items: vec![self] }
    }
}

pub struct DirectoryItemIter  {
    items: Vec<DirectoryItem>,
}

impl Iterator for DirectoryItemIter {
    type Item = DirectoryItem;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.items.pop()?;

        if let DirectoryItem::Directory { name: _, ref dir_items } = item {
            self.items.extend(dir_items.clone());
        }

        Some(item)
    }
}





}

#[cfg(test)]
mod tests {
    use super::directory::*;
    
    fn sample_directory() -> DirectoryItem {
        let mut root = DirectoryItem::Directory{ name: String::from("root"), dir_items: vec![] };
        root.add( &DirectoryItem::File{name: String::from("r 1"), size: 100} );
        root.add( &DirectoryItem::File{name: String::from("r 2"), size: 200} );
        root.add( &DirectoryItem::File{name: String::from("r 3"), size: 300} );

        let mut a = DirectoryItem::Directory{name: String::from("a"), dir_items: vec![]};
        a.add( &DirectoryItem::File{name: String::from("a 1"), size: 100} );
        a.add( &DirectoryItem::File{name: String::from("a 2"), size: 2000} );
        a.add( &DirectoryItem::File{name: String::from("a 3"), size: 3000} );
        root.add(&a);

        let mut aa = DirectoryItem::Directory{name: String::from("aa"), dir_items: vec![]};
        aa.add( &DirectoryItem::File{name: String::from("aa 1"), size: 1100} );
        aa.add( &DirectoryItem::File{name: String::from("aa 2"), size: 205} );
        aa.add( &DirectoryItem::File{name: String::from("aa 3"), size: 3003} );
        root.add(&aa);

        let mut b = DirectoryItem::Directory{name: String::from("b"), dir_items: vec![]};
        b.add( &DirectoryItem::File{name: String::from("b 1"), size: 1007} );
        b.add( &DirectoryItem::File{name: String::from("b 2"), size: 2002} );
        b.add( &DirectoryItem::File{name: String::from("b 3"), size: 4500} );
        root.add(&b);

        return root;
    }
    
    
    #[test]
    fn test_create_directory() {
        let root = sample_directory();
        println!("the name is {} and it has size {}", root.name(), root.size());

        for item in root.clone() {
            let item_size = &item.size();
            match item {
                DirectoryItem::File { name, size } => {
                    println!("Found file {} with size {}", name, size);
                }
                DirectoryItem::Directory {  name,  dir_items } => {
                    println!("Found directory {} with {} items and size {}", name, dir_items.len(), item_size);
                }
            }
        };

        let ( v, total ) = root.get_directory_sizes(5000);
        for (dir_size, dir_name) in v {
            println!("{}\t{}",dir_size,dir_name);
        }
        println!("the total is {}",total);

    }

}
