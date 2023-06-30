const ALPHABET: &str = "abcdefghijklmnopqrstuvwxyz";

pub struct Namer {
    letter: usize,
    index: usize
}

impl Namer {
    pub fn new() -> Namer {
        return Namer { letter: 0, index: 0 }
    }

    pub fn name(&mut self) -> String {
        let new_id = if self.index != 0 {
            format!("{}{}", ALPHABET.chars().nth(self.letter).unwrap(), self.index) 
        } else {
            format!("{}", ALPHABET.chars().nth(self.letter).unwrap()) 
        };

        self.letter += 1;
        if self.letter > ALPHABET.len() {
            self.letter = 0;
            self.index += 1;
        }
        new_id
    }
}