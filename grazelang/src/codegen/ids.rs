use crate::parser::parse_context;
use parse_context::IdString;
use rand::{
    Rng,
    distr::{Distribution, Uniform},
};
use serde::{Deserialize, Serialize};
use std::sync::OnceLock;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdCounter {
    pub index: usize,
}

impl IdCounter {
    pub fn new() -> Self {
        Self { index: 0 }
    }

    pub fn get_new_id(&mut self) -> <Self as Iterator>::Item {
        // yea i prematurely optimized it a bit
        let mut n = self.index;
        self.index += 1;
        let mut string = [0u8; 22];
        let mut cursor = string.len();
        loop {
            cursor -= 1;
            string[cursor] = ID_SOUP[n % ID_SOUP.len()];
            n /= ID_SOUP.len();
            if n == 0 {
                break;
            }
        }
        str::from_utf8(&string[cursor..]).unwrap().into()
    }
}

impl Default for IdCounter {
    fn default() -> Self {
        Self::new()
    }
}

impl Iterator for IdCounter {
    type Item = IdString;
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.get_new_id())
    }
}

const ID_SOUP: &[u8] =
    b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$%()*+,-./:;=?@[]^_`{|}~";
const ID_LENGTH: usize = 20;

pub fn generate_random_id<T: Rng>(rng: &mut T) -> IdString {
    static UNIFORM_LOCK: OnceLock<Uniform<usize>> = OnceLock::new();
    let uniform = UNIFORM_LOCK.get_or_init(|| Uniform::new(0, ID_SOUP.len()).unwrap());
    let mut id = String::with_capacity(ID_LENGTH);
    for char_idx in uniform.sample_iter(rng).take(ID_LENGTH) {
        id.push(ID_SOUP[char_idx] as char);
    }
    id.into()
}
