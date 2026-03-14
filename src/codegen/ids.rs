use crate::parser::parse_context;
use parse_context::{IdString, ParseContext};
use rand::{
    Rng, SeedableRng,
    distr::{Distribution, Uniform},
};
use rand_xoshiro::Xoshiro256StarStar;
use serde::{Deserialize, Serialize};
use std::sync::OnceLock;

impl From<ParseContext> for Xoshiro256StarStar {
    fn from(value: ParseContext) -> Self {
        Self::from_seed(value.random_seed)
    }
}

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

pub fn generate_ids_for_context<T: Rng>(context: &mut ParseContext, rng: &mut T) {
    for target in context.parsed_targets.iter_mut() {
        for descriptor in target.borrow_symbols_mut().values_mut() {
            descriptor.assign_id(Some(
                descriptor
                    .derive_id_if_possible()
                    .unwrap_or_else(|| generate_random_id(rng)),
            ));
        }
    }
    for descriptor in context.broadcasts.values_mut() {
        descriptor.id = Some(generate_random_id(rng));
    }
}
