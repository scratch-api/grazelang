/*
 * This file contains slightly modified code from https://github.com/hack-ink/unescaper/
 *
 * MIT License
 *
 * Copyright (c) 2023 Hack Ink
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

//! Unescape the given string.
//! This is the opposite operation of [`std::ascii::escape_default`].

use std::str::Chars;
use thiserror::Error as ThisError;

/// Unescaper's `Result`.
pub type UResult<T> = Result<T, Error>;

/// Unescaper's `Error`.
#[allow(missing_docs)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, ThisError)]
pub enum Error {
    #[error("incomplete str, break at {0}")]
    IncompleteStr(usize),
    #[error("invalid char, {char:?} break at {pos}")]
    InvalidChar { char: char, pos: usize },
    #[error("parse int error, break at {pos}")]
    ParseIntError {
        source: ::std::num::ParseIntError,
        pos: usize,
    },
}
use Error as E;

#[derive(Debug, Clone)]
pub struct CountAwareChars<'a> {
    pub chars: Chars<'a>,
    pub remaining: usize,
    pub peeked: Option<char>,
}

impl<'a> CountAwareChars<'a> {
    pub fn new(mut chars: Chars<'a>, len: usize) -> Self {
        let peeked = chars.next();
        CountAwareChars {
            chars,
            remaining: len,
            peeked,
        }
    }

    pub fn peek(&self) -> Option<char> {
        self.peeked
    }

    pub fn len(&self) -> usize {
        self.remaining
    }

    pub fn is_empty(&self) -> bool {
        self.remaining == 0
    }
}

impl<'a> Iterator for CountAwareChars<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        if self.remaining == 0 {
            return None;
        }
        self.remaining -= 1;
        std::mem::replace(&mut self.peeked, self.chars.next())
    }
}

impl<'a> From<Chars<'a>> for CountAwareChars<'a> {
    fn from(value: Chars<'a>) -> Self {
        CountAwareChars::new(value.clone(), value.count())
    }
}

impl<'a> From<&'a str> for CountAwareChars<'a> {
    fn from(value: &'a str) -> Self {
        CountAwareChars::from(value.chars())
    }
}

/// Unescaper struct which holding the chars cache for unescaping.
#[derive(Debug, Clone)]
pub struct Unescaper<'a> {
    /// [`str`] cache, in reverse order.
    pub chars: CountAwareChars<'a>,
}
impl<'a> Unescaper<'a> {
    /// Build a new [`Unescaper`] from the given [`str`].
    pub fn new(s: &'a str) -> Self {
        Self {
            chars: s.chars().into(),
        }
    }

    /// Unescape the given [`str`].
    pub fn unescape(&mut self) -> UResult<String> {
        let chars_count = self.chars.len();
        let offset = |mut e, remaining_count| {
            let (E::IncompleteStr(pos) | E::InvalidChar { pos, .. } | E::ParseIntError { pos, .. }) =
                &mut e;

            *pos += chars_count - remaining_count - 1;

            e
        };
        let mut unescaped = String::new();

        while let Some(c) = self.chars.next() {
            if c != '\\' {
                unescaped.push(c);

                continue;
            }

            let c = self
                .chars
                .next()
                .ok_or(E::IncompleteStr(chars_count - self.chars.len() - 1))?;
            let c = match c {
                'b' => '\u{0008}',
                'f' => '\u{000c}',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                // https://github.com/hack-ink/unescaper/pull/10#issuecomment-1676443635
                //
                // https://www.ecma-international.org/wp-content/uploads/ECMA-404_2nd_edition_december_2017.pdf
                // On page 4 it says: "\/ represents the solidus character (U+002F)."

                // Added '`' for canonical identifiers
                '\'' | '\"' | '`' | '\\' | '/' => c,
                'u' => self
                    .unescape_unicode_internal()
                    .map_err(|e| offset(e, self.chars.len()))?,
                'x' => self
                    .unescape_byte_internal()
                    .map_err(|e| offset(e, self.chars.len()))?,
                _ => self
                    .unescape_octal_internal(c)
                    .map_err(|e| offset(e, self.chars.len()))?,
            };

            unescaped.push(c);
        }

        Ok(unescaped)
    }

    fn unescape_unicode_internal(&mut self) -> UResult<char> {
        let c = self.chars.next().ok_or(E::IncompleteStr(0))?;

        // \u + { + regex(d*) + }
        if c == '{' {
            let mut unicode = String::new();
            for n in self.chars.by_ref() {
                if n == '}' {
                    break;
                }

                unicode.push(n);
            }

            char::from_u32(
                u32::from_str_radix(&unicode, 16)
                    .map_err(|e| E::ParseIntError { source: e, pos: 0 })?,
            )
            .ok_or(E::InvalidChar {
                char: unicode
                    .chars()
                    .last()
                    .expect("empty unicode will exit earlier; qed"),
                pos: 0,
            })
        }
        // \u + regex(d{4})
        else {
            let mut val = c.to_digit(16).ok_or(E::InvalidChar { char: c, pos: 0 })?;

            for i in 1..4 {
                let next_c = self.chars.next().ok_or(E::IncompleteStr(i))?;
                let digit = next_c.to_digit(16).ok_or(E::InvalidChar {
                    char: next_c,
                    pos: i,
                })?;
                val = (val << 4) | digit;
            }

            char::from_u32(val).ok_or(E::InvalidChar { char: '?', pos: 0 })
        }
    }

    // pub fn unescape_byte(&mut self) -> Result<char> {}
    fn unescape_byte_internal(&mut self) -> UResult<char> {
        let mut val = 0;

        // [0, 256), 16^2
        for i in 0..2 {
            let next_c = self.chars.next().ok_or(E::IncompleteStr(i))?;
            let digit = next_c.to_digit(16).ok_or(E::InvalidChar {
                char: next_c,
                pos: i,
            })?;
            val = (val << 4) | digit;
        }

        char::from_u32(val).ok_or(E::InvalidChar { char: '?', pos: 0 })
    }

    // pub fn unescape_octal(&mut self) -> Result<char> {}
    fn unescape_octal_internal(&mut self, c: char) -> UResult<char> {
        let mut try_push_next = |val: &mut u32| {
            if let Some(digit) = self
                .chars
                .peek()
                .filter(|c| c.is_digit(8))
                .and_then(|_| self.chars.next())
                .and_then(|c| c.to_digit(8))
            {
                *val = (*val << 3) | digit;
            }
        };

        let val = match c {
            // decimal [0, 256) == octal [0, 400)
            // 0 <= first digit < 4
            // \ + regex(d{1,3})
            '0' | '1' | '2' | '3' => {
                let mut val = c.to_digit(8).unwrap();

                try_push_next(&mut val);
                try_push_next(&mut val);

                val
            }
            // \ + regex(d{1,2})
            '4' | '5' | '6' | '7' => {
                let mut val = c.to_digit(8).unwrap();

                try_push_next(&mut val);

                val
            }
            _ => Err(E::InvalidChar { char: c, pos: 0 })?,
        };

        char::from_u32(val).ok_or(E::InvalidChar { char: '?', pos: 0 })
    }
}

/// Unescape the given [`str`].
pub fn unescape(s: &str) -> UResult<String> {
    Unescaper::new(s).unescape()
}
