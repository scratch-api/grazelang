use std::{fmt::Write, str};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StringEscaper<'a> {
    pub data: &'a str,
    pub capacity: usize,
    pub escape_quote: bool,
    pub escape_backtick: bool,
    pub escape_dollar_sign: bool,
}

impl<'a> StringEscaper<'a> {
    pub fn new(
        s: &'a str,
        escape_quote: bool,
        escape_backtick: bool,
        escape_dollar_sign: bool,
    ) -> Self {
        Self {
            data: s,
            capacity: s.len(),
            escape_quote,
            escape_backtick,
            escape_dollar_sign,
        }
    }

    pub fn escape_to_string(self) -> String {
        let mut value = String::with_capacity(self.capacity);
        self.escape_into(&mut value).unwrap();
        value
    }

    pub fn escape_into<W>(self, f: &mut W) -> std::fmt::Result
    where
        W: Write,
    {
        let mut buf: [u8; 6] = [0; 6];
        let mut start = 0;
        for (idx, &c) in self.data.as_bytes().iter().enumerate() {
            let escape = match c {
                b'"' => self.escape_quote,
                b'`' => self.escape_backtick,
                b'$' => self.escape_dollar_sign,
                b'\\' => true,
                0x00..=0x1f => true,
                _ => false,
            };
            if escape {
                if start < idx {
                    // utf-8 boundaries are correct because we matched valid chars
                    f.write_str(&self.data[start..idx])?;
                }
                f.write_str(Self::escape_character(
                    c as char,
                    &mut buf,
                    self.escape_quote,
                    self.escape_backtick,
                    self.escape_dollar_sign,
                ))?;
                start = idx + 1;
            }
        }
        if start < self.data.len() {
            // utf-8 boundaries are correct because we matched valid chars
            f.write_str(&self.data[start..])?;
        }
        Ok(())
    }

    pub fn escape_into_io<W>(self, f: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        let mut buf: [u8; 6] = [0; 6];
        let mut start = 0;
        let bytes = self.data.as_bytes();
        for (idx, &c) in bytes.iter().enumerate() {
            let escape = match c {
                b'"' => self.escape_quote,
                b'`' => self.escape_backtick,
                b'$' => self.escape_dollar_sign,
                b'\\' => true,
                0x00..=0x1f => true,
                _ => false,
            };
            if escape {
                if start < idx {
                    // utf-8 boundaries are correct because we matched valid chars
                    f.write_all(&bytes[start..idx])?;
                }
                f.write_all(
                    Self::escape_character(
                        c as char,
                        &mut buf,
                        self.escape_quote,
                        self.escape_backtick,
                        self.escape_dollar_sign,
                    )
                    .as_bytes(),
                )?;
                start = idx + 1;
            }
        }
        if start < self.data.len() {
            // utf-8 boundaries are correct because we matched valid chars
            f.write_all(&bytes[start..])?;
        }
        Ok(())
    }

    pub fn escape_character(
        c: char,
        buf: &mut [u8; 6],
        escape_quote: bool,
        escape_backtick: bool,
        escape_dollar_sign: bool,
    ) -> &str {
        let c = match c {
            '"' if escape_quote => c,
            '`' if escape_backtick => c,
            '$' if escape_dollar_sign => c,
            '\\' => c,
            '\n' => 'n',
            '\r' => 'r',
            '\t' => 't',
            '\u{0008}' => 'b',
            '\u{000c}' => 'f',
            c @ '\u{0000}'..='\u{001f}' => return Self::escaped_code(c as u8, buf),
            _ => return c.encode_utf8(buf),
        } as u8;
        unsafe { Self::escaped_single_character(c, buf) }
    }

    /// `c` needs to be a valid one byte character
    unsafe fn escaped_single_character(c: u8, buf: &mut [u8; 6]) -> &str {
        buf[0] = b'\\';
        buf[1] = c;
        let buf = &buf[..2];
        // SAFETY: First byte is the one byte character '\\' and the second byte is a valid one byte character.
        // Therefore the buffer contains valid utf-8.
        unsafe { str::from_utf8_unchecked(buf) }
    }

    fn escaped_code(code: u8, buf: &mut [u8; 6]) -> &str {
        buf[..4].copy_from_slice(b"\\u00");
        const HEX: &[u8; 16] = b"0123456789abcdef";
        buf[4] = HEX[(code >> 4) as usize];
        buf[5] = HEX[(code & 15) as usize];
        // SAFETY: First four bytes are one byte characters and the next two chars are elements of HEX.
        // HEX consists of one byte characters.
        // The length of the buffer is 6 bytes or 4 bytes + 2 bytes.
        // Therefore the buffer contains valid utf-8.
        unsafe { str::from_utf8_unchecked(buf) }
    }
}

impl<'a> std::fmt::Display for StringEscaper<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.escape_into(f)
    }
}

#[inline]
pub fn escape(
    s: &str,
    escape_quote: bool,
    escape_backtick: bool,
    escape_dollar_sign: bool,
) -> String {
    StringEscaper::new(s, escape_quote, escape_backtick, escape_dollar_sign).escape_to_string()
}

pub fn normal_string_escaper(s: &str) -> StringEscaper<'_> {
    StringEscaper::new(s, true, false, false)
}

pub fn format_string_escaper(s: &str) -> StringEscaper<'_> {
    StringEscaper::new(s, true, false, true)
}

pub fn canonical_name_escaper(s: &str) -> StringEscaper<'_> {
    StringEscaper::new(s, false, true, false)
}
