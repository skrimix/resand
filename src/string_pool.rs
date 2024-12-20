use std::{
    io::SeekFrom,
    string::{FromUtf16Error, FromUtf8Error},
};

use binrw::{file_ptr::parse_from_iter, BinRead, BinWrite};

#[derive(Debug, BinRead, BinWrite, PartialEq)]
pub struct ResStringPool_ref {
    pub index: u32,
}

#[derive(Debug, BinRead, BinWrite, PartialEq)]
pub struct StringPoolFlags {
    pub flags: u32,
}

impl StringPoolFlags {
    /// If set, the string index is sorted by the string values (based on strcmp16()).
    pub fn sorted(&self) -> bool {
        self.flags & (1 << 0) != 0
    }

    /// String pool is encoded in UTF-8
    pub fn utf8(&self) -> bool {
        self.flags & (1 << 8) != 0
    }
}

/// A set of strings that can be referenced by others through a ResStringPool_ref.
///
/// Definition for a pool of strings. The data of this chunk is an array of u32 providing indices
/// into the pool, relative to stringsStart. At stringsStart are all of the UTF-16 strings
/// concatenated together; each starts with a u16 of the string's length and each ends with a
/// 0x0000 terminator. If a string is > 32767 characters, the high bit of the length is set meaning
/// to take those 15 bits as a high word and it will be followed by another u16 containg the low
/// word.
///
/// If styleCount is not zero, then immediately following the array of u32 indices into the string
/// table is another array of indices into a style table starting at stylesStart. Each entry in the
/// style table is an arry of ResStringPool_span structures.
#[derive(Debug, PartialEq, BinRead, BinWrite)]
#[br(assert(strings8.is_some() || strings16.is_some()))]
#[br(assert(stringCount == (string_indices.len() as u32)))]
#[br(assert(styleCount == (style_indices.len() as u32)))]
#[br(import(header_offset: u64))]
pub struct StringPool {
    /// Number of strings in this pool (number of u32 indices that follow in the data).
    pub stringCount: u32,

    /// Number of style span arrays in the pool (number of u32 indices that follow the string
    /// indices).
    pub styleCount: u32,

    /// Flags.
    pub flags: StringPoolFlags,

    /// Index from header of the string data.
    pub stringsStart: u32,

    /// Index from header of the style data.
    pub stylesStart: u32,

    #[br(count=stringCount)]
    pub string_indices: Vec<u32>,

    #[br(count=styleCount)]
    pub style_indices: Vec<u32>,

    #[br(if(flags.utf8()))]
    #[br(parse_with = parse_from_iter(string_indices.clone().into_iter()), restore_position, seek_before(SeekFrom::Start(header_offset + (stringsStart as u64))))]
    strings8: Option<Vec<StringPoolString8>>,

    #[br(if(!flags.utf8()))]
    #[br(parse_with = parse_from_iter(string_indices.clone().into_iter()), restore_position, seek_before(SeekFrom::Start(header_offset + (stringsStart as u64))))]
    strings16: Option<Vec<StringPoolString16>>,

    #[br(parse_with = parse_from_iter(style_indices.iter().copied()), restore_position, seek_before(SeekFrom::Start(header_offset + (stylesStart as u64))))]
    pub styles: Vec<ResStringPool_span>,
}

#[derive(Debug)]
pub enum StringDecodeError {
    UTF8(FromUtf8Error),
    UTF16(FromUtf16Error),
}

impl From<FromUtf8Error> for StringDecodeError {
    fn from(value: FromUtf8Error) -> Self {
        Self::UTF8(value)
    }
}

impl From<FromUtf16Error> for StringDecodeError {
    fn from(value: FromUtf16Error) -> Self {
        Self::UTF16(value)
    }
}

impl StringPool {
    pub fn get_string_pool_strings(&self) -> StringPoolStrings {
        if let Some(utf16) = &self.strings16 {
            return StringPoolStrings::UTF16(utf16.to_vec());
        }
        if let Some(utf8) = &self.strings8 {
            return StringPoolStrings::UTF8(utf8.to_vec());
        }

        panic!("We should never get here.")
    }

    pub fn get_strings(&self) -> Result<Vec<String>, StringDecodeError> {
        let sps = self.get_string_pool_strings();

        let mut strings: Vec<String> = Vec::new();

        match sps {
            StringPoolStrings::UTF8(utf8) => {
                for str in utf8 {
                    strings.push(str.get_string_owned()?);
                }
            }
            StringPoolStrings::UTF16(utf16) => {
                for str in utf16 {
                    strings.push(str.get_string()?);
                }
            }
        }

        Ok(strings)
    }
}

#[derive(Debug, PartialEq)]
pub enum StringPoolStrings {
    UTF8(Vec<StringPoolString8>),
    UTF16(Vec<StringPoolString16>),
}

/// Strings in UTF-16 format have length indicated by a length encoded in the stored data. It is
/// either 1 or 2 characters of length data. This allows a maximum length of 0x7fffffff (2147483647
/// bytes).
///
/// If the high bit is set, then there are two characters or 4 bytes of length data encoded. In
/// that case, drop the high bit of the first character and add it together with the next
/// character.
#[derive(Debug, BinWrite, BinRead, PartialEq, Clone)]
#[br(assert(null == 0))]
pub struct StringPoolString16 {
    pub length1: u16,
    #[brw(if(length1 & 0x8000 != 0))]
    pub length2: Option<u16>,

    #[br(count=match length2 {
        None => length1 as u32,
        Some(l2) => (((length1 as u32) & 0x7fff) << 16) | (l2 as u32)
    })]
    pub data: Vec<u16>,
    pub null: u16,
}

impl StringPoolString16 {
    pub fn get_string(&self) -> Result<String, FromUtf16Error> {
        String::from_utf16(&self.data)
    }

    pub fn new(string: String) -> Result<Self, StringTooLong> {
        let string_length = string.len();
        if string_length > 0x7fffffff {
            return Err(StringTooLong {
                length: string_length,
                max_length: 0x7fffffff,
            });
        }

        let bytes: Vec<u16> = string.encode_utf16().collect();

        let (string_length1, string_length2) = match string_length >= 0x8000 {
            true => (
                ((string_length >> 16) | 1 << 15) as u16,
                Some((string_length & 0xffff) as u16),
            ),
            false => (string_length as u16, None),
        };

        Ok(StringPoolString16 {
            length1: string_length1,
            length2: string_length2,
            data: bytes.to_vec(),
            null: 0,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct StringTooLong {
    length: usize,
    max_length: usize,
}

/// Strings in UTF-8 format have their length indicated by a length encoded in the stored data. It
/// is either 1 or 2 characters of length data. This allows a maximum length of 0x7fff (32767 bytes).
///
/// If the high bit is set, then there are two characters or 2 bytes of length data encoded. In
/// that case, drop the high bit of the first character and add it together with the next
/// character.
#[derive(Debug, BinWrite, BinRead, PartialEq, Clone)]
#[br(assert(null == 0))]
pub struct StringPoolString8 {
    pub strlength_1: u8,
    #[brw(if(strlength_1 & 0x80 != 0))]
    pub strlength_2: Option<u8>,

    pub bytelength_1: u8,
    #[brw(if(bytelength_1 & 0x80 != 0))]
    pub bytelength_2: Option<u8>,

    #[br(count=match bytelength_2 {
        None => bytelength_1 as u32,
        Some(l2) => (((bytelength_1 as u32) & 0x7f) << 8) | (l2 as u32)
    })]
    pub data: Vec<u8>,
    pub null: u8,
}

impl StringPoolString8 {
    pub fn get_string_clone(&self) -> Result<String, FromUtf8Error> {
        String::from_utf8(self.data.clone())
    }
    pub fn get_string_owned(self) -> Result<String, FromUtf8Error> {
        String::from_utf8(self.data)
    }

    pub fn new(string: String) -> Result<Self, StringTooLong> {
        let string_length = string.len();
        if string_length > 0x7fff {
            return Err(StringTooLong {
                length: string_length,
                max_length: 0x7fff,
            });
        }

        let bytes = string.as_bytes();
        let byte_length = bytes.len();

        if byte_length > 0x7fff {
            return Err(StringTooLong {
                length: byte_length,
                max_length: 0x7fff,
            });
        }

        let (string_length1, string_length2) = match string_length >= 0x80 {
            true => (
                ((string_length >> 8) | 1 << 7) as u8,
                Some((string_length & 0xff) as u8),
            ),
            false => (string_length as u8, None),
        };

        let (byte_length1, byte_length2) = match byte_length >= 0x80 {
            true => (
                ((byte_length >> 8) | 1 << 7) as u8,
                Some((byte_length & 0xff) as u8),
            ),
            false => (byte_length as u8, None),
        };

        Ok(StringPoolString8 {
            strlength_1: string_length1,
            strlength_2: string_length2,
            bytelength_1: byte_length1,
            bytelength_2: byte_length2,
            data: bytes.to_vec(),
            null: 0,
        })
    }
}

/// This structure defines a span of style information associated with a string in the pool.
#[derive(Debug, PartialEq, BinWrite, BinRead)]
pub struct ResStringPool_span {
    /// This is the name of the span -- that is, the name of the XML tag that defined it. The
    /// special value END (0xffffffff) indicates the end of an array of spans.
    pub name: ResStringPool_ref,
    /// The first character in the string that this span applies to.
    pub firstChar: u32,
    /// The last character in the string that this span applies to.
    pub lastChar: u32,
}
