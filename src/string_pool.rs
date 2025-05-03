use std::{
    collections::HashMap,
    io::SeekFrom,
    iter::zip,
    string::{FromUtf16Error, FromUtf8Error},
};

use binrw::{binrw, file_ptr::parse_from_iter, BinRead, BinResult, BinWrite, VecArgs};

use crate::{
    align,
    defs::{HeaderSizeStatic, ResChunk},
};

#[derive(Debug, BinRead, BinWrite, PartialEq, Clone, Copy, Eq, Hash)]
pub struct ResStringPoolRef {
    pub index: u32,
}

impl ResStringPoolRef {
    pub fn resolve(self, strings: &StringPoolHandler) -> Option<&str> {
        strings.resolve(self)
    }

    pub fn null() -> ResStringPoolRef {
        ResStringPoolRef { index: 0xffffffff }
    }
}

#[derive(Debug, BinRead, BinWrite, PartialEq, Default, Copy, Clone)]
pub struct StringPoolFlags {
    pub flags: u32,
}

impl StringPoolFlags {
    /// If set, the string index is sorted by the string values (based on strcmp16()).
    pub fn sorted(&self) -> bool {
        self.flags & (1 << 0) != 0
    }

    /// String pool is encoded in UTF-8.
    pub fn utf8(&self) -> bool {
        self.flags & (1 << 8) != 0
    }

    /// Create new StringPoolFlags from separate utf8 and sorted boolean flags.
    pub fn new(sorted: bool, utf8: bool) -> Self {
        Self {
            flags: (sorted as u32) | ((utf8 as u32) << 8),
        }
    }
}

/// A set of strings that can be referenced by others through a ResStringPool_ref.
///
/// Definition for a pool of strings. The data of this chunk is an array of u32 providing indices
/// into the pool, relative to stringsStart. At stringsStart are all of the UTF-8 or UTF-16 strings
/// concatenated together.
///
/// If styleCount is not zero, then immediately following the array of u32 indices into the string
/// table is another array of indices into a style table starting at stylesStart. Each entry in the
/// style table is an arry of ResStringPool_span structures.
#[binrw]
#[derive(Debug, PartialEq, Default, Clone)]
#[brw(stream = s)]
#[br(assert(string_count == (string_indices.len() as u32)))]
#[br(assert(style_count == (style_indices.len() as u32)))]
pub struct StringPool {
    #[brw(try_calc=Ok::<u64, binrw::Error>(ResChunk::get_header_offset(s.stream_position()?)))]
    #[brw(restore_position)]
    header_offset: u64,
    /// Number of strings in this pool (number of u32 indices that follow in the data).
    #[br(temp)]
    #[bw(calc=strings.len() as u32)]
    string_count: u32,

    /// Number of style span arrays in the pool (number of u32 indices that follow the string
    /// indices).
    #[br(temp)]
    #[bw(calc=styles.len() as u32)]
    style_count: u32,

    /// Flags.
    pub flags: StringPoolFlags,

    /// Index from header of the string data.
    #[br(temp)]
    #[bw(try_calc = Ok::<u32, binrw::Error>(calc_strings_start(header_offset, s.stream_position()?, string_count, style_count)))]
    strings_start: u32,

    /// Index from header of the style data.
    #[br(temp)]
    #[bw(try_calc = Ok::<u32, binrw::Error>(calc_styles_start(header_offset, s.stream_position()?, strings_start, strings, style_count == 0)))]
    styles_start: u32,

    #[br(count=string_count)]
    #[br(temp)]
    #[bw(calc = calc_string_indices(strings))]
    string_indices: Vec<u32>,

    #[br(count=style_count)]
    #[br(temp)]
    #[bw(calc = calc_style_indices(style_count))]
    style_indices: Vec<u32>,

    #[br(parse_with = StringPoolStrings::parse, args((flags.utf8(), &string_indices)))]
    #[bw(write_with = |sps,w,e,d| StringPoolStrings::write((&string_indices,sps), w, e, d))]
    #[br(seek_before(SeekFrom::Start(header_offset + (strings_start as u64))))]
    pub strings: StringPoolStrings,

    #[br(if(styles_start != 0))]
    #[br(parse_with = parse_from_iter(style_indices.iter().copied()), restore_position, seek_before(SeekFrom::Start(header_offset + (styles_start as u64))))]
    #[bw(align_after = 4)]
    pub styles: Vec<ResStringPoolSpan>,
}

impl From<StringPool> for ResChunk {
    fn from(value: StringPool) -> Self {
        Self {
            data: crate::defs::ResTypeValue::StringPool(value),
        }
    }
}

impl HeaderSizeStatic for StringPool {
    fn header_size() -> usize {
        20
    }
}

fn calc_strings_start(header_pos: u64, current_pos: u64, total_str: u32, total_style: u32) -> u32 {
    let from_current = 4 + // stringsStart
    4 + // stylesStart
    (4 * total_str) + // string_indices
    (4 * total_style); // style_indices

    let abs_pos = current_pos + (from_current as u64);

    let aligned_pos = align(abs_pos, 4); // TODO: check if necessary

    (aligned_pos - header_pos) as u32
}

fn calc_string_indices(strings: &StringPoolStrings) -> Vec<u32> {
    let mut indices: Vec<u32> = Vec::new();

    let mut current: u32 = 0;

    match strings {
        StringPoolStrings::UTF8(s) => {
            for str in s {
                indices.push(current);
                current += str.total_bytes() as u32;
            }
        }
        StringPoolStrings::UTF16(s) => {
            for str in s {
                indices.push(current);
                current += str.total_bytes() as u32;
            }
        }
    };

    indices
}

fn calc_style_indices(total_style: u32) -> Vec<u32> {
    let mut indices: Vec<u32> = Vec::new();

    let mut current: u32 = 0;

    for _ in 0..total_style {
        indices.push(current);
        current += ResStringPoolSpan::total_bytes() as u32;
    }

    indices
}

fn calc_styles_start(
    header_pos: u64,
    current_pos: u64,
    strings_start: u32,
    strings: &StringPoolStrings,
    no_styles: bool,
) -> u32 {
    if no_styles {
        return 0;
    }
    let abs_pos = (strings_start as u64) + (strings.total_bytes() as u64) + current_pos;

    let aligned_pos = align(abs_pos, 4); // TODO: check if necessary

    (aligned_pos - header_pos) as u32
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

#[derive(Debug, Clone, PartialEq, Default)]
pub struct StringPoolHandler {
    pub string_pool: StringPool,
    strings: HashMap<String, ResStringPoolRef>,
}

impl StringPoolHandler {
    pub fn resolve(&self, reference: ResStringPoolRef) -> Option<&str> {
        self.string_pool.resolve(reference)
    }

    pub fn allocate(&mut self, string: String) -> ResStringPoolRef {
        let exists = self.strings.get(string.as_str());
        match exists {
            Some(index) => *index,
            None => {
                let index = self.string_pool.push_string(string.clone());
                self.strings.insert(string, index);

                index
            }
        }
    }

    pub fn new(string_pool: StringPool) -> Self {
        let mut strings: HashMap<String, ResStringPoolRef> =
            HashMap::with_capacity(string_pool.strings.len());

        // these to_string calls are ok since this new should only be called once
        for (i, string) in string_pool.get_strings().enumerate() {
            strings.insert(string.to_string(), ResStringPoolRef { index: i as u32 });
        }

        Self {
            string_pool,
            strings,
        }
    }
}

impl From<StringPool> for StringPoolHandler {
    fn from(value: StringPool) -> Self {
        Self::new(value)
    }
}

impl From<StringPoolHandler> for StringPool {
    fn from(value: StringPoolHandler) -> Self {
        value.string_pool
    }
}

impl StringPool {
    pub fn resolve(&self, reference: ResStringPoolRef) -> Option<&str> {
        if reference.index == 0xffffffff {
            return None;
        }
        match &self.strings {
            StringPoolStrings::UTF8(utf8) => utf8
                .get(reference.index as usize)
                .map(|v| v.string.as_str()),
            StringPoolStrings::UTF16(utf16) => utf16
                .get(reference.index as usize)
                .map(|v| v.string.as_str()),
        }
    }
    pub fn get_strings(&self) -> Box<dyn Iterator<Item = &str> + '_> {
        match &self.strings {
            StringPoolStrings::UTF8(utf8) => Box::new(utf8.iter().map(|v| v.string.as_str())),
            StringPoolStrings::UTF16(utf16) => Box::new(utf16.iter().map(|v| v.string.as_str())),
        }
    }

    pub fn into_strings(self) -> Box<dyn Iterator<Item = String>> {
        match self.strings {
            StringPoolStrings::UTF8(utf8) => Box::new(utf8.into_iter().map(|v| v.string)),
            StringPoolStrings::UTF16(utf16) => Box::new(utf16.into_iter().map(|v| v.string)),
        }
    }

    pub fn push_string(&mut self, string: String) -> ResStringPoolRef {
        match self.strings {
            StringPoolStrings::UTF16(ref mut utf16) => {
                utf16.push(StringPoolString16 { string });
                ResStringPoolRef {
                    index: (utf16.len() - 1) as u32,
                }
            }
            StringPoolStrings::UTF8(ref mut utf8) => {
                utf8.push(StringPoolString8 { string });
                ResStringPoolRef {
                    index: (utf8.len() - 1) as u32,
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StringPoolStrings {
    UTF8(Vec<StringPoolString8>),
    UTF16(Vec<StringPoolString16>),
}

impl Default for StringPoolStrings {
    fn default() -> Self {
        Self::UTF16(Vec::new())
    }
}

impl StringPoolStrings {
    #[binrw::parser(reader, endian)]
    fn parse((utf8, string_indices): (bool, &[u32])) -> binrw::BinResult<Self> {
        if utf8 {
            let strings: Vec<StringPoolString8> =
                parse_from_iter(string_indices.iter().cloned())(reader, endian, ())?;
            Ok(StringPoolStrings::UTF8(strings))
        } else {
            let strings = parse_from_iter(string_indices.iter().cloned())(reader, endian, ())?;

            Ok(StringPoolStrings::UTF16(strings))
        }
    }

    #[binrw::writer(writer, endian)]
    fn write((string_indices, strings): (&[u32], &StringPoolStrings)) -> binrw::BinResult<()> {
        let pos: u64 = writer.stream_position()?;
        match strings {
            StringPoolStrings::UTF8(strings8) => {
                for (offset, string) in zip(string_indices, strings8) {
                    writer.seek(SeekFrom::Start(pos + (*offset as u64)))?;
                    string.write_options(writer, endian, ())?;
                }
            }
            StringPoolStrings::UTF16(strings16) => {
                for (offset, string) in zip(string_indices, strings16) {
                    writer.seek(SeekFrom::Start(pos + (*offset as u64)))?;
                    string.write_options(writer, endian, ())?;
                }
            }
        }
        Ok(())
    }

    pub fn len(&self) -> usize {
        match self {
            StringPoolStrings::UTF8(s) => s.len(),
            StringPoolStrings::UTF16(s) => s.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn total_bytes(&self) -> usize {
        match self {
            StringPoolStrings::UTF8(s) => s.iter().fold(0, |_, v| v.total_bytes()),
            StringPoolStrings::UTF16(s) => s.iter().fold(0, |_, v| v.total_bytes()),
        }
    }
}
/// Strings in UTF-16 format have length indicated by a length encoded in the stored data. It is
/// either 1 or 2 characters of length data. This allows a maximum length of 0x7fffffff (2147483647
/// bytes).
///
/// If the high bit is set, then there are two characters or 4 bytes of length data encoded. In
/// that case, drop the high bit of the first character and add it together with the next
/// character.
#[binrw]
#[derive(Debug, PartialEq, Clone)]
#[br(assert(null == 0))]
pub struct StringPoolString16 {
    #[br(temp)]
    #[bw(calc=calc_length16(string.len()).0)]
    length1: u16,

    #[br(temp)]
    #[bw(calc=calc_length16(string.len()).1)]
    #[brw(if(length1 & 0x8000 != 0))]
    length2: Option<u16>,

    #[bw(write_with = |s,w,e,a| write_utf16_str(s, w, e, a))]
    #[br(parse_with=read_utf16_str)]
    #[br(args(new_length16(length1, length2)))]
    pub string: String,

    #[br(temp)]
    #[bw(calc = 0)]
    #[br(assert(null == 0))]
    null: u16,
}

impl StringPoolString16 {
    fn total_bytes(&self) -> usize {
        let large = self.string.len() >= 0x8000;

        let size = 2 + self.string.len() * 2 + 2;

        match large {
            true => size + 2,
            false => size,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StringTooLong {
    pub length: usize,
    pub max_length: usize,
}

fn get_string_length(str: &str) -> usize {
    str.chars().count()
}

/// Strings in UTF-8 format have their length indicated by a length encoded in the stored data. It
/// is either 1 or 2 characters of length data. This allows a maximum length of 0x7fff (32767 bytes).
///
/// If the high bit is set, then there are two characters or 2 bytes of length data encoded. In
/// that case, drop the high bit of the first character and add it together with the next
/// character.
#[binrw]
#[derive(Debug, PartialEq, Clone)]
#[br(assert(new_length8(strlength_1, strlength_2) == (get_string_length(&string) as u32)))]
pub struct StringPoolString8 {
    #[br(temp)]
    #[bw(calc=calc_length8(get_string_length(string)).0)]
    strlength_1: u8,

    #[br(temp)]
    #[bw(calc=calc_length8(get_string_length(string)).1)]
    #[brw(if(strlength_1 & 0x80 != 0))]
    strlength_2: Option<u8>,

    #[br(temp)]
    #[bw(calc=calc_length8(string.len()).0)]
    bytelength_1: u8,

    #[br(temp)]
    #[bw(calc=calc_length8(string.len()).1)]
    #[brw(if(bytelength_1 & 0x80 != 0))]
    bytelength_2: Option<u8>,

    #[bw(write_with=|s, w, e, a| write_utf8_str(s,w,e,a))]
    #[br(parse_with=read_utf8_str)]
    #[br(args(new_length8(bytelength_1, bytelength_2)))]
    pub string: String,

    #[br(temp)]
    #[bw(calc = 0)]
    #[br(assert(null == 0))]
    null: u8,
}

impl StringPoolString8 {
    pub fn total_bytes(&self) -> usize {
        let strlen_big = self.string.len() >= 0x80;
        let bytes = self.string.as_bytes();

        let bytelen_big = bytes.len() >= 0x80;

        let mut size = 1 + 1 + bytes.len() + 1;

        if strlen_big {
            size += 1;
        }
        if bytelen_big {
            size += 1;
        }
        size
    }
}

#[derive(Debug)]
pub enum ReadStrError8 {
    BinRw(binrw::Error),
    Decode(FromUtf8Error),
}

impl From<binrw::Error> for ReadStrError8 {
    fn from(value: binrw::Error) -> Self {
        Self::BinRw(value)
    }
}

impl From<FromUtf8Error> for ReadStrError8 {
    fn from(value: FromUtf8Error) -> Self {
        Self::Decode(value)
    }
}

#[binrw::parser(reader, endian)]
pub fn read_utf8_str(size: u32) -> BinResult<String> {
    let data: Vec<u8> = <_>::read_options(
        reader,
        endian,
        VecArgs {
            count: size as usize,
            inner: (),
        },
    )?;

    let pos = reader.stream_position()?;

    String::from_utf8(data).map_err(|e| binrw::Error::Custom {
        pos,
        err: Box::new(e),
    })
}

#[binrw::writer(writer, endian)]
pub fn write_utf8_str(string: &str) -> BinResult<()> {
    let data = string.as_bytes();
    data.write_options(writer, endian, ())?;

    Ok(())
}

pub fn calc_length8(length: usize) -> (u8, Option<u8>) {
    match length >= 0x80 {
        true => (
            ((length >> 8) | (1 << 7)) as u8,
            Some((length & 0xff) as u8),
        ),
        false => (length as u8, None),
    }
}

pub fn new_length8(l1: u8, l2: Option<u8>) -> u32 {
    match l2 {
        None => l1 as u32,
        Some(le2) => (((l1 as u32) & 0x7f) << 8) | (le2 as u32),
    }
}

#[binrw::parser(reader, endian)]
pub fn read_utf16_str(size: u32) -> BinResult<String> {
    let data: Vec<u16> = <_>::read_options(
        reader,
        endian,
        VecArgs {
            count: size as usize,
            inner: (),
        },
    )?;

    let pos = reader.stream_position()?;

    String::from_utf16(&data).map_err(|e| binrw::Error::Custom {
        pos,
        err: Box::new(e),
    })
}

#[binrw::writer(writer, endian)]
pub fn write_utf16_str(string: &str) -> BinResult<()> {
    let data: Vec<u16> = string.encode_utf16().collect();
    data.write_options(writer, endian, ())?;

    Ok(())
}

pub fn calc_length16(length: usize) -> (u16, Option<u16>) {
    match length >= 0x8000 {
        true => (
            ((length >> 16) | (1 << 15)) as u16,
            Some((length & 0xffff) as u16),
        ),
        false => (length as u16, None),
    }
}

pub fn new_length16(l1: u16, l2: Option<u16>) -> u32 {
    match l2 {
        None => l1 as u32,
        Some(le2) => (((l1 as u32) & 0x7fff) << 16) | (le2 as u32),
    }
}

/// This structure defines a span of style information associated with a string in the pool.
#[derive(Debug, PartialEq, BinWrite, BinRead, Copy, Clone)]
pub struct ResStringPoolSpan {
    /// This is the name of the span -- that is, the name of the XML tag that defined it. The
    /// special value END (0xffffffff) indicates the end of an array of spans.
    pub name: ResStringPoolRef,
    /// The first character in the string that this span applies to.
    pub first_char: u32,
    /// The last character in the string that this span applies to.
    pub last_char: u32,
}

impl ResStringPoolSpan {
    pub fn total_bytes() -> usize {
        4 + 4 + 4
    }
}
