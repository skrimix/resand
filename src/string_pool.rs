/*
    Copyright (C) 2024 fieryhenry

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

use std::{
    io::SeekFrom,
    iter::zip,
    string::{FromUtf16Error, FromUtf8Error},
};

use binrw::{binrw, file_ptr::parse_from_iter, BinRead, BinResult, BinWrite, VecArgs};

use crate::defs::ResChunk_header;

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

    pub fn new(utf8: bool, sorted: bool) -> Self {
        Self {
            flags: (utf8 as u32) | (sorted as u32) << 8,
        }
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
#[binrw]
#[derive(Debug, PartialEq)]
#[brw(stream = s)]
#[br(assert(stringCount == (string_indices.len() as u32)))]
#[br(assert(styleCount == (style_indices.len() as u32)))]
pub struct StringPool {
    #[brw(try_calc=Ok::<u64, binrw::Error>(ResChunk_header::get_header_offset(s.stream_position()?)))]
    #[brw(restore_position)]
    header_offset: u64,
    /// Number of strings in this pool (number of u32 indices that follow in the data).
    #[br(temp)]
    #[bw(calc=strings.len() as u32)]
    stringCount: u32,

    /// Number of style span arrays in the pool (number of u32 indices that follow the string
    /// indices).
    #[br(temp)]
    #[bw(calc=styles.len() as u32)]
    styleCount: u32,

    /// Flags.
    pub flags: StringPoolFlags,

    /// Index from header of the string data.
    #[br(temp)]
    #[bw(try_calc = Ok::<u32, binrw::Error>(calc_strings_start(header_offset, s.stream_position()?, stringCount, styleCount)))]
    stringsStart: u32,

    /// Index from header of the style data.
    #[br(temp)]
    #[bw(try_calc = Ok::<u32, binrw::Error>(calc_styles_start(header_offset, s.stream_position()?, stringsStart, strings)))]
    stylesStart: u32,

    #[br(count=stringCount)]
    #[br(temp)]
    #[bw(calc = calc_string_indices(strings))]
    string_indices: Vec<u32>,

    #[br(count=styleCount)]
    #[br(temp)]
    #[bw(calc = calc_style_indices(styleCount))]
    style_indices: Vec<u32>,

    #[br(parse_with = StringPoolStrings::parse, args((flags.utf8(), string_indices.clone())))]
    #[bw(write_with = |sps,w,e,d| StringPoolStrings::write((&string_indices,sps), w, e, d))]
    #[br(seek_before(SeekFrom::Start(header_offset + (stringsStart as u64))))]
    pub strings: StringPoolStrings,

    #[br(parse_with = parse_from_iter(style_indices.iter().copied()), restore_position, seek_before(SeekFrom::Start(header_offset + (stylesStart as u64))))]
    pub styles: Vec<ResStringPool_span>,
}

pub fn align(pos: u64, alignment: u64) -> u64 {
    let remaning = pos % alignment;
    if remaning == 0 {
        return pos;
    }

    pos + (alignment - remaning)
}

pub fn calc_strings_start(
    header_pos: u64,
    current_pos: u64,
    total_str: u32,
    total_style: u32,
) -> u32 {
    let from_current = 4 + // stringsStart
    4 + // stylesStart
    (4 * total_str) + // string_indices
    (4 * total_style); // style_indices

    let abs_pos = current_pos + (from_current as u64);

    let aligned_pos = align(abs_pos, 4); // TODO: check if necessary

    (aligned_pos - header_pos) as u32
}

pub fn calc_string_indices(strings: &StringPoolStrings) -> Vec<u32> {
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

pub fn calc_style_indices(total_style: u32) -> Vec<u32> {
    let mut indices: Vec<u32> = Vec::new();

    let mut current: u32 = 0;

    for _ in 0..total_style {
        indices.push(current);
        current += ResStringPool_span::total_bytes() as u32;
    }

    indices
}

pub fn calc_styles_start(
    header_pos: u64,
    current_pos: u64,
    strings_start: u32,
    strings: &StringPoolStrings,
) -> u32 {
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

impl StringPool {
    pub fn get_strings(&self) -> Vec<String> {
        let sps = &self.strings;

        let mut strings: Vec<String> = Vec::new();

        match sps {
            StringPoolStrings::UTF8(utf8) => {
                for str in utf8 {
                    strings.push(str.string.clone());
                }
            }
            StringPoolStrings::UTF16(utf16) => {
                for str in utf16 {
                    strings.push(str.string.clone());
                }
            }
        }

        strings
    }
}

#[derive(Debug, PartialEq)]
pub enum StringPoolStrings {
    UTF8(Vec<StringPoolString8>),
    UTF16(Vec<StringPoolString16>),
}

impl StringPoolStrings {
    #[binrw::parser(reader, endian)]
    fn parse((utf8, string_indices): (bool, Vec<u32>)) -> binrw::BinResult<Self> {
        if utf8 {
            let strings: Vec<StringPoolString8> =
                parse_from_iter(string_indices.clone().into_iter())(reader, endian, ())?;
            Ok(StringPoolStrings::UTF8(strings))
        } else {
            let strings = parse_from_iter(string_indices.clone().into_iter())(reader, endian, ())?;

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

/// Strings in UTF-8 format have their length indicated by a length encoded in the stored data. It
/// is either 1 or 2 characters of length data. This allows a maximum length of 0x7fff (32767 bytes).
///
/// If the high bit is set, then there are two characters or 2 bytes of length data encoded. In
/// that case, drop the high bit of the first character and add it together with the next
/// character.
#[binrw]
#[derive(Debug, PartialEq, Clone)]
#[br(assert(new_length8(strlength_1, strlength_2) == (string.len() as u32)))]
pub struct StringPoolString8 {
    #[br(temp)]
    #[bw(calc=calc_length8(string.len()).0)]
    strlength_1: u8,

    #[br(temp)]
    #[bw(calc=calc_length8(string.len()).1)]
    #[brw(if(strlength_1 & 0x80 != 0))]
    strlength_2: Option<u8>,

    #[br(temp)]
    #[bw(calc=calc_length8(string.as_bytes().len()).0)]
    bytelength_1: u8,

    #[br(temp)]
    #[bw(calc=calc_length8(string.as_bytes().len()).1)]
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
        true => (((length >> 8) | 1 << 7) as u8, Some((length & 0xff) as u8)),
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
            ((length >> 16) | 1 << 15) as u16,
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

impl ResStringPool_span {
    pub fn total_bytes() -> usize {
        4 + 4 + 4
    }
}
