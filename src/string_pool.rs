use std::{
    collections::HashMap,
    io::{Read, Seek, SeekFrom, Write},
    iter::zip,
    string::{FromUtf16Error, FromUtf8Error},
};

use crate::{
    align,
    defs::{HeaderSizeStatic, ResChunk},
    stream::{
        NewResultCtx, Readable, ReadableNoOptions, ResultCtx, StreamError, StreamResult,
        VecReadable, VecWritable, Writeable, WriteableNoOptions,
    },
};

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub struct ResStringPoolRef {
    pub index: u32,
}

impl Readable for ResStringPoolRef {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        Ok(Self {
            index: u32::read_no_opts(reader).add_context(|| "read index for ResStringPoolRef")?,
        })
    }
}

impl Writeable for ResStringPoolRef {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        self.index
            .write_no_opts(writer)
            .add_context(|| "write index for ResStringPoolRef")
    }
}

impl ResStringPoolRef {
    pub fn resolve(self, strings: &StringPoolHandler) -> Option<&str> {
        strings.resolve(self)
    }

    pub fn null() -> ResStringPoolRef {
        ResStringPoolRef { index: 0xffffffff }
    }
}

#[derive(Debug, PartialEq, Default, Copy, Clone)]
pub struct StringPoolFlags {
    pub flags: u32,
}

impl Readable for StringPoolFlags {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        Ok(Self {
            flags: u32::read_no_opts(reader).add_context(|| "read flags for StringPoolFlags")?,
        })
    }
}

impl Writeable for StringPoolFlags {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        self.flags
            .write_no_opts(writer)
            .add_context(|| "write flags for StringPoolFlags")
    }
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
#[derive(Debug, PartialEq, Default, Clone)]
pub struct StringPool {
    pub flags: StringPoolFlags,
    pub strings: StringPoolStrings,
    pub styles: Vec<ResStringPoolSpan>,
}

impl Writeable for StringPool {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        let header_offset = ResChunk::get_header_offset(writer.stream_position()?);

        let string_count: u32 = self.strings.len() as u32;
        string_count
            .write_no_opts(writer)
            .add_context(|| "write string_count for StringPool")?;

        let style_count: u32 = self.styles.len() as u32;
        style_count
            .write_no_opts(writer)
            .add_context(|| "write style_count for StringPool")?;

        self.flags.write_no_opts(writer)?;

        let strings_start = calc_strings_start(
            header_offset,
            writer.stream_position()?,
            string_count,
            style_count,
        );
        strings_start
            .write_no_opts(writer)
            .add_context(|| "write strings_start for StringPool")?;

        let styles_start = calc_styles_start(
            header_offset,
            writer.stream_position()?,
            strings_start,
            &self.strings,
            style_count == 0,
        );
        styles_start
            .write_no_opts(writer)
            .add_context(|| "write styles_start for StringPool")?;

        // FIXME: don't really like this clones

        let string_indicies = calc_string_indices(&self.strings);
        string_indicies
            .clone()
            .write_vec(writer)
            .add_context(|| "write string_indicies for StringPool")?;

        let style_indicies = calc_style_indices(style_count);
        style_indicies
            .clone()
            .write_vec(writer)
            .add_context(|| "write style_indicies for StringPool")?;

        self.strings
            .write(writer, string_indicies)
            .add_context(|| "write strings for StringPool")?;
        self.styles
            .write(writer, style_indicies)
            .add_context(|| "write styles for StringPool")?;

        let current_pos = writer.stream_position()?;
        let new_pos = align(current_pos, 4);
        if new_pos > current_pos {
            let new_data = vec![0u8; (new_pos - current_pos) as usize];
            new_data
                .write_vec(writer)
                .add_context(|| "write padding for StringPool")?;
        }

        Ok(())
    }
}

impl Readable for StringPool {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let header_offset = ResChunk::get_header_offset(reader.stream_position()?);
        let string_count =
            u32::read_no_opts(reader).add_context(|| "read string_count for StringPool")?;
        let style_count =
            u32::read_no_opts(reader).add_context(|| "read style count for StringPool")?;
        let flags =
            StringPoolFlags::read_no_opts(reader).add_context(|| "read flags for StringPool")?;

        let strings_start =
            u32::read_no_opts(reader).add_context(|| "read strings_start for StringPool")?;
        let styles_start =
            u32::read_no_opts(reader).add_context(|| "read styles_start for StringPool")?;

        let string_indicies = <Vec<u32>>::read_vec(reader, string_count as usize)
            .add_context(|| "read string_indicies for StringPool")?;
        let style_indicies = <Vec<u32>>::read_vec(reader, style_count as usize)
            .add_context(|| "read style_indicies for StringPool")?;

        reader
            .seek(SeekFrom::Start(header_offset + strings_start as u64))
            .stream_context(|| {
                format!(
                    "seek to strings position: header_offset: {}, strings_start: {} for StringPool",
                    header_offset, strings_start
                )
            })?;

        let strings = StringPoolStrings::read(reader, (flags.utf8(), string_indicies))
            .add_context(|| "read strings for StringPool")?;

        reader
            .seek(SeekFrom::Start(header_offset + styles_start as u64))
            .stream_context(|| {
                format!(
                    "seek to styles position: header_offset: {}, styles_start: {} for StringPool",
                    header_offset, styles_start
                )
            })?;

        let styles = <Vec<ResStringPoolSpan>>::read(reader, style_indicies)
            .add_context(|| "read styles for StringPool")?;

        Ok(Self {
            flags,
            strings,
            styles,
        })
    }
}

impl Readable for Vec<ResStringPoolSpan> {
    type Args = Vec<u32>;
    fn read<R: Read + Seek>(reader: &mut R, args: Self::Args) -> StreamResult<Self> {
        let start_pos = reader.stream_position()?;
        let mut styles = Vec::with_capacity(args.len());

        for index in args {
            reader
                .seek(SeekFrom::Start(start_pos + index as u64))
                .stream_context(|| {
                    format!(
                        "seek to style position: start_pos: {}, index: {} for Vec<ResStringPoolSpan>",
                        start_pos, index
                    )
                })?;
            styles.push(
                ResStringPoolSpan::read_no_opts(reader)
                    .add_context(|| "read ResStringPoolSpan for Vec<ResStringPoolSpan>")?,
            );
        }

        Ok(styles)
    }
}

impl Writeable for Vec<ResStringPoolSpan> {
    type Args = Vec<u32>;
    fn write<W: Write + Seek>(self, writer: &mut W, args: Self::Args) -> StreamResult<()> {
        let start_pos = writer.stream_position()?;

        for (offset, style) in zip(args, self) {
            writer.seek(SeekFrom::Start(start_pos + offset as u64))?;
            style
                .write_no_opts(writer)
                .add_context(|| "write ResStringPoolSpan for Vec<ResStringPoolSpan>")?;
        }

        Ok(())
    }
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
    string_map: HashMap<String, ResStringPoolRef>,
}

impl StringPoolHandler {
    pub fn resolve(&self, reference: ResStringPoolRef) -> Option<&str> {
        self.string_pool.resolve(reference)
    }

    pub fn allocate(&mut self, string: String) -> ResStringPoolRef {
        let exists = self.string_map.get(string.as_str());
        match exists {
            Some(index) => *index,
            None => {
                let index = self.string_pool.push_string(string.clone());
                self.string_map.insert(string, index);

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
            string_map: strings,
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

impl Readable for StringPoolStrings {
    type Args = (bool, Vec<u32>);
    fn read<R: Read + Seek>(reader: &mut R, args: Self::Args) -> StreamResult<Self> {
        match args.0 {
            true => {
                let start_pos = reader.stream_position()?;
                let mut strings = Vec::with_capacity(args.1.len());
                for index in args.1 {
                    reader.seek(SeekFrom::Start(start_pos + index as u64))?;
                    strings.push(
                        StringPoolString8::read_no_opts(reader)
                            .add_context(|| "read string for StringPoolStrings::UTF8")?,
                    );
                }
                Ok(StringPoolStrings::UTF8(strings))
            }
            false => {
                let start_pos = reader.stream_position()?;
                let mut strings = Vec::with_capacity(args.1.len());
                for index in args.1 {
                    reader.seek(SeekFrom::Start(start_pos + index as u64))?;
                    strings.push(
                        StringPoolString16::read_no_opts(reader)
                            .add_context(|| "read string for StringPoolStrings::UTF16")?,
                    );
                }
                Ok(StringPoolStrings::UTF16(strings))
            }
        }
    }
}

impl Writeable for StringPoolStrings {
    type Args = Vec<u32>;
    fn write<W: Write + Seek>(self, writer: &mut W, args: Self::Args) -> StreamResult<()> {
        let pos = writer.stream_position()?;
        match self {
            Self::UTF8(strings) => {
                for (offset, string) in zip(args, strings) {
                    writer.seek(SeekFrom::Start(pos + offset as u64))?;
                    string
                        .write_no_opts(writer)
                        .add_context(|| "write string for StringPoolStrings::UTF8")?;
                }
            }
            Self::UTF16(strings) => {
                for (offset, string) in zip(args, strings) {
                    writer.seek(SeekFrom::Start(pos + offset as u64))?;
                    string
                        .write_no_opts(writer)
                        .add_context(|| "write string for StringPoolStrings::UTF16")?;
                }
            }
        };
        Ok(())
    }
}

impl StringPoolStrings {
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
#[derive(Debug, PartialEq, Clone)]
pub struct StringPoolString16 {
    pub string: String,
}

impl Writeable for StringPoolString16 {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        let (length1, length2) = calc_length16(self.string.len());

        length1
            .write_no_opts(writer)
            .add_context(|| "write length1 for StringPoolString16")?;
        if let Some(length2) = length2 {
            length2.write_no_opts(writer)?;
        }

        write_utf16_str(writer, &self.string)
            .add_context(|| "write utf16 string for StringPoolString16")?;

        let null: u16 = 0;
        null.write_no_opts(writer)
            .add_context(|| "write null bytes for StringPoolString16")
    }
}

impl Readable for StringPoolString16 {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let length1 =
            u16::read_no_opts(reader).add_context(|| "read length1 for StringPoolString16")?;
        let length2 = if length1 & 0x8000 != 0 {
            Some(u16::read_no_opts(reader).add_context(|| "read length2 for StringPoolString16")?)
        } else {
            None
        };

        let string = read_utf16_str(reader, new_length16(length1, length2))
            .add_context(|| "read utf16 string for StringPoolString16")?;

        let null: u16 =
            u16::read_no_opts(reader).add_context(|| "read null for StringPoolString16")?;

        if null != 0 {
            return Err(StreamError::new_string_context(
                format!("invalid null value {}, expected 0x0000", null),
                reader.stream_position()?,
                "validate null bytes for StringPoolString16",
            ));
        }

        Ok(Self { string })
    }
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
#[derive(Debug, PartialEq, Clone)]
pub struct StringPoolString8 {
    pub string: String,
}

impl Writeable for StringPoolString8 {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        let str_len = get_string_length(&self.string);
        let (strlength_1, strlength_2) = calc_length8(str_len);

        strlength_1
            .write_no_opts(writer)
            .add_context(|| "write strlength_1 for StringPoolString8")?;
        if let Some(strlength_2) = strlength_2 {
            strlength_2
                .write_no_opts(writer)
                .add_context(|| "write strlength_2 for StringPoolString8")?;
        }

        let (bytelength_1, bytelength_2) = calc_length8(self.string.len());

        bytelength_1
            .write_no_opts(writer)
            .add_context(|| "write bytelength_1 for StringPoolString8")?;
        if let Some(bytelength_2) = bytelength_2 {
            bytelength_2
                .write_no_opts(writer)
                .add_context(|| "write bytelength_2 for StringPoolString8")?;
        }

        write_utf8_str(writer, self.string)
            .add_context(|| "write utf8 string for StringPoolString8")?;

        let null: u8 = 0;
        null.write_no_opts(writer)
            .add_context(|| "write null byte for StringPoolString8")
    }
}

impl Readable for StringPoolString8 {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let strlength_1 =
            u8::read_no_opts(reader).add_context(|| "read strlength_1 for StringPoolString8")?;
        let strlength_2 = if strlength_1 & 0x80 != 0 {
            Some(
                u8::read_no_opts(reader)
                    .add_context(|| "read strlength_2 for StringPoolString8")?,
            )
        } else {
            None
        };

        let bytelength_1 =
            u8::read_no_opts(reader).add_context(|| "read bytelength_1 for StringPoolString8")?;
        let bytelength_2 = if bytelength_1 & 0x80 != 0 {
            Some(
                u8::read_no_opts(reader)
                    .add_context(|| "read bytelength_2 for StringPoolString8")?,
            )
        } else {
            None
        };

        let string = read_utf8_str(reader, new_length8(bytelength_1, bytelength_2))
            .add_context(|| "read utf8 string for StringPoolString8")?;

        let str_length = new_length8(strlength_1, strlength_2);
        let read_length = get_string_length(&string) as u32;
        if str_length != read_length {
            return Err(StreamError::new_string_context(
                format!(
                    "the read string had a length of {}, but a length of {} was expected",
                    read_length, str_length
                ),
                reader.stream_position()?,
                "validate read string for StringPoolString8",
            ));
        }

        let null =
            u8::read_no_opts(reader).add_context(|| "read null byte for StringPoolString8")?;

        if null != 0 {
            return Err(StreamError::new_string_context(
                format!("invalid null value: {}, expected 0", null),
                reader.stream_position()?,
                "validate null for StringPoolString8",
            ));
        }

        Ok(Self { string })
    }
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

pub fn read_utf8_str<R: Read + Seek>(reader: &mut R, size: u32) -> StreamResult<String> {
    let data: Vec<u8> = <_>::read_vec(reader, size as usize)
        .add_context(|| "read encoded utf8 data for read_utf8_str")?;

    let pos = reader.stream_position()?;

    String::from_utf8(data).map_err(|e| {
        StreamError::new_string_context(e, pos, "decode utf8 string for read_utf8_str")
    })
}

pub fn write_utf8_str<W: Write + Seek>(writer: &mut W, string: String) -> StreamResult<()> {
    let data = string.into_bytes();
    data.write_vec(writer)
        .add_context(|| "write utf8 encoded bytes for write_utf8_str")
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

pub fn read_utf16_str<R: Read + Seek>(reader: &mut R, size: u32) -> StreamResult<String> {
    let data: Vec<u16> = <_>::read_vec(reader, size as usize)
        .add_context(|| "read raw encoded utf16 data for read_utf16_str")?;

    let pos = reader.stream_position()?;

    String::from_utf16(&data).map_err(|e| {
        StreamError::new_string_context(e, pos, "decode utf16 string for read_utf16_str")
    })
}

pub fn write_utf16_str<W: Write + Seek>(writer: &mut W, string: &str) -> StreamResult<()> {
    let data: Vec<u16> = string.encode_utf16().collect();
    data.write_vec(writer)
        .add_context(|| "write utf16 encoded string")?;

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
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct ResStringPoolSpan {
    /// This is the name of the span -- that is, the name of the XML tag that defined it. The
    /// special value END (0xffffffff) indicates the end of an array of spans.
    pub name: ResStringPoolRef,
    /// The first character in the string that this span applies to.
    pub first_char: u32,
    /// The last character in the string that this span applies to.
    pub last_char: u32,
}

impl Readable for ResStringPoolSpan {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        Ok(Self {
            name: ResStringPoolRef::read_no_opts(reader)
                .add_context(|| "read name for ResStringPoolSpan")?,
            first_char: u32::read_no_opts(reader)
                .add_context(|| "read first_char for ResStringPoolSpan")?,
            last_char: u32::read_no_opts(reader)
                .add_context(|| "read last_char for ResStringPoolSpan")?,
        })
    }
}

impl Writeable for ResStringPoolSpan {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        self.name
            .write_no_opts(writer)
            .add_context(|| "write name for ResStringPoolSpan")?;
        self.first_char
            .write_no_opts(writer)
            .add_context(|| "write first_char for ResStringPoolSpan")?;
        self.last_char
            .write_no_opts(writer)
            .add_context(|| "write last_char for ResStringPoolSpan")
    }
}

impl ResStringPoolSpan {
    pub fn total_bytes() -> usize {
        4 + 4 + 4
    }
}
