use std::{borrow::Cow, fmt::Display, str::FromStr};

use crate::{
    defs::ResTableRef,
    stream::{
        NewResultCtx, Readable, ReadableNoOptions, StreamError, StreamResult, Writeable,
        WriteableNoOptions,
    },
    string_pool::{ResStringPoolRef, StringPoolHandler},
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ResValue {
    /// The data for this item
    pub data: ResValueType,
}

impl Readable for ResValue {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        let size = u16::read_no_opts(reader).add_context(|| "read size for ResValue")?;
        if size != 8 {
            return Err(StreamError::new_string_context(
                format!("invalid size {size}, expected 8"),
                reader.stream_position()?,
                "validate size for ResValue",
            ));
        }
        let res0 = u8::read_no_opts(reader).add_context(|| "read res0 for ResValue")?;
        if res0 != 0 {
            return Err(StreamError::new_string_context(
                format!("invalid res0 {res0}, expected 0"),
                reader.stream_position()?,
                "validate res0 for ResValue",
            ));
        }
        Ok(Self {
            data: ResValueType::read_no_opts(reader).add_context(|| "read data for ResValue")?,
        })
    }
}

impl Writeable for ResValue {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        let size: u16 = 8;
        size.write_no_opts(writer)
            .add_context(|| "write size for ResValue")?;
        let res0: u8 = 0;
        res0.write_no_opts(writer)
            .add_context(|| "write res0 for ResValue")?;
        self.data
            .write_no_opts(writer)
            .add_context(|| "write data for ResValue")
    }
}

impl ResValue {
    /// Create a new ResValue struct
    pub fn new(data: ResValueType) -> Self {
        Self { data }
    }

    /// Set the value to be a string, allocating the string in the string pool if necessary
    pub fn write_string(&mut self, string: Cow<'_, str>, string_pool: &mut StringPoolHandler) {
        let reference = string_pool.allocate(string);

        self.data = ResValueType::String(reference);
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ResTypeNullType {
    Undefined,
    Empty,
}

impl Readable for ResTypeNullType {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        let res_type =
            u32::read_no_opts(reader).add_context(|| "read value id for ResTypeNullType")?;
        Ok(match res_type {
            0 => Self::Undefined,
            1 => Self::Empty,
            v => {
                return Err(StreamError::new_string_context(
                    format!("invalid null value: {v}, expected 0 or 1"),
                    reader.stream_position()?,
                    "read ResTypeNullType",
                ));
            }
        })
    }
}

impl Writeable for ResTypeNullType {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        let res_type: u32 = match self {
            Self::Undefined => 0,
            Self::Empty => 1,
        };
        res_type
            .write_no_opts(writer)
            .add_context(|| "write ResTypeNullType value")
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ResTypeBoolType {
    False,
    True,
    Null,
}

impl Readable for ResTypeBoolType {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        let bool_type = u32::read_no_opts(reader)?;
        Ok(match bool_type {
            0 => Self::False,
            1 => Self::True,
            0xffffffff => Self::Null,
            v => {
                return Err(StreamError::new_string_context(
                    format!("invalid bool type {v}, expected 0, 1 or 0xffffffff"),
                    reader.stream_position()?,
                    "read ResTypeBoolType",
                ));
            }
        })
    }
}

impl Writeable for ResTypeBoolType {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        let bool_type: u32 = match self {
            Self::False => 0,
            Self::True => 1,
            Self::Null => 0xffffffff,
        };
        bool_type
            .write_no_opts(writer)
            .add_context(|| "write ResTypeBoolType")
    }
}

impl Display for ResTypeBoolType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            ResTypeBoolType::Null => "null",
            ResTypeBoolType::True => "true",
            ResTypeBoolType::False => "false",
        };
        write!(f, "{str}")
    }
}

#[derive(Debug)]
pub struct InvalidBool {
    pub input: String,
}

impl FromStr for ResTypeBoolType {
    type Err = InvalidBool;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "null" => Self::Null,
            "true" => Self::True,
            "false" => Self::False,
            _ => Err(InvalidBool {
                input: s.to_string(),
            })?,
        })
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct ARGB8 {
    pub aarrggbb: u32,
}

impl Readable for ARGB8 {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        Ok(Self {
            aarrggbb: u32::read_no_opts(reader).add_context(|| "read ARGB8")?,
        })
    }
}

impl Writeable for ARGB8 {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        self.aarrggbb
            .write_no_opts(writer)
            .add_context(|| "write ARGB8")
    }
}

impl ARGB8 {
    pub fn new(alpha: u32, red: u32, green: u32, blue: u32) -> Self {
        Self {
            aarrggbb: blue | (green << 8) | (red << 16) | (alpha << 24),
        }
    }
    pub fn alpha(&self) -> u8 {
        (self.aarrggbb >> 24) as u8
    }
    pub fn red(&self) -> u8 {
        (self.aarrggbb >> 16) as u8
    }
    pub fn green(&self) -> u8 {
        (self.aarrggbb >> 8) as u8
    }
    pub fn blue(&self) -> u8 {
        (self.aarrggbb) as u8
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct RGB8 {
    pub rrggbb: u32,
}

impl Readable for RGB8 {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        Ok(Self {
            rrggbb: u32::read_no_opts(reader).add_context(|| "read RGB8")?,
        })
    }
}

impl Writeable for RGB8 {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        self.rrggbb
            .write_no_opts(writer)
            .add_context(|| "read RGB8")
    }
}

impl RGB8 {
    pub fn new(red: u32, green: u32, blue: u32) -> Self {
        Self {
            rrggbb: blue | (green << 8) | (red << 16),
        }
    }

    pub fn red(&self) -> u8 {
        (self.rrggbb >> 16) as u8
    }
    pub fn green(&self) -> u8 {
        (self.rrggbb >> 8) as u8
    }
    pub fn blue(&self) -> u8 {
        (self.rrggbb) as u8
    }
}
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct ARGB4 {
    pub argb: u32,
}

impl Readable for ARGB4 {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        Ok(Self {
            argb: u32::read_no_opts(reader).add_context(|| "read ARGB4")?,
        })
    }
}

impl Writeable for ARGB4 {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        self.argb
            .write_no_opts(writer)
            .add_context(|| "write ARGB4")
    }
}

impl ARGB4 {
    pub fn new(alpha: u32, red: u32, green: u32, blue: u32) -> Self {
        Self {
            argb: blue | (green << 4) | (red << 8) | (alpha << 12),
        }
    }

    pub fn alpha(&self) -> u8 {
        (self.argb >> 12) as u8 & 0xF
    }
    pub fn red(&self) -> u8 {
        (self.argb >> 8) as u8 & 0xF
    }
    pub fn green(&self) -> u8 {
        (self.argb >> 4) as u8 & 0xF
    }
    pub fn blue(&self) -> u8 {
        (self.argb) as u8 & 0xF
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct RGB4 {
    pub rgb: u32,
}

impl Readable for RGB4 {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        Ok(Self {
            rgb: u32::read_no_opts(reader).add_context(|| "read RGB4")?,
        })
    }
}

impl Writeable for RGB4 {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        self.rgb.write_no_opts(writer).add_context(|| "write RGB4")
    }
}
impl RGB4 {
    pub fn new(red: u32, green: u32, blue: u32) -> Self {
        Self {
            rgb: blue | (green << 4) | (red << 8),
        }
    }

    pub fn red(&self) -> u8 {
        (self.rgb >> 8) as u8 & 0xF
    }
    pub fn green(&self) -> u8 {
        (self.rgb >> 4) as u8 & 0xF
    }
    pub fn blue(&self) -> u8 {
        (self.rgb) as u8 & 0xF
    }
}

impl From<bool> for ResTypeBoolType {
    fn from(value: bool) -> Self {
        match value {
            true => ResTypeBoolType::True,
            false => ResTypeBoolType::False,
        }
    }
}

impl From<ResTypeBoolType> for bool {
    fn from(value: ResTypeBoolType) -> Self {
        match value {
            ResTypeBoolType::True => true,
            ResTypeBoolType::False => false,
            ResTypeBoolType::Null => false,
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ResValueType {
    /// The 'data' is either 0 or 1, specifying this resource is either undefined or empty,
    /// respectively.
    Null(ResTypeNullType),
    /// The 'data' holds a ResTable_ref, a reference to another resource table entry.
    Reference(ResTableRef),
    /// The 'data' holds an attribute resource identifier.
    Attribute(u32),
    /// The 'data' holds a ResStringPool_ref, a reference into the containing resource table's global value string pool.
    String(ResStringPoolRef),
    /// The 'data' holds a single-precision floating point number.
    Float(f32),
    /// The 'data' holds a complex number encoding a dimension value, such as "100in".
    Dimension(u32),
    /// The 'data' holds a complex number encoding a fraction of a container.
    Fraction(u32),
    /// The 'data' holds a dynamic ResTable_ref which needs to be resolved before it can be used
    /// like a TYPE_REFERENCE.
    DynamicReference(ResTableRef),
    /// The 'data' holds an attribute resource identifier, which needs to be resolved before it can
    /// be used like a TYPE_ATTRIBUTE.
    DynamicAttribute(u32),
    /// The 'data' is a raw integer value of the form n..n.
    IntDec(u32),
    /// The 'data' is a raw integer value of the form 0xn..n.
    IntHex(u32),
    /// The 'data' is either 0 or 1, for input "false" or "true" respectively.
    IntBoolean(ResTypeBoolType),
    /// The 'data' is a raw integer value of the form #aarrggbb.
    IntColorARGB8(ARGB8),
    /// The 'data' is a raw integer value of the form #rrggbb.
    IntColorRGB8(RGB8),
    /// The 'data' is a raw integer value of the form #argb.
    IntColorARGB4(ARGB4),
    /// The 'data' is a raw integer value of the form #rgb.
    IntColorRGB4(RGB4),
}

impl Readable for ResValueType {
    type Args = ();

    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        let res_type =
            u8::read_no_opts(reader).add_context(|| "read res_type id for ResValueType")?;
        Ok(match res_type {
            0x0 => ResValueType::Null(ResTypeNullType::read_no_opts(reader)?),
            0x1 => ResValueType::Reference(ResTableRef::read_no_opts(reader)?),
            0x2 => ResValueType::Attribute(
                u32::read_no_opts(reader).add_context(|| "read attribute for ResValueType")?,
            ),
            0x3 => ResValueType::String(ResStringPoolRef::read_no_opts(reader)?),
            0x4 => ResValueType::Float(
                f32::read_no_opts(reader).add_context(|| "read float for ResValueType")?,
            ),
            0x5 => ResValueType::Dimension(
                u32::read_no_opts(reader).add_context(|| "read dimension for ResValueType")?,
            ),
            0x6 => ResValueType::Fraction(
                u32::read_no_opts(reader).add_context(|| "read fraction for ResValueType")?,
            ),
            0x7 => ResValueType::DynamicReference(ResTableRef::read_no_opts(reader)?),
            0x8 => ResValueType::DynamicAttribute(
                u32::read_no_opts(reader)
                    .add_context(|| "read dynamic attribute for ResValueType")?,
            ),
            0x10 => ResValueType::IntDec(
                u32::read_no_opts(reader).add_context(|| "read int dec for ResValueType")?,
            ),
            0x11 => ResValueType::IntHex(
                u32::read_no_opts(reader).add_context(|| "read int hex for ResValueType")?,
            ),
            0x12 => ResValueType::IntBoolean(ResTypeBoolType::read_no_opts(reader)?),
            0x1c => ResValueType::IntColorARGB8(ARGB8::read_no_opts(reader)?),
            0x1d => ResValueType::IntColorRGB8(RGB8::read_no_opts(reader)?),
            0x1e => ResValueType::IntColorARGB4(ARGB4::read_no_opts(reader)?),
            0x1f => ResValueType::IntColorRGB4(RGB4::read_no_opts(reader)?),
            v => {
                return Err(StreamError::new_string_context(
                    format!("invalid res_type {v}"),
                    reader.stream_position()?,
                    "match res_type for ResValueType",
                ));
            }
        })
    }
}

impl Writeable for ResValueType {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        let pos = writer.stream_position()?;
        writer.seek_relative(1)?;
        let res_type: u8 = match self {
            Self::Null(v) => {
                v.write_no_opts(writer)?;
                0
            }
            Self::Reference(v) => {
                v.write_no_opts(writer)?;
                0x1
            }
            Self::Attribute(v) => {
                v.write_no_opts(writer)?;
                0x2
            }
            Self::String(v) => {
                v.write_no_opts(writer)?;
                0x3
            }
            Self::Float(v) => {
                v.write_no_opts(writer)?;
                0x4
            }
            Self::Dimension(v) => {
                v.write_no_opts(writer)?;
                0x5
            }
            Self::Fraction(v) => {
                v.write_no_opts(writer)?;
                0x6
            }
            Self::DynamicReference(v) => {
                v.write_no_opts(writer)?;
                0x7
            }
            Self::DynamicAttribute(v) => {
                v.write_no_opts(writer)?;
                0x8
            }
            Self::IntDec(v) => {
                v.write_no_opts(writer)?;
                0x10
            }
            Self::IntHex(v) => {
                v.write_no_opts(writer)?;
                0x11
            }
            Self::IntBoolean(v) => {
                v.write_no_opts(writer)?;
                0x12
            }
            Self::IntColorARGB8(v) => {
                v.write_no_opts(writer)?;
                0x1c
            }
            Self::IntColorRGB8(v) => {
                v.write_no_opts(writer)?;
                0x1d
            }
            Self::IntColorARGB4(v) => {
                v.write_no_opts(writer)?;
                0x1e
            }
            Self::IntColorRGB4(v) => {
                v.write_no_opts(writer)?;
                0x1f
            }
        };
        writer.seek(std::io::SeekFrom::Start(pos))?;
        res_type
            .write_no_opts(writer)
            .add_context(|| "write res_type for ResValueType")?;
        Ok(writer.seek_relative(4)?)
    }
}
