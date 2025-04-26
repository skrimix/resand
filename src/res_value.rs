/*
    Copyright (C) 2025 fieryhenry

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

use std::{fmt::Display, num::ParseIntError, str::FromStr};

use binrw::{binrw, BinRead, BinWrite};

use crate::{
    defs::ResTableRef,
    string_pool::{ResStringPoolRef, StringPool},
};

#[binrw]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ResValue {
    /// Number of bytes in this structure.
    #[br(temp)]
    #[bw(calc = 8)]
    #[br(assert(size==8))]
    size: u16,

    /// Always set to 0.
    #[br(temp)]
    #[bw(calc = 0)]
    #[br(assert(res0==0))]
    res0: u8,

    /// The data for this item, as interpreted according to dataType.
    pub data: ResValueType,
}

impl ResValue {
    pub fn new(data: ResValueType) -> Self {
        Self { data }
    }

    pub fn write_string(&mut self, string: String, string_pool: &mut StringPool) {
        let reference = string_pool.allocate(string);

        self.data = ResValueType::String(reference);
    }
}

#[derive(Debug, BinRead, BinWrite, PartialEq, Copy, Clone)]
pub enum ResTypeNullType {
    #[brw(magic(0u32))]
    Undefined,
    #[brw(magic(1u32))]
    Empty,
}

#[derive(Debug, BinRead, BinWrite, PartialEq, Copy, Clone)]
pub enum ResTypeBoolType {
    #[brw(magic(0u32))]
    False,
    #[brw(magic(1u32))]
    True,
    #[brw(magic(0xffffffffu32))]
    Null,
}

impl Display for ResTypeBoolType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            ResTypeBoolType::Null => "null",
            ResTypeBoolType::True => "true",
            ResTypeBoolType::False => "false",
        };
        write!(f, "{}", str)
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

#[derive(Debug, BinRead, BinWrite, PartialEq, Copy, Clone)]
pub struct ARGB8 {
    pub aarrggbb: u32,
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

#[derive(Debug, BinRead, BinWrite, PartialEq, Copy, Clone)]
pub struct RGB8 {
    pub rrggbb: u32,
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
#[derive(Debug, BinRead, BinWrite, PartialEq, Copy, Clone)]
pub struct ARGB4 {
    pub argb: u32,
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

#[derive(Debug, BinRead, BinWrite, PartialEq, Clone, Copy)]
pub struct RGB4 {
    pub rgb: u32,
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

#[derive(Debug, BinRead, BinWrite, PartialEq, Copy, Clone)]
pub enum ResValueType {
    /// The 'data' is either 0 or 1, specifying this resource is either undefined or empty,
    /// respectively.
    #[brw(magic(0x00u8))]
    Null(ResTypeNullType),
    /// The 'data' holds a ResTable_ref, a reference to another resource table entry.
    #[brw(magic(0x01u8))]
    Reference(ResTableRef),
    /// The 'data' holds an attribute resource identifier.
    #[brw(magic(0x02u8))]
    Attribute(u32),
    /// The 'data' holds a ResStringPool_ref, a reference into the containing resource table's global value string pool.
    #[brw(magic(0x03u8))]
    String(ResStringPoolRef),
    /// The 'data' holds a single-precision floating point number.
    #[brw(magic(0x04u8))]
    Float(f32),
    /// The 'data' holds a complex number encoding a dimension value, such as "100in".
    #[brw(magic(0x05u8))]
    Dimension(u32),
    /// The 'data' holds a complex number encoding a fraction of a container.
    #[brw(magic(0x06u8))]
    Fraction(u32),
    /// The 'data' holds a dynamic ResTable_ref which needs to be resolved before it can be used
    /// like a TYPE_REFERENCE.
    #[brw(magic(0x07u8))]
    DyanmicReference(ResTableRef),
    /// The 'data' holds an attribute resource identifier, which needs to be resolved before it can
    /// be used like a TYPE_ATTRIBUTE.
    #[brw(magic(0x08u8))]
    DynamicAttribute(u32),
    /// The 'data' is a raw integer value of the form n..n.
    #[brw(magic(0x010u8))]
    IntDec(u32),
    /// The 'data' is a raw integer value of the form 0xn..n.
    #[brw(magic(0x11u8))]
    IntHex(u32),
    /// The 'data' is either 0 or 1, for input "false" or "true" respectively.
    #[brw(magic(0x12u8))]
    IntBoolean(ResTypeBoolType),
    /// The 'data' is a raw integer value of the form #aarrggbb.
    #[brw(magic(0x1cu8))]
    IntColorARGB8(ARGB8),
    /// The 'data' is a raw integer value of the form #rrggbb.
    #[brw(magic(0x1du8))]
    IntColorRGB8(RGB8),
    /// The 'data' is a raw integer value of the form #argb.
    #[brw(magic(0x1eu8))]
    IntColorARGB4(ARGB4),
    /// The 'data' is a raw integer value of the form #rgb.
    #[brw(magic(0x1fu8))]
    IntColorRGB4(RGB4),
}

#[derive(Debug)]
pub enum InvalidResType {
    InvalidInteger(ParseIntError),
}

impl From<ParseIntError> for InvalidResType {
    fn from(value: ParseIntError) -> Self {
        Self::InvalidInteger(value)
    }
}

impl ResValueType {
    pub fn unresolve(string: &str, pool: &mut StringPool, key: &str) -> Self {
        let non_int_keys = [
            "versionName",
            "compileSdkVersionCodename",
            "platformBuildVersionName",
        ];
        let non_float = ["compileSdkVersionCodename", "versionName"];
        if let Ok(reference) = string.parse() {
            return ResValueType::Reference(reference);
        }
        if let Ok(bool) = string.parse() {
            return ResValueType::IntBoolean(bool);
        }

        if let Ok(int) = string.parse() {
            if !non_int_keys.contains(&key) {
                return ResValueType::IntDec(int);
            }
        }

        if string.chars().take(2).collect::<String>() == "0x" {
            let str: String = string.chars().skip(2).collect();
            if let Ok(int) = u32::from_str_radix(&str, 16) {
                if !non_int_keys.contains(&key) {
                    return ResValueType::IntHex(int);
                }
            }
        }
        if let Ok(float) = string.parse() {
            if !non_float.contains(&key) {
                return ResValueType::Float(float);
            }
        }

        ResValueType::String(pool.allocate(string.to_string()))
    }
}
