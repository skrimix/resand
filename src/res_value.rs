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

use binrw::{BinRead, BinWrite};

use crate::{defs::ResTable_ref, string_pool::ResStringPool_ref};

#[derive(Debug, BinRead, BinWrite)]
pub struct Res_value {
    /// Number of bytes in this structure.
    pub size: u16,

    /// Always set to 0.
    pub res0: u8,

    /// The data for this item, as interpreted according to dataType.
    pub data: ResValueType,
}

impl Res_value {
    pub fn new(data: ResValueType) -> Self {
        Self {
            size: 8,
            res0: 0,
            data,
        }
    }
}

#[derive(Debug, BinRead, BinWrite, PartialEq)]
pub enum ResTypeNullType {
    #[brw(magic(0u32))]
    DATA_NULL_UNDEFINED,
    #[brw(magic(1u32))]
    DATA_NULL_EMPTY,
}
#[derive(Debug, BinRead, BinWrite, PartialEq)]
pub enum ResTypeBoolType {
    #[brw(magic(0u32))]
    FALSE,
    #[brw(magic(1u32))]
    TRUE,
}

#[derive(Debug, BinRead, BinWrite, PartialEq)]
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

#[derive(Debug, BinRead, BinWrite, PartialEq)]
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
#[derive(Debug, BinRead, BinWrite, PartialEq)]
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

#[derive(Debug, BinRead, BinWrite, PartialEq)]
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
            true => ResTypeBoolType::TRUE,
            false => ResTypeBoolType::FALSE,
        }
    }
}

impl From<ResTypeBoolType> for bool {
    fn from(value: ResTypeBoolType) -> Self {
        match value {
            ResTypeBoolType::TRUE => true,
            ResTypeBoolType::FALSE => false,
        }
    }
}

#[derive(Debug, BinRead, BinWrite, PartialEq)]
pub enum ResValueType {
    /// The 'data' is either 0 or 1, specifying this resource is either undefined or empty,
    /// respectively.
    #[brw(magic(0x00u8))]
    TYPE_NULL(ResTypeNullType),
    /// The 'data' holds a ResTable_ref, a reference to another resource table entry.
    #[brw(magic(0x01u8))]
    TYPE_REFERENCE(ResTable_ref),
    /// The 'data' holds an attribute resource identifier.
    #[brw(magic(0x02u8))]
    TYPE_ATTRIBUTE(u32),
    /// The 'data' holds a ResStringPool_ref, a reference into the containing resource table's global value string pool.
    #[brw(magic(0x03u8))]
    TYPE_STRING(ResStringPool_ref),
    /// The 'data' holds a single-precision floating point number.
    #[brw(magic(0x04u8))]
    TYPE_FLOAT(f32),
    /// The 'data' holds a complex number encoding a dimension value, such as "100in".
    #[brw(magic(0x05u8))]
    TYPE_DIMENSION(u32),
    /// The 'data' holds a complex number encoding a fraction of a container.
    #[brw(magic(0x06u8))]
    TYPE_FRACTION(u32),
    /// The 'data' holds a dynamic ResTable_ref which needs to be resolved before it can be used
    /// like a TYPE_REFERENCE.
    #[brw(magic(0x07u8))]
    TYPE_DYNAMIC_REFERENCE(ResTable_ref),
    /// The 'data' holds an attribute resource identifier, which needs to be resolved before it can
    /// be used like a TYPE_ATTRIBUTE.
    #[brw(magic(0x08u8))]
    TYPE_DYNAMIC_ATTRIBUTE(u32),
    /// The 'data' is a raw integer value of the form n..n.
    #[brw(magic(0x010u8))]
    TYPE_INT_DEC(u32),
    /// The 'data' is a raw integer value of the form 0xn..n.
    #[brw(magic(0x11u8))]
    TYPE_INT_HEX(u32),
    /// The 'data' is either 0 or 1, for input "false" or "true" respectively.
    #[brw(magic(0x12u8))]
    TYPE_INT_BOOLEAN(ResTypeBoolType),
    /// The 'data' is a raw integer value of the form #aarrggbb.
    #[brw(magic(0x1cu8))]
    TYPE_INT_COLOR_ARGB8(ARGB8),
    /// The 'data' is a raw integer value of the form #rrggbb.
    #[brw(magic(0x1du8))]
    TYPE_INT_COLOR_RGB8(RGB8),
    /// The 'data' is a raw integer value of the form #argb.
    #[brw(magic(0x1eu8))]
    TYPE_INT_COLOR_ARGB4(ARGB4),
    /// The 'data' is a raw integer value of the form #rgb.
    #[brw(magic(0x1fu8))]
    TYPE_INT_COLOR_RGB4(RGB4),
}
