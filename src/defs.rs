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

// Everything here is based off of https://android.googlesource.com/platform/frameworks/base/+/master/libs/androidfw/include/androidfw/ResourceTypes.h

use std::{fmt::Display, io::SeekFrom, num::ParseIntError, str::FromStr};

use binrw::{binread, binrw, BinRead, BinResult, BinWrite};

use crate::{
    string_pool::StringPool,
    xmltree::{
        RawXMLTree, ResXMLTree_attrExt, ResXMLTree_cdataExt, ResXMLTree_endElementExt,
        ResXMLTree_namespaceExt,
    },
};

// So that identifier names can be the same

/// Header that appears at the front of every data chunk in a resource.
//#[binrw]#
#[binread]
#[br(stream = s)]
//#[brw(stream = s)]
#[derive(Debug, PartialEq, Clone)]
pub struct ResChunk_header {
    #[br(temp)]
    #[br(try_calc = s.stream_position())]
    //#[brw(try_calc=s.stream_position())]
    //#[brw(restore_position)]
    start_pos: u64,
    /// Type identifier for this chunk. The meaning of this value depends on the containing chunk.
    #[br(temp)]
    pub r#type: ResType,
    /// Size of the chunk header (in bytes). Adding this value to the address of the chunk allows
    /// you to find its associated data (if any).
    #[br(temp)]
    pub headerSize: u16,
    /// Total size of this chunk (in bytes). This is the chunkSize plus the size of any data
    /// associated with the chunk. Adding this value to the chunk allows you to completely skip its
    /// contents (including any child chunks). If this value is the same as chunkSize, there is no
    /// data associated with the chunk.
    #[br(temp)]
    pub size: u32,

    #[br(parse_with=parse_res_type_value, args(&r#type, size))]
    #[br(pad_size_to=(size as u64) - 2 - 2 - 4)]
    pub data: ResTypeValue,
}

impl BinWrite for ResChunk_header {
    type Args<'a> = ();
    fn write_options<W: std::io::Write + std::io::Seek>(
        &self,
        writer: &mut W,
        endian: binrw::Endian,
        _args: Self::Args<'_>,
    ) -> BinResult<()> {
        let r#type: ResType = (&self.data).into();
        r#type.write_options(writer, endian, ())?;

        let header_size = self.data.get_header_size() as u16;
        writer.write_all(&header_size.to_le_bytes())?;

        let size_pos = writer.stream_position()?;

        writer.seek(SeekFrom::Current(4))?;

        let start_pos = writer.stream_position()?;

        write_res_type_value(&self.data, writer, endian, ())?;

        let end_pos = writer.stream_position()?;

        let size = (end_pos - start_pos) as u32;

        writer.seek(SeekFrom::Start(size_pos))?;

        writer.write_all(&size.to_le_bytes())?;

        writer.seek(SeekFrom::Start(end_pos))?;

        Ok(())
    }
}

impl From<&ResTypeValue> for ResType {
    fn from(value: &ResTypeValue) -> Self {
        match value {
            ResTypeValue::NULL => ResType::RES_NULL_TYPE,
            ResTypeValue::TABLE => ResType::RES_TABLE_TYPE,
            ResTypeValue::STRING_POOL(_) => ResType::RES_STRING_POOL_TYPE,
            ResTypeValue::XML(_) => ResType::RES_XML_TYPE,
            ResTypeValue::START_ELEMENT(_) => ResType::RES_XML_START_ELEMENT_TYPE,
            ResTypeValue::CDATA(_) => ResType::RES_XML_CDATA_TYPE,
            ResTypeValue::TABLE_TYPE => ResType::RES_TABLE_TYPE_TYPE,
            ResTypeValue::TABLE_SPEC => ResType::RES_TABLE_TYPE_SPEC_TYPE,
            ResTypeValue::TABLE_PACKAGE => ResType::RES_TABLE_PACKAGE_TYPE,
            ResTypeValue::TABLE_LIBRARY => ResType::RES_TABLE_LIBRARY_TYPE,
            ResTypeValue::END_ELEMENT(_) => ResType::RES_XML_END_ELEMENT_TYPE,
            ResTypeValue::RESOURCE_MAP(_) => ResType::RES_XML_RESOURCE_MAP_TYPE,
            ResTypeValue::TYPE_STAGED_ALIAS => ResType::RES_TABLE_STAGED_ALIAS_TYPE,
            ResTypeValue::END_NAMESPACE(_) => ResType::RES_XML_END_NAMESPACE_TYPE,
            ResTypeValue::TABLE_OVERLAYABALE => ResType::RES_TABLE_OVERLAYABLE_TYPE,
            ResTypeValue::START_NAMESPACE(_) => ResType::RES_XML_START_NAMESPACE_TYPE,
            ResTypeValue::TABLE_OVERLAYABALE_POLICY => ResType::RES_TABLE_OVERLAYABLE_POLICY_TYPE,
        }
    }
}

impl From<ResTypeValue> for ResChunk_header {
    fn from(value: ResTypeValue) -> Self {
        return Self { data: value };
    }
}

#[derive(Debug)]
pub struct NotImplimentedError;

impl Display for NotImplimentedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "not implimented error")
    }
}

#[binrw]
#[derive(Debug, PartialEq, Clone)]
#[br(import(size: u32))]
/// This contains a uint32_t array mapping strings in the string pool back to resource identifiers.
/// It is optional.
///
/// Not actually optional though, apk will fail to install without it.
pub struct ResourceMap {
    #[br(count=(size-ResourceMap::get_header_size() as u32) / 4)]
    pub mapping: Vec<u32>,
}

impl ResourceMap {
    pub fn get_header_size() -> usize {
        0x8
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ResTypeValue {
    NULL,
    STRING_POOL(StringPool),
    TABLE,
    XML(RawXMLTree),
    START_NAMESPACE(ResXMLTree_namespaceExt),
    END_NAMESPACE(ResXMLTree_namespaceExt),
    START_ELEMENT(ResXMLTree_attrExt),
    END_ELEMENT(ResXMLTree_endElementExt),
    CDATA(ResXMLTree_cdataExt),
    RESOURCE_MAP(ResourceMap),
    TABLE_PACKAGE,
    TABLE_TYPE,
    TABLE_SPEC,
    TABLE_LIBRARY,
    TABLE_OVERLAYABALE,
    TABLE_OVERLAYABALE_POLICY,
    TYPE_STAGED_ALIAS,
}

impl ResTypeValue {
    pub fn get_header_size(&self) -> usize {
        match self {
            ResTypeValue::XML(_) => RawXMLTree::get_header_size(),
            ResTypeValue::STRING_POOL(_) => StringPool::get_header_size(),
            ResTypeValue::RESOURCE_MAP(_) => ResourceMap::get_header_size(),
            ResTypeValue::START_NAMESPACE(_) => ResXMLTree_namespaceExt::get_header_size(),
            ResTypeValue::START_ELEMENT(_) => ResXMLTree_attrExt::get_header_size(),
            ResTypeValue::END_ELEMENT(_) => ResXMLTree_endElementExt::get_header_size(),
            ResTypeValue::END_NAMESPACE(_) => ResXMLTree_namespaceExt::get_header_size(),
            _ => todo!(),
        }
    }
}

#[binrw::parser(reader, endian)]
pub fn parse_res_type_value(res_type: &ResType, size: u32) -> BinResult<ResTypeValue> {
    Ok(match res_type {
        ResType::RES_NULL_TYPE => ResTypeValue::NULL,
        ResType::RES_XML_TYPE => {
            ResTypeValue::XML(RawXMLTree::read_options(reader, endian, (size,))?)
        }
        ResType::RES_STRING_POOL_TYPE => {
            ResTypeValue::STRING_POOL(StringPool::read_options(reader, endian, ())?)
        }
        ResType::RES_XML_RESOURCE_MAP_TYPE => {
            ResTypeValue::RESOURCE_MAP(ResourceMap::read_options(reader, endian, (size,))?)
        }
        ResType::RES_XML_START_NAMESPACE_TYPE => ResTypeValue::START_NAMESPACE(
            ResXMLTree_namespaceExt::read_options(reader, endian, ())?,
        ),
        ResType::RES_XML_START_ELEMENT_TYPE => {
            ResTypeValue::START_ELEMENT(ResXMLTree_attrExt::read_options(reader, endian, ())?)
        }
        ResType::RES_XML_END_ELEMENT_TYPE => {
            ResTypeValue::END_ELEMENT(ResXMLTree_endElementExt::read_options(reader, endian, ())?)
        }
        ResType::RES_XML_END_NAMESPACE_TYPE => {
            ResTypeValue::END_NAMESPACE(ResXMLTree_namespaceExt::read_options(reader, endian, ())?)
        }
        _ => todo!(),
    })
}

#[binrw::writer(writer, endian)]
pub fn write_res_type_value(value: &ResTypeValue) -> BinResult<()> {
    Ok(match value {
        ResTypeValue::STRING_POOL(sp) => sp.write_options(writer, endian, ())?,
        ResTypeValue::START_ELEMENT(st) => st.write_options(writer, endian, ())?,
        ResTypeValue::END_ELEMENT(ee) => ee.write_options(writer, endian, ())?,
        ResTypeValue::XML(xml) => xml.write_options(writer, endian, ())?,
        v => {
            dbg!(v);
            todo!()
        }
    })
}

impl ResChunk_header {
    /// Subtracts the header size (8) from the current pos. Useful when a struct needs to find the
    /// start offset of the header. If pos is < 8, 0 is returned.
    pub fn get_header_offset(pos: u64) -> u64 {
        if pos <= 8 {
            return 0;
        }
        pos - 8 // header size (2 + 2 + 4)
    }
}

#[derive(Debug, PartialEq, BinRead, BinWrite)]
pub enum ResType {
    #[brw(magic(0x0000u16))]
    RES_NULL_TYPE,
    #[brw(magic(0x0001u16))]
    RES_STRING_POOL_TYPE,
    #[brw(magic(0x0002u16))]
    RES_TABLE_TYPE,
    #[brw(magic(0x0003u16))]
    RES_XML_TYPE,
    // Chunk types in RES_XML_TYPE
    // RES_XML_FIRST_CHUNK_TYPE = 0x0100,
    #[brw(magic(0x0100u16))]
    RES_XML_START_NAMESPACE_TYPE,
    #[brw(magic(0x0101u16))]
    RES_XML_END_NAMESPACE_TYPE,
    #[brw(magic(0x0102u16))]
    RES_XML_START_ELEMENT_TYPE,
    #[brw(magic(0x0103u16))]
    RES_XML_END_ELEMENT_TYPE,
    #[brw(magic(0x0104u16))]
    RES_XML_CDATA_TYPE,
    //#[brw(magic(0x017fu16))]
    //RES_XML_LAST_CHUNK_TYPE,
    /// This contains a uint32_t array mapping strings in the string
    /// pool back to resource identifiers.  It is optional.
    #[brw(magic(0x0180u16))]
    RES_XML_RESOURCE_MAP_TYPE,
    // Chunk types in RES_TABLE_TYPE
    #[brw(magic(0x0200u16))]
    RES_TABLE_PACKAGE_TYPE,
    #[brw(magic(0x0201u16))]
    RES_TABLE_TYPE_TYPE,
    #[brw(magic(0x0202u16))]
    RES_TABLE_TYPE_SPEC_TYPE,
    #[brw(magic(0x0203u16))]
    RES_TABLE_LIBRARY_TYPE,
    #[brw(magic(0x0204u16))]
    RES_TABLE_OVERLAYABLE_TYPE,
    #[brw(magic(0x0205u16))]
    RES_TABLE_OVERLAYABLE_POLICY_TYPE,
    #[brw(magic(0x0206u16))]
    RES_TABLE_STAGED_ALIAS_TYPE,
}

/// This is a reference to a unique entry (a ResTable_entry structure) in a resource table. The
/// value is structured as 0xpptteee, where pp is the package index, tt is the type index in that
/// package, and eeee is the entry index in that type. The package and type values start a 1 for
/// the first item, to help catch cases where they have been supplied.
#[derive(Debug, BinRead, BinWrite, PartialEq, Copy, Clone)]
pub struct ResTable_ref {
    pub entry_index: u16,
    pub type_index: u8,
    pub package_index: u8,
}

impl Display for ResTable_ref {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{:x}", <ResTable_ref as Into<u32>>::into(*self))
    }
}

impl From<ResTable_ref> for u32 {
    fn from(value: ResTable_ref) -> Self {
        (value.entry_index as u32)
            + ((value.type_index as u32) << 16)
            + ((value.package_index as u32) << 24)
    }
}

impl From<u32> for ResTable_ref {
    fn from(value: u32) -> Self {
        Self {
            package_index: (value >> 24) as u8, // as u8 does & 0xff
            type_index: (value >> 16) as u8,    // as u8 does & 0xff
            entry_index: value as u16,          // as u16 does & 0xffff
        }
    }
}

#[derive(Debug)]
pub enum ParseResTable_refError {
    Int(ParseIntError),
    InvalidStartChar,
}

impl FromStr for ResTable_ref {
    type Err = ParseResTable_refError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let first = s
            .chars()
            .next()
            .ok_or(ParseResTable_refError::InvalidStartChar)?;
        if first != '@' {
            return Err(ParseResTable_refError::InvalidStartChar);
        }
        let rest: String = s.chars().skip(1).collect();

        let val: u32 = rest.parse().map_err(ParseResTable_refError::Int)?;

        Ok(val.into())
    }
}
