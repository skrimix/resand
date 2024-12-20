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

use binrw::{BinRead, BinWrite};

// So that identifier names can be the same

/// Header that appears at the front of every data chunk in a resource.
#[derive(Debug, BinRead, BinWrite, PartialEq)]
pub struct ResChunk_header {
    /// Type identifier for this chunk. The meaning of this value depends on the containing chunk.
    pub r#type: ResType,
    /// Size of the chunk header (in bytes). Adding this value to the address of the chunk allows
    /// you to find its associated data (if any).
    pub headerSize: u16,
    /// Total size of this chunk (in bytes). This is the chunkSize plus the size of any data
    /// associated with the chunk. Adding this value to the chunk allows you to completely skip its
    /// contents (including any child chunks). If this value is the same as chunkSize, there is no
    /// data associated with the chunk.
    pub size: u32,
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
    #[brw(magic(0x017fu16))]
    RES_XML_LAST_CHUNK_TYPE,
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
#[derive(Debug, BinRead, BinWrite, PartialEq)]
pub struct ResTable_ref {
    pub entry_index: u16,
    pub type_index: u8,
    pub package_index: u8,
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

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use binrw::BinReaderExt;

    use super::*;
    use binrw::BinWriterExt;

    #[test]
    fn header_read() {
        let mut reader = Cursor::new(b"\x02\x00\x10\x00\xf0\x0a\x00\x00");
        let header: ResChunk_header = reader.read_le().unwrap();

        assert_eq!(header.r#type, ResType::RES_TABLE_TYPE);
        assert_eq!(header.headerSize, 16);
        assert_eq!(header.size, 2800);
    }

    #[test]
    fn header_write() {
        let mut writer = Cursor::new(Vec::new());
        writer
            .write_le(&ResChunk_header {
                r#type: ResType::RES_XML_RESOURCE_MAP_TYPE,
                headerSize: 32,
                size: 100001,
            })
            .unwrap();

        assert_eq!(writer.into_inner(), b"\x80\x01\x20\x00\xa1\x86\x01\x00");
    }
}
