// Everything here is based off of https://android.googlesource.com/platform/frameworks/base/+/master/libs/androidfw/include/androidfw/ResourceTypes.h

use std::{fmt::Display, io::SeekFrom, num::ParseIntError, str::FromStr};

use binrw::{binread, binrw, BinRead, BinResult, BinWrite};

use crate::{
    align,
    string_pool::StringPool,
    table::{ResTable, ResTablePackage, ResTableType, ResTableTypeSpec},
    xmltree::{
        RawXMLTree, ResXMLTreeAttrExt, ResXMLTreeCDataExt, ResXMLTreeEndElementExt,
        ResXMLTreeNameSpaceExt,
    },
};

/// Header that appears at the front of every data chunk in a resource.
#[binread]
#[br(stream = s)]
#[derive(Debug, PartialEq, Clone)]
pub struct ResChunk {
    /// Type identifier for this chunk. The meaning of this value depends on the containing chunk.
    #[br(temp)]
    pub r#type: ResType,
    /// Size of the chunk header (in bytes). Adding this value to the address of the chunk allows
    /// you to find its associated data (if any).
    #[br(temp)]
    pub header_size: u16,
    /// Total size of this chunk (in bytes). This is the chunkSize plus the size of any data
    /// associated with the chunk. Adding this value to the chunk allows you to completely skip its
    /// contents (including any child chunks). If this value is the same as chunkSize, there is no
    /// data associated with the chunk.
    #[br(temp)]
    pub size: u32,

    #[br(parse_with=parse_res_type_value, args(&r#type, size, header_size))]
    #[br(pad_size_to=(size as u64) - 2 - 2 - 4)]
    pub data: ResTypeValue,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ResChunkWriteRef<'a> {
    pub data: ResTypeValueWriteRef<'a>,
}

impl BinWrite for ResChunkWriteRef<'_> {
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
        header_size.write_options(writer, endian, ())?;

        let size_pos = writer.stream_position()?;

        writer.seek(SeekFrom::Current(4))?;

        let start_pos = writer.stream_position()?;

        write_res_type_write_ref_value(&self.data, writer, endian, ())?;

        let aligned_pos = align(writer.stream_position()?, 4);

        let to_write = aligned_pos - writer.stream_position()?;

        if to_write > 0 {
            writer.write_all(vec![0; to_write as usize].as_slice())?;
        }

        let end_pos = writer.stream_position()?;

        let size = (end_pos - start_pos) as u32 + 8;

        writer.seek(SeekFrom::Start(size_pos))?;

        size.write_options(writer, endian, ())?;

        writer.seek(SeekFrom::Start(end_pos))?;

        Ok(())
    }
}

impl BinWrite for ResChunk {
    type Args<'a> = ();
    fn write_options<W: std::io::Write + std::io::Seek>(
        &self,
        writer: &mut W,
        endian: binrw::Endian,
        _args: Self::Args<'_>,
    ) -> BinResult<()> {
        let writer_ref = ResChunkWriteRef {
            data: (&self.data).into(),
        };
        writer_ref.write_options(writer, endian, ())
    }
}

impl From<&ResTypeValue> for ResType {
    fn from(value: &ResTypeValue) -> Self {
        match value {
            ResTypeValue::Null => ResType::Null,
            ResTypeValue::Table(_) => ResType::Table,
            ResTypeValue::StringPool(_) => ResType::StringPool,
            ResTypeValue::XML(_) => ResType::XML,
            ResTypeValue::StartElement(_) => ResType::StartElement,
            ResTypeValue::CData(_) => ResType::CData,
            ResTypeValue::TableType(_) => ResType::TableType,
            ResTypeValue::TableSpec(_) => ResType::TableSpecType,
            ResTypeValue::TablePackage(_) => ResType::PackageType,
            ResTypeValue::TableLibrary => ResType::TableLibrary,
            ResTypeValue::EndElement(_) => ResType::EndElement,
            ResTypeValue::ResourceMap(_) => ResType::ResourceMap,
            ResTypeValue::TableStagedAlias => ResType::TableStagedAlias,
            ResTypeValue::EndNameSpace(_) => ResType::EndNameSpace,
            ResTypeValue::TableOverlayabale => ResType::TableOverlayable,
            ResTypeValue::StartNameSpace(_) => ResType::StartNameSpace,
            ResTypeValue::TableOverlayablePolicy => ResType::TableOverlayabalePolicy,
        }
    }
}

impl From<&ResTypeValueWriteRef<'_>> for ResType {
    fn from(value: &ResTypeValueWriteRef) -> Self {
        match value {
            ResTypeValueWriteRef::Null => ResType::Null,
            ResTypeValueWriteRef::Table(_) => ResType::Table,
            ResTypeValueWriteRef::StringPool(_) => ResType::StringPool,
            ResTypeValueWriteRef::XML(_) => ResType::XML,
            ResTypeValueWriteRef::StartElement(_) => ResType::StartElement,
            ResTypeValueWriteRef::CData(_) => ResType::CData,
            ResTypeValueWriteRef::TableType(_) => ResType::TableType,
            ResTypeValueWriteRef::TableSpec(_) => ResType::TableSpecType,
            ResTypeValueWriteRef::TablePackage(_) => ResType::PackageType,
            ResTypeValueWriteRef::TableLibrary => ResType::TableLibrary,
            ResTypeValueWriteRef::EndElement(_) => ResType::EndElement,
            ResTypeValueWriteRef::ResourceMap(_) => ResType::ResourceMap,
            ResTypeValueWriteRef::TableStagedAlias => ResType::TableStagedAlias,
            ResTypeValueWriteRef::EndNameSpace(_) => ResType::EndNameSpace,
            ResTypeValueWriteRef::TableOverlayabale => ResType::TableOverlayable,
            ResTypeValueWriteRef::StartNameSpace(_) => ResType::StartNameSpace,
            ResTypeValueWriteRef::TableOverlayabalePolicy => ResType::TableOverlayabalePolicy,
        }
    }
}

impl<'a> From<&'a ResTypeValue> for ResTypeValueWriteRef<'a> {
    fn from(value: &'a ResTypeValue) -> Self {
        match value {
            ResTypeValue::Null => ResTypeValueWriteRef::Null,
            ResTypeValue::Table(t) => ResTypeValueWriteRef::Table(t),
            ResTypeValue::StringPool(sp) => ResTypeValueWriteRef::StringPool(sp),
            ResTypeValue::XML(x) => ResTypeValueWriteRef::XML(x),
            ResTypeValue::StartElement(s) => ResTypeValueWriteRef::StartElement(s),
            ResTypeValue::TableType(t) => ResTypeValueWriteRef::TableType(t),
            ResTypeValue::TableSpec(s) => ResTypeValueWriteRef::TableSpec(s),
            ResTypeValue::TablePackage(p) => ResTypeValueWriteRef::TablePackage(p),
            ResTypeValue::TableLibrary => ResTypeValueWriteRef::TableLibrary,
            ResTypeValue::ResourceMap(m) => ResTypeValueWriteRef::ResourceMap(m),
            ResTypeValue::TableStagedAlias => ResTypeValueWriteRef::TableStagedAlias,
            ResTypeValue::EndNameSpace(e) => ResTypeValueWriteRef::EndNameSpace(e),
            ResTypeValue::TableOverlayabale => ResTypeValueWriteRef::TableOverlayabale,
            ResTypeValue::StartNameSpace(s) => ResTypeValueWriteRef::StartNameSpace(s),
            ResTypeValue::TableOverlayablePolicy => ResTypeValueWriteRef::TableOverlayabalePolicy,
            ResTypeValue::EndElement(e) => ResTypeValueWriteRef::EndElement(e),
            _ => todo!(),
        }
    }
}

impl From<ResTypeValue> for ResChunk {
    fn from(value: ResTypeValue) -> Self {
        Self { data: value }
    }
}

#[derive(Debug)]
pub struct NotImplimentedError;

impl Display for NotImplimentedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "not implimented error")
    }
}

/// This contains a uint32_t array mapping strings in the string pool back to resource identifiers.
/// It is optional.
///
/// Not actually optional though, apk will fail to install without it.
#[binrw]
#[derive(Debug, PartialEq, Clone, Default)]
#[br(import(size: u32))]
pub struct ResourceMap {
    #[br(count=(size-ResourceMap::get_header_size() as u32) / 4)]
    pub mapping: Vec<ResTableRef>,
}

impl From<ResourceMap> for ResChunk {
    fn from(value: ResourceMap) -> Self {
        Self {
            data: ResTypeValue::ResourceMap(value),
        }
    }
}

impl ResourceMap {
    pub fn get_header_size() -> usize {
        0x8
    }

    pub fn new() -> ResourceMap {
        ResourceMap::default()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ResTypeValue {
    Null,
    StringPool(StringPool),
    Table(ResTable),
    XML(RawXMLTree),
    StartNameSpace(ResXMLTreeNameSpaceExt),
    EndNameSpace(ResXMLTreeNameSpaceExt),
    StartElement(ResXMLTreeAttrExt),
    EndElement(ResXMLTreeEndElementExt),
    CData(ResXMLTreeCDataExt),
    ResourceMap(ResourceMap),
    TablePackage(ResTablePackage),
    TableType(ResTableType),
    TableSpec(ResTableTypeSpec),
    TableLibrary,
    TableOverlayabale,
    TableOverlayablePolicy,
    TableStagedAlias,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ResTypeValueWriteRef<'a> {
    Null,
    StringPool(&'a StringPool),
    Table(&'a ResTable),
    XML(&'a RawXMLTree),
    StartNameSpace(&'a ResXMLTreeNameSpaceExt),
    EndNameSpace(&'a ResXMLTreeNameSpaceExt),
    StartElement(&'a ResXMLTreeAttrExt),
    EndElement(&'a ResXMLTreeEndElementExt),
    CData(&'a ResXMLTreeCDataExt),
    ResourceMap(&'a ResourceMap),
    TablePackage(&'a ResTablePackage),
    TableType(&'a ResTableType),
    TableSpec(&'a ResTableTypeSpec),
    TableLibrary,
    TableOverlayabale,
    TableOverlayabalePolicy,
    TableStagedAlias,
}

impl ResTypeValue {
    pub fn get_header_size(&self) -> usize {
        let wref: ResTypeValueWriteRef = self.into();
        wref.get_header_size()
    }
}

impl ResTypeValueWriteRef<'_> {
    pub fn get_header_size(&self) -> usize {
        match self {
            ResTypeValueWriteRef::XML(_) => RawXMLTree::get_header_size(),
            ResTypeValueWriteRef::StringPool(_) => StringPool::get_header_size(),
            ResTypeValueWriteRef::ResourceMap(_) => ResourceMap::get_header_size(),
            ResTypeValueWriteRef::StartNameSpace(_) => ResXMLTreeNameSpaceExt::get_header_size(),
            ResTypeValueWriteRef::StartElement(_) => ResXMLTreeAttrExt::get_header_size(),
            ResTypeValueWriteRef::EndElement(_) => ResXMLTreeEndElementExt::get_header_size(),
            ResTypeValueWriteRef::EndNameSpace(_) => ResXMLTreeNameSpaceExt::get_header_size(),
            ResTypeValueWriteRef::Table(_) => ResTable::get_header_size(),
            ResTypeValueWriteRef::TableSpec(_) => ResTableTypeSpec::get_header_size(),
            ResTypeValueWriteRef::TableType(t) => t.get_header_size(),
            ResTypeValueWriteRef::TablePackage(_) => ResTablePackage::get_header_size(),
            t => {
                dbg!(t);
                todo!()
            }
        }
    }
}

#[binrw::parser(reader, endian)]
pub fn parse_res_type_value(
    res_type: &ResType,
    size: u32,
    header_size: u16,
) -> BinResult<ResTypeValue> {
    Ok(match res_type {
        ResType::Null => ResTypeValue::Null,
        ResType::XML => ResTypeValue::XML(RawXMLTree::read_options(reader, endian, (size,))?),
        ResType::StringPool => {
            ResTypeValue::StringPool(StringPool::read_options(reader, endian, ())?)
        }
        ResType::ResourceMap => {
            ResTypeValue::ResourceMap(ResourceMap::read_options(reader, endian, (size,))?)
        }
        ResType::StartNameSpace => {
            ResTypeValue::StartNameSpace(ResXMLTreeNameSpaceExt::read_options(reader, endian, ())?)
        }
        ResType::StartElement => {
            ResTypeValue::StartElement(ResXMLTreeAttrExt::read_options(reader, endian, ())?)
        }
        ResType::EndElement => {
            ResTypeValue::EndElement(ResXMLTreeEndElementExt::read_options(reader, endian, ())?)
        }
        ResType::EndNameSpace => {
            ResTypeValue::EndNameSpace(ResXMLTreeNameSpaceExt::read_options(reader, endian, ())?)
        }
        ResType::Table => ResTypeValue::Table(ResTable::read_options(reader, endian, ())?),
        ResType::PackageType => {
            ResTypeValue::TablePackage(ResTablePackage::read_options(reader, endian, (size,))?)
        }
        ResType::TableSpecType => {
            ResTypeValue::TableSpec(ResTableTypeSpec::read_options(reader, endian, ())?)
        }
        ResType::TableType => {
            ResTypeValue::TableType(ResTableType::read_options(reader, endian, (header_size,))?)
        }
        _ => todo!(),
    })
}

#[binrw::writer(writer, endian)]
pub fn write_res_type_value(value: &ResTypeValue) -> BinResult<()> {
    let wref: ResTypeValueWriteRef = value.into();
    write_res_type_write_ref_value(&wref, writer, endian, ())
}

#[binrw::writer(writer, endian)]
pub fn write_res_type_write_ref_value(value: &ResTypeValueWriteRef) -> BinResult<()> {
    match value {
        ResTypeValueWriteRef::StringPool(sp) => sp.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::StartElement(st) => st.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::EndElement(ee) => ee.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::XML(xml) => xml.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::StartNameSpace(sn) => sn.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::EndNameSpace(en) => en.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::ResourceMap(rm) => rm.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::Table(tb) => tb.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::TablePackage(tp) => tp.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::TableSpec(sp) => sp.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::TableType(tt) => tt.write_options(writer, endian, ())?,
        v => {
            dbg!(v);
            todo!()
        }
    }

    Ok(())
}

impl ResChunk {
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
    Null,
    #[brw(magic(0x0001u16))]
    StringPool,
    #[brw(magic(0x0002u16))]
    Table,
    #[brw(magic(0x0003u16))]
    XML,
    // Chunk types in RES_XML_TYPE
    // RES_XML_FIRST_CHUNK_TYPE = 0x0100,
    #[brw(magic(0x0100u16))]
    StartNameSpace,
    #[brw(magic(0x0101u16))]
    EndNameSpace,
    #[brw(magic(0x0102u16))]
    StartElement,
    #[brw(magic(0x0103u16))]
    EndElement,
    #[brw(magic(0x0104u16))]
    CData,
    //#[brw(magic(0x017fu16))]
    //RES_XML_LAST_CHUNK_TYPE,
    /// This contains a uint32_t array mapping strings in the string
    /// pool back to resource identifiers.  It is optional.
    #[brw(magic(0x0180u16))]
    ResourceMap,
    // Chunk types in RES_TABLE_TYPE
    #[brw(magic(0x0200u16))]
    PackageType,
    #[brw(magic(0x0201u16))]
    TableType,
    #[brw(magic(0x0202u16))]
    TableSpecType,
    #[brw(magic(0x0203u16))]
    TableLibrary,
    #[brw(magic(0x0204u16))]
    TableOverlayable,
    #[brw(magic(0x0205u16))]
    TableOverlayabalePolicy,
    #[brw(magic(0x0206u16))]
    TableStagedAlias,
}

impl Display for ResType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            ResType::Null => "null",
            ResType::StringPool => "string pool",
            ResType::Table => "table",
            ResType::XML => "xml",
            ResType::StartNameSpace => "xml start namespace",
            ResType::EndNameSpace => "xml end namespace",
            ResType::StartElement => "xml start element",
            ResType::EndElement => "xml end element",
            ResType::CData => "xml comment",
            ResType::ResourceMap => "xml resource map",
            ResType::PackageType => "table package",
            ResType::TableType => "table type",
            ResType::TableSpecType => "table spec type",
            ResType::TableLibrary => "table library",
            ResType::TableOverlayable => "table overlayable",
            ResType::TableOverlayabalePolicy => "table overlayable policy",
            ResType::TableStagedAlias => "table staged alias",
        };
        write!(f, "{}", str)
    }
}

/// This is a reference to a unique entry (a ResTable_entry structure) in a resource table. The
/// value is structured as 0xpptteee, where pp is the package index, tt is the type index in that
/// package, and eeee is the entry index in that type. The package and type values start a 1 for
/// the first item, to help catch cases where they have been supplied.
#[derive(Debug, BinRead, BinWrite, PartialEq, Copy, Clone)]
pub struct ResTableRef {
    pub entry_index: u16,
    pub type_index: u8,
    pub package_index: u8,
}

impl Display for ResTableRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{:x}", <ResTableRef as Into<u32>>::into(*self))
    }
}

impl From<ResTableRef> for u32 {
    fn from(value: ResTableRef) -> Self {
        (value.entry_index as u32)
            + ((value.type_index as u32) << 16)
            + ((value.package_index as u32) << 24)
    }
}

impl From<u32> for ResTableRef {
    fn from(value: u32) -> Self {
        Self {
            package_index: (value >> 24) as u8, // as u8 does & 0xff
            type_index: (value >> 16) as u8,    // as u8 does & 0xff
            entry_index: value as u16,          // as u16 does & 0xffff
        }
    }
}

#[derive(Debug)]
pub enum ParseResTableRefError {
    Int(ParseIntError),
    InvalidStartChar,
}

impl FromStr for ResTableRef {
    type Err = ParseResTableRefError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let first = s
            .chars()
            .next()
            .ok_or(ParseResTableRefError::InvalidStartChar)?;
        if first != '@' {
            return Err(ParseResTableRefError::InvalidStartChar);
        }
        let rest: String = s.chars().skip(1).collect();

        let val: u32 = u32::from_str_radix(&rest, 16).map_err(ParseResTableRefError::Int)?;

        Ok(val.into())
    }
}

#[binrw::parser(reader, endian)]
pub fn parse_chunks(size: u32) -> BinResult<Vec<ResChunk>> {
    let start_pos = ResChunk::get_header_offset(reader.stream_position()?);

    let mut chunks: Vec<ResChunk> = Vec::new();

    while reader.stream_position()? - start_pos < (size as u64) {
        let chunk: ResChunk = ResChunk::read_options(reader, endian, ())?;

        chunks.push(chunk);
    }

    Ok(chunks)
}
