// Everything here is based off of https://android.googlesource.com/platform/frameworks/base/+/master/libs/androidfw/include/androidfw/ResourceTypes.h

use std::{fmt::Display, io::SeekFrom};

use binrw::{binread, binrw, BinRead, BinResult, BinWrite};

use crate::{
    align,
    string_pool::StringPool,
    table::{
        ResTable, ResTableLib, ResTableOverlayable, ResTableOverlayablePolicy, ResTablePackage,
        ResTableStagedAlias, ResTableType, ResTableTypeSpec,
    },
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
    r#type: ResType,
    /// Size of the chunk header (in bytes). Adding this value to the address of the chunk allows
    /// you to find its associated data (if any).
    #[br(temp)]
    header_size: u16,
    /// Total size of this chunk (in bytes). This is the chunkSize plus the size of any data
    /// associated with the chunk. Adding this value to the chunk allows you to completely skip its
    /// contents (including any child chunks). If this value is the same as chunkSize, there is no
    /// data associated with the chunk.
    #[br(temp)]
    size: u32,

    /// The actual data of this chunk. The type is based on the rtype field. The type is a
    /// [`ResTypeValue`], if not all of the bytes specified by [`size`] are read by the type, then
    /// the size will be padded so that end position is correct.
    #[br(parse_with=parse_res_type_value, args(&r#type, size, header_size))]
    #[br(pad_size_to=(size as u64) - 2 - 2 - 4)]
    pub data: ResTypeValue,
}

impl HeaderSizeStatic for ResChunk {
    fn header_size() -> usize {
        // 2 for res type,
        // 2 for header size
        // 4 for data size
        8
    }
}

/// Like a normal res chunk, except it takes a reference to a res type value rather than an
/// owned value. Used because binrw::write_options takes a reference, whereas the ResChunk data
/// type wants an owned ResTypeValue.
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

        let header_size = (self.data.header_size() + ResChunk::header_size()) as u16;
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
            ResTypeValue::XMLStartElement(_) => ResType::XMLStartElement,
            ResTypeValue::XMLCData(_) => ResType::XMLCData,
            ResTypeValue::TableType(_) => ResType::TableType,
            ResTypeValue::TableSpec(_) => ResType::TableSpecType,
            ResTypeValue::TablePackage(_) => ResType::PackageType,
            ResTypeValue::TableLibrary(_) => ResType::TableLibrary,
            ResTypeValue::XMLEndElement(_) => ResType::XMLEndElement,
            ResTypeValue::ResourceMap(_) => ResType::ResourceMap,
            ResTypeValue::TableStagedAlias(_) => ResType::TableStagedAlias,
            ResTypeValue::XMLEndNameSpace(_) => ResType::XMLEndNameSpace,
            ResTypeValue::TableOverlayable(_) => ResType::TableOverlayable,
            ResTypeValue::XMLStartNameSpace(_) => ResType::XMLStartNameSpace,
            ResTypeValue::TableOverlayablePolicy(_) => ResType::TableOverlayablePolicy,
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
            ResTypeValueWriteRef::XMLStartElement(_) => ResType::XMLStartElement,
            ResTypeValueWriteRef::XMLCData(_) => ResType::XMLCData,
            ResTypeValueWriteRef::TableType(_) => ResType::TableType,
            ResTypeValueWriteRef::TableSpec(_) => ResType::TableSpecType,
            ResTypeValueWriteRef::TablePackage(_) => ResType::PackageType,
            ResTypeValueWriteRef::TableLibrary(_) => ResType::TableLibrary,
            ResTypeValueWriteRef::XMLEndElement(_) => ResType::XMLEndElement,
            ResTypeValueWriteRef::ResourceMap(_) => ResType::ResourceMap,
            ResTypeValueWriteRef::TableStagedAlias(_) => ResType::TableStagedAlias,
            ResTypeValueWriteRef::XMLEndNameSpace(_) => ResType::XMLEndNameSpace,
            ResTypeValueWriteRef::TableOverlayable(_) => ResType::TableOverlayable,
            ResTypeValueWriteRef::XMLStartNameSpace(_) => ResType::XMLStartNameSpace,
            ResTypeValueWriteRef::TableOverlayablePolicy(_) => ResType::TableOverlayablePolicy,
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
            ResTypeValue::XMLStartElement(s) => ResTypeValueWriteRef::XMLStartElement(s),
            ResTypeValue::TableType(t) => ResTypeValueWriteRef::TableType(t),
            ResTypeValue::TableSpec(s) => ResTypeValueWriteRef::TableSpec(s),
            ResTypeValue::TablePackage(p) => ResTypeValueWriteRef::TablePackage(p),
            ResTypeValue::TableLibrary(l) => ResTypeValueWriteRef::TableLibrary(l),
            ResTypeValue::ResourceMap(m) => ResTypeValueWriteRef::ResourceMap(m),
            ResTypeValue::TableStagedAlias(a) => ResTypeValueWriteRef::TableStagedAlias(a),
            ResTypeValue::XMLEndNameSpace(e) => ResTypeValueWriteRef::XMLEndNameSpace(e),
            ResTypeValue::TableOverlayable(o) => ResTypeValueWriteRef::TableOverlayable(o),
            ResTypeValue::XMLStartNameSpace(s) => ResTypeValueWriteRef::XMLStartNameSpace(s),
            ResTypeValue::TableOverlayablePolicy(p) => {
                ResTypeValueWriteRef::TableOverlayablePolicy(p)
            }
            ResTypeValue::XMLEndElement(e) => ResTypeValueWriteRef::XMLEndElement(e),
            ResTypeValue::XMLCData(c) => ResTypeValueWriteRef::XMLCData(c),
        }
    }
}

impl From<ResTypeValue> for ResChunk {
    fn from(value: ResTypeValue) -> Self {
        Self { data: value }
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
    #[br(count=(size-(ResourceMap::header_size() + ResChunk::header_size()) as u32) / 4)]
    pub mapping: Vec<ResTableRef>,
}

impl From<ResourceMap> for ResChunk {
    fn from(value: ResourceMap) -> Self {
        Self {
            data: ResTypeValue::ResourceMap(value),
        }
    }
}

/// Trait for getting the header size from an instance of a type
pub trait HeaderSizeInstance {
    /// Get the header size, does not include the size of the res chunk header, or any other parent
    /// wrappers
    fn header_size(&self) -> usize;
}

/// Trait for getting the header size from a type
pub trait HeaderSizeStatic {
    /// Get the header size, does not include the size of the res chunk header, or any other parent
    /// wrappers
    fn header_size() -> usize;
}

impl HeaderSizeStatic for ResourceMap {
    fn header_size() -> usize {
        0
    }
}

impl ResourceMap {
    /// Create a new empty resource map
    pub fn new() -> ResourceMap {
        ResourceMap::default()
    }
}

/// Enum for all possible values for a [`ResChunk`]
#[derive(Debug, PartialEq, Clone)]
pub enum ResTypeValue {
    Null,
    StringPool(StringPool),
    Table(ResTable),
    XML(RawXMLTree),
    XMLStartNameSpace(ResXMLTreeNameSpaceExt),
    XMLEndNameSpace(ResXMLTreeNameSpaceExt),
    XMLStartElement(ResXMLTreeAttrExt),
    XMLEndElement(ResXMLTreeEndElementExt),
    XMLCData(ResXMLTreeCDataExt),
    ResourceMap(ResourceMap),
    TablePackage(ResTablePackage),
    TableType(ResTableType),
    TableSpec(ResTableTypeSpec),
    TableLibrary(ResTableLib),
    TableOverlayable(ResTableOverlayable),
    TableOverlayablePolicy(ResTableOverlayablePolicy),
    TableStagedAlias(ResTableStagedAlias),
}

/// Enum for all possible values for a [`ResChunk`]
/// Differs to [`ResTypeValue`] because each variant holds a reference to the associated type, not
/// an owned value.
#[derive(Debug, PartialEq, Clone)]
pub enum ResTypeValueWriteRef<'a> {
    Null,
    StringPool(&'a StringPool),
    Table(&'a ResTable),
    XML(&'a RawXMLTree),
    XMLStartNameSpace(&'a ResXMLTreeNameSpaceExt),
    XMLEndNameSpace(&'a ResXMLTreeNameSpaceExt),
    XMLStartElement(&'a ResXMLTreeAttrExt),
    XMLEndElement(&'a ResXMLTreeEndElementExt),
    XMLCData(&'a ResXMLTreeCDataExt),
    ResourceMap(&'a ResourceMap),
    TablePackage(&'a ResTablePackage),
    TableType(&'a ResTableType),
    TableSpec(&'a ResTableTypeSpec),
    TableLibrary(&'a ResTableLib),
    TableOverlayable(&'a ResTableOverlayable),
    TableOverlayablePolicy(&'a ResTableOverlayablePolicy),
    TableStagedAlias(&'a ResTableStagedAlias),
}

impl HeaderSizeInstance for ResTypeValue {
    fn header_size(&self) -> usize {
        let wref: ResTypeValueWriteRef = self.into();
        wref.header_size()
    }
}

impl HeaderSizeInstance for ResTypeValueWriteRef<'_> {
    fn header_size(&self) -> usize {
        match self {
            ResTypeValueWriteRef::Null => 0,
            ResTypeValueWriteRef::XML(_) => RawXMLTree::header_size(),
            ResTypeValueWriteRef::StringPool(_) => StringPool::header_size(),
            ResTypeValueWriteRef::ResourceMap(_) => ResourceMap::header_size(),
            ResTypeValueWriteRef::XMLStartNameSpace(_) => ResXMLTreeNameSpaceExt::header_size(),
            ResTypeValueWriteRef::XMLStartElement(_) => ResXMLTreeAttrExt::header_size(),
            ResTypeValueWriteRef::XMLEndElement(_) => ResXMLTreeEndElementExt::header_size(),
            ResTypeValueWriteRef::XMLCData(_) => ResXMLTreeCDataExt::header_size(),
            ResTypeValueWriteRef::XMLEndNameSpace(_) => ResXMLTreeNameSpaceExt::header_size(),
            ResTypeValueWriteRef::Table(_) => ResTable::header_size(),
            ResTypeValueWriteRef::TableSpec(_) => ResTableTypeSpec::header_size(),
            ResTypeValueWriteRef::TableType(t) => t.header_size(),
            ResTypeValueWriteRef::TablePackage(_) => ResTablePackage::header_size(),
            ResTypeValueWriteRef::TableLibrary(_) => ResTableLib::header_size(),
            ResTypeValueWriteRef::TableOverlayable(_) => ResTableOverlayable::header_size(),
            ResTypeValueWriteRef::TableOverlayablePolicy(_) => {
                ResTableOverlayablePolicy::header_size()
            }

            ResTypeValueWriteRef::TableStagedAlias(_) => ResTableStagedAlias::header_size(),
        }
    }
}

#[binrw::parser(reader, endian)]
fn parse_res_type_value(
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
        ResType::XMLStartNameSpace => ResTypeValue::XMLStartNameSpace(
            ResXMLTreeNameSpaceExt::read_options(reader, endian, ())?,
        ),
        ResType::XMLStartElement => {
            ResTypeValue::XMLStartElement(ResXMLTreeAttrExt::read_options(reader, endian, ())?)
        }
        ResType::XMLEndElement => {
            ResTypeValue::XMLEndElement(ResXMLTreeEndElementExt::read_options(reader, endian, ())?)
        }
        ResType::XMLCData => {
            ResTypeValue::XMLCData(ResXMLTreeCDataExt::read_options(reader, endian, ())?)
        }
        ResType::XMLEndNameSpace => {
            ResTypeValue::XMLEndNameSpace(ResXMLTreeNameSpaceExt::read_options(reader, endian, ())?)
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
        ResType::TableLibrary => {
            ResTypeValue::TableLibrary(ResTableLib::read_options(reader, endian, ())?)
        }
        ResType::TableOverlayable => {
            ResTypeValue::TableOverlayable(ResTableOverlayable::read_options(reader, endian, ())?)
        }
        ResType::TableOverlayablePolicy => ResTypeValue::TableOverlayablePolicy(
            ResTableOverlayablePolicy::read_options(reader, endian, ())?,
        ),
        ResType::TableStagedAlias => {
            ResTypeValue::TableStagedAlias(ResTableStagedAlias::read_options(reader, endian, ())?)
        }
    })
}

#[binrw::writer(writer, endian)]
fn write_res_type_write_ref_value(value: &ResTypeValueWriteRef) -> BinResult<()> {
    match value {
        ResTypeValueWriteRef::Null => (),
        ResTypeValueWriteRef::StringPool(sp) => sp.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::XMLStartElement(st) => st.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::XMLEndElement(ee) => ee.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::XMLCData(d) => d.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::XML(xml) => xml.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::XMLStartNameSpace(sn) => sn.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::XMLEndNameSpace(en) => en.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::ResourceMap(rm) => rm.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::Table(tb) => tb.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::TablePackage(tp) => tp.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::TableSpec(sp) => sp.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::TableType(tt) => tt.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::TableLibrary(tl) => tl.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::TableOverlayable(to) => to.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::TableOverlayablePolicy(tp) => tp.write_options(writer, endian, ())?,
        ResTypeValueWriteRef::TableStagedAlias(ts) => ts.write_options(writer, endian, ())?,
    }

    Ok(())
}

impl ResChunk {
    /// Subtracts the header size (8) from the current pos. Useful when a struct needs to find the
    /// start offset of the header. If pos is <= 8, 0 is returned.
    pub fn get_header_offset(pos: u64) -> u64 {
        let header_size: u64 = ResChunk::header_size() as u64;
        if pos <= header_size {
            return 0;
        }
        pos - header_size
    }
}

/// Possible types for the data in a [`ResChunk`]. Note that this does not hold the actual data of
/// the chunk, that is specified in [`ResTypeValue`], this is just for identification, parsing, and
/// writing purposes.
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
    #[brw(magic(0x0100u16))]
    XMLStartNameSpace,
    #[brw(magic(0x0101u16))]
    XMLEndNameSpace,
    #[brw(magic(0x0102u16))]
    XMLStartElement,
    #[brw(magic(0x0103u16))]
    XMLEndElement,
    #[brw(magic(0x0104u16))]
    XMLCData,
    #[brw(magic(0x0180u16))]
    ResourceMap,
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
    TableOverlayablePolicy,
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
            ResType::XMLStartNameSpace => "xml start namespace",
            ResType::XMLEndNameSpace => "xml end namespace",
            ResType::XMLStartElement => "xml start element",
            ResType::XMLEndElement => "xml end element",
            ResType::XMLCData => "xml comment",
            ResType::ResourceMap => "xml resource map",
            ResType::PackageType => "table package",
            ResType::TableType => "table type",
            ResType::TableSpecType => "table spec type",
            ResType::TableLibrary => "table library",
            ResType::TableOverlayable => "table overlayable",
            ResType::TableOverlayablePolicy => "table overlayable policy",
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
