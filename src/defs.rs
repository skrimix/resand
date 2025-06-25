// Everything here is based off of https://android.googlesource.com/platform/frameworks/base/+/master/libs/androidfw/include/androidfw/ResourceTypes.h

use std::{fmt::Display, io::SeekFrom};

use crate::{
    align,
    stream::{
        NewResultCtx, Readable, ReadableNoOptions, ResultCtx, StreamResult, VecReadable,
        VecWritable, Writeable, WriteableNoOptions,
    },
    string_pool::{ResStringPoolRef, StringPool},
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
#[derive(Debug, PartialEq, Clone)]
pub struct ResChunk {
    /// The actual data of this chunk. The type is based on the rtype field. The type is a
    /// [`ResTypeValue`], if not all of the bytes specified by [`size`] are read by the type, then
    /// the size will be padded so that end position is correct.
    pub data: ResTypeValue,
}

impl Readable for ResChunk {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        let start_pos = reader.stream_position()?;
        let res_type =
            ResType::read_no_opts(reader).add_context(|| "read res_type for ResChunk")?;
        let header_size = u16::read_no_opts(reader)
            .add_context(|| format!("read header_size for ResChunk {res_type}"))?;
        let size = u32::read_no_opts(reader)
            .add_context(|| format!("read size for ResChunk {res_type}"))?;

        let data = ResTypeValue::read(reader, (res_type, size, header_size))
            .add_context(|| format!("read data for ResChunk {res_type}"))?;

        reader
            .seek(SeekFrom::Start(start_pos + size as u64))
            .stream_context(|| {
                format!("seek to end of ResChunk {res_type}, start_pos: {start_pos}, size: {size}",)
            })?;

        Ok(Self { data })
    }
}

impl Writeable for ResChunk {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        let res_type: ResType = (&self.data).into();
        res_type
            .write_no_opts(writer)
            .add_context(|| format!("write res_type for ResChunk {res_type}"))?;

        let header_size = (self.data.header_size() + ResChunk::header_size()) as u16;
        header_size
            .write_no_opts(writer)
            .add_context(|| format!("write header_size for ResChunk {res_type}"))?;

        let size_pos = writer.stream_position()?;

        writer.seek_relative(4).stream_context(|| {
            format!("skip 4 bytes for size_pos when writing ResChunk {res_type}",)
        })?;

        let start_pos = writer.stream_position()?;

        self.data
            .write_no_opts(writer)
            .add_context(|| format!("write data for ResChunk {res_type}"))?;

        let aligned_pos = align(writer.stream_position()?, 4);

        let to_write = aligned_pos - writer.stream_position()?;

        if to_write > 0 {
            vec![0u8; to_write as usize]
                .write_vec(writer)
                .add_context(|| format!("add padding for ResChunk {res_type}"))?;
        }

        let end_pos = writer.stream_position()?;

        let size = (end_pos - start_pos) as u32 + 8;

        writer.seek(SeekFrom::Start(size_pos))?;

        size.write_no_opts(writer)
            .add_context(|| format!("write size for ResChunk {res_type}"))?;

        writer.seek(SeekFrom::Start(end_pos))?;

        Ok(())
    }
}

impl HeaderSizeStatic for ResChunk {
    fn header_size() -> usize {
        // 2 for res type,
        // 2 for header size
        // 4 for data size
        8
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

impl From<ResTypeValue> for ResChunk {
    fn from(value: ResTypeValue) -> Self {
        Self { data: value }
    }
}

/// This contains a uint32_t array mapping strings in the string pool back to resource identifiers.
/// It is optional.
///
/// Not actually optional though, apk will fail to install without it.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct ResourceMap {
    pub mapping: Vec<ResTableRef>,
}

impl ResourceMap {
    /// Set the table reference for a given string
    ///
    /// # Arguments
    ///
    /// * `index` - the string pool reference
    /// * `reference` - the table reference
    pub fn insert(&mut self, index: ResStringPoolRef, reference: ResTableRef) {
        let index_usize = index.index as usize;

        let item = self.mapping.get_mut(index_usize);

        if let Some(item) = item {
            *item = reference;
        } else {
            self.mapping.resize(index_usize, ResTableRef::null());
            self.mapping.push(reference);
        }
    }

    /// Get a table reference for a given string
    ///
    /// # Arguments
    ///
    /// * `index` - the string pool reference
    ///
    /// # Returns
    ///
    /// An Option<&ResTableRef>. Some(..) if it was found, None otherwise
    pub fn get(&self, index: ResStringPoolRef) -> Option<&ResTableRef> {
        self.mapping.get(index.index as usize)
    }
}

impl Readable for ResourceMap {
    type Args = usize;
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        args: Self::Args,
    ) -> StreamResult<Self> {
        let count = (args - (Self::header_size() + ResChunk::header_size())) / 4;
        Ok(Self {
            mapping: <Vec<ResTableRef>>::read_vec(reader, count)
                .add_context(|| "read mapping for ResourceMap")?,
        })
    }
}

impl Writeable for ResourceMap {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        self.mapping
            .write_vec(writer)
            .add_context(|| "write mapping for ResourceMap")
    }
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
impl HeaderSizeInstance for ResTypeValue {
    fn header_size(&self) -> usize {
        match self {
            Self::Null => 0,
            Self::StringPool(_) => StringPool::header_size(),
            Self::Table(_) => ResTable::header_size(),
            Self::XML(_) => RawXMLTree::header_size(),
            Self::XMLStartNameSpace(_) => ResXMLTreeNameSpaceExt::header_size(),
            Self::XMLEndNameSpace(_) => ResXMLTreeNameSpaceExt::header_size(),
            Self::XMLStartElement(_) => ResXMLTreeAttrExt::header_size(),
            Self::XMLEndElement(_) => ResXMLTreeEndElementExt::header_size(),
            Self::XMLCData(_) => ResXMLTreeCDataExt::header_size(),
            Self::ResourceMap(_) => ResourceMap::header_size(),
            Self::TablePackage(_) => ResTablePackage::header_size(),
            Self::TableType(t) => t.header_size(),
            Self::TableSpec(_) => ResTableTypeSpec::header_size(),
            Self::TableLibrary(_) => ResTableLib::header_size(),
            Self::TableOverlayable(_) => ResTableOverlayable::header_size(),
            Self::TableOverlayablePolicy(_) => ResTableOverlayablePolicy::header_size(),
            Self::TableStagedAlias(_) => ResTableStagedAlias::header_size(),
        }
    }
}

impl Readable for ResTypeValue {
    type Args = (ResType, u32, u16);

    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        args: Self::Args,
    ) -> StreamResult<Self> {
        Ok(match args.0 {
            ResType::Null => ResTypeValue::Null,
            ResType::XML => ResTypeValue::XML(RawXMLTree::read(reader, args.1 as u64)?),
            ResType::StringPool => ResTypeValue::StringPool(StringPool::read_no_opts(reader)?),
            ResType::ResourceMap => {
                ResTypeValue::ResourceMap(ResourceMap::read(reader, args.1 as usize)?)
            }
            ResType::XMLStartNameSpace => {
                ResTypeValue::XMLStartNameSpace(ResXMLTreeNameSpaceExt::read_no_opts(reader)?)
            }
            ResType::XMLStartElement => {
                ResTypeValue::XMLStartElement(ResXMLTreeAttrExt::read_no_opts(reader)?)
            }
            ResType::XMLEndElement => {
                ResTypeValue::XMLEndElement(ResXMLTreeEndElementExt::read_no_opts(reader)?)
            }
            ResType::XMLCData => ResTypeValue::XMLCData(ResXMLTreeCDataExt::read_no_opts(reader)?),
            ResType::XMLEndNameSpace => {
                ResTypeValue::XMLEndNameSpace(ResXMLTreeNameSpaceExt::read_no_opts(reader)?)
            }
            ResType::Table => ResTypeValue::Table(ResTable::read_no_opts(reader)?),
            ResType::PackageType => {
                ResTypeValue::TablePackage(ResTablePackage::read(reader, args.1 as usize)?)
            }
            ResType::TableSpecType => {
                ResTypeValue::TableSpec(ResTableTypeSpec::read_no_opts(reader)?)
            }
            ResType::TableType => {
                ResTypeValue::TableType(ResTableType::read(reader, args.2 as usize)?)
            }
            ResType::TableLibrary => ResTypeValue::TableLibrary(ResTableLib::read_no_opts(reader)?),
            ResType::TableOverlayable => {
                ResTypeValue::TableOverlayable(ResTableOverlayable::read_no_opts(reader)?)
            }
            ResType::TableOverlayablePolicy => ResTypeValue::TableOverlayablePolicy(
                ResTableOverlayablePolicy::read_no_opts(reader)?,
            ),
            ResType::TableStagedAlias => {
                ResTypeValue::TableStagedAlias(ResTableStagedAlias::read_no_opts(reader)?)
            }
        })
    }
}

impl Writeable for ResTypeValue {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        match self {
            ResTypeValue::Null => Ok(()),
            ResTypeValue::StringPool(sp) => sp.write_no_opts(writer),
            ResTypeValue::XMLStartElement(st) => st.write_no_opts(writer),
            ResTypeValue::XMLEndElement(ee) => ee.write_no_opts(writer),
            ResTypeValue::XMLCData(d) => d.write_no_opts(writer),
            ResTypeValue::XML(xml) => xml.write_no_opts(writer),
            ResTypeValue::XMLStartNameSpace(sn) => sn.write_no_opts(writer),
            ResTypeValue::XMLEndNameSpace(en) => en.write_no_opts(writer),
            ResTypeValue::ResourceMap(rm) => rm.write_no_opts(writer),
            ResTypeValue::Table(tb) => tb.write_no_opts(writer),
            ResTypeValue::TablePackage(tp) => tp.write_no_opts(writer),
            ResTypeValue::TableSpec(sp) => sp.write_no_opts(writer),
            ResTypeValue::TableType(tt) => tt.write_no_opts(writer),
            ResTypeValue::TableLibrary(tl) => tl.write_no_opts(writer),
            ResTypeValue::TableOverlayable(to) => to.write_no_opts(writer),
            ResTypeValue::TableOverlayablePolicy(tp) => tp.write_no_opts(writer),
            ResTypeValue::TableStagedAlias(ts) => ts.write_no_opts(writer),
        }
    }
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
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ResType {
    Null,
    StringPool,
    Table,
    XML,
    XMLStartNameSpace,
    XMLEndNameSpace,
    XMLStartElement,
    XMLEndElement,
    XMLCData,
    ResourceMap,
    PackageType,
    TableType,
    TableSpecType,
    TableLibrary,
    TableOverlayable,
    TableOverlayablePolicy,
    TableStagedAlias,
}

impl Readable for ResType {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        let res_type: u16 = u16::read_no_opts(reader).add_context(|| "read res_type id")?;
        Ok(match res_type {
            0 => Self::Null,
            1 => Self::StringPool,
            2 => Self::Table,
            3 => Self::XML,
            0x100 => Self::XMLStartNameSpace,
            0x101 => Self::XMLEndNameSpace,
            0x102 => Self::XMLStartElement,
            0x103 => Self::XMLEndElement,
            0x104 => Self::XMLCData,
            0x180 => Self::ResourceMap,
            0x200 => Self::PackageType,
            0x201 => Self::TableType,
            0x202 => Self::TableSpecType,
            0x203 => Self::TableLibrary,
            0x204 => Self::TableOverlayable,
            0x205 => Self::TableOverlayablePolicy,
            0x206 => Self::TableStagedAlias,
            v => Err(std::io::Error::other(format!("invalid type: {v}")))
                .with_context(reader.stream_position()?, || "match res_type id")?,
        })
    }
}

impl Writeable for ResType {
    type Args = ();

    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        let res_type: u16 = match self {
            Self::Null => 0,
            Self::StringPool => 1,
            Self::Table => 2,
            Self::XML => 3,
            Self::XMLStartNameSpace => 0x100,
            Self::XMLEndNameSpace => 0x101,
            Self::XMLStartElement => 0x102,
            Self::XMLEndElement => 0x103,
            Self::XMLCData => 0x104,
            Self::ResourceMap => 0x180,
            Self::PackageType => 0x200,
            Self::TableType => 0x201,
            Self::TableSpecType => 0x202,
            Self::TableLibrary => 0x203,
            Self::TableOverlayable => 0x204,
            Self::TableOverlayablePolicy => 0x205,
            Self::TableStagedAlias => 0x206,
        };
        res_type
            .write_no_opts(writer)
            .add_context(|| "write res_type id")
    }
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
        write!(f, "{str}")
    }
}

/// This is a reference to a unique entry (a ResTable_entry structure) in a resource table. The
/// value is structured as 0xpptteee, where pp is the package index, tt is the type index in that
/// package, and eeee is the entry index in that type. The package and type values start a 1 for
/// the first item, to help catch cases where they have been supplied.
#[derive(Debug, PartialEq, Copy, Clone, Eq)]
pub struct ResTableRef {
    pub entry_index: u16,
    pub type_index: u8,
    pub package_index: u8,
}

impl PartialOrd for ResTableRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ResTableRef {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let s_i: u32 = (*self).into();
        let o_i: u32 = (*other).into();

        s_i.cmp(&o_i)
    }
}

impl ResTableRef {
    /// Empty table reference
    pub fn null() -> Self {
        0.into()
    }
}

impl Readable for ResTableRef {
    type Args = ();
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        _args: Self::Args,
    ) -> StreamResult<Self> {
        Ok(Self {
            entry_index: u16::read_no_opts(reader)
                .add_context(|| "read entry_index for ResTableRef")?,
            type_index: u8::read_no_opts(reader)
                .add_context(|| "read type_index for ResTableRef")?,
            package_index: u8::read_no_opts(reader)
                .add_context(|| "read package_index for ResTableRef")?,
        })
    }
}

impl Writeable for ResTableRef {
    type Args = ();
    fn write<W: std::io::Write + std::io::Seek>(
        self,
        writer: &mut W,
        _args: Self::Args,
    ) -> StreamResult<()> {
        self.entry_index
            .write_no_opts(writer)
            .add_context(|| "write entry_index for ResTableRef")?;
        self.type_index
            .write_no_opts(writer)
            .add_context(|| "write type_index for ResTableRef")?;
        self.package_index
            .write_no_opts(writer)
            .add_context(|| "write package_index for ResTableRef")?;

        Ok(())
    }
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

impl Readable for Vec<ResChunk> {
    type Args = u64;
    fn read<R: std::io::Read + std::io::Seek>(
        reader: &mut R,
        args: Self::Args,
    ) -> StreamResult<Self> {
        let start_pos = ResChunk::get_header_offset(reader.stream_position()?);
        let mut chunks: Vec<ResChunk> = Vec::new();

        let mut i = 0;

        while reader.stream_position()? - start_pos < (args) {
            let chunk =
                ResChunk::read_no_opts(reader).add_context(|| format!("read res_chunk {i}"))?;

            chunks.push(chunk);

            i += 1;
        }

        Ok(chunks)
    }
}
