// Everything here is based off of https://android.googlesource.com/platform/frameworks/base/+/master/libs/androidfw/include/androidfw/ResourceTypes.h

use std::{
    fmt::Display,
    io::{Cursor, Read, Seek, SeekFrom, Write},
};

use crate::{
    align,
    defs::{HeaderSizeInstance, HeaderSizeStatic, ResChunk, ResTableRef, ResType, ResTypeValue},
    res_value::ResValue,
    stream::{
        NewResultCtx, Readable, ReadableNoOptions, StreamError, StreamResult, VecReadable,
        VecWritable, Writeable, WriteableNoOptions,
    },
    string_pool::{ResStringPoolRef, StringPoolHandler},
};

/// A specification of the resources defined by a particular type.
///
/// There should be one of these chunks for each resource type.
///
/// This structure is followed by an array of integers providing the set of configuration change
/// flags (ResTable_config::CONFIG_*) that have multiple resources for that configuration. In
/// addition, the high bi is set if that resource has been made public.
#[derive(Debug, Clone, PartialEq)]
pub struct ResTableTypeSpec {
    /// The type identifier this chunk is holding. Type IDs start at 1 (corresponding to the value
    /// of the type bits in a resource identifier). 0 is invalid.
    pub id: u8,

    /// Used to be reserved, if >0 specifies the number of ResTable_type entries for this spec. TODO: this?
    pub types_count: u16,

    pub config_masks: Vec<u32>,
}

impl Readable for ResTableTypeSpec {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let id = u8::read_no_opts(reader).add_context(|| "read id for ResTableTypeSpec")?;
        let res0 = u8::read_no_opts(reader).add_context(|| "read res0 for ResTableTypeSpec")?;

        if res0 != 0 {
            return Err(StreamError::new_string_context(
                format!("invalid res0 {}, expected 0", res0),
                reader.stream_position()?,
                "validate res0 for ResTableTypeSpec",
            ));
        }

        let types_count =
            u16::read_no_opts(reader).add_context(|| "read types_count for ResTableTypeSpec")?;
        let entry_count =
            u32::read_no_opts(reader).add_context(|| "read entry_count for ResTableTypeSpec")?;

        let config_masks = <Vec<u32>>::read_vec(reader, entry_count as usize)
            .add_context(|| "read config_masks for ResTableTypeSpec")?;

        Ok(Self {
            id,
            types_count,
            config_masks,
        })
    }
}

impl Writeable for ResTableTypeSpec {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.id
            .write_no_opts(writer)
            .add_context(|| "write id for ResTableTypeSpec")?;
        let res0: u8 = 0;
        res0.write_no_opts(writer)
            .add_context(|| "write res0 for ResTableTypeSpec")?;

        self.types_count
            .write_no_opts(writer)
            .add_context(|| "write types_count for ResTableTypeSpec")?;
        let entry_count: u32 = self.config_masks.len() as u32;
        entry_count
            .write_no_opts(writer)
            .add_context(|| "write entry_count for ResTableTypeSpec")?;

        self.config_masks
            .write_vec(writer)
            .add_context(|| "write config_masks for ResTableTypeSpec")
    }
}

impl HeaderSizeStatic for ResTableTypeSpec {
    fn header_size() -> usize {
        8
    }
}

#[derive(Debug, PartialEq, Default, Copy, Clone)]
pub struct ResTableTypeFlags {
    pub flags: u8,
}

impl Readable for ResTableTypeFlags {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            flags: u8::read_no_opts(reader).add_context(|| "read flags for ResTableTypeFlags")?,
        })
    }
}

impl Writeable for ResTableTypeFlags {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.flags
            .write_no_opts(writer)
            .add_context(|| "write flags for ResTableTypeFlags")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ResTableConfig {
    pub imsi: Option<u32>,
    pub locale: Option<u32>,
    pub screen_type: Option<u32>,
    pub input: Option<u64>,
    pub screen_size: Option<u32>,
    pub version: Option<u32>,
    pub screen_config: Option<u32>,
    pub screen_size_dp: Option<u32>,
    pub locale_script: Option<FixedUTF8String<4>>,
    pub locale_variant: Option<FixedUTF8String<8>>,
    pub screen_config_2: Option<u32>,
    pub locale_script_was_computed: Option<bool>,
    pub locale_numbering_system: Option<FixedUTF8String<8>>,
}

impl Writeable for ResTableConfig {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        let size: u32 = self.get_size() as u32;
        size.write_no_opts(writer)?;
        let start_pos = writer.stream_position()?;
        if let Some(imsi) = self.imsi {
            imsi.write_no_opts(writer)?;
        }
        if let Some(locale) = self.locale {
            locale.write_no_opts(writer)?;
        }
        if let Some(screen_type) = self.screen_type {
            screen_type.write_no_opts(writer)?;
        }
        if let Some(input) = self.input {
            input.write_no_opts(writer)?;
        }
        if let Some(screen_size) = self.screen_size {
            screen_size.write_no_opts(writer)?;
        }
        if let Some(version) = self.version {
            version.write_no_opts(writer)?;
        }
        if let Some(screen_config) = self.screen_config {
            screen_config.write_no_opts(writer)?;
        }
        if let Some(screen_size_dp) = self.screen_size_dp {
            screen_size_dp.write_no_opts(writer)?;
        }
        if let Some(locale_script) = self.locale_script {
            locale_script.write_no_opts(writer)?;
        }
        if let Some(locale_variant) = self.locale_variant {
            locale_variant.write_no_opts(writer)?;
        }
        if let Some(screen_config_2) = self.screen_config_2 {
            screen_config_2.write_no_opts(writer)?;
        }
        if let Some(locale_script_was_computed) = self.locale_script_was_computed {
            locale_script_was_computed.write_no_opts(writer)?;
        }
        if let Some(locale_numbering_system) = self.locale_numbering_system {
            locale_numbering_system.write_no_opts(writer)?;
        }

        let current_pos = writer.stream_position()?;
        let new_pos = align(current_pos, 4);

        if new_pos > current_pos {
            let to_pad = new_pos - current_pos;
            let data = vec![0u8; to_pad as usize];
            data.write_vec(writer)?;
        }

        assert_eq!(writer.stream_position()?, size as u64 + start_pos);

        Ok(())
    }
}

impl Readable for ResTableConfig {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let size = u32::read_no_opts(reader)?;
        let start_offset = reader.stream_position()?;

        let imsi = if size >= 4 {
            Some(u32::read_no_opts(reader)?)
        } else {
            None
        };

        let locale = if size >= 8 {
            Some(u32::read_no_opts(reader)?)
        } else {
            None
        };

        let screen_type = if size >= 12 {
            Some(u32::read_no_opts(reader)?)
        } else {
            None
        };

        let input = if size >= 20 {
            Some(u64::read_no_opts(reader)?)
        } else {
            None
        };

        let screen_size = if size >= 24 {
            Some(u32::read_no_opts(reader)?)
        } else {
            None
        };

        let version = if size >= 28 {
            Some(u32::read_no_opts(reader)?)
        } else {
            None
        };

        let screen_config = if size >= 32 {
            Some(u32::read_no_opts(reader)?)
        } else {
            None
        };

        let screen_size_dp = if size >= 36 {
            Some(u32::read_no_opts(reader)?)
        } else {
            None
        };

        let locale_script = if size >= 40 {
            Some(<FixedUTF8String<4>>::read_no_opts(reader)?)
        } else {
            None
        };

        let locale_variant = if size >= 48 {
            Some(<FixedUTF8String<8>>::read_no_opts(reader)?)
        } else {
            None
        };

        let screen_config_2 = if size >= 42 {
            Some(u32::read_no_opts(reader)?)
        } else {
            None
        };

        let locale_script_was_computed = if size >= 43 {
            Some(bool::read_no_opts(reader)?)
        } else {
            None
        };

        let locale_numbering_system = if size >= 51 {
            Some(<FixedUTF8String<8>>::read_no_opts(reader)?)
        } else {
            None
        };

        reader.seek(SeekFrom::Start(start_offset + size as u64))?;

        Ok(Self {
            imsi,
            locale,
            screen_type,
            input,
            screen_size,
            version,
            screen_config,
            screen_size_dp,
            locale_script,
            locale_variant,
            screen_config_2,
            locale_script_was_computed,
            locale_numbering_system,
        })
    }
}
impl ResTableConfig {
    pub fn get_size(&self) -> usize {
        let mut size = 0;

        if self.imsi.is_none() {
            return size;
        }

        size += 4;

        if self.locale.is_none() {
            return size;
        }

        size += 4;

        if self.screen_type.is_none() {
            return size;
        }

        size += 4;

        if self.input.is_none() {
            return size;
        }

        size += 8;

        if self.screen_size.is_none() {
            return size;
        }

        size += 4;

        if self.version.is_none() {
            return size;
        }

        size += 4;

        if self.screen_config.is_none() {
            return size;
        }

        size += 4;

        if self.screen_size_dp.is_none() {
            return size;
        }

        size += 4;

        if self.locale_script.is_none() {
            return size;
        }

        size += 4;

        if self.locale_variant.is_none() {
            return size;
        }

        size += 8;

        if self.screen_config_2.is_none() {
            return size;
        }

        size += 4;

        if self.locale_script_was_computed.is_none() {
            return size;
        }

        size += 1;

        if self.locale_numbering_system.is_none() {
            return size;
        }

        size += 8;

        size = align(size as u64, 4) as usize;

        size
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FixedUTF8String<const N: usize> {
    data: Vec<u8>,
}

impl<const N: usize> Readable for FixedUTF8String<N> {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            data: <Vec<u8>>::read_vec(reader, N)
                .add_context(|| format!("read data for FixedUTF8String<{}>", N))?,
        })
    }
}

impl<const N: usize> Writeable for FixedUTF8String<N> {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.data
            .write_vec(writer)
            .add_context(|| format!("write data for FixedUTF8String<{}>", N))
    }
}

impl<const N: usize> Display for FixedUTF8String<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(&self.data))
    }
}

#[derive(Debug)]
pub struct InvalidLength {
    pub expected: usize,
    pub got: usize,
}

impl<const N: usize> TryFrom<String> for FixedUTF8String<N> {
    type Error = InvalidLength;
    fn try_from(value: String) -> Result<Self, InvalidLength> {
        if value.len() != N {
            return Err(InvalidLength {
                expected: N,
                got: value.len(),
            });
        }
        Ok(Self {
            data: value.into_bytes(),
        })
    }
}

impl ResTableTypeFlags {
    /// If set, the entry is sparse, and encodes both the entry ID and offset into each entry, and
    /// a binary search is used to find the key. Only availiable on platforms >= O.
    /// Mark anu types that use this with a v26 qualifier to prevent runtime issues on older
    /// platforms.
    pub fn sparse(&self) -> bool {
        (self.flags & 0x01) != 0
    }

    /// If set, the offsets to the entries are encoded in 16-bit, real_offset = offset * 4u
    /// A 16-bit offset of 0xffffu means a NO_ENTRY
    // TODO: do stuff with this...
    // FIXME: probably causes errors for apks that use this
    pub fn offset_16(&self) -> bool {
        (self.flags & 0x02) != 0
    }
}

/// A collection of resource entries for a particular resource data type.
///
/// If the flag FLAG_SPARSE is not set in `flags`, then this struct is followed by an array of
/// uint32_t defining the resource values, corresponding to the array of the type strings in the
/// ResTable_package::type_strings string block. Each of these hold an index from entries_start; a
/// value of NO_ENTRY means that entry is not defined.
///
/// If the flag FLAG_SPARSE is set in `flags`, then this struct is followed by an array of
/// ResTable_sparseTypeEntry defining only the entries that have values for this type. Each entry
/// is sroted by their entry ID such that a binary search can be performed over the entries. The ID
/// and offset are encoded in a uint32_t. See ResTable_sparseTypeEntry
///
/// There may be multiple of these chunks for a particular resource type, supply different
/// configuration variations for the resource values of that type.
#[derive(Debug, PartialEq, Clone)]
pub struct ResTableType {
    /// The type identifier this chunk is holding. Type IDs start at 1 (corresponding the the value
    /// of the type bits in a resource identifier). 0 is invalid.
    pub id: u8,
    pub flags: ResTableTypeFlags,

    /// Configuration this collection of entries is designed for. This must always be last.
    pub config: ResTableConfig,
    pub entries: Vec<(usize, Option<ResTableEntry>)>,
}

impl Writeable for ResTableType {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        let header_size = self.header_size();
        let header_offset = ResChunk::get_header_offset(writer.stream_position()?);

        let sparse = self.flags.sparse();

        self.id
            .write_no_opts(writer)
            .add_context(|| "write id for ResTableType")?;
        self.flags
            .write_no_opts(writer)
            .add_context(|| "write flags for ResTableType")?;

        let reserved: u16 = 0;
        reserved
            .write_no_opts(writer)
            .add_context(|| "write reserved for ResTableType")?;

        let entry_count: u32 = self.entries.len() as u32;
        entry_count
            .write_no_opts(writer)
            .add_context(|| "write entry_count for ResTableType")?;

        let entries_start = calc_entries_start(&self.config, self.entries.len());
        entries_start
            .write_no_opts(writer)
            .add_context(|| "write entries_start for ResTableType")?;

        self.config
            .write_no_opts(writer)
            .add_context(|| "write config for ResTableType")?;

        let pos = writer.stream_position()?;

        let entry_indicies = calculate_entry_indicies(&self.entries, sparse).map_err(|e| {
            StreamError::new_string_context(e, pos, "calculate entry_indicies for ResTableType")
        })?;
        writer.seek(SeekFrom::Start(header_offset + 8 + header_size as u64))?; // FIXME: don't like this, we end up moving back 4 bytes
        entry_indicies
            .write_no_opts(writer)
            .add_context(|| "write entry_indicies for ResTableType")?;

        writer.seek(SeekFrom::Start(header_offset + entries_start as u64))?;

        self.entries
            .write_no_opts(writer)
            .add_context(|| "write entries for ResTableType")
    }
}

impl Readable for ResTableType {
    type Args = usize;
    fn read<R: Read + Seek>(reader: &mut R, args: Self::Args) -> StreamResult<Self> {
        let header_offset = ResChunk::get_header_offset(reader.stream_position()?);
        let id = u8::read_no_opts(reader).add_context(|| "read id for ResTableType")?;
        let flags = ResTableTypeFlags::read_no_opts(reader)
            .add_context(|| "read flags for ResTableType")?;
        let reserved =
            u16::read_no_opts(reader).add_context(|| "read reserved for ResTableType")?;

        if reserved != 0 {
            return Err(StreamError::new_string_context(
                format!("invalid reserved value: {}, expected 0", reserved),
                reader.stream_position()?,
                "validate reserved for ResTableType",
            ));
        }
        let entry_count =
            u32::read_no_opts(reader).add_context(|| "read entry_count for ResTableType")?;
        let entries_start =
            u32::read_no_opts(reader).add_context(|| "read entries_start for ResTableType")?;
        let config =
            ResTableConfig::read_no_opts(reader).add_context(|| "read config for ResTableType")?;

        reader.seek(SeekFrom::Start(header_offset + args as u64))?;

        let entry_indicies =
            ResTableTypeEntryIndicies::read(reader, (entry_count as usize, flags.sparse()))
                .add_context(|| "read entry_indicies for ResTableType")?;

        reader.seek(SeekFrom::Start(header_offset + entries_start as u64))?;

        let entries = <Vec<(usize, Option<ResTableEntry>)>>::read(reader, entry_indicies)
            .add_context(|| "read entries for ResTableType")?;

        Ok(Self {
            id,
            flags,
            config,
            entries,
        })
    }
}

impl Writeable for Vec<(usize, Option<ResTableEntry>)> {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        for val in self {
            if let Some(entry) = val.1 {
                entry
                    .write_no_opts(writer)
                    .add_context(|| "write entry for Vec<(usize, Option<ResTableEntry>)>")?;
            }
        }

        Ok(())
    }
}

impl HeaderSizeInstance for ResTableType {
    fn header_size(&self) -> usize {
        1 + 1 + 2 + 4 + 4 + self.config.get_size()
    }
}

impl ResTableType {
    pub fn get_entry(&self, id: usize) -> Option<&ResTableEntry> {
        if !self.flags.sparse() {
            let entry = self.entries.get(id)?;

            if entry.0 != id {
                return None;
            }

            entry.1.as_ref()
        } else {
            for entry in &self.entries {
                if entry.0 == id {
                    return entry.1.as_ref();
                }
            }
            None
        }
    }

    pub fn get_entry_mut(&mut self, id: usize) -> Option<&mut ResTableEntry> {
        if !self.flags.sparse() {
            let entry = self.entries.get_mut(id)?;

            if entry.0 != id {
                return None;
            }

            entry.1.as_mut()
        } else {
            for entry in self.entries.iter_mut() {
                if entry.0 == id {
                    return entry.1.as_mut();
                }
            }
            None
        }
    }
}

#[derive(Debug)]
pub enum InvalidEntry {
    InvalidID { expected_id: usize, got_id: usize },
    InvalidEntry,
}

impl Display for InvalidEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidID {
                expected_id,
                got_id,
            } => write!(
                f,
                "invalid entry, expected entry id {}, got {}",
                expected_id, got_id
            ),
            Self::InvalidEntry => write!(f, "invalid entry, entry cannot be None"),
        }
    }
}

fn calculate_entry_indicies(
    entries: &Vec<(usize, Option<ResTableEntry>)>,
    sparse: bool,
) -> Result<ResTableTypeEntryIndicies, InvalidEntry> {
    let mut pos: usize = 0;

    Ok(match sparse {
        true => {
            let mut indicies: Vec<ResTableSparseTypeEntry> = Vec::new();
            for (id, entry) in entries {
                if entry.is_none() {
                    return Err(InvalidEntry::InvalidEntry);
                }
                let size = (entry.as_ref())
                    .ok_or(InvalidEntry::InvalidEntry)?
                    .get_size();
                indicies.push(ResTableSparseTypeEntry {
                    idx: (*id) as u16,
                    offset: pos as u16,
                });

                pos += size;
            }
            ResTableTypeEntryIndicies::Sparse(indicies)
        }
        false => {
            let mut indicies: Vec<u32> = Vec::new();
            for (i, (id, entry)) in entries.iter().enumerate() {
                if i != *id {
                    return Err(InvalidEntry::InvalidID {
                        expected_id: i,
                        got_id: *id,
                    });
                }
                if let Some(entry) = entry {
                    indicies.push(pos as u32);
                    pos += entry.get_size();
                } else {
                    indicies.push(0xffffffff);
                }
            }
            ResTableTypeEntryIndicies::NoSparse(indicies)
        }
    })
}

impl Readable for Vec<(usize, Option<ResTableEntry>)> {
    type Args = ResTableTypeEntryIndicies;
    fn read<R: Read + Seek>(reader: &mut R, args: Self::Args) -> StreamResult<Self> {
        let start_pos = reader.stream_position()?;
        match args {
            ResTableTypeEntryIndicies::NoSparse(indcs) => {
                let mut data: Vec<(usize, Option<ResTableEntry>)> = Vec::with_capacity(indcs.len());
                for (i, offset) in indcs.into_iter().enumerate() {
                    if offset == 0xffffffff {
                        data.push((i, None));
                    } else {
                        reader.seek(SeekFrom::Start(start_pos + offset as u64))?;
                        data.push((
                            i,
                            Some(ResTableEntry::read_no_opts(reader).add_context(|| {
                                "read entry for ResTableTypeEntryIndicies::NoSparse"
                            })?),
                        ));
                    }
                }

                Ok(data)
            }
            ResTableTypeEntryIndicies::Sparse(sp) => {
                let mut data: Vec<(usize, Option<ResTableEntry>)> = Vec::with_capacity(sp.len());

                for v in sp {
                    reader.seek(SeekFrom::Start(start_pos + v.offset as u64))?;
                    data.push((
                        v.idx as usize,
                        Some(
                            ResTableEntry::read_no_opts(reader).add_context(|| {
                                "read entry for ResTableTypeEntryIndicies::Sparse"
                            })?,
                        ),
                    ));
                }

                Ok(data)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ResTableEntryFlags(u16);

impl Readable for ResTableEntryFlags {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self(
            u16::read_no_opts(reader).add_context(|| "read flags for ResTableEntryFlags")?,
        ))
    }
}

impl Writeable for ResTableEntryFlags {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.0
            .write_no_opts(writer)
            .add_context(|| "write flags for ResTableEntryFlags")
    }
}

impl ResTableEntryFlags {
    /// If set, this is a compex entry, holding a set of name/value mappings. It is followed by an
    /// array of ResTableMap structures.
    pub fn complex(&self) -> bool {
        self.0 & 0x1 != 0
    }

    /// If set, this resource has been declared public, so libraries are allowed to reference it.
    pub fn public(&self) -> bool {
        self.0 & 0x2 != 0
    }

    /// If set, this is a weak resource and may be overriden by strong resources of the same
    /// name/types. This is only useful during linking with other resource tables.
    pub fn weak(&self) -> bool {
        self.0 & 0x4 != 0
    }

    /// If set, this is a compact entry with data type and value directly encoded in the entry, see
    /// ResTable_entry::compact
    pub fn compact(&self) -> bool {
        self.0 & 0x8 != 0
    }
}

/// This is the beginning of information about an entry in the resource table. It holds the
/// reference to the name of this entry, and is immediately followed by one of:
///
/// - A ResValue structure, if FLAG_COMPLEX is -not- set.
/// - An array of ResTableMap structures, if FLAG_COMPLEX is set.
/// - If FLAG_COMPACT is set, this entry is a compact entry for simple values only
#[derive(Debug, PartialEq, Clone)]
pub struct ResTableEntry {
    /// Number of bytes in this structure
    pub flags: ResTableEntryFlags,

    pub data: ResTableEntryValue,
}

impl Readable for ResTableEntry {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let _header_size =
            u16::read_no_opts(reader).add_context(|| "read header_size for ResTableEntry")?;
        let flags = ResTableEntryFlags::read_no_opts(reader)
            .add_context(|| "read flags for ResTableEntry")?;

        let data = ResTableEntryValue::read(reader, flags)
            .add_context(|| "read data for ResTableEntryValue")?;

        Ok(Self { flags, data })
    }
}

impl Writeable for ResTableEntry {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        let header_size: u16 = self.header_size() as u16;
        header_size
            .write_no_opts(writer)
            .add_context(|| "write header_size for ResTableEntry")?;

        self.flags
            .write_no_opts(writer)
            .add_context(|| "write flags for ResTableEntry")?;

        self.data
            .write_no_opts(writer)
            .add_context(|| "write data for ResTableEntry")
    }
}

impl HeaderSizeInstance for ResTableEntry {
    fn header_size(&self) -> usize {
        4 + self.data.header_size()
    }
}

impl ResTableEntry {
    pub fn get_size(&self) -> usize {
        4 + self.data.get_size()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ResTableEntryValue {
    ResValue(ResTableResValueEntry),
    Map(ResTableMapEntry),
    Compact(u32),
}

impl HeaderSizeInstance for ResTableEntryValue {
    fn header_size(&self) -> usize {
        match self {
            ResTableEntryValue::Compact(_) => 0,
            ResTableEntryValue::ResValue(_) => 4,
            ResTableEntryValue::Map(_) => 12,
        }
    }
}

impl ResTableEntryValue {
    pub fn is_compact(&self) -> bool {
        matches!(self, ResTableEntryValue::Compact(_))
    }

    pub fn is_complex(&self) -> bool {
        matches!(self, ResTableEntryValue::Map(_))
    }

    pub fn get_size(&self) -> usize {
        match self {
            ResTableEntryValue::ResValue(r) => r.get_size(),
            ResTableEntryValue::Map(m) => m.get_size(),
            ResTableEntryValue::Compact(_) => 0x4,
        }
    }
}

/// Extended form of a ResTable_entry for map entries, defining a parent map resource from which to
/// inherit values.
#[derive(Debug, PartialEq, Clone)]
pub struct ResTableMapEntry {
    /// Reference into ResTable_package::key_strings identifying this entry.
    pub key: ResStringPoolRef,
    /// Resource identifier of the parent mapping, or 0 if there is none.
    /// This is always treated as a TYPE_DYNAMIC_REFERENCE.
    pub parent: ResTableRef,

    pub map: Vec<ResTableMap>,
}

impl Readable for ResTableMapEntry {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let key = ResStringPoolRef::read_no_opts(reader)
            .add_context(|| "read key for ResTableMapEntry")?;
        let parent =
            ResTableRef::read_no_opts(reader).add_context(|| "read parent for ResTableMapEntry")?;
        let count = u32::read_no_opts(reader).add_context(|| "read count for ResTableMapEntry")?;
        let map = <Vec<ResTableMap>>::read_vec(reader, count as usize)
            .add_context(|| "read map for ResTableMapEntry")?;

        Ok(Self { key, parent, map })
    }
}

impl Writeable for ResTableMapEntry {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.key
            .write_no_opts(writer)
            .add_context(|| "write key for ResTableMapEntry")?;
        self.parent
            .write_no_opts(writer)
            .add_context(|| "write parent for ResTableMapEntry")?;
        let count: u32 = self.map.len() as u32;
        count
            .write_no_opts(writer)
            .add_context(|| "write count for ResTableMapEntry")?;
        self.map
            .write_vec(writer)
            .add_context(|| "write map for ResTableMapEntry")
    }
}

impl ResTableMapEntry {
    pub fn get_size(&self) -> usize {
        4 + 4 + 4 + self.map.len() * (4 + 8)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ResTableResValueEntry {
    /// Reference into ResTable_package::key_strings identifying this entry.
    pub key: ResTableRef,
    pub data: ResValue,
}

impl Readable for ResTableResValueEntry {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            key: ResTableRef::read_no_opts(reader)
                .add_context(|| "read key for ResTableResValueEntry")?,
            data: ResValue::read_no_opts(reader)
                .add_context(|| "read data for ResTableValueEntry")?,
        })
    }
}

impl Writeable for ResTableResValueEntry {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.key
            .write_no_opts(writer)
            .add_context(|| "write key for ResTableResValueEntry")?;
        self.data
            .write_no_opts(writer)
            .add_context(|| "write data for ResTableResValueEntry")
    }
}

impl ResTableResValueEntry {
    pub fn get_size(&self) -> usize {
        4 + 8
    }
}

/// A single name/value mapping that is part of a complex resource entry.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ResTableMap {
    /// The resource identifier defining this mapping's name. For attribute resources, 'name' can
    /// be one of the following special resource types to supply meta-data about the attribute; for
    /// all other resource types it must be an attribute resource.
    pub name: ResTableRef,

    /// This mapping's value
    pub value: ResValue,
}

impl Readable for ResTableMap {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            name: ResTableRef::read_no_opts(reader).add_context(|| "read name for ResTableMap")?,
            value: ResValue::read_no_opts(reader).add_context(|| "read value for ResTableMap")?,
        })
    }
}

impl Writeable for ResTableMap {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.name
            .write_no_opts(writer)
            .add_context(|| "write name for ResTableMap")?;
        self.value
            .write_no_opts(writer)
            .add_context(|| "write value for ResTableMap")
    }
}

impl Readable for ResTableEntryValue {
    type Args = ResTableEntryFlags;
    fn read<R: Read + Seek>(reader: &mut R, args: Self::Args) -> StreamResult<Self> {
        if args.compact() {
            Ok(ResTableEntryValue::Compact(
                u32::read_no_opts(reader)
                    .add_context(|| "read entries for ResTableEntryValue::Compact")?,
            ))
        } else if args.complex() {
            Ok(ResTableEntryValue::Map(
                ResTableMapEntry::read_no_opts(reader)
                    .add_context(|| "read entries for ResTableEntryValue::Map")?,
            ))
        } else {
            Ok(ResTableEntryValue::ResValue(
                ResTableResValueEntry::read_no_opts(reader)
                    .add_context(|| "read entries for ResTableEntryValue::ResValue")?,
            ))
        }
    }
}

impl Writeable for ResTableEntryValue {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        match self {
            Self::ResValue(v) => v
                .write_no_opts(writer)
                .add_context(|| "write value for ResTableEntryValue::ResValue"),
            Self::Map(v) => v
                .write_no_opts(writer)
                .add_context(|| "write value for ResTableEntryValue::Map"),
            Self::Compact(v) => v
                .write_no_opts(writer)
                .add_context(|| "write value for ResTableEntryValue::Compact"),
        }
    }
}

fn calc_entries_start(config: &ResTableConfig, total_entries: usize) -> u32 {
    8 + 1 + 1 + 2 + 4 + 4 + config.get_size() as u32 + (total_entries as u32 * 4)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ResTableSparseTypeEntry {
    pub idx: u16,
    pub offset: u16,
}

impl Readable for ResTableSparseTypeEntry {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            idx: u16::read_no_opts(reader)
                .add_context(|| "read idx for ResTableSparseTypeEntry")?,
            offset: u16::read_no_opts(reader)
                .add_context(|| "read offset for ResTableSparseTypeEntry")?,
        })
    }
}

impl Writeable for ResTableSparseTypeEntry {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.idx
            .write_no_opts(writer)
            .add_context(|| "write idx for ResTableSparseTypeEntry")?;
        self.offset
            .write_no_opts(writer)
            .add_context(|| "write offset for ResTableSparseTypeEntry")
    }
}

impl ResTableSparseTypeEntry {
    pub fn get_size() -> usize {
        2 + 2
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResTableTypeEntryIndicies {
    NoSparse(Vec<u32>),
    Sparse(Vec<ResTableSparseTypeEntry>),
}

impl Writeable for ResTableTypeEntryIndicies {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        match self {
            Self::Sparse(e) => e
                .write_vec(writer)
                .add_context(|| "write entries for ResTableTypeEntryIndicies::Sparse"),
            Self::NoSparse(e) => e
                .write_vec(writer)
                .add_context(|| "write entries for ResTableTypeEntryIndicies::NoSparse"),
        }
    }
}

impl Readable for ResTableTypeEntryIndicies {
    type Args = (usize, bool);
    fn read<R: Read + Seek>(reader: &mut R, args: Self::Args) -> StreamResult<Self> {
        if args.1 {
            Ok(ResTableTypeEntryIndicies::Sparse(
                <Vec<ResTableSparseTypeEntry>>::read_vec(reader, args.0)
                    .add_context(|| "read entries for ResTableTypeEntryIndicies::Sparse")?,
            ))
        } else {
            Ok(ResTableTypeEntryIndicies::NoSparse(
                <Vec<u32>>::read_vec(reader, args.0)
                    .add_context(|| "read entries for RestableTypeEntryIndicies::NoSparse")?,
            ))
        }
    }
}

impl ResTableTypeEntryIndicies {
    pub fn is_sparse(&self) -> bool {
        match self {
            ResTableTypeEntryIndicies::Sparse(_) => true,
            ResTableTypeEntryIndicies::NoSparse(_) => false,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            ResTableTypeEntryIndicies::NoSparse(ent) => ent.len(),
            ResTableTypeEntryIndicies::Sparse(ent) => ent.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// A collection of resource data types within a package. Followed by one or more ResTable_type and
/// ResTable_typeSpec structures containing the entry values for each resource type.
#[derive(Debug, PartialEq, Clone)]
pub struct ResTablePackage {
    /// If this is a base package, its ID. Package IDs start at 1 (corresponding to the value of
    /// the package bits in a resource identifier). 0 means this is not a base package.
    pub id: u32,

    /// Actual name of this package, null terminated
    pub name: String,

    /// Last index into type_strings that is for public use by others.
    pub last_public_type: u32,

    /// Last index into key_strings that is for public use by others.
    pub last_public_key: u32,
    pub type_id_offset: u32,
    pub string_pool_type: StringPoolHandler,
    pub string_pool_key: StringPoolHandler,
    pub chunks: Vec<ResChunk>,
}

impl Readable for ResTablePackage {
    type Args = usize;
    fn read<R: Read + Seek>(reader: &mut R, args: Self::Args) -> StreamResult<Self> {
        let header_offset = ResChunk::get_header_offset(reader.stream_position()?);

        let id = u32::read_no_opts(reader).add_context(|| "read id for ResTablePackage")?;
        let name = read_utf16_fixed_null_string(reader, 128)
            .add_context(|| "read name for ResTablePackage")?;
        let type_strings =
            u32::read_no_opts(reader).add_context(|| "read type_strings for ResTablePackage")?;
        let last_public_type = u32::read_no_opts(reader)
            .add_context(|| "read last_public_type for ResTablePackage")?;

        let key_strings =
            u32::read_no_opts(reader).add_context(|| "read key_strings for ResTablePackage")?;
        let last_public_key =
            u32::read_no_opts(reader).add_context(|| "read last_public_key for ResTablePackage")?;

        let type_id_offset =
            u32::read_no_opts(reader).add_context(|| "read type_id_offset for ResTablePackage")?;

        reader.seek(SeekFrom::Start(header_offset + type_strings as u64))?;
        let string_pool_type = parse_string_pool(reader)
            .add_context(|| "read string_pool_type for ResTablePackage")?;

        reader.seek(SeekFrom::Start(header_offset + key_strings as u64))?;
        let string_pool_key =
            parse_string_pool(reader).add_context(|| "read string_pool_key for ResTablePackage")?;

        let size = args as u64 - (reader.stream_position()? - header_offset);

        let chunks = <Vec<ResChunk>>::read(reader, size)
            .add_context(|| "read chunks for ResTablePackage")?;

        Ok(Self {
            id,
            name,
            last_public_type,
            last_public_key,
            type_id_offset,
            string_pool_type,
            string_pool_key,
            chunks,
        })
    }
}

impl HeaderSizeStatic for ResTablePackage {
    fn header_size() -> usize {
        280
    }
}

impl ResTablePackage {
    pub fn resolve_ref(&self, reference: ResTableRef) -> Option<&ResTableEntry> {
        for chunk in &self.chunks {
            if let ResTypeValue::TableType(table_type) = &chunk.data {
                if table_type.id != reference.type_index {
                    continue;
                }
                return table_type.get_entry(reference.entry_index as usize);
            }
        }

        None
    }
    pub fn resolve_ref_mut(&mut self, reference: ResTableRef) -> Option<&mut ResTableEntry> {
        for chunk in self.chunks.iter_mut() {
            if let ResTypeValue::TableType(table_type) = &mut chunk.data {
                if table_type.id != reference.type_index {
                    continue;
                }
                return table_type.get_entry_mut(reference.entry_index as usize);
            }
        }

        None
    }
}

impl Writeable for ResTablePackage {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        let header_offset = ResChunk::get_header_offset(writer.stream_position()?);
        self.id
            .write_no_opts(writer)
            .add_context(|| "write id for ResTablePackage")?;

        write_utf16_fixed_null_string(writer, &self.name, 128)
            .add_context(|| "write name for ResTablePackage")?;

        let type_strings_pos_pos = writer.stream_position()?;

        writer.seek_relative(4)?;

        self.last_public_type
            .write_no_opts(writer)
            .add_context(|| "write last_public_type for ResTablePackage")?;

        let key_strings_pos_pos = writer.stream_position()?;

        writer.seek_relative(4)?;

        self.last_public_key
            .write_no_opts(writer)
            .add_context(|| "write last_public_key for ResTablePackage")?;

        self.type_id_offset
            .write_no_opts(writer)
            .add_context(|| "write type_id_offset for ResTablePackage")?;

        // go back to write type_strings pos

        let type_string_pos = writer.stream_position()?;

        writer.seek(std::io::SeekFrom::Start(type_strings_pos_pos))?;

        let type_strings_pos2: u32 = (type_string_pos - header_offset) as u32;

        type_strings_pos2
            .write_no_opts(writer)
            .add_context(|| "write type_strings_pos for ResTablePackage")?;

        // go forward and write type_strings

        writer.seek(std::io::SeekFrom::Start(type_string_pos))?;

        write_string_pool(writer, self.string_pool_type)
            .add_context(|| "write string_pool_type for ResTablePackage")?;

        let key_string_pos = writer.stream_position()?;

        // go back and write key_strings pos

        writer.seek(std::io::SeekFrom::Start(key_strings_pos_pos))?;

        let key_strings_pos2: u32 = (key_string_pos - header_offset) as u32;

        key_strings_pos2
            .write_no_opts(writer)
            .add_context(|| "write key_strings_pos for ResTablePackage")?;

        // go forward and write key_strings

        writer.seek(std::io::SeekFrom::Start(key_string_pos))?;

        write_string_pool(writer, self.string_pool_key)
            .add_context(|| "write string_pool_key for ResTablePackage")?;

        self.chunks
            .write_vec(writer)
            .add_context(|| "write chunks for ResTablePackage")?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct InvalidStringPoolResType;

impl Display for InvalidStringPoolResType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "expected string pool got something else")
    }
}

fn parse_string_pool<R: Read + Seek>(reader: &mut R) -> StreamResult<StringPoolHandler> {
    let start = reader.stream_position()?;
    let chunk =
        ResChunk::read_no_opts(reader).add_context(|| "read chunk for parse_string_pool")?;

    if let ResTypeValue::StringPool(sp) = chunk.data {
        return Ok(sp.into());
    }

    let res_type: ResType = (&chunk.data).into();

    Err(StreamError::new_string_context(
        format!("invalid res type: {}, expected StringPool", res_type),
        start,
        "validate chunk for parse_string_pool",
    ))
}

fn write_string_pool<W: Write + Seek>(
    writer: &mut W,
    string_pool: StringPoolHandler,
) -> StreamResult<()> {
    let chunk = ResChunk {
        data: ResTypeValue::StringPool(string_pool.string_pool),
    };
    chunk
        .write_no_opts(writer)
        .add_context(|| "write string pool chunk for write_string_pool")
}

fn read_utf16_fixed_null_string<R: Read + Seek>(
    reader: &mut R,
    length: usize,
) -> StreamResult<String> {
    let mut data: Vec<u16> = Vec::new();
    let start = reader.stream_position()?;
    let end = start + (length as u64) * 2;
    for _ in 0..length {
        let val = <u16>::read_no_opts(reader)
            .add_context(|| "read utf16 char for read_utf16_fixed_null_string")?;
        if val == 0 {
            reader.seek(std::io::SeekFrom::Start(end))?;
            break;
        }
        data.push(val);
    }

    String::from_utf16(data.as_slice()).map_err(|e| {
        StreamError::new_string_context(e, start, "decode utf16 for read_utf16_fixed_null_string")
    })
}

#[derive(Debug)]
pub struct PackageNameError(usize);

impl Display for PackageNameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "package name was too long, expected a length of less than 128, got a length of {}",
            self.0
        )
    }
}

fn write_utf16_fixed_null_string<W: Write + Seek>(
    writer: &mut W,
    string: &str,
    length: usize,
) -> StreamResult<()> {
    let mut data: Vec<u16> = string.encode_utf16().collect();
    if data.len() >= length {
        return Err(StreamError::new_string_context(
            format!("invalid data length {}, expected {}", data.len(), length),
            writer.stream_position()?,
            "validate data length for write_utf16_fixed_null_string",
        ));
    }
    data.resize(length, 0);
    data.write_vec(writer)
        .add_context(|| "write encoded utf16 data for write_utf16_fixed_null_string")?;

    Ok(())
}

/// **************
/// RESOURCE TABLE
/// **************
///
/// Header for a resource table. Its data contains a series of additional chunks:
///
/// - A ResStringPool_header containg all table values. This string pool contains all of the string
///     values in the entire resource table (not the names of entries or type identifiers however).
/// - One or more ResTable_package chunks.
///
/// Specific entries within a resource table can be uniquely identified with a single integer as
/// defined by the ResTable_ref structure.
#[derive(Debug, PartialEq, Clone)]
pub struct ResTable {
    pub string_pool: StringPoolHandler,
    pub packages: Packages,
}

impl Readable for ResTable {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let package_count =
            u32::read_no_opts(reader).add_context(|| "read package_count for ResTable")?;
        let string_pool =
            parse_string_pool(reader).add_context(|| "read string_pool for ResTable")?;
        let packages = Packages::read(reader, package_count as usize)
            .add_context(|| "read packages for ResTable")?;

        Ok(Self {
            string_pool,
            packages,
        })
    }
}

impl Writeable for ResTable {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        let package_count: u32 = self.packages.packages.len() as u32;
        package_count
            .write_no_opts(writer)
            .add_context(|| "write package_count for ResTable")?;
        write_string_pool(writer, self.string_pool)
            .add_context(|| "write string_pool for ResTable")?;
        self.packages
            .write_no_opts(writer)
            .add_context(|| "write packages for ResTable")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Packages {
    packages: Vec<ResChunk>,
}

impl Readable for Packages {
    type Args = usize;
    fn read<R: Read + Seek>(reader: &mut R, args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            packages: <Vec<ResChunk>>::read_vec(reader, args)
                .add_context(|| "read packages for Packages")?,
        })
    }
}

impl Writeable for Packages {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.packages
            .write_vec(writer)
            .add_context(|| "write packages for Packages")
    }
}

impl Packages {
    pub fn get(&self, index: usize) -> Option<&ResTablePackage> {
        let item = self.packages.get(index)?;

        if let ResTypeValue::TablePackage(pkg) = &item.data {
            return Some(pkg);
        }
        None
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut ResTablePackage> {
        let item = self.packages.get_mut(index)?;

        if let ResTypeValue::TablePackage(pkg) = &mut item.data {
            return Some(pkg);
        }
        None
    }

    pub fn first(&self) -> Option<&ResTablePackage> {
        self.get(0)
    }

    pub fn first_mut(&mut self) -> Option<&mut ResTablePackage> {
        self.get_mut(0)
    }
}

impl HeaderSizeStatic for ResTable {
    fn header_size() -> usize {
        4
    }
}

impl ResTable {
    pub fn read_all<R: Seek + Read>(reader: &mut R) -> StreamResult<Self> {
        let pos = reader.stream_position()?;
        let header = ResChunk::read_no_opts(reader).add_context(|| "read chunk for ResTable")?;

        if let ResTypeValue::Table(table) = header.data {
            return Ok(table);
        }

        let res_type: ResType = (&header.data).into();

        Err(StreamError::new_string_context(
            format!("invalid res_type: {}, expected ResTable", res_type),
            pos,
            "validate read chunk for ResTable",
        ))
    }

    pub fn write_all<W: Seek + Write>(self, writer: &mut W) -> StreamResult<()> {
        let header = ResChunk {
            data: ResTypeValue::Table(self),
        };

        header
            .write_no_opts(writer)
            .add_context(|| "write chunk for ResTable")
    }
}

#[derive(Debug)]
pub struct WriteARSCError(pub std::io::Error);

impl Display for WriteARSCError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl From<std::io::Error> for WriteARSCError {
    fn from(value: std::io::Error) -> Self {
        Self(value)
    }
}

impl TryFrom<&[u8]> for ResTable {
    type Error = StreamError;
    fn try_from(value: &[u8]) -> Result<Self, StreamError> {
        let mut stream = Cursor::new(value);

        let val = ResTable::read_all(&mut stream)?;

        Ok(val)
    }
}

impl TryFrom<ResTable> for Vec<u8> {
    type Error = StreamError;
    fn try_from(value: ResTable) -> Result<Self, StreamError> {
        let mut stream = Cursor::new(Vec::new());

        value.write_all(&mut stream)?;

        Ok(stream.into_inner())
    }
}

/// A package-id to package name mapping for any shared libraries used in this resource table. The
/// package-id's encoded in this resource table may be different that the id's assigned at runtime.
/// We must be able to translate the package-id's based on the package name.
#[derive(Debug, PartialEq, Clone)]
pub struct ResTableLib {
    pub entries: Vec<ResTableLibEntry>,
}

impl Readable for ResTableLib {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let count = u32::read_no_opts(reader).add_context(|| "read count for ResTableLib")?;
        let entries = <Vec<ResTableLibEntry>>::read_vec(reader, count as usize)
            .add_context(|| "read entries for ResTableLib")?;

        Ok(Self { entries })
    }
}

impl Writeable for ResTableLib {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        let count: u32 = self.entries.len() as u32;
        count
            .write_no_opts(writer)
            .add_context(|| "write count for ResTableLib")?;

        self.entries
            .write_vec(writer)
            .add_context(|| "write entries for ResTableLib")
    }
}

impl HeaderSizeStatic for ResTableLib {
    fn header_size() -> usize {
        4
    }
}

/// A shared library package-id to package name entry.
#[derive(Debug, PartialEq, Clone)]
pub struct ResTableLibEntry {
    /// The package-id of this shared library was assigned at build time.
    /// We use a u32 to keep the structure aligned on a u32 boundary.
    pub package_id: u32,

    /// The package name of the shared library, \0 terminated
    pub package_name: String,
}

impl Readable for ResTableLibEntry {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            package_id: u32::read_no_opts(reader)
                .add_context(|| "read package_id for ResTableLibEntry")?,
            package_name: read_utf16_fixed_null_string(reader, 128)
                .add_context(|| "read package_name for ResTableLibEntry")?,
        })
    }
}

impl Writeable for ResTableLibEntry {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.package_id
            .write_no_opts(writer)
            .add_context(|| "write package_id for ResTableLibEntry")?;
        write_utf16_fixed_null_string(writer, &self.package_name, 128)
            .add_context(|| "write package_name for ResTableLibEntry")
    }
}

/// A map that allows rewriting staged (non-finalized) resource ids to their finalized counterparts
#[derive(Debug, PartialEq, Clone)]
pub struct ResTableStagedAlias {
    pub entries: Vec<ResTableStagedAliasEntry>,
}

impl Readable for ResTableStagedAlias {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let count =
            u32::read_no_opts(reader).add_context(|| "read count for ResTableStagedAlias")?;

        let entries = <Vec<ResTableStagedAliasEntry>>::read_vec(reader, count as usize)
            .add_context(|| "read entries for ResTableStagedAlias")?;

        Ok(Self { entries })
    }
}

impl Writeable for ResTableStagedAlias {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        let count: u32 = self.entries.len() as u32;
        count
            .write_no_opts(writer)
            .add_context(|| "write count for ResTableStagedAlias")?;

        self.entries
            .write_vec(writer)
            .add_context(|| "write entries for ResTableStagedAlias")
    }
}

impl HeaderSizeStatic for ResTableStagedAlias {
    fn header_size() -> usize {
        4
    }
}

/// Maps the staged (non-finalized) resource id to its finalized resource id.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ResTableStagedAliasEntry {
    /// The compile-time staged resource id to rewrite.
    pub staged_res_id: u32,

    /// The compile-time finalized resource id to which the staged resource id should be rewritten.
    pub finalized_res_id: u32,
}

impl Readable for ResTableStagedAliasEntry {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            staged_res_id: u32::read_no_opts(reader)
                .add_context(|| "read staged_res_id for ResTableStagedAliasEntry")?,
            finalized_res_id: u32::read_no_opts(reader)
                .add_context(|| "read finalized_res_id for ResTableStagedAliasEntry")?,
        })
    }
}

impl Writeable for ResTableStagedAliasEntry {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.staged_res_id
            .write_no_opts(writer)
            .add_context(|| "write staged_res_id for ResTableStagedAliasEntry")?;
        self.finalized_res_id
            .write_no_opts(writer)
            .add_context(|| "write finalized_res_id for ResTableStagedAliasEntry")
    }
}

/// Specifies the set of resources that are explicitly allowed to be overlaid by RROs.
#[derive(Debug, Clone, PartialEq)]
pub struct ResTableOverlayable {
    /// The name of the overlayable set of resources that overlays target.
    pub name: String,

    /// The component responsible for enabling and disabling overlays targeting this chunk.
    pub actor: String,
}

impl Readable for ResTableOverlayable {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            name: read_utf16_fixed_null_string(reader, 256)
                .add_context(|| "read name for ResTableOverlayable")?,
            actor: read_utf16_fixed_null_string(reader, 256)
                .add_context(|| "read actor for ResTableOverlayable")?,
        })
    }
}

impl Writeable for ResTableOverlayable {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        write_utf16_fixed_null_string(writer, &self.name, 256)
            .add_context(|| "write name for ResTableOverlayable")?;
        write_utf16_fixed_null_string(writer, &self.actor, 256)
            .add_context(|| "write actor for ResTableOverlayable")
    }
}

impl HeaderSizeStatic for ResTableOverlayable {
    fn header_size() -> usize {
        1024
    }
}

/// Flags for a bitmask for all possible overlayable policy options.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct PolicyFlags {
    pub flags: u32,
}

impl Readable for PolicyFlags {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            flags: u32::read_no_opts(reader).add_context(|| "read flags for PolicyFlags")?,
        })
    }
}

impl Writeable for PolicyFlags {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.flags
            .write_no_opts(writer)
            .add_context(|| "write flags for PolicyFlags")
    }
}

impl PolicyFlags {
    /// Any overlay can overlay these resources.
    pub fn public(&self) -> bool {
        self.flags & 1 != 0
    }
    /// The overlay must reside of the system partition or must have existed on the system
    /// partition before an upgrade to overlay these resources.
    pub fn system_partition(&self) -> bool {
        self.flags & 2 != 0
    }
    /// The overlay must reside of the vendor parition or must have existed on the vendor partition
    /// before an upgrade to overlay these resources.
    pub fn vendor_partition(&self) -> bool {
        self.flags & 4 != 0
    }
    /// The overlay must reside of the product partition or must have existed on the product
    /// partition before an upgrade to overlay these resources.
    pub fn product_partition(&self) -> bool {
        self.flags & 8 != 0
    }
    /// The overlay must be signed with the same signature as the package containing the target
    /// resource.
    pub fn signature(&self) -> bool {
        self.flags & 0x10 != 0
    }
    /// The overlay must reside of the odm partition or must have existed on the odm partition
    /// before an upgrade to overlay these resources.
    pub fn odm_parition(&self) -> bool {
        self.flags & 0x20 != 0
    }
    /// The overlay must reside of the oem partition or must have existed on the oem partition
    /// before an upgrade to overlay these resources.
    pub fn oem_partition(&self) -> bool {
        self.flags & 0x40 != 0
    }
    /// The overlay must be signed with the same signature as the actor declared for the target
    /// resource.
    pub fn actor_signature(&self) -> bool {
        self.flags & 0x80 != 0
    }
    /// The overlay must be signed with the same signature as the reference package declared in the
    /// SystemConfig
    pub fn config_signature(&self) -> bool {
        self.flags & 0x100 != 0
    }
}

/// Holds a list of resource ids that are protected from being overlaid by a set of policies. If
/// the overlay fulfils at least one of the policies, then the overlay can overlay the list of
/// resources.
#[derive(Debug, Clone, PartialEq)]
pub struct ResTableOverlayablePolicy {
    pub policy_flags: PolicyFlags,
    pub entries: Vec<ResTableRef>,
}

impl HeaderSizeStatic for ResTableOverlayablePolicy {
    fn header_size() -> usize {
        8
    }
}

impl Readable for ResTableOverlayablePolicy {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let policy_flags = PolicyFlags::read_no_opts(reader)
            .add_context(|| "read policy_flags for ResTableOverlayablePolicy")?;
        let entry_count = u32::read_no_opts(reader)
            .add_context(|| "read entry_count for ResTableOverlayablePolicy")?;

        let entries = <Vec<ResTableRef>>::read_vec(reader, entry_count as usize)
            .add_context(|| "read entries for ResTableOverlayablePolicy")?;

        Ok(Self {
            policy_flags,
            entries,
        })
    }
}

impl Writeable for ResTableOverlayablePolicy {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.policy_flags
            .write_no_opts(writer)
            .add_context(|| "write policy flags for ResTableOverlayablePolicy")?;
        let entry_count: u32 = self.entries.len() as u32;
        entry_count
            .write_no_opts(writer)
            .add_context(|| "write entry_count for ResTableOverlayablePolicy")?;
        self.entries
            .write_vec(writer)
            .add_context(|| "write entries for ResTableOverlayablePolicy")
    }
}
