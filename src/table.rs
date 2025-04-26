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

// Everything here is based off of https://android.googlesource.com/platform/frameworks/base/+/master/libs/androidfw/include/androidfw/ResourceTypes.h

use std::{
    fmt::Display,
    io::{Cursor, Read, Seek, SeekFrom, Write},
};

use binrw::{binread, binrw, BinRead, BinResult, BinWrite, VecArgs};
use thiserror::Error;

use crate::{
    align,
    defs::{
        parse_chunks, ResChunk, ResChunkWriteRef, ResTableRef, ResType, ResTypeValue,
        ResTypeValueWriteRef,
    },
    res_value::ResValue,
    string_pool::{ResStringPoolRef, StringPool},
};

/// A specification of the resources defined by a particular type.
///
/// There should be one of these chunks for each resource type.
///
/// This structure is followed by an array of integers providing the set of configuration change
/// flags (ResTable_config::CONFIG_*) that have multiple resources for that configuration. In
/// addition, the high bi is set if that resource has been made public.
#[binrw]
#[derive(Debug, Clone, PartialEq)]
pub struct ResTableTypeSpec {
    /// The type identifier this chunk is holding. Type IDs start at 1 (corresponding to the value
    /// of the type bits in a resource identifier). 0 is invalid.
    pub id: u8,

    /// Must be 0.
    #[br(assert(res0 == 0))]
    #[br(temp)]
    #[bw(calc = 0)]
    res0: u8,

    /// Used to be reserved, if >0 specifies the number of ResTable_type entries for this spec.
    pub types_count: u16,

    /// Number of uint32_t entry configuration masks that follow.
    #[br(temp)]
    #[bw(calc=config_masks.len() as u32)]
    entry_count: u32,

    #[br(count=entry_count)]
    pub config_masks: Vec<u32>,
}

impl ResTableTypeSpec {
    pub fn get_header_size() -> usize {
        0x10
    }
}

#[derive(Debug, BinRead, BinWrite, PartialEq, Default, Copy, Clone)]
pub struct ResTableTypeFlags {
    pub flags: u8,
}

#[derive(Debug, BinWrite, BinRead, PartialEq, Clone, Copy)]
pub enum Orientation {
    #[brw(magic = 0x0000u8)]
    Any,
    #[brw(magic = 0x0001u8)]
    Portrait,
    #[brw(magic = 0x0002u8)]
    Landscape,
    #[brw(magic = 0x0003u8)]
    Square,
}

#[derive(Debug, BinWrite, BinRead, PartialEq, Clone, Copy)]
pub enum TouchScreen {
    #[brw(magic = 0x0000u8)]
    Any,
    #[brw(magic = 0x0001u8)]
    NoTouch,
    #[brw(magic = 0x0002u8)]
    Stylus,
    #[brw(magic = 0x0003u8)]
    Finger,
}

#[derive(Debug, BinWrite, BinRead, PartialEq, Clone, Copy)]
pub enum Density {
    #[brw(magic = 0u16)]
    Default,
    #[brw(magic = 120u16)]
    Low,
    #[brw(magic = 160u16)]
    Medium,
    #[brw(magic = 213u16)]
    Tv,
    #[brw(magic = 240u16)]
    High,
    #[brw(magic = 320u16)]
    XHigh,
    #[brw(magic = 480u16)]
    XXHigh,
    #[brw(magic = 640u16)]
    XXXHigh,
    #[brw(magic = 0xfffeu16)]
    Any,
    #[brw(magic = 0xffffu16)]
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, BinWrite, BinRead)]
pub struct Imsi(u32);

impl Imsi {
    /// Mobile country code (from SIM). 0 means "any".
    pub fn mcc(&self) -> u16 {
        self.0 as u16
    }

    /// Mobile network code (from SIM). 0 means "any".
    pub fn mnc(&self) -> u16 {
        (self.0 >> 16) as u16
    }
}

#[derive(Debug, Clone, Copy, PartialEq, BinWrite, BinRead)]
pub struct ScreenType(u32);

#[allow(clippy::int_plus_one)]
#[binrw]
#[brw(stream = s)]
#[derive(Debug, PartialEq, Clone)]
pub struct ResTableConfig {
    #[brw(try_calc=s.stream_position())]
    #[brw(restore_position)]
    start_offset: u64,
    /// Number of byte in this structure.
    #[br(temp)]
    #[bw(calc=self.get_size() as u32)]
    size: u32,

    #[br(if(size >= 4))]
    pub imsi: Option<Imsi>,

    /// TODO: impliment all of this
    #[br(if(size >= 4 + 4))]
    pub locale: Option<u32>,

    #[br(if(size >= 4 + 4 + 4))]
    pub screen_type: Option<ScreenType>,

    #[br(if(size >= 4 + 4 + 4 + 8))]
    pub input: Option<u64>,

    #[br(if(size >= 4 + 4 + 4 + 8 + 4))]
    pub screen_size: Option<u32>,

    #[br(if(size >= 4 + 4 + 4 + 8 + 4 + 4))]
    pub version: Option<u32>,

    #[br(if(size >= 4 + 4 + 4 + 8 + 4 + 4 + 4))]
    pub screen_config: Option<u32>,

    #[br(if(size >= 4 + 4 + 4 + 8 + 4 + 4 + 4 + 4))]
    pub screen_size_dp: Option<u32>,

    #[br(if(size >= 4 + 4 + 4 + 8 + 4 + 4 + 4 + 4 + 4))]
    pub locale_script: Option<FixedUTF8String<4>>,

    #[br(if(size >= 4 + 4 + 4 + 8 + 4 + 4 + 4 + 4 + 4 + 8))]
    pub locale_variant: Option<FixedUTF8String<8>>,

    #[br(if(size >= 4 + 4 + 4 + 8 + 4 + 4 + 4 + 4 + 4 + 8 + 4))]
    pub screen_config_2: Option<u32>,

    #[bw(write_with=write_bool)]
    #[br(parse_with=|reader, endian, ()| read_bool(reader, endian, (size >= 4 + 4 + 4 + 4 + 8 + 4 + 4 + 4 + 4 + 4 + 8 + 4 + 1,)))]
    #[br(if(size >= 4 + 4 + 4 + 8 + 4 + 4 + 4 + 4 + 4 + 8 + 4 + 1))]
    pub locale_script_was_computed: Option<bool>,

    #[br(if(size >= 4 + 4 + 4 + 8 + 4 + 4 + 4 + 4 + 4 + 8 + 4 + 1 + 8))]
    pub locale_numbering_system: Option<FixedUTF8String<8>>,

    #[br(temp)]
    #[bw(ignore)]
    #[brw(seek_before=SeekFrom::Start(size as u64 + start_offset))]
    _temp: (),
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

#[binrw::writer(writer, endian)]
pub fn write_bool(val: &Option<bool>, _args: ()) -> BinResult<()> {
    if let Some(val) = val {
        let val: u8 = (*val).into();

        return val.write_options(writer, endian, ());
    }
    Ok(())
}

#[binrw::parser(reader, endian)]
pub fn read_bool(should_read: bool) -> BinResult<Option<bool>> {
    if !should_read {
        return Ok(None);
    }
    let val: u8 = <u8>::read_options(reader, endian, ())?;

    if val == 0 {
        return Ok(Some(false));
    }
    Ok(Some(true))
}

#[derive(Debug, Clone, BinRead, BinWrite, PartialEq)]
pub struct FixedUTF8String<const N: usize> {
    #[br(count=N)]
    data: Vec<u8>,
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
    fn try_from(value: String) -> Result<Self, Self::Error> {
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
#[binrw]
#[brw(stream = s)]
#[br(import(header_size: u16))]
#[derive(Debug, PartialEq, Clone)]
pub struct ResTableType {
    #[brw(try_calc=Ok::<u64, binrw::Error>(ResChunk::get_header_offset(s.stream_position()?)))]
    #[brw(restore_position)]
    header_offset: u64,
    /// The type identifier this chunk is holding. Type IDs start at 1 (corresponding the the value
    /// of the type bits in a resource identifier). 0 is invalid.
    pub id: u8,

    pub flags: ResTableTypeFlags,

    /// Must be 0.
    #[br(temp)]
    #[bw(calc = 0)]
    #[br(assert(reserved == 0))]
    reserved: u16,

    /// Number of uint32_t entry indicies that follow.
    #[br(temp)]
    #[bw(calc=entries.len() as u32)]
    entry_count: u32,

    /// Offset from header where ResTable_entry data starts.
    #[br(temp)]
    #[bw(calc=calc_entries_start(config, entries.len()))]
    entries_start: u32,

    /// Configuration this collection of entries is designed for. This must always be last.
    pub config: ResTableConfig,

    #[br(parse_with=ResTableTypeEntryIndicies::parse, args(entry_count, flags.sparse()), seek_before=std::io::SeekFrom::Start(header_offset + header_size as u64))]
    #[br(temp)]
    #[bw(try_calc=calculate_entry_indicies(entries, flags.sparse()))]
    // #[bw(write_with=ResTableTypeEntryIndicies::write, assert(self.entry_indicies.is_sparse() == self.flags.sparse()))]
    entry_indicies: ResTableTypeEntryIndicies,

    #[br(seek_before=std::io::SeekFrom::Start(header_offset + entries_start as u64))]
    #[bw(seek_before=std::io::SeekFrom::Start(header_offset + entries_start as u64))]
    #[br(parse_with=parse_res_table_entries, args(&entry_indicies))]
    #[bw(write_with=write_res_table_entries)]
    pub entries: Vec<(usize, Option<ResTableEntry>)>,
}

#[binrw::writer(writer, endian)]
fn write_res_table_entries(data: &Vec<(usize, Option<ResTableEntry>)>) -> binrw::BinResult<()> {
    for val in data {
        if let Some(entry) = &val.1 {
            entry.write_options(writer, endian, ())?;
        }
    }
    Ok(())
}

impl ResTableType {
    pub fn get_header_size(&self) -> usize {
        calc_entry_indicies(&self.config) as usize
    }

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

#[derive(Debug, Error)]
pub enum InvalidEntry {
    #[error("invalid entry, expected entry id {expected_id}, got {got_id}")]
    InvalidID { expected_id: usize, got_id: usize },
    #[error("invalid entry, entry cannot be None")]
    InvalidEntry,
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
                    .get_full_size();
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
                    pos += entry.get_full_size();
                } else {
                    indicies.push(0xffffffff);
                }
            }
            ResTableTypeEntryIndicies::NoSparse(indicies)
        }
    })
}

#[binrw::parser(reader, endian)]
fn parse_res_table_entries(
    entry_indicies: &ResTableTypeEntryIndicies,
) -> BinResult<Vec<(usize, Option<ResTableEntry>)>> {
    match entry_indicies {
        ResTableTypeEntryIndicies::NoSparse(indcs) => {
            let start_pos = reader.stream_position()?;
            let mut data: Vec<(usize, Option<ResTableEntry>)> = Vec::with_capacity(indcs.len());
            for (i, offset) in indcs.iter().enumerate() {
                if *offset == 0xffffffff {
                    data.push((i, None));
                    continue;
                }
                reader.seek(SeekFrom::Start(start_pos + (*offset) as u64))?;
                data.push((i, Some(ResTableEntry::read_options(reader, endian, ())?)));
            }

            Ok(data)
        }
        ResTableTypeEntryIndicies::Sparse(sp) => {
            let start_pos = reader.stream_position()?;
            let mut data: Vec<(usize, Option<ResTableEntry>)> = Vec::with_capacity(sp.len());
            for v in sp {
                reader.seek(SeekFrom::Start(start_pos + v.offset as u64))?;
                data.push((
                    v.idx as usize,
                    Some(ResTableEntry::read_options(reader, endian, ())?),
                ));
            }

            Ok(data)
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy, BinRead, BinWrite)]
pub struct ResTableEntryFlags(u16);

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
#[binrw]
#[derive(Debug, PartialEq, Clone)]
#[brw(stream = s)]
pub struct ResTableEntry {
    /// Number of bytes in this structure
    #[br(temp)]
    #[bw(calc=self.get_size() as u16)]
    _size: u16,
    flags: ResTableEntryFlags,

    #[br(parse_with=parse_table_entry_data, args(flags))]
    #[bw(write_with=write_table_entry_data)]
    #[bw(assert(flags.compact() == data.is_compact()))]
    #[bw(assert(flags.complex() == data.is_complex()))]
    // #[brw(pad_size_to=(size as u64) - 2 - 2)]
    pub data: ResTableEntryValue,
}

impl ResTableEntry {
    pub fn get_size(&self) -> usize {
        self.data.get_size()
    }

    pub fn get_full_size(&self) -> usize {
        2 + 2 + self.data.get_full_size()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ResTableEntryValue {
    ResValue(ResTableResValueEntry),
    Map(ResTableMapEntry),
    Compact(u32),
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
            ResTableEntryValue::Map(_) => 0x10,
            ResTableEntryValue::Compact(_) => 0x4,
            ResTableEntryValue::ResValue(_) => 0x8,
        }
    }

    pub fn get_full_size(&self) -> usize {
        match self {
            ResTableEntryValue::ResValue(r) => r.get_size(),
            ResTableEntryValue::Map(m) => m.get_size(),
            ResTableEntryValue::Compact(_) => 0x4,
        }
    }
}

/// Extended form of a ResTable_entry for map entries, defining a parent map resource from which to
/// inherit values.
#[binrw]
#[derive(Debug, PartialEq, Clone)]
pub struct ResTableMapEntry {
    /// Reference into ResTable_package::key_strings identifying this entry.
    pub key: ResStringPoolRef,
    /// Resource identifier of the parent mapping, or 0 if there is none.
    /// This is always treated as a TYPE_DYNAMIC_REFERENCE.
    pub parent: ResTableRef,
    /// Number of name/value pairs that follow for FLAG_COMPLEX

    #[br(temp)]
    #[bw(calc=map.len() as u32)]
    count: u32,

    #[br(count=count)]
    pub map: Vec<ResTableMap>,
}

impl ResTableMapEntry {
    pub fn get_size(&self) -> usize {
        4 + 4 + 4 + self.map.len() * (4 + 8)
    }
}

#[derive(Debug, PartialEq, Clone, Copy, BinRead, BinWrite)]
pub struct ResTableResValueEntry {
    /// Reference into ResTable_package::key_strings identifying this entry.
    pub key: ResTableRef,
    pub data: ResValue,
}

impl ResTableResValueEntry {
    pub fn get_size(&self) -> usize {
        4 + 8
    }
}

/// A single name/value mapping that is part of a complex resource entry.
#[derive(Debug, PartialEq, Clone, Copy, BinRead, BinWrite)]
pub struct ResTableMap {
    /// The resource identifier defining this mapping's name. For attribute resources, 'name' can
    /// be one of the following special resource types to supply meta-data about the attribute; for
    /// all other resource types it must be an attribute resource.
    pub name: ResTableRef,

    /// This mapping's value
    pub value: ResValue,
}

#[binrw::parser(reader, endian)]
fn parse_table_entry_data(flags: ResTableEntryFlags) -> BinResult<ResTableEntryValue> {
    if flags.compact() {
        return Ok(ResTableEntryValue::Compact(u32::read_options(
            reader,
            endian,
            (),
        )?));
    } else if flags.complex() {
        return Ok(ResTableEntryValue::Map(ResTableMapEntry::read_options(
            reader,
            endian,
            (),
        )?));
    }
    Ok(ResTableEntryValue::ResValue(
        ResTableResValueEntry::read_options(reader, endian, ())?,
    ))
}

#[binrw::writer(writer, endian)]
fn write_table_entry_data(data: &ResTableEntryValue) -> BinResult<()> {
    match data {
        ResTableEntryValue::ResValue(map) => map.write_options(writer, endian, ()),
        ResTableEntryValue::Map(map) => map.write_options(writer, endian, ()),
        ResTableEntryValue::Compact(cm) => cm.write_options(writer, endian, ()),
    }
}

fn calc_entry_indicies(config: &ResTableConfig) -> u32 {
    8 + 1 + 1 + 2 + 4 + 4 + config.get_size() as u32
}

fn calc_entries_start(config: &ResTableConfig, total_entries: usize) -> u32 {
    calc_entry_indicies(config) + (total_entries * 4) as u32
}

#[binrw]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ResTableSparseTypeEntry {
    pub idx: u16,
    pub offset: u16,
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

impl BinWrite for ResTableTypeEntryIndicies {
    type Args<'a> = ();
    fn write_options<W: Write + Seek>(
        &self,
        writer: &mut W,
        endian: binrw::Endian,
        _args: Self::Args<'_>,
    ) -> BinResult<()> {
        match self {
            ResTableTypeEntryIndicies::Sparse(entries) => entries.write_options(writer, endian, ()),
            ResTableTypeEntryIndicies::NoSparse(entries) => {
                entries.write_options(writer, endian, ())
            }
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
    #[binrw::parser(reader, endian)]
    pub fn parse(count: u32, is_sparse: bool) -> BinResult<ResTableTypeEntryIndicies> {
        if is_sparse {
            let data: Vec<ResTableSparseTypeEntry> = <_>::read_options(
                reader,
                endian,
                VecArgs {
                    count: count as usize,
                    inner: (),
                },
            )?;

            return Ok(ResTableTypeEntryIndicies::Sparse(data));
        }
        let data: Vec<u32> = <_>::read_options(
            reader,
            endian,
            VecArgs {
                count: count as usize,
                inner: (),
            },
        )?;

        Ok(ResTableTypeEntryIndicies::NoSparse(data))
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
#[binread]
#[derive(Debug, PartialEq, Clone)]
#[br(import(size: u32))]
#[br(stream = s)]
pub struct ResTablePackage {
    #[br(try_calc=Ok::<u64, binrw::Error>(ResChunk::get_header_offset(s.stream_position()?)))]
    #[br(restore_position)]
    #[br(temp)]
    header_offset: u64,

    /// If this is a base package, its ID. Package IDs start at 1 (corresponding to the value of
    /// the package bits in a resource identifier). 0 means this is not a base package.
    pub id: u32,

    /// Actual name of this package, null terminated
    #[br(parse_with=parse_name_string)]
    pub name: String,

    /// Offset to a ResStringPool_header defining the resource type symbol table. If zero, this
    /// package is inherting from another base package (overriding specific values in it).
    #[br(temp)]
    type_strings: u32,

    /// Last index into type_strings that is for public use by others.
    pub last_public_type: u32,

    /// Offset to a ResStringPool_header defining the resource key symbol table. If zero, this
    /// package is inheriting from another base package (overriding specific values in it).
    #[br(temp)]
    key_strings: u32,

    /// Last index into key_strings that is for public use by others.
    pub last_public_key: u32,

    pub type_id_offset: u32,

    #[br(parse_with=parse_string_pool, seek_before=std::io::SeekFrom::Start(header_offset + type_strings as u64))]
    #[br(if(type_strings != 0))]
    pub string_pool_type: StringPool,

    #[br(parse_with=parse_string_pool, seek_before=std::io::SeekFrom::Start(header_offset + key_strings as u64))]
    #[br(if(key_strings != 0))]
    pub string_pool_key: StringPool,

    #[br(parse_with=parse_chunks, args(((size as u64)-(s.stream_position()? - (header_offset as u64))) as u32))]
    pub chunks: Vec<ResChunk>,
}

impl ResTablePackage {
    pub fn get_header_size() -> usize {
        0x120
    }

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

impl BinWrite for ResTablePackage {
    type Args<'a> = ();
    fn write_options<W: std::io::Write + std::io::Seek>(
        &self,
        writer: &mut W,
        endian: binrw::Endian,
        _args: Self::Args<'_>,
    ) -> BinResult<()> {
        let header_offset = ResChunk::get_header_offset(writer.stream_position()?);
        self.id.write_options(writer, endian, ())?;

        write_name_string(&self.name, writer, endian, ())?;

        let type_strings_pos_pos = writer.stream_position()?;

        writer.seek_relative(4)?;

        self.last_public_type.write_options(writer, endian, ())?;

        let key_strings_pos_pos = writer.stream_position()?;

        writer.seek_relative(4)?;

        self.last_public_key.write_options(writer, endian, ())?;

        self.type_id_offset.write_options(writer, endian, ())?;

        // go back to write type_strings pos

        let type_string_pos = writer.stream_position()?;

        writer.seek(std::io::SeekFrom::Start(type_strings_pos_pos))?;

        let type_strings_pos2: u32 = (type_string_pos - header_offset) as u32;

        type_strings_pos2.write_options(writer, endian, ())?;

        // go forward and write type_strings

        writer.seek(std::io::SeekFrom::Start(type_string_pos))?;

        write_string_pool(&self.string_pool_type, writer, endian, ())?;

        let key_string_pos = writer.stream_position()?;

        // go back and write key_strings pos

        writer.seek(std::io::SeekFrom::Start(key_strings_pos_pos))?;

        let key_strings_pos2: u32 = (key_string_pos - header_offset) as u32;

        key_strings_pos2.write_options(writer, endian, ())?;

        // go forward and write key_strings

        writer.seek(std::io::SeekFrom::Start(key_string_pos))?;

        write_string_pool(&self.string_pool_key, writer, endian, ())?;

        self.chunks.write_options(writer, endian, ())?;

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

#[binrw::parser(reader, endian)]
fn parse_string_pool() -> binrw::BinResult<StringPool> {
    let chunk = ResChunk::read_options(reader, endian, ())?;

    if let ResTypeValue::StringPool(sp) = chunk.data {
        return Ok(sp);
    }

    Err(binrw::Error::Custom {
        pos: reader.stream_position()?,
        err: Box::new(InvalidStringPoolResType),
    })
}

#[binrw::writer(writer, endian)]
fn write_string_pool(string_pool: &StringPool) -> binrw::BinResult<()> {
    let chunk = ResChunkWriteRef {
        data: ResTypeValueWriteRef::StringPool(string_pool),
    };
    chunk.write_options(writer, endian, ())
}

#[binrw::parser(reader, endian)]
fn parse_name_string() -> binrw::BinResult<String> {
    let mut data: Vec<u16> = Vec::new();
    let start = reader.stream_position()?;
    let end = start + 256;
    for _ in 0..128 {
        let val = <u16>::read_options(reader, endian, ())?;
        if val == 0 {
            reader.seek(std::io::SeekFrom::Start(end))?;
            break;
        }
        data.push(val);
    }

    String::from_utf16(data.as_slice()).map_err(|e| binrw::Error::Custom {
        pos: start,
        err: Box::new(e),
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

#[binrw::writer(writer, endian)]
fn write_name_string(string: &str) -> binrw::BinResult<()> {
    let mut data: Vec<u16> = string.encode_utf16().collect();
    if data.len() >= 128 {
        return Err(binrw::Error::Custom {
            pos: writer.stream_position()?,
            err: Box::new(PackageNameError(data.len())),
        });
    }
    data.resize(128, 0);
    for val in data {
        val.write_options(writer, endian, ())?;
    }

    Ok(())
}

/// **************
/// RESOURCE TABLE
/// **************
///
/// Header for a resource table. Its data contains a series of additional chunks:
///
/// - A ResStringPool_header containg all table values. This string pool contains all of the string
/// values in the entire resource table (not the names of entries or type identifiers however).
/// - One or more ResTable_package chunks.
///
/// Specific entries within a resource table can be uniquely identified with a single integer as
/// defined by the ResTable_ref structure.
#[binrw]
#[derive(Debug, PartialEq, Clone)]
pub struct ResTable {
    #[br(temp)]
    #[bw(calc=packages.packages.len() as u32)]
    package_count: u32,

    #[br(parse_with=parse_string_pool)]
    #[bw(write_with=write_string_pool)]
    pub string_pool: StringPool,

    #[br(args(package_count))]
    pub packages: Packages,
}

#[binrw]
#[derive(Debug, PartialEq, Clone)]
#[br(import(count: u32))]
pub struct Packages {
    #[br(count=count)]
    packages: Vec<ResChunk>,
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

impl ResTable {
    pub fn get_header_size() -> usize {
        0xc
    }

    pub fn read<R: Seek + Read>(reader: &mut R) -> Result<Self, ReadARSCError> {
        let header = ResChunk::read_le(reader).map_err(ReadARSCError::ReadError)?;

        if let ResTypeValue::Table(table) = header.data {
            return Ok(table);
        }
        Err(ReadARSCError::InvalidType((&header.data).into()))
    }

    pub fn write<W: Seek + Write>(&self, writer: &mut W) -> Result<(), WriteARSCError> {
        let header = ResChunkWriteRef {
            data: ResTypeValueWriteRef::Table(self),
        };

        Ok(header.write_le(writer)?)
    }
}

#[derive(Debug, Error)]
pub enum ReadARSCError {
    #[error("an error occured while reading the arsc {0}")]
    ReadError(binrw::Error),
    #[error("invalid type {0} expected table")]
    InvalidType(ResType),
}

#[derive(Debug, Error)]
#[error("{0}")]
pub struct WriteARSCError(pub binrw::Error);

impl From<binrw::Error> for WriteARSCError {
    fn from(value: binrw::Error) -> Self {
        Self(value)
    }
}

impl TryFrom<&[u8]> for ResTable {
    type Error = ReadARSCError;
    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        let mut stream = Cursor::new(value);

        let val = ResTable::read(&mut stream)?;

        Ok(val)
    }
}

impl TryFrom<&ResTable> for Vec<u8> {
    type Error = WriteARSCError;
    fn try_from(value: &ResTable) -> Result<Self, Self::Error> {
        let mut stream = Cursor::new(Vec::new());

        value.write(&mut stream)?;

        Ok(stream.into_inner())
    }
}
