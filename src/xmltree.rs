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

use std::io::{Cursor, Read, Seek, Write};

use binrw::{binrw, BinRead, BinWrite};
use thiserror::Error;

use crate::{
    defs::{parse_chunks, ResChunk, ResType, ResTypeValue, ResourceMap},
    res_value::{ResValue, ResValueType},
    string_pool::{ResStringPoolRef, StringPool},
};

/// Basic XML tree node. A single item in the XML document. Extended info about the node can be
/// found after header.headerSize.
#[derive(Debug, PartialEq, BinRead, BinWrite, Clone, Copy)]
pub struct ResXMLTreeNode {
    /// Line number in original source file at which this element appeared.
    pub line_number: u32,
    /// Optional XML comment that was associated with the element; -1 if none.
    pub comment: ResStringPoolRef,
}

/// Extended XML tree node for CDATA tags -- includes the CDATA string. Appears header.headerSize
/// bytes after a ResXMLTree_node
#[derive(Debug, PartialEq, BinRead, BinWrite, Clone, Copy)]
pub struct ResXMLTreeCDataExt {
    pub node: ResXMLTreeNode,
    /// The raw CDATA character data.
    pub data: ResStringPoolRef,
    /// The typed value of the character data if this is a CDATA node.
    pub typed_data: ResValue,
}

/// Extended XML tree node for namespace start / end nodes. Appears header.headerSize bytes after a
/// ResXMLTree_node.
#[derive(Debug, PartialEq, BinRead, BinWrite, Copy, Clone)]
pub struct ResXMLTreeNameSpaceExt {
    pub node: ResXMLTreeNode,
    /// The prefix of the namespace.
    pub prefix: ResStringPoolRef,
    /// The URI of the namespace.
    pub uri: ResStringPoolRef,
}

impl ResXMLTreeNameSpaceExt {
    pub fn get_header_size() -> usize {
        0x10
    }
}

/// Extended XML tree node for element start / end nodes. Appears header.headerSize bytes after a
/// ResXMLTree_node.
#[derive(Debug, PartialEq, BinRead, BinWrite, Copy, Clone)]
pub struct ResXMLTreeEndElementExt {
    pub node: ResXMLTreeNode,
    /// String of the full namespace of this element.
    pub ns: ResStringPoolRef,
    /// String name of this node if it is an ELEMENT; the raw character data if this is a CDATA
    /// node.
    pub name: ResStringPoolRef,
}
impl ResXMLTreeEndElementExt {
    pub fn get_header_size() -> usize {
        0x10
    }
}
/// Extended XML tree node for start tags -- includes attribute information.
/// Appears header.headerSize bytes after a ResXMLTree_node.
#[binrw]
#[brw(stream = s)]
#[derive(Debug, PartialEq, Clone)]
pub struct ResXMLTreeAttrExt {
    pub node: ResXMLTreeNode,

    #[br(temp)]
    #[brw(try_calc=s.stream_position())]
    #[bw(restore_position)]
    start_offset: u64,

    /// String of the full namespace of this element.
    pub ns: ResStringPoolRef,

    /// String name of this node if it is an ELEMENT; the raw character data if this is a CDATA
    /// node.
    pub name: ResStringPoolRef,

    /// Byte offset from the start of this structure where the attributes start.
    #[br(temp)]
    #[bw(calc=4 + 4 + 2 + 2 + 2 + 2 + 2 + 2)]
    attribute_start: u16,

    /// Size of the ResXMLTree_attribute structures that follow.
    #[bw(calc = 20)]
    #[br(temp)]
    #[br(assert(attribute_size==20))]
    attribute_size: u16,

    /// Number of attributes associated with an ELEMENT. These are availiable as an array of
    /// ResXMLTree_attribute structures immediately following this node.
    #[br(temp)]
    #[bw(calc=attributes.len() as u16)]
    attribute_count: u16,

    /// Index (1-based) of the "id" attribute. 0 if none.
    pub id_index: u16,
    /// Index (1-based) of the "class" attribute. 0 if none.
    pub class_index: u16,
    /// Index (1-based) of the "style" attribute. 0 if none.
    pub style_index: u16,

    #[br(count=attribute_count)]
    #[brw(seek_before=std::io::SeekFrom::Start(start_offset + (attribute_start as u64)))]
    pub attributes: Vec<ResXMLTreeAttribute>,
}

impl ResXMLTreeAttrExt {
    pub fn get_header_size() -> usize {
        0x10
    }
}

#[derive(Debug)]
pub enum NodeToElementError {
    NoAttrName { index: u32 },
    NoAttrValue { index: u32 },
    NoNodeName { index: u32 },
}

#[derive(Debug, PartialEq, BinRead, BinWrite, Copy, Clone)]
pub struct ResXMLTreeAttribute {
    /// Namespace of this attribute.
    pub ns: ResStringPoolRef,
    /// Name of this attribute.
    pub name: ResStringPoolRef,
    /// The original raw string value of this attribute.
    pub raw_value: ResStringPoolRef,
    /// Processed typed value of this attribute.
    pub typed_value: ResValue,
}

impl ResXMLTreeAttribute {
    pub fn write_string(&mut self, string: String, strings: &mut StringPool) {
        let string_pool_ref = strings.allocate(string);
        self.set_value(ResValue::new(ResValueType::String(string_pool_ref)));
    }

    pub fn write_bool(&mut self, value: bool) {
        self.set_value(ResValue::new(ResValueType::IntBoolean(value.into())));
    }

    pub fn set_value(&mut self, value: ResValue) {
        self.typed_value = value;

        match value.data {
            ResValueType::String(string_ref) => self.raw_value = string_ref,
            _ => self.raw_value = ResStringPoolRef::null(),
        };
    }
}

#[derive(Debug, BinRead, BinWrite, PartialEq, Clone)]
#[br(import(size: u32))]
pub struct RawXMLTree {
    #[br(parse_with=parse_chunks, args(size))]
    pub chunks: Vec<ResChunk>,
}

impl RawXMLTree {
    pub fn get_header_size() -> usize {
        0x8
    }

    pub fn read<R: Seek + Read>(reader: &mut R) -> Result<RawXMLTree, ReadAXMLError> {
        let header = ResChunk::read_le(reader).map_err(ReadAXMLError::ReadError)?;

        if let ResTypeValue::XML(xml) = header.data {
            return Ok(xml);
        }
        Err(ReadAXMLError::InvalidType((&header.data).into()))
    }

    pub fn write<W: Seek + Write>(self, writer: &mut W) -> Result<(), WriteAXMLError> {
        let header = ResChunk {
            data: ResTypeValue::XML(self),
        };

        Ok(header.write_le(writer)?)
    }
}

#[derive(Debug, Error)]
pub enum ReadAXMLError {
    #[error("an error occured while reading the axml {0}")]
    ReadError(binrw::Error),
    #[error("invalid type {0} expected xml")]
    InvalidType(ResType),
}

#[derive(Debug, Error)]
#[error("{0}")]
pub struct WriteAXMLError(pub binrw::Error);

impl From<binrw::Error> for WriteAXMLError {
    fn from(value: binrw::Error) -> Self {
        Self(value)
    }
}

impl TryFrom<RawXMLTree> for Vec<u8> {
    type Error = WriteAXMLError;
    fn try_from(value: RawXMLTree) -> Result<Self, Self::Error> {
        let mut stream = Cursor::new(Vec::new());

        value.write(&mut stream)?;

        Ok(stream.into_inner())
    }
}

#[derive(Debug)]
pub enum TreeToElementError {
    ReadError(binrw::Error),
    InvalidType(Box<ResTypeValue>),
    NoElements,
    InvalidNameSpace,
    NoStringPool,
    UnbalancedElements,
    NoRootElement,
    NodeToElementError(NodeToElementError),
}

impl From<NodeToElementError> for TreeToElementError {
    fn from(value: NodeToElementError) -> Self {
        Self::NodeToElementError(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct XMLNameSpace {
    pub prefix: ResStringPoolRef,
    pub uri: ResStringPoolRef,
}

#[derive(Debug, Clone)]
pub struct XMLTreeNode {
    pub element: ResXMLTreeAttrExt,
    pub children: Vec<XMLTreeNode>,
    pub namespace: Option<XMLNameSpace>,
}

#[derive(Debug, Clone)]
pub struct XMLTree {
    pub string_pool: StringPool,
    pub resource_map: Option<ResourceMap>,
    pub root: XMLTreeNode,
}

#[derive(Debug, Error)]
pub enum ReadXMLTreeError {
    #[error("{0}")]
    ReadData(ReadAXMLError),
    #[error("{0}")]
    ConvertRaw(XMLTreeParseError),
}

impl XMLTree {
    pub fn read<R: Read + Seek>(reader: &mut R) -> Result<XMLTree, ReadXMLTreeError> {
        let raw_xml: RawXMLTree = RawXMLTree::read(reader).map_err(ReadXMLTreeError::ReadData)?;

        raw_xml.try_into().map_err(ReadXMLTreeError::ConvertRaw)
    }

    pub fn write<W: Write + Seek>(self, writer: &mut W) -> Result<(), WriteAXMLError> {
        let raw_xml: RawXMLTree = self.into();

        raw_xml.write(writer)?;

        Ok(())
    }
}

impl TryFrom<&[u8]> for XMLTree {
    type Error = ReadXMLTreeError;
    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        let mut reader = Cursor::new(value);

        XMLTree::read(&mut reader)
    }
}

impl TryFrom<XMLTree> for Vec<u8> {
    type Error = WriteAXMLError;
    fn try_from(value: XMLTree) -> Result<Self, Self::Error> {
        let mut writer = Cursor::new(Vec::new());

        value.write(&mut writer)?;

        Ok(writer.into_inner())
    }
}

#[derive(Debug, Error)]
pub enum XMLTreeParseError {
    #[error("failed to find string pool in xml tree")]
    NoStringPool,
    #[error("no root element found")]
    NoRoot,
    #[error("too many EndNamespace chunks")]
    TooManyEndNamespaces,
    #[error("too few EndNamespace chunks")]
    TooFewEndNamespaces,
    #[error("too many EndElement chunks")]
    TooManyEndElements,
    #[error("too few EndElement chunks")]
    TooFewEndElements,
    #[error("unrecognised chunk type: {0}")]
    UnrecognisedChunk(ResType),
    #[error("multiple root nodes found")]
    MultipleRootNodes,
}

impl XMLTreeNode {
    pub fn write_chunks(self, chunks: &mut Vec<ResChunk>) {
        let node = self.element.node;
        let ns = self.element.ns;
        let name = self.element.name;

        let parent_ns = self.namespace;

        chunks.push(ResChunk {
            data: ResTypeValue::StartElement(self.element),
        });

        for child in self.children {
            match child.namespace {
                None => match parent_ns {
                    None => (),
                    Some(parent_ns) => chunks.push(ResChunk {
                        data: ResTypeValue::EndNameSpace(ResXMLTreeNameSpaceExt {
                            node,
                            prefix: parent_ns.prefix,
                            uri: parent_ns.uri,
                        }),
                    }),
                },
                Some(child_ns) => {
                    if parent_ns.is_none_or(|pns| pns != child_ns) {
                        chunks.push(ResChunk {
                            data: ResTypeValue::StartNameSpace(ResXMLTreeNameSpaceExt {
                                node: child.element.node,
                                prefix: child_ns.prefix,
                                uri: child_ns.uri,
                            }),
                        })
                    }
                }
            }
            child.write_chunks(chunks);
        }

        chunks.push(ResChunk {
            data: ResTypeValue::EndElement(ResXMLTreeEndElementExt { node, ns, name }),
        });
    }
}

impl From<XMLTree> for RawXMLTree {
    fn from(value: XMLTree) -> Self {
        let mut chunks: Vec<ResChunk> = Vec::new();

        chunks.push(value.string_pool.into());

        if let Some(rn) = value.resource_map {
            chunks.push(rn.into())
        }

        let ns_ext = if let Some(ns) = value.root.namespace {
            Some(ResXMLTreeNameSpaceExt {
                node: value.root.element.node,
                prefix: ns.prefix,
                uri: ns.uri,
            })
        } else {
            None
        };
        if let Some(ns) = ns_ext {
            chunks.push(ResChunk {
                data: ResTypeValue::StartNameSpace(ns),
            });
        }

        value.root.write_chunks(&mut chunks);

        if let Some(ns) = ns_ext {
            chunks.push(ResChunk {
                data: ResTypeValue::EndNameSpace(ns),
            });
        }

        Self { chunks }
    }
}

impl XMLTreeNode {
    pub fn find_element<'a>(&'a self, name: &str, strings: &StringPool) -> Option<&'a XMLTreeNode> {
        if self.element.name.resolve(strings) == Some(name) {
            return Some(self);
        }
        for child in &self.children {
            if let Some(el) = child.find_element(name, strings) {
                return Some(el);
            }
        }
        None
    }

    pub fn get_child<'a>(&'a self, name: &str, strings: &StringPool) -> Option<&'a XMLTreeNode> {
        self.children
            .iter()
            .find(|c| c.element.name.resolve(strings) == Some(name))
    }

    pub fn get_elements_mut<'a>(
        &'a mut self,
        path: &[&str],
        strings: &StringPool,
    ) -> Vec<&'a mut XMLTreeNode> {
        let mut elements: Vec<&mut XMLTreeNode> = Vec::new();

        let mut stack: Vec<(usize, &mut XMLTreeNode)> = Vec::new();

        stack.push((0, self));

        while let Some((index, element)) = stack.pop() {
            let item = path.get(index);

            if item.is_none() {
                continue;
            }

            if item == element.element.name.resolve(strings).as_ref() {
                if index == path.len() - 1 {
                    elements.push(element);
                    continue;
                }
                for child in element.children.iter_mut() {
                    stack.push((index + 1, child));
                }
            }
        }

        elements
    }

    pub fn get_elements<'a>(&'a self, path: &[&str], strings: &StringPool) -> Vec<&'a XMLTreeNode> {
        let mut elements: Vec<&XMLTreeNode> = Vec::new();

        let mut stack: Vec<(usize, &XMLTreeNode)> = Vec::new();

        stack.push((0, self));

        while let Some((index, element)) = stack.pop() {
            let item = path.get(index);

            if item.is_none() {
                continue;
            }

            if item == element.element.name.resolve(strings).as_ref() {
                if index == path.len() - 1 {
                    elements.push(element);
                    continue;
                }
                for child in &element.children {
                    stack.push((index + 1, child));
                }
            }
        }

        elements
    }

    pub fn get_attribute<'a>(
        &'a self,
        name: &str,
        strings: &StringPool,
    ) -> Option<&'a ResXMLTreeAttribute> {
        self.element
            .attributes
            .iter()
            .find(|attr| attr.name.resolve(strings) == Some(name))
    }

    pub fn get_attribute_mut<'a>(
        &'a mut self,
        name: &str,
        strings: &StringPool,
    ) -> Option<&'a mut ResXMLTreeAttribute> {
        self.element
            .attributes
            .iter_mut()
            .find(|attr| attr.name.resolve(strings) == Some(name))
    }

    pub fn set_attribute(
        &mut self,
        name: &str,
        value: ResValue,
        strings: &StringPool,
    ) -> Option<&mut ResXMLTreeAttribute> {
        let attr = self
            .element
            .attributes
            .iter_mut()
            .find(|attr| attr.name.resolve(strings) == Some(name));

        if let Some(attr) = attr {
            attr.set_value(value);
            return Some(attr);
        }
        None
    }
}

impl TryFrom<RawXMLTree> for XMLTree {
    type Error = XMLTreeParseError;
    fn try_from(value: RawXMLTree) -> Result<Self, Self::Error> {
        let mut string_pool: Option<StringPool> = None;
        let mut resource_map: Option<ResourceMap> = None;

        let mut root: Option<XMLTreeNode> = None;
        let mut stack: Vec<XMLTreeNode> = Vec::new();

        let mut namespace_stack: Vec<XMLNameSpace> = Vec::new();

        for chunk in value.chunks {
            match chunk.data {
                ResTypeValue::StringPool(sp) => string_pool = Some(sp),
                ResTypeValue::StartElement(start_element) => {
                    let el = XMLTreeNode {
                        namespace: namespace_stack.last().copied(),
                        element: start_element,
                        children: Vec::new(),
                    };
                    stack.push(el);
                }
                ResTypeValue::EndElement(_) => {
                    let el = stack.pop().ok_or(XMLTreeParseError::TooManyEndElements)?;
                    if let Some(parent) = stack.last_mut() {
                        parent.children.push(el)
                    } else {
                        if root.is_some() {
                            return Err(XMLTreeParseError::MultipleRootNodes);
                        }
                        root = Some(el);
                    }
                }
                ResTypeValue::StartNameSpace(start_namespace) => {
                    namespace_stack.push(XMLNameSpace {
                        prefix: start_namespace.prefix,
                        uri: start_namespace.uri,
                    })
                }
                ResTypeValue::EndNameSpace(_) => {
                    _ = namespace_stack
                        .pop()
                        .ok_or(XMLTreeParseError::TooManyEndNamespaces)?
                }
                ResTypeValue::ResourceMap(rm) => resource_map = Some(rm),
                v => return Err(XMLTreeParseError::UnrecognisedChunk((&v).into())),
            }
        }

        if !stack.is_empty() {
            return Err(XMLTreeParseError::TooFewEndElements);
        }

        if !namespace_stack.is_empty() {
            return Err(XMLTreeParseError::TooFewEndNamespaces);
        }

        Ok(Self {
            string_pool: string_pool.ok_or(XMLTreeParseError::NoStringPool)?,
            resource_map,
            root: root.ok_or(XMLTreeParseError::NoRoot)?,
        })
    }
}
