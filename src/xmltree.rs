use std::{
    fmt::Display,
    io::{Cursor, Read, Seek, Write},
};

use crate::{
    defs::{HeaderSizeStatic, ResChunk, ResType, ResTypeValue, ResourceMap},
    res_value::{ResValue, ResValueType},
    stream::{
        NewResultCtx, Readable, ReadableNoOptions, StreamError, StreamResult, VecReadable,
        VecWritable, Writeable, WriteableNoOptions,
    },
    string_pool::{ResStringPoolRef, StringPool, StringPoolHandler},
};

/// Basic XML tree node. A single item in the XML document. Extended info about the node can be
/// found after header.headerSize.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ResXMLTreeNode {
    /// Line number in original source file at which this element appeared.
    pub line_number: u32,
    /// Optional XML comment that was associated with the element; -1 if none.
    pub comment: ResStringPoolRef,
}

impl Readable for ResXMLTreeNode {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            line_number: u32::read_no_opts(reader)
                .add_context(|| "read line_number for ResXMLTreeNode")?,
            comment: ResStringPoolRef::read_no_opts(reader)
                .add_context(|| "read comment for ResXMLTreeNode")?,
        })
    }
}

impl Writeable for ResXMLTreeNode {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.line_number
            .write_no_opts(writer)
            .add_context(|| "write line_number for ResXMLTreeNode")?;
        self.comment
            .write_no_opts(writer)
            .add_context(|| "write comment for ResXMLTreeNode")?;
        Ok(())
    }
}

impl HeaderSizeStatic for ResXMLTreeNode {
    fn header_size() -> usize {
        8
    }
}

/// Extended XML tree node for CDATA tags -- includes the CDATA string. Appears header.headerSize
/// bytes after a ResXMLTree_node
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ResXMLTreeCDataExt {
    pub node: ResXMLTreeNode,
    /// The raw CDATA character data.
    pub data: ResStringPoolRef,
    /// The typed value of the character data if this is a CDATA node.
    pub typed_data: ResValue,
}

impl Readable for ResXMLTreeCDataExt {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            node: ResXMLTreeNode::read_no_opts(reader)
                .add_context(|| "read node for ResXMLTreeCDataExt")?,
            data: ResStringPoolRef::read_no_opts(reader)
                .add_context(|| "read data for ResXMLTreeCDataExt")?,
            typed_data: ResValue::read_no_opts(reader)
                .add_context(|| "read typed_data for ResXMLTreeCDataExt")?,
        })
    }
}

impl Writeable for ResXMLTreeCDataExt {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.node
            .write_no_opts(writer)
            .add_context(|| "write node for ResXMLTreeCDataExt")?;
        self.data
            .write_no_opts(writer)
            .add_context(|| "write data for ResXMLTreeCDataExt")?;
        self.typed_data
            .write_no_opts(writer)
            .add_context(|| "write typed_data for ResXMLTreeCDataExt")?;

        Ok(())
    }
}

impl HeaderSizeStatic for ResXMLTreeCDataExt {
    fn header_size() -> usize {
        ResXMLTreeNode::header_size()
    }
}

/// Extended XML tree node for namespace start / end nodes. Appears header.headerSize bytes after a
/// ResXMLTree_node.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct ResXMLTreeNameSpaceExt {
    pub node: ResXMLTreeNode,
    /// The prefix of the namespace.
    pub prefix: ResStringPoolRef,
    /// The URI of the namespace.
    pub uri: ResStringPoolRef,
}

impl Readable for ResXMLTreeNameSpaceExt {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            node: ResXMLTreeNode::read_no_opts(reader)
                .add_context(|| "read node for ResXMLTreeNameSpaceExt")?,
            prefix: ResStringPoolRef::read_no_opts(reader)
                .add_context(|| "read prefix for ResXMLTreeNameSpaceExt")?,
            uri: ResStringPoolRef::read_no_opts(reader)
                .add_context(|| "read uri for ResXMLTreeNameSpaceExt")?,
        })
    }
}

impl Writeable for ResXMLTreeNameSpaceExt {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.node
            .write_no_opts(writer)
            .add_context(|| "write node for ResXMLTreeNameSpaceExt")?;
        self.prefix
            .write_no_opts(writer)
            .add_context(|| "write prefix for ResXMLTreeNameSpaceExt")?;
        self.uri
            .write_no_opts(writer)
            .add_context(|| "write uri ResXMLTreeNameSpaceExt")?;

        Ok(())
    }
}

impl HeaderSizeStatic for ResXMLTreeNameSpaceExt {
    fn header_size() -> usize {
        ResXMLTreeNode::header_size()
    }
}

/// Extended XML tree node for element start / end nodes. Appears header.headerSize bytes after a
/// ResXMLTree_node.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct ResXMLTreeEndElementExt {
    pub node: ResXMLTreeNode,
    /// String of the full namespace of this element.
    pub ns: ResStringPoolRef,
    /// String name of this node if it is an ELEMENT; the raw character data if this is a CDATA
    /// node.
    pub name: ResStringPoolRef,
}

impl Readable for ResXMLTreeEndElementExt {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            node: ResXMLTreeNode::read_no_opts(reader)
                .add_context(|| "read node for ResXMLTreeEndElementExt")?,
            ns: ResStringPoolRef::read_no_opts(reader)
                .add_context(|| "read ns for ResXMLTreeEndElementExt")?,
            name: ResStringPoolRef::read_no_opts(reader)
                .add_context(|| "read name for ResXMLTreeEndElementExt")?,
        })
    }
}

impl Writeable for ResXMLTreeEndElementExt {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.node
            .write_no_opts(writer)
            .add_context(|| "write node for ResXMLTreeEndElementExt")?;
        self.ns
            .write_no_opts(writer)
            .add_context(|| "write ns for ResXMLTreeEndElementExt")?;
        self.name
            .write_no_opts(writer)
            .add_context(|| "write name for ResXMLTreeEndElementExt")?;

        Ok(())
    }
}

impl HeaderSizeStatic for ResXMLTreeEndElementExt {
    fn header_size() -> usize {
        ResXMLTreeNode::header_size()
    }
}
/// Extended XML tree node for start tags -- includes attribute information.
/// Appears header.headerSize bytes after a ResXMLTree_node.
#[derive(Debug, PartialEq, Clone)]
pub struct ResXMLTreeAttrExt {
    pub node: ResXMLTreeNode,

    /// String of the full namespace of this element.
    pub ns: ResStringPoolRef,

    /// String name of this node if it is an ELEMENT; the raw character data if this is a CDATA
    /// node.
    pub name: ResStringPoolRef,

    /// Index (1-based) of the "id" attribute. 0 if none.
    pub id_index: u16,
    /// Index (1-based) of the "class" attribute. 0 if none.
    pub class_index: u16,
    /// Index (1-based) of the "style" attribute. 0 if none.
    pub style_index: u16,

    pub attributes: Vec<ResXMLTreeAttribute>,
}

impl Writeable for ResXMLTreeAttrExt {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.node
            .write_no_opts(writer)
            .add_context(|| "write node for ResXMLTreeAttrExt")?;

        self.ns
            .write_no_opts(writer)
            .add_context(|| "write ns for ResXMLTreeAttrExt")?;
        self.name
            .write_no_opts(writer)
            .add_context(|| "write name for ResXMLTreeAttrExt")?;

        let attribute_start: u16 = 20;
        attribute_start
            .write_no_opts(writer)
            .add_context(|| "write attribute_start for ResXMLTreeAttrExt")?;

        let attribute_size: u16 = 20;
        attribute_size
            .write_no_opts(writer)
            .add_context(|| "write attribute_size for ResXMLTreeAttrExt")?;

        let attribute_count: u16 = self.attributes.len() as u16;
        attribute_count
            .write_no_opts(writer)
            .add_context(|| "write attribute_count for ResXMLTreeAttrExt")?;

        self.id_index
            .write_no_opts(writer)
            .add_context(|| "write id_index for ResXMLTeeAttrExt")?;
        self.class_index
            .write_no_opts(writer)
            .add_context(|| "write class_index for ResXMLTreeAttrExt")?;
        self.style_index
            .write_no_opts(writer)
            .add_context(|| "write style_index for ResXMLTreeAttrExt")?;

        self.attributes
            .write_vec(writer)
            .add_context(|| "write attributes for ResXMLTreeAttrExt")?;

        Ok(())
    }
}

impl Readable for ResXMLTreeAttrExt {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let node = ResXMLTreeNode::read_no_opts(reader)
            .add_context(|| "read node for ResXMLTreeAttrExt")?;
        let start_pos = reader.stream_position()?;

        let ns = ResStringPoolRef::read_no_opts(reader)
            .add_context(|| "read ns for ResXMLTreeAttrExt")?;
        let name = ResStringPoolRef::read_no_opts(reader)
            .add_context(|| "read name for ResXMLTreeAttrExt")?;

        let attribute_start = u16::read_no_opts(reader)
            .add_context(|| "read attribute_start for ResXMLTreeAttrExt")?;

        let attribute_size = u16::read_no_opts(reader)
            .add_context(|| "read attribute_size for ResXMLTreeAttrExt")?;

        if attribute_size != 20 {
            return Err(StreamError::new_string_context(
                format!("invalid attribute_size: {}, expected 20", attribute_size),
                reader.stream_position()?,
                "validate attribute size for ResXMLTreeAttrExt",
            ));
        }

        let attribute_count = u16::read_no_opts(reader)
            .add_context(|| "read attribute_count for ResXMLTreeAttrExt")?;

        let id_index =
            u16::read_no_opts(reader).add_context(|| "read id_index for ResXMLTreeAttrExt")?;
        let class_index =
            u16::read_no_opts(reader).add_context(|| "read class_index for ResXMLTreeAttrExt")?;
        let style_index =
            u16::read_no_opts(reader).add_context(|| "read style_index for ResXMLTreeAttrExt")?;

        reader.seek(std::io::SeekFrom::Start(start_pos + attribute_start as u64))?;

        let attributes = <Vec<ResXMLTreeAttribute>>::read_vec(reader, attribute_count as usize)
            .add_context(|| "read attributes for ResXMLTreeAttrExt")?;

        Ok(Self {
            node,
            ns,
            name,
            id_index,
            class_index,
            style_index,
            attributes,
        })
    }
}

impl HeaderSizeStatic for ResXMLTreeAttrExt {
    fn header_size() -> usize {
        ResXMLTreeNode::header_size()
    }
}

#[derive(Debug)]
pub enum NodeToElementError {
    NoAttrName { index: u32 },
    NoAttrValue { index: u32 },
    NoNodeName { index: u32 },
}

#[derive(Debug, PartialEq, Copy, Clone)]
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

impl Readable for ResXMLTreeAttribute {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            ns: ResStringPoolRef::read_no_opts(reader)
                .add_context(|| "read ns for ResXMLTreeAttribute")?,
            name: ResStringPoolRef::read_no_opts(reader)
                .add_context(|| "read name for ResXMLTreeAttribute")?,
            raw_value: ResStringPoolRef::read_no_opts(reader)
                .add_context(|| "read raw_value for ResXMLTreeAttribute")?,
            typed_value: ResValue::read_no_opts(reader)
                .add_context(|| "read typed_value for ResXMLTreeAttribute")?,
        })
    }
}

impl Writeable for ResXMLTreeAttribute {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.ns
            .write_no_opts(writer)
            .add_context(|| "write ns for ResXMLTreeAttribute")?;
        self.name
            .write_no_opts(writer)
            .add_context(|| "write name for ResXMLTreeAttribute")?;
        self.raw_value
            .write_no_opts(writer)
            .add_context(|| "write raw_value for ResXMLTreeAttribute")?;
        self.typed_value
            .write_no_opts(writer)
            .add_context(|| "write typed_value for ResXMLTreeAttribute")
    }
}

impl ResXMLTreeAttribute {
    pub fn write_string(&mut self, string: String, strings: &mut StringPoolHandler) {
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

#[derive(Debug, PartialEq, Clone)]
pub struct RawXMLTree {
    pub chunks: Vec<ResChunk>,
}

impl Readable for RawXMLTree {
    type Args = u64;
    fn read<R: Read + Seek>(reader: &mut R, args: Self::Args) -> StreamResult<Self> {
        Ok(Self {
            chunks: <Vec<ResChunk>>::read(reader, args)
                .add_context(|| "read chunks for RawXMLTree")?,
        })
    }
}

impl Writeable for RawXMLTree {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        self.chunks
            .write_vec(writer)
            .add_context(|| "write chunks for RawXMLTree")
    }
}

impl HeaderSizeStatic for RawXMLTree {
    fn header_size() -> usize {
        0
    }
}

impl RawXMLTree {
    pub fn read_full<R: Seek + Read>(reader: &mut R) -> StreamResult<RawXMLTree> {
        let pos = reader.stream_position()?;
        let header = ResChunk::read_no_opts(reader).add_context(|| "read chunk for RawXMLTree")?;

        if let ResTypeValue::XML(xml) = header.data {
            return Ok(xml);
        }
        let res_type: ResType = (&header.data).into();
        Err(StreamError::new_string_context(
            format!("invalid res_type: {}, expected XML", res_type),
            pos,
            "validate read chunk for RawXMLTree",
        ))
    }

    pub fn write_full<W: Seek + Write>(self, writer: &mut W) -> StreamResult<()> {
        let header = ResChunk {
            data: ResTypeValue::XML(self),
        };

        header
            .write_no_opts(writer)
            .add_context(|| "write chunk for RawXMLTree")
    }
}

#[derive(Debug)]
pub enum ReadAXMLError {
    ReadError(std::io::Error),
    InvalidType(ResType),
}

impl Display for ReadAXMLError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ReadError(e) => write!(f, "failed to read res xml tree: {}", e),
            Self::InvalidType(t) => write!(f, "invalid type: {} expected XML Tree", t),
        }
    }
}

impl TryFrom<RawXMLTree> for Vec<u8> {
    type Error = StreamError;
    fn try_from(value: RawXMLTree) -> Result<Self, Self::Error> {
        let mut stream = Cursor::new(Vec::new());

        value.write_full(&mut stream)?;

        Ok(stream.into_inner())
    }
}

#[derive(Debug)]
pub enum TreeToElementError {
    ReadError(std::io::Error),
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
    pub string_pool: StringPoolHandler,
    pub resource_map: Option<ResourceMap>,
    pub root: XMLTreeNode,
}

#[derive(Debug)]
pub enum ReadXMLTreeError {
    ReadData(StreamError),
    ConvertRaw(XMLTreeParseError),
}

impl Display for ReadXMLTreeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ReadData(r) => r.fmt(f),
            Self::ConvertRaw(c) => c.fmt(f),
        }
    }
}

impl XMLTree {
    pub fn read<R: Read + Seek>(reader: &mut R) -> Result<XMLTree, ReadXMLTreeError> {
        let raw_xml: RawXMLTree =
            RawXMLTree::read_full(reader).map_err(ReadXMLTreeError::ReadData)?;

        raw_xml.try_into().map_err(ReadXMLTreeError::ConvertRaw)
    }

    pub fn write<W: Write + Seek>(self, writer: &mut W) -> Result<(), StreamError> {
        let raw_xml: RawXMLTree = self.into();

        raw_xml.write_full(writer)?;

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
    type Error = StreamError;
    fn try_from(value: XMLTree) -> Result<Self, Self::Error> {
        let mut writer = Cursor::new(Vec::new());

        value.write(&mut writer)?;

        Ok(writer.into_inner())
    }
}

#[derive(Debug)]
pub enum XMLTreeParseError {
    NoStringPool,
    NoRoot,
    TooManyEndNamespaces,
    TooFewEndNamespaces,
    TooManyEndElements,
    TooFewEndElements,
    UnrecognisedChunk(ResType),
    MultipleRootNodes,
}

impl Display for XMLTreeParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::NoStringPool => "failed to find string pool in xml tree".to_string(),
            Self::NoRoot => "no root element found in xml tree".to_string(),
            Self::TooManyEndNamespaces => "too many end namespace chunks in xml tree".to_string(),
            Self::TooFewEndNamespaces => "too few end namespace chunks in xml tree".to_string(),
            Self::TooManyEndElements => "too many end element chunks in xml tree".to_string(),
            Self::TooFewEndElements => "too few end element chunks in xml tree".to_string(),
            Self::UnrecognisedChunk(t) => format!("unrecognised chunk type: {} in xml tree", t),
            Self::MultipleRootNodes => "multiple root nodes found in xml tree".to_string(),
        };
        write!(f, "{}", str)
    }
}

impl XMLTreeNode {
    pub fn write_chunks(self, chunks: &mut Vec<ResChunk>) {
        let node = self.element.node;
        let ns = self.element.ns;
        let name = self.element.name;

        let parent_ns = self.namespace;

        chunks.push(ResChunk {
            data: ResTypeValue::XMLStartElement(self.element),
        });

        for child in self.children {
            match child.namespace {
                None => match parent_ns {
                    None => (),
                    Some(parent_ns) => chunks.push(ResChunk {
                        data: ResTypeValue::XMLEndNameSpace(ResXMLTreeNameSpaceExt {
                            node,
                            prefix: parent_ns.prefix,
                            uri: parent_ns.uri,
                        }),
                    }),
                },
                Some(child_ns) => {
                    if parent_ns.is_none_or(|pns| pns != child_ns) {
                        chunks.push(ResChunk {
                            data: ResTypeValue::XMLStartNameSpace(ResXMLTreeNameSpaceExt {
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
            data: ResTypeValue::XMLEndElement(ResXMLTreeEndElementExt { node, ns, name }),
        });
    }
}

impl From<XMLTree> for RawXMLTree {
    fn from(value: XMLTree) -> Self {
        let mut chunks: Vec<ResChunk> = Vec::new();

        chunks.push(value.string_pool.string_pool.into());

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
                data: ResTypeValue::XMLStartNameSpace(ns),
            });
        }

        value.root.write_chunks(&mut chunks);

        if let Some(ns) = ns_ext {
            chunks.push(ResChunk {
                data: ResTypeValue::XMLEndNameSpace(ns),
            });
        }

        Self { chunks }
    }
}

impl XMLTreeNode {
    pub fn find_element<'a>(
        &'a self,
        name: &str,
        strings: &StringPoolHandler,
    ) -> Option<&'a XMLTreeNode> {
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

    pub fn get_child<'a>(
        &'a self,
        name: &str,
        strings: &StringPoolHandler,
    ) -> Option<&'a XMLTreeNode> {
        self.children
            .iter()
            .find(|c| c.element.name.resolve(strings) == Some(name))
    }

    pub fn get_elements_mut<'a>(
        &'a mut self,
        path: &[&str],
        strings: &StringPoolHandler,
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

    pub fn get_elements<'a>(
        &'a self,
        path: &[&str],
        strings: &StringPoolHandler,
    ) -> Vec<&'a XMLTreeNode> {
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
        strings: &StringPoolHandler,
    ) -> Option<&'a ResXMLTreeAttribute> {
        self.element
            .attributes
            .iter()
            .find(|attr| attr.name.resolve(strings) == Some(name))
    }

    pub fn get_attribute_mut<'a>(
        &'a mut self,
        name: &str,
        strings: &StringPoolHandler,
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
        strings: &StringPoolHandler,
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
                ResTypeValue::XMLStartElement(start_element) => {
                    let el = XMLTreeNode {
                        namespace: namespace_stack.last().copied(),
                        element: start_element,
                        children: Vec::new(),
                    };
                    stack.push(el);
                }
                ResTypeValue::XMLEndElement(_) => {
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
                ResTypeValue::XMLStartNameSpace(start_namespace) => {
                    namespace_stack.push(XMLNameSpace {
                        prefix: start_namespace.prefix,
                        uri: start_namespace.uri,
                    })
                }
                ResTypeValue::XMLEndNameSpace(_) => {
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
            string_pool: string_pool.ok_or(XMLTreeParseError::NoStringPool)?.into(),
            resource_map,
            root: root.ok_or(XMLTreeParseError::NoRoot)?,
        })
    }
}
