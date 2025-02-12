use std::collections::HashMap;

use binrw::{binrw, BinRead, BinResult, BinWrite};
use xmltree::Element;

use crate::{
    defs::{ResChunk_header, ResType, ResTypeValue},
    res_value::{ResValueType, Res_value},
    string_pool::{ResStringPool_ref, StringPool},
};

pub struct ResXMLTree {
    pub nodes: Vec<ResXMLTree_node>,
}

/// Basic XML tree node. A single item in the XML document. Extended info about the node can be
/// found after header.headerSize.
#[derive(Debug, PartialEq, BinRead, BinWrite, Clone, Copy)]
pub struct ResXMLTree_node {
    /// Line number in original source file at which this element appeared.
    pub lineNumber: u32,
    /// Optional XML comment that was associated with the element; -1 if none.
    pub comment: ResStringPool_ref,
}

/// Extended XML tree node for CDATA tags -- includes the CDATA string. Appears header.headerSize
/// bytes after a ResXMLTree_node
#[derive(Debug, PartialEq, BinRead, BinWrite, Clone, Copy)]
pub struct ResXMLTree_cdataExt {
    pub node: ResXMLTree_node,
    /// The raw CDATA character data.
    pub data: ResStringPool_ref,
    /// The typed value of the character data if this is a CDATA node.
    pub typedData: Res_value,
}

/// Extended XML tree node for namespace start / end nodes. Appears header.headerSize bytes after a
/// ResXMLTree_node.
#[derive(Debug, PartialEq, BinRead, BinWrite, Copy, Clone)]
pub struct ResXMLTree_namespaceExt {
    pub node: ResXMLTree_node,
    /// The prefix of the namespace.
    pub prefix: ResStringPool_ref,
    /// The URI of the namespace.
    pub uri: ResStringPool_ref,
}

impl ResXMLTree_namespaceExt {
    pub fn get_header_size() -> usize {
        0x10
    }
}

/// Extended XML tree node for element start / end nodes. Appears header.headerSize bytes after a
/// ResXMLTree_node.
#[derive(Debug, PartialEq, BinRead, BinWrite, Copy, Clone)]
pub struct ResXMLTree_endElementExt {
    pub node: ResXMLTree_node,
    /// String of the full namespace of this element.
    pub ns: ResStringPool_ref,
    /// String name of this node if it is an ELEMENT; the raw character data if this is a CDATA
    /// node.
    pub name: ResStringPool_ref,
}
impl ResXMLTree_endElementExt {
    pub fn get_header_size() -> usize {
        0x10
    }
}
/// Extended XML tree node for start tags -- includes attribute information.
/// Appears header.headerSize bytes after a ResXMLTree_node.
#[binrw]
#[brw(stream = s)]
#[derive(Debug, PartialEq, Clone)]
pub struct ResXMLTree_attrExt {
    pub node: ResXMLTree_node,

    #[br(temp)]
    #[brw(try_calc=s.stream_position())]
    #[bw(restore_position)]
    start_offset: u64,

    /// String of the full namespace of this element.
    pub ns: ResStringPool_ref,

    /// String name of this node if it is an ELEMENT; the raw character data if this is a CDATA
    /// node.
    pub name: ResStringPool_ref,

    /// Byte offset from the start of this structure where the attributes start.
    #[br(temp)]
    #[bw(calc=4 + 4 + 2 + 2 + 2 + 2 + 2 + 2)]
    attributeStart: u16,

    /// Size of the ResXMLTree_attribute structures that follow.
    #[bw(calc = 20)]
    #[br(temp)]
    #[br(assert(attributeSize==20))]
    attributeSize: u16,

    /// Number of attributes associated with an ELEMENT. These are availiable as an array of
    /// ResXMLTree_attribute structures immediately following this node.
    #[br(temp)]
    #[bw(calc=attributes.len() as u16)]
    attributeCount: u16,

    /// Index (1-based) of the "id" attribute. 0 if none.
    pub idIndex: u16,
    /// Index (1-based) of the "class" attribute. 0 if none.
    pub classIndex: u16,
    /// Index (1-based) of the "style" attribute. 0 if none.
    pub styleIndex: u16,

    #[br(count=attributeCount)]
    #[brw(seek_before=std::io::SeekFrom::Start(start_offset + (attributeStart as u64)))]
    pub attributes: Vec<ResXMLTree_attribute>,
}

impl ResXMLTree_attrExt {
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

impl ResXMLTree_attrExt {
    pub fn to_element(
        self: &ResXMLTree_attrExt,
        strings: &[String],
    ) -> Result<Element, NodeToElementError> {
        let mut attributes: HashMap<String, String> = HashMap::new();
        for attr in &self.attributes {
            attributes.insert(
                attr.name
                    .resolve(strings)
                    .ok_or(NodeToElementError::NoAttrName {
                        index: attr.name.index,
                    })?,
                attr.rawValue
                    .resolve(strings)
                    .or(attr.typedValue.data.resolve(strings))
                    .ok_or(NodeToElementError::NoAttrValue {
                        index: attr.rawValue.index,
                    })?,
            );
        }
        Ok(Element {
            name: self
                .name
                .resolve(strings)
                .ok_or(NodeToElementError::NoNodeName {
                    index: self.name.index,
                })?,
            namespace: self.ns.resolve(strings),
            prefix: None,
            namespaces: None,
            attributes,
            children: Vec::new(),
        })
    }

    pub fn from_element(element: &Element, string_pool: &mut StringPool) -> ResXMLTree_attrExt {
        let mut attributes: Vec<ResXMLTree_attribute> = Vec::new();
        let ns = if let Some(nss) = &element.namespace {
            string_pool.allocate(nss.to_string())
        } else {
            ResStringPool_ref::null()
        };

        for (key, value) in &element.attributes {
            attributes.push(ResXMLTree_attribute {
                name: string_pool.allocate(key.to_string()),
                rawValue: ResStringPool_ref::null(),
                typedValue: Res_value {
                    data: ResValueType::unresolve(value, string_pool),
                },
                ns,
            });
        }
        Self {
            attributes,
            name: string_pool.allocate(element.name.to_string()),
            node: ResXMLTree_node {
                comment: ResStringPool_ref::null(),
                lineNumber: 0,
            },
            ns,
            classIndex: 0, // TODO: finish this
            idIndex: 0,
            styleIndex: 0,
        }
    }
}

#[derive(Debug, PartialEq, BinRead, BinWrite, Copy, Clone)]
pub struct ResXMLTree_attribute {
    /// Namespace of this attribute.
    pub ns: ResStringPool_ref,
    /// Name of this attribute.
    pub name: ResStringPool_ref,
    /// The original raw string value of this attribute.
    pub rawValue: ResStringPool_ref,
    /// Processed typed value of this attribute.
    pub typedValue: Res_value,
}

#[derive(Debug, BinRead, BinWrite, PartialEq, Clone)]
#[br(import(size: u32))]
pub struct RawXMLTree {
    #[br(parse_with=parse_chunks, args(size))]
    pub chunks: Vec<ResChunk_header>,
}

impl RawXMLTree {
    pub fn get_header_size() -> usize {
        return 0x8;
    }
}

#[binrw::parser(reader, endian)]
pub fn parse_chunks(size: u32) -> BinResult<Vec<ResChunk_header>> {
    let start_pos = ResChunk_header::get_header_offset(reader.stream_position()?);

    let mut chunks: Vec<ResChunk_header> = Vec::new();

    while reader.stream_position()? - start_pos < (size as u64) {
        let chunk: ResChunk_header = ResChunk_header::read_options(reader, endian, ())?;
        chunks.push(chunk);
    }

    Ok(chunks)
}
#[derive(Debug)]
pub enum TreeToElementError {
    NoElements,
    NoStringPool,
    UnbalancedElements,
    NoRootElement,
    NodeToElementError(NodeToElementError),
}

#[derive(Debug)]
pub enum ElementToTreeError {
    UnbalancedElements,
}

impl From<NodeToElementError> for TreeToElementError {
    fn from(value: NodeToElementError) -> Self {
        Self::NodeToElementError(value)
    }
}

fn process_node(element: &Element, chunks: &mut Vec<ResChunk_header>, str_pool: &mut StringPool) {
    let el = ResXMLTree_attrExt::from_element(element, str_pool);
    let node = el.node;
    let ns = el.ns;
    let name = el.name;
    let start_element = ResTypeValue::START_ELEMENT(el);

    chunks.push(start_element.into());

    for child in &element.children {
        if let xmltree::XMLNode::Element(ref child_element) = child {
            process_node(child_element, chunks, str_pool);
        }
    }

    let end_element = ResTypeValue::END_ELEMENT(ResXMLTree_endElementExt { node, ns, name });

    chunks.push(end_element.into());
}

impl TryFrom<Element> for RawXMLTree {
    type Error = ElementToTreeError;
    fn try_from(value: Element) -> Result<Self, Self::Error> {
        let mut str_pool = StringPool::default();
        let mut chunks: Vec<ResChunk_header> = Vec::new();

        process_node(&value, &mut chunks, &mut str_pool);

        chunks.insert(0, ResTypeValue::STRING_POOL(str_pool).into());

        Ok(Self { chunks })
    }
}

impl TryFrom<RawXMLTree> for xmltree::Element {
    type Error = TreeToElementError;

    fn try_from(value: RawXMLTree) -> Result<Self, Self::Error> {
        let first_chunk = value.chunks.first().ok_or(TreeToElementError::NoElements)?;

        let str_pool = if let ResTypeValue::STRING_POOL(sp) = &first_chunk.data {
            sp
        } else {
            return Err(TreeToElementError::NoStringPool);
        };

        let second_chunk = value.chunks.get(1).ok_or(TreeToElementError::NoElements)?;
        let (resource_map, skip) = if let ResTypeValue::RESOURCE_MAP(mp) = &second_chunk.data {
            (Some(mp), 2)
        } else {
            (None, 1)
        };

        // Find the string pool in the chunks
        let strings = str_pool.get_strings();

        let mut stack: Vec<xmltree::Element> = Vec::new();

        // skip first string pool and possible resource map
        for chunk in value.chunks.into_iter().skip(skip) {
            match chunk.data {
                ResTypeValue::START_ELEMENT(node) => {
                    // Convert the start element node to an xmltree::Element
                    let el = node.to_element(&strings)?;
                    stack.push(el);
                }
                ResTypeValue::END_ELEMENT(_) => {
                    // Pop the top element and attach it to its parent if exists
                    if let Some(el) = stack.pop() {
                        if let Some(parent) = stack.last_mut() {
                            parent.children.push(xmltree::XMLNode::Element(el));
                        } else {
                            // This is the root element; return it
                            return Ok(el);
                        }
                    } else {
                        return Err(TreeToElementError::UnbalancedElements);
                    }
                }
                ResTypeValue::START_NAMESPACE(_) => {}
                ResTypeValue::END_NAMESPACE(_) => {}
                _ => {
                    dbg!(chunk);
                    todo!();
                }
            }
        }

        // If the stack is not empty, it means the XML was not properly balanced
        if !stack.is_empty() {
            Err(TreeToElementError::UnbalancedElements)
        } else {
            Err(TreeToElementError::NoRootElement)
        }
    }
}
