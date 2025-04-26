use std::io::Cursor;

use binrw::BinReaderExt;
use libaxml::{
    defs::{ResChunk, ResTypeValue},
    xmltree::XMLTree,
};
