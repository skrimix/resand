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

use std::io::{Cursor, Seek, SeekFrom};

use binrw::{BinRead, BinWrite};
use libaxml::{
    defs::{ResChunk_header, ResType, ResTypeValue},
    string_pool::StringPool,
    xmltree::RawXMLTree,
};
use xmltree::{Element, EmitterConfig};

#[test]
fn test_android_manifest() {
    let data = include_bytes!("data/AndroidManifest.xml");

    let mut stream = Cursor::new(data);

    let header = ResChunk_header::read_le(&mut stream).unwrap();

    let _h_pos = stream.position();

    //dbg!(&header);

    if let ResTypeValue::XML(xml_tree) = header.data {
        let el: Element = xml_tree.try_into().unwrap();
        let mut output = Cursor::new(Vec::new());
        el.write_with_config(&mut output, EmitterConfig::new().perform_indent(true))
            .unwrap();

        let data = output.into_inner();

        std::fs::write(format!("p{}", 0), &data).unwrap();
        println!("{}", String::from_utf8(data).unwrap());

        let conv: RawXMLTree = el.try_into().unwrap();

        let header = ResChunk_header {
            data: ResTypeValue::XML(conv),
        };

        let mut output = Cursor::new(Vec::new());

        header.write_le(&mut output).unwrap();

        print_xml(&output.into_inner(), 1);
    }

    panic!();
}

fn print_xml(data: &[u8], index: usize) {
    let mut stream = Cursor::new(data);

    let header = ResChunk_header::read_le(&mut stream).unwrap();

    let _h_pos = stream.position();

    //dbg!(&header);

    if let ResTypeValue::XML(xml_tree) = header.data {
        let el: Element = xml_tree.try_into().unwrap();
        let mut output = Cursor::new(Vec::new());
        el.write_with_config(&mut output, EmitterConfig::new().perform_indent(true))
            .unwrap();

        let data = output.into_inner();

        std::fs::write(format!("p{}", index), &data).unwrap();
        println!("{}", String::from_utf8(data).unwrap());
    }
}

#[test]
fn test_android_manifest_full() {
    let data = include_bytes!("data/AndroidManifest.xml");

    let mut stream = Cursor::new(data);

    let main_header = ResChunk_header::read_le(&mut stream).unwrap();
}
