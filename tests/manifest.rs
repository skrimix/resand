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

use std::{
    io::{Cursor, Seek},
    path::PathBuf,
};

use binrw::{BinRead, BinWrite};
use libaxml::{defs::ResChunk_header, string_pool::StringPool};

#[test]
fn test_android_manifest() {
    let data = include_bytes!("data/AndroidManifest.xml");

    let mut stream = Cursor::new(data);

    let header = ResChunk_header::read_le(&mut stream);

    let h_pos = stream.position();

    let sph = ResChunk_header::read_le(&mut stream).unwrap();

    dbg!("hi");
    dbg!(sph.headerSize);
    let string_pool = StringPool::read_options(&mut stream, binrw::Endian::Little, ()).unwrap();

    let mut writer = Cursor::new(Vec::new());

    dbg!("hi");
    string_pool
        .write_options(&mut writer, binrw::Endian::Little, ())
        .unwrap();

    let mut s2 = Cursor::new(writer.into_inner());

    dbg!("hi");

    let new_pool = StringPool::read_options(&mut s2, binrw::Endian::Little, ()).unwrap();

    let strings = string_pool.get_strings();
    let s2 = new_pool.get_strings();

    dbg!(strings);
    dbg!(s2);

    //panic!()
}
