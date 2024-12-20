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

use std::{io::Cursor, path::PathBuf};

use binrw::BinRead;
use libaxml::{defs::ResChunk_header, string_pool::StringPool};

#[test]
fn test_android_manifest() {
    let data = include_bytes!("data/AndroidManifest.xml");

    let mut stream = Cursor::new(data);

    let header = ResChunk_header::read_le(&mut stream);

    let h_pos = stream.position();

    let sph = ResChunk_header::read_le(&mut stream);

    let string_pool =
        StringPool::read_options(&mut stream, binrw::Endian::Little, (h_pos,)).unwrap();

    let strings = string_pool.get_strings().unwrap();

    dbg!(strings);
}
