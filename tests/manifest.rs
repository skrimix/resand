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
