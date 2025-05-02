use std::io::Cursor;

use binrw::{BinReaderExt, BinWriterExt};
use resand::string_pool::{
    calc_length16, calc_length8, new_length16, new_length8, ResStringPoolRef, ResStringPoolSpan,
    StringPoolFlags, StringPoolString16, StringPoolString8,
};

#[test]
fn test_read_res_string_pool_ref() {
    let mut reader = Cursor::new(b"\x10\xef\xcd\xab");
    let rspr: ResStringPoolRef = reader.read_le().unwrap();

    assert_eq!(rspr.index, 0xabcdef10)
}

#[test]
fn test_write_res_string_pool_ref() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResStringPoolRef { index: 0x00120950 })
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x50\x09\x12\x00")
}

#[test]
fn test_string_pool_flags_from_int() {
    let value = StringPoolFlags { flags: 0 };
    assert!(!value.sorted());
    assert!(!value.utf8());

    let value = StringPoolFlags { flags: 1 };
    assert!(value.sorted());
    assert!(!value.utf8());

    let value = StringPoolFlags { flags: 0x100 };
    assert!(!value.sorted());
    assert!(value.utf8());

    let value = StringPoolFlags { flags: 0x101 };
    assert!(value.sorted());
    assert!(value.utf8());
}

#[test]
fn test_string_pool_flags_to_int() {
    let flags = StringPoolFlags::new(false, false);
    assert!(!flags.sorted());
    assert!(!flags.utf8());
    assert_eq!(flags.flags, 0x0);

    let flags = StringPoolFlags::new(true, false);
    assert!(flags.sorted());
    assert!(!flags.utf8());
    assert_eq!(flags.flags, 0x1);

    let flags = StringPoolFlags::new(false, true);
    assert!(!flags.sorted());
    assert!(flags.utf8());
    assert_eq!(flags.flags, 0x100);

    let flags = StringPoolFlags::new(true, true);
    assert!(flags.sorted());
    assert!(flags.utf8());
    assert_eq!(flags.flags, 0x101);
}

#[test]
fn test_read_res_string_pool_span() {
    let mut reader = Cursor::new(b"\x01\x00\x00\x50\x05\x00\x00\x00\x00\x05\x00\x00");
    let value: ResStringPoolSpan = reader.read_le().unwrap();

    assert_eq!(value.name, ResStringPoolRef { index: 0x50000001 });
    assert_eq!(value.first_char, 0x5);
    assert_eq!(value.last_char, 0x500);
}

#[test]
fn test_write_res_string_pool_span() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResStringPoolSpan {
            name: ResStringPoolRef { index: 0x00120950 },
            first_char: 0x0123abcd,
            last_char: 0x12345678,
        })
        .unwrap();

    assert_eq!(
        writer.into_inner(),
        b"\x50\x09\x12\x00\xcd\xab\x23\x01\x78\x56\x34\x12"
    )
}

#[test]
fn test_read_string_pool_string8_normal() {
    let mut reader = Cursor::new(b"\x0d\x0dHello, World!\x00");
    let value: StringPoolString8 = reader.read_le().unwrap();

    assert_eq!(value.string, "Hello, World!");
}

#[test]
fn test_read_string_pool_string8_max_length() {
    let mut test_input = b"\xff\xff\xff\xff".to_vec();
    let test_str = "A".repeat(0x7fff);
    test_input.extend(test_str.as_bytes());
    test_input.push(0);
    let mut reader = Cursor::new(test_input);
    let value: StringPoolString8 = reader.read_le().unwrap();

    assert_eq!(value.string, test_str);
}

#[test]
fn test_read_string_pool_string8_very_long() {
    let mut test_input = b"\x85\x01\x85\x01".to_vec();
    let test_str = "A".repeat(0x0501);
    test_input.extend(test_str.as_bytes());
    test_input.push(0);
    let mut reader = Cursor::new(test_input);
    let value: StringPoolString8 = reader.read_le().unwrap();

    assert_eq!(value.string, test_str);
}

#[test]
fn test_read_string_pool_string16_normal() {
    let mut reader = Cursor::new(
        b"\x0d\x00H\x00e\x00l\x00l\x00o\x00,\x00 \x00W\x00o\x00r\x00l\x00d\x00!\x00\x00\x00",
    );
    let value: StringPoolString16 = reader.read_le().unwrap();

    assert_eq!(value.string, "Hello, World!");
}

#[test]
fn test_read_string_pool_string16_very_long() {
    let mut test_input = b"\x01\x80\x01\x00".to_vec();
    let test_data: Vec<u8> = b"A\x00".repeat(0x10001).to_vec();
    test_input.extend(&test_data);
    test_input.push(0);
    test_input.push(0);
    let mut reader = Cursor::new(test_input);
    let value: StringPoolString16 = reader.read_le().unwrap();

    assert_eq!(
        value.string,
        String::from_utf16(
            test_data
                .chunks_exact(2)
                .map(|a| u16::from_ne_bytes([a[0], a[1]]))
                .collect::<Vec<u16>>()
                .as_slice()
        )
        .unwrap()
    );
}

#[test]
fn test_write_string_pool_string8_normal() {
    let mut writer = Cursor::new(Vec::new());

    writer
        .write_le(&StringPoolString8 {
            string: "Hello, World!".to_string(),
        })
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x0d\x0dHello, World!\x00")
}

#[test]
fn test_write_string_pool_string8_max_length() {
    let mut writer = Cursor::new(Vec::new());

    let test_str = "A".repeat(0x7fff);

    writer
        .write_le(&StringPoolString8 {
            string: test_str.clone(),
        })
        .unwrap();

    let mut test_input = b"\xff\xff\xff\xff".to_vec();
    test_input.extend(test_str.as_bytes());
    test_input.push(0);

    assert_eq!(writer.into_inner(), test_input)
}

#[test]
fn test_write_string_pool_string8_very_long() {
    let mut writer = Cursor::new(Vec::new());

    let test_str = "A".repeat(0x0501);

    writer
        .write_le(&StringPoolString8 {
            string: test_str.clone(),
        })
        .unwrap();

    let mut test_input = b"\x85\x01\x85\x01".to_vec();
    test_input.extend(test_str.as_bytes());
    test_input.push(0);
    assert_eq!(writer.into_inner(), test_input);
}

#[test]
fn test_write_string_pool_string16_normal() {
    let mut writer = Cursor::new(Vec::new());

    writer
        .write_le(&StringPoolString16 {
            string: "Hello, World!".to_string(),
        })
        .unwrap();
    assert_eq!(
        writer.into_inner(),
        b"\x0d\x00H\x00e\x00l\x00l\x00o\x00,\x00 \x00W\x00o\x00r\x00l\x00d\x00!\x00\x00\x00",
    )
}

#[test]
fn test_write_string_pool_string16_very_long() {
    let mut writer = Cursor::new(Vec::new());

    let test_data: Vec<u8> = b"A\x00".repeat(0x10001).to_vec();

    let test_str = String::from_utf16(
        test_data
            .chunks_exact(2)
            .map(|a| u16::from_ne_bytes([a[0], a[1]]))
            .collect::<Vec<u16>>()
            .as_slice(),
    )
    .unwrap();

    writer
        .write_le(&StringPoolString16 {
            string: test_str.clone(),
        })
        .unwrap();

    let mut test_input = b"\x01\x80\x01\x00".to_vec();
    test_input.extend(&test_data);
    test_input.push(0);
    test_input.push(0);

    assert_eq!(writer.into_inner(), test_input);
}
// TODO: test more interesting utf8 stuff

#[test]
fn test_new_string_pool_length() {
    let (l1, l2) = calc_length8(0x50);
    assert_eq!(new_length8(l1, l2), 0x50);

    let (l1, l2) = calc_length8(0x1000);
    assert_eq!(new_length8(l1, l2), 0x1000);

    let (l1, l2) = calc_length16(0x50);
    assert_eq!(new_length16(l1, l2), 0x50);

    let (l1, l2) = calc_length16(0x1000);
    assert_eq!(new_length16(l1, l2), 0x1000);

    let (l1, l2) = calc_length16(0x10000);
    assert_eq!(new_length16(l1, l2), 0x10000);
}
