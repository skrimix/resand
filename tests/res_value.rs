use std::io::Cursor;

use binrw::{BinReaderExt, BinWriterExt};
use libaxml::defs::{
    ResStringPool_ref, ResTable_ref, ResTypeBoolType, ResTypeNullType, ResValueType, Res_value,
};

#[test]
fn res_value_read_null_undefined() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x00\x00\x00\x00\x00");
    let value: Res_value = reader.read_le().unwrap();

    assert_eq!(value.size, 8);
    assert_eq!(value.res0, 0);
    assert_eq!(
        value.data,
        ResValueType::TYPE_NULL(ResTypeNullType::DATA_NULL_UNDEFINED)
    );
}

#[test]
fn res_value_write_null_undefined() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&Res_value::new(ResValueType::TYPE_NULL(
            ResTypeNullType::DATA_NULL_UNDEFINED,
        )))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x00\x00\x00\x00\x00");
}

#[test]
fn res_value_read_null_empty() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x00\x01\x00\x00\x00");
    let value: Res_value = reader.read_le().unwrap();

    assert_eq!(value.size, 8);
    assert_eq!(value.res0, 0);
    assert_eq!(
        value.data,
        ResValueType::TYPE_NULL(ResTypeNullType::DATA_NULL_EMPTY)
    );
}

#[test]
fn res_value_write_null_empty() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&Res_value::new(ResValueType::TYPE_NULL(
            ResTypeNullType::DATA_NULL_EMPTY,
        )))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x00\x01\x00\x00\x00");
}

#[test]
fn res_value_reference_to_int() {
    let reference = ResTable_ref {
        package_index: 0x01,
        type_index: 0x76,
        entry_index: 0xff81,
    };

    assert_eq!(<ResTable_ref as Into<u32>>::into(reference), 0x0176ff81)
}
#[test]
fn res_value_int_to_reference() {
    let value = 0xff127690;
    assert_eq!(
        <u32 as Into<ResTable_ref>>::into(value),
        ResTable_ref {
            package_index: 0xff,
            type_index: 0x12,
            entry_index: 0x7690
        }
    );
}

#[test]
fn res_value_read_reference() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x01\x58\x20\x15\x01");
    let value: Res_value = reader.read_le().unwrap();

    assert_eq!(value.size, 8);
    assert_eq!(value.res0, 0);
    assert_eq!(
        value.data,
        ResValueType::TYPE_REFERENCE(ResTable_ref {
            package_index: 0x01,
            type_index: 0x15,
            entry_index: 0x2058
        })
    );
}

#[test]
fn res_value_write_reference() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&Res_value::new(ResValueType::TYPE_REFERENCE(
            ResTable_ref {
                package_index: 0x2,
                type_index: 0x90,
                entry_index: 0x1234,
            },
        )))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x01\x34\x12\x90\x02");
}

#[test]
fn res_value_read_attribute() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x02\x01\x04\x00\x00");
    let value: Res_value = reader.read_le().unwrap();

    assert_eq!(value.size, 8);
    assert_eq!(value.res0, 0);
    assert_eq!(value.data, ResValueType::TYPE_ATTRIBUTE(0x0401));
}

#[test]
fn res_value_write_attribute() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&Res_value::new(ResValueType::TYPE_ATTRIBUTE(0x9812ffff)))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x02\xff\xff\x12\x98");
}

#[test]
fn res_value_read_string() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x03\x00\x01\x00\x00");
    let value: Res_value = reader.read_le().unwrap();

    assert_eq!(value.size, 8);
    assert_eq!(value.res0, 0);
    assert_eq!(
        value.data,
        ResValueType::TYPE_STRING(ResStringPool_ref { index: 0x00000100 })
    );
}

#[test]
fn res_value_write_string() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&Res_value::new(ResValueType::TYPE_STRING(
            ResStringPool_ref { index: 0xabaabaac },
        )))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x03\xac\xba\xaa\xab");
}

#[test]
fn res_value_read_float() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x04\x00\x00\xc0\x3f");
    let value: Res_value = reader.read_le().unwrap();

    assert_eq!(value.size, 8);
    assert_eq!(value.res0, 0);
    assert_eq!(value.data, ResValueType::TYPE_FLOAT(1.5));
}

#[test]
fn res_value_write_float() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&Res_value::new(ResValueType::TYPE_FLOAT(1e38)))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x04\x99\x76\x96\x7e");
}
#[test]
fn res_value_read_dynamic_reference() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x07\x72\x18\x61\xff");
    let value: Res_value = reader.read_le().unwrap();

    assert_eq!(value.size, 8);
    assert_eq!(value.res0, 0);
    assert_eq!(
        value.data,
        ResValueType::TYPE_DYNAMIC_REFERENCE(ResTable_ref {
            package_index: 0xff,
            type_index: 0x61,
            entry_index: 0x1872
        })
    );
}

#[test]
fn res_value_write_dynamic_reference() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&Res_value::new(ResValueType::TYPE_DYNAMIC_REFERENCE(
            ResTable_ref {
                package_index: 0x20,
                type_index: 0x65,
                entry_index: 0x42fe,
            },
        )))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x07\xfe\x42\x65\x20");
}

#[test]
fn res_value_read_dynamic_attribute() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x08\xef\xbe\xad\x0b");
    let value: Res_value = reader.read_le().unwrap();

    assert_eq!(value.size, 8);
    assert_eq!(value.res0, 0);
    assert_eq!(value.data, ResValueType::TYPE_DYNAMIC_ATTRIBUTE(0x0badbeef));
}

#[test]
fn res_value_write_dynamic_attribute() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&Res_value::new(ResValueType::TYPE_DYNAMIC_ATTRIBUTE(
            0x1012abcd,
        )))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x08\xcd\xab\x12\x10");
}

#[test]
fn res_value_read_int_dec() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x10\x78\x56\x34\x12");
    let value: Res_value = reader.read_le().unwrap();

    assert_eq!(value.size, 8);
    assert_eq!(value.res0, 0);
    assert_eq!(value.data, ResValueType::TYPE_INT_DEC(0x12345678));
}

#[test]
fn res_value_write_int_dec() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&Res_value::new(ResValueType::TYPE_INT_DEC(0x82610182)))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x10\x82\x01\x61\x82");
}

#[test]
fn res_value_read_int_hex() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x11\x32\x54\x76\x98");
    let value: Res_value = reader.read_le().unwrap();

    assert_eq!(value.size, 8);
    assert_eq!(value.res0, 0);
    assert_eq!(value.data, ResValueType::TYPE_INT_HEX(0x98765432));
}

#[test]
fn res_value_write_int_hex() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&Res_value::new(ResValueType::TYPE_INT_HEX(0x0abc1def)))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x11\xef\x1d\xbc\x0a");
}

#[test]
fn res_value_read_int_boolean_false() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x12\x00\x00\x00\x00");
    let value: Res_value = reader.read_le().unwrap();

    assert_eq!(value.size, 8);
    assert_eq!(value.res0, 0);
    assert_eq!(
        value.data,
        ResValueType::TYPE_INT_BOOLEAN(ResTypeBoolType::FALSE)
    );
}

#[test]
fn res_value_write_int_boolean_false() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&Res_value::new(ResValueType::TYPE_INT_BOOLEAN(
            ResTypeBoolType::FALSE,
        )))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x12\x00\x00\x00\x00");
}

#[test]
fn res_value_read_int_boolean_true() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x12\x01\x00\x00\x00");
    let value: Res_value = reader.read_le().unwrap();

    assert_eq!(value.size, 8);
    assert_eq!(value.res0, 0);
    assert_eq!(
        value.data,
        ResValueType::TYPE_INT_BOOLEAN(ResTypeBoolType::TRUE)
    );
}

#[test]
fn res_value_write_int_boolean_true() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&Res_value::new(ResValueType::TYPE_INT_BOOLEAN(
            ResTypeBoolType::TRUE,
        )))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x12\x01\x00\x00\x00");
}

// TODO: colors + complex stuff
