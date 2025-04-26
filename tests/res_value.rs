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

use std::io::Cursor;

use binrw::{BinReaderExt, BinWriterExt};
use libaxml::{
    defs::ResTableRef,
    res_value::{
        ResTypeBoolType, ResTypeNullType, ResValue, ResValueType, ARGB4, ARGB8, RGB4, RGB8,
    },
    string_pool::ResStringPoolRef,
};

#[test]
fn res_value_read_null_undefined() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x00\x00\x00\x00\x00");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(value.data, ResValueType::Null(ResTypeNullType::Undefined));
}

#[test]
fn res_value_write_null_undefined() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::Null(
            ResTypeNullType::Undefined,
        )))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x00\x00\x00\x00\x00");
}

#[test]
fn res_value_read_null_empty() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x00\x01\x00\x00\x00");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(value.data, ResValueType::Null(ResTypeNullType::Empty));
}

#[test]
fn res_value_write_null_empty() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::Null(ResTypeNullType::Empty)))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x00\x01\x00\x00\x00");
}

#[test]
fn res_value_reference_to_int() {
    let reference = ResTableRef {
        package_index: 0x01,
        type_index: 0x76,
        entry_index: 0xff81,
    };

    assert_eq!(<ResTableRef as Into<u32>>::into(reference), 0x0176ff81)
}
#[test]
fn res_value_int_to_reference() {
    let value = 0xff127690;
    assert_eq!(
        <u32 as Into<ResTableRef>>::into(value),
        ResTableRef {
            package_index: 0xff,
            type_index: 0x12,
            entry_index: 0x7690
        }
    );
}

#[test]
fn res_value_read_reference() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x01\x58\x20\x15\x01");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(
        value.data,
        ResValueType::Reference(ResTableRef {
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
        .write_le(&ResValue::new(ResValueType::Reference(ResTableRef {
            package_index: 0x2,
            type_index: 0x90,
            entry_index: 0x1234,
        })))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x01\x34\x12\x90\x02");
}

#[test]
fn res_value_read_attribute() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x02\x01\x04\x00\x00");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(value.data, ResValueType::Attribute(0x0401));
}

#[test]
fn res_value_write_attribute() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::Attribute(0x9812ffff)))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x02\xff\xff\x12\x98");
}

#[test]
fn res_value_read_string() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x03\x00\x01\x00\x00");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(
        value.data,
        ResValueType::String(ResStringPoolRef { index: 0x00000100 })
    );
}

#[test]
fn res_value_write_string() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::String(ResStringPoolRef {
            index: 0xabaabaac,
        })))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x03\xac\xba\xaa\xab");
}

#[test]
fn res_value_read_float() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x04\x00\x00\xc0\x3f");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(value.data, ResValueType::Float(1.5));
}

#[test]
fn res_value_write_float() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::Float(1e38)))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x04\x99\x76\x96\x7e");
}
#[test]
fn res_value_read_dynamic_reference() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x07\x72\x18\x61\xff");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(
        value.data,
        ResValueType::DyanmicReference(ResTableRef {
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
        .write_le(&ResValue::new(ResValueType::DyanmicReference(
            ResTableRef {
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
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(value.data, ResValueType::DynamicAttribute(0x0badbeef));
}

#[test]
fn res_value_write_dynamic_attribute() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::DynamicAttribute(0x1012abcd)))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x08\xcd\xab\x12\x10");
}

#[test]
fn res_value_read_int_dec() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x10\x78\x56\x34\x12");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(value.data, ResValueType::IntDec(0x12345678));
}

#[test]
fn res_value_write_int_dec() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::IntDec(0x82610182)))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x10\x82\x01\x61\x82");
}

#[test]
fn res_value_read_int_hex() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x11\x32\x54\x76\x98");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(value.data, ResValueType::IntHex(0x98765432));
}

#[test]
fn res_value_write_int_hex() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::IntHex(0x0abc1def)))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x11\xef\x1d\xbc\x0a");
}

#[test]
fn res_value_read_int_boolean_false() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x12\x00\x00\x00\x00");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(value.data, ResValueType::IntBoolean(ResTypeBoolType::False));
}

#[test]
fn res_value_write_int_boolean_false() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::IntBoolean(
            ResTypeBoolType::False,
        )))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x12\x00\x00\x00\x00");
}

#[test]
fn res_value_read_int_boolean_true() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x12\x01\x00\x00\x00");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(value.data, ResValueType::IntBoolean(ResTypeBoolType::True));
}

#[test]
fn res_value_write_int_boolean_true() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::IntBoolean(
            ResTypeBoolType::True,
        )))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x12\x01\x00\x00\x00");
}

#[test]
fn res_value_argb8() {
    assert_eq!(
        ARGB8::new(0xff, 0x41, 0x81, 0xa6),
        ARGB8 {
            aarrggbb: 0xff4181a6
        }
    );
    let value = ARGB8 {
        aarrggbb: 0x12345678,
    };
    assert_eq!(value.alpha(), 0x12);
    assert_eq!(value.red(), 0x34);
    assert_eq!(value.green(), 0x56);
    assert_eq!(value.blue(), 0x78);
}

#[test]
fn res_value_read_int_color_argb8() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x1c\x25\x26\x27\x28");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(
        value.data,
        ResValueType::IntColorARGB8(ARGB8::new(0x28, 0x27, 0x26, 0x25))
    );
}
#[test]
fn res_value_write_int_color_argb8() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::IntColorARGB8(ARGB8::new(
            0xff, 0x50, 0x12, 0x55,
        ))))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x1c\x55\x12\x50\xff");
}

#[test]
fn res_value_rgb8() {
    assert_eq!(RGB8::new(0x41, 0x81, 0xa6), RGB8 { rrggbb: 0x4181a6 });
    let value = RGB8 { rrggbb: 0x123456 };
    assert_eq!(value.red(), 0x12);
    assert_eq!(value.green(), 0x34);
    assert_eq!(value.blue(), 0x56);
}

#[test]
fn res_value_read_int_color_rgb8() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x1d\x25\x26\x27\x00");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(
        value.data,
        ResValueType::IntColorRGB8(RGB8::new(0x27, 0x26, 0x25))
    );
}
#[test]
fn res_value_write_int_color_rgb8() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::IntColorRGB8(RGB8::new(
            0x50, 0x12, 0x55,
        ))))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x1d\x55\x12\x50\x00");
}

#[test]
fn res_value_rgb4() {
    assert_eq!(RGB4::new(0x4, 0x8, 0xa), RGB4 { rgb: 0x48a });
    let value = RGB4 { rgb: 0x246 };
    assert_eq!(value.red(), 0x2);
    assert_eq!(value.green(), 0x4);
    assert_eq!(value.blue(), 0x6);
}

#[test]
fn res_value_read_int_color_rgb4() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x1f\x65\x02\x00\x00");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(
        value.data,
        ResValueType::IntColorRGB4(RGB4::new(0x2, 0x6, 0x5))
    );
}
#[test]
fn res_value_write_int_color_rgb4() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::IntColorRGB4(RGB4::new(
            0xf, 0xa, 0x0,
        ))))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x1f\xa0\x0f\x00\x00");
}

#[test]
fn res_value_argb4() {
    assert_eq!(ARGB4::new(0xb, 0x4, 0x8, 0xa), ARGB4 { argb: 0xb48a });
    let value = ARGB4 { argb: 0xd246 };
    assert_eq!(value.alpha(), 0xd);
    assert_eq!(value.red(), 0x2);
    assert_eq!(value.green(), 0x4);
    assert_eq!(value.blue(), 0x6);
}

#[test]
fn res_value_read_int_color_argb4() {
    let mut reader = Cursor::new(b"\x08\x00\x00\x1e\x65\x92\x00\x00");
    let value: ResValue = reader.read_le().unwrap();

    assert_eq!(
        value.data,
        ResValueType::IntColorARGB4(ARGB4::new(0x9, 0x2, 0x6, 0x5))
    );
}
#[test]
fn res_value_write_int_color_argb4() {
    let mut writer = Cursor::new(Vec::new());
    writer
        .write_le(&ResValue::new(ResValueType::IntColorARGB4(ARGB4::new(
            0xd, 0xf, 0xa, 0x0,
        ))))
        .unwrap();

    assert_eq!(writer.into_inner(), b"\x08\x00\x00\x1e\xa0\xdf\x00\x00");
}

// TODO: complex stuff
