pub mod defs;
pub mod res_value;
#[allow(clippy::unit_arg)]
pub mod string_pool;
#[allow(clippy::int_plus_one)]
pub mod table;
pub mod xml_names;
pub mod xmltree;

pub fn align(pos: u64, alignment: u64) -> u64 {
    let remaning = pos % alignment;
    if remaning == 0 {
        return pos;
    }

    pos + (alignment - remaning)
}
