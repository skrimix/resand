pub mod defs;
pub mod res_value;
pub mod stream;
pub mod string_pool;
pub mod table;
pub mod xml_names;
pub mod xmltree;

/// Align an offset to a certain boundary
///
/// # Arguments
///
/// * `pos` - position to align
/// * `alignment` - number of bytes to align the position to
///
/// # Returns
///
/// The next position which is aligned to the specified boundary
pub fn align(pos: u64, alignment: u64) -> u64 {
    let remaning = pos % alignment;
    if remaning == 0 {
        return pos;
    }

    pos + (alignment - remaning)
}
