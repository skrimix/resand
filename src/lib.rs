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

#![allow(non_snake_case)]
#![allow(non_camel_case_types)]

pub mod defs;
pub mod res_value;
pub mod string_pool;
pub mod xmltree;

pub fn align(pos: u64, alignment: u64) -> u64 {
    let remaning = pos % alignment;
    if remaning == 0 {
        return pos;
    }

    pos + (alignment - remaning)
}
