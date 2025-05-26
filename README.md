# Resand

Resand (RESesource ANDroid) is a library for android resource reading and writing. It can read and
write AXML (ResXML) and ARSC (ResTable) files.

It is pretty minimal, it does not turn the resource data into xml or any other more friendly
format, modifying or reading the data from a resource has to be done manually in rust.

The purpose of this crate is that I wanted to be able to change the app name and package name of
an apk without including a large dependency such as apktool, or aapt, or any other similar external
tool.

There is not much documentation as this was just something I needed for one of my other projects.
And it definitely has bugs and stuff. So good luck using this lol.

A lot of the code is based off of the
[Android Source Code](https://android.googlesource.com/platform/frameworks/base/+/master/libs/androidfw/include/androidfw/ResourceTypes.h)

## Usage

The following examples use .unwrap(), but obviously you should handle errors properly.

Basic reading and writing of and xml tree:

```rust
use resand::xmltree::XMLTree;
use std::fs::File;

let mut file = File::open("./AndroidManifest.xml").unwrap();
let tree = XMLTree::read(&mut file).unwrap();

dbg!(&tree);

let mut output = File::create("./NewAndroidManifest.xml").unwrap();

tree.write(&mut output).unwrap();
```

Basic reading and writing of a resource table:

```rust
use resand::table::ResTable
use std::fs::File;

let mut file = File::open("./resources.arsc").unwrap();
let table = ResTable::read_all(&mut file).unwrap();

dbg!(&table);

let mut output = File::create("./new_resources.arsc").unwrap();

table.write_all(&mut output).unwrap();
```

Setting the app name:

```rust
let xml = XMLTree::read(...);
let mut table = ResTable::read_all(...)

let application = xml
    .root
    .get_elements(&["manifest", "application"], &xml.string_pool)
    .pop()
    .unwrap();

let attr = application
    .get_attribute("label", &xml.string_pool).unwrap();

if let ResValueType::Reference(attr) = attr.typed_value.data {
    let package = table.packages.first_mut().unwrap();
    let entry = package
        .resolve_ref_mut(attr).unwrap();
    if let ResTableEntryValue::ResValue(value) = &mut entry.data {
        value
            .data
            .write_string("New Name".to_string(), &mut table.string_pool);
    }
}

table.write_all(...)
```

Setting the package name:

```rust

let mut xml = (...)
let mut table = (...)

let package = table.packages.first_mut().unwrap();
package.name = "new.package.name".to_string();

let root_element = xml
    .root
    .get_elements_mut(&["manifest"], &xml.string_pool)
    .pop().unwrap();

let attr = root_element
    .get_attribute_mut("package", &xml.string_pool).unwrap();

attr.write_string("new.package.name".to_string(), &mut xml.string_pool);

// you might need to modify other parts of the manifest if they also refer to the package name

xml.write(...);
table.write_all(...);

```

## License

Resand is licensed under the Apache-2.0 license which can be read [here](./LICENSE)
