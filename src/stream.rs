use std::{
    any::type_name,
    fmt::Display,
    io::{Read, Seek, Write},
};

#[derive(Debug)]
pub struct StreamError {
    pub error: std::io::Error,
    pub pos: u64,
    pub context: Vec<String>,
}

impl StreamError {
    pub fn new_context(error: std::io::Error, pos: u64, context: Vec<String>) -> Self {
        Self {
            error,
            pos,
            context,
        }
    }

    pub fn new(error: std::io::Error, pos: u64) -> Self {
        Self {
            error,
            pos,
            context: Vec::new(),
        }
    }

    pub fn add_context<C: ToString>(self, new_context: C) -> Self {
        let mut context = self.context;
        context.push(new_context.to_string());
        Self::new_context(self.error, self.pos, context)
    }

    pub fn new_string_context<E: ToString, C: ToString>(error: E, pos: u64, context: C) -> Self {
        Self {
            error: std::io::Error::other(error.to_string()),
            pos,
            context: vec![context.to_string()],
        }
    }
}

impl Display for StreamError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = format!("error: {} at {} with context:", self.error, self.pos);

        let mut context_str = String::new();

        for ctx in &self.context {
            context_str.push_str(&format!("\n{ctx}"));
        }

        write!(f, "{string}\n{context_str}")
    }
}

pub type StreamResult<T> = Result<T, StreamError>;

pub trait NewResultCtx {
    fn add_context<C: ToString, F: FnOnce() -> C>(self, context: F) -> Self;
}

impl<T> NewResultCtx for StreamResult<T> {
    fn add_context<C: ToString, F: FnOnce() -> C>(self, context: F) -> Self {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(e.add_context(context())),
        }
    }
}

pub trait Readable: Sized {
    type Args;
    fn read<R: Read + Seek>(reader: &mut R, args: Self::Args) -> StreamResult<Self>;
}

pub trait Writeable: Sized {
    type Args;
    fn write<W: Write + Seek>(self, writer: &mut W, args: Self::Args) -> StreamResult<()>;
}

pub trait ResultCtx: Sized {
    type OkT;
    fn with_context<C: ToString, F: FnOnce() -> C>(
        self,
        pos: u64,
        context: F,
    ) -> StreamResult<Self::OkT>;
    fn stream_context<C: ToString, F: FnOnce() -> C>(self, context: F) -> StreamResult<Self::OkT>;
    fn with_pos(self, pos: u64) -> StreamResult<Self::OkT>;
}

impl From<std::io::Error> for StreamError {
    fn from(value: std::io::Error) -> Self {
        Self::new(value, u64::MAX)
    }
}

impl<T> ResultCtx for Result<T, std::io::Error> {
    type OkT = T;
    fn with_context<C: ToString, F: FnOnce() -> C>(
        self,
        pos: u64,
        context: F,
    ) -> StreamResult<<Self as ResultCtx>::OkT> {
        match self {
            Ok(v) => StreamResult::Ok(v),
            Err(e) => StreamResult::Err(StreamError::new_context(e, pos, vec![
                context().to_string(),
            ])),
        }
    }
    fn with_pos(self, pos: u64) -> StreamResult<Self::OkT> {
        match self {
            Ok(v) => StreamResult::Ok(v),
            Err(e) => StreamResult::Err(StreamError::new(e, pos)),
        }
    }
    fn stream_context<C: ToString, F: FnOnce() -> C>(self, context: F) -> StreamResult<Self::OkT> {
        self.with_context(u64::MAX, context)
    }
}

fn read_data<R: Read + Seek, const N: usize>(reader: &mut R) -> StreamResult<[u8; N]> {
    let mut buf = [0; N];
    reader
        .read_exact(&mut buf)
        .with_context(reader.stream_position()?, || format!("read_data<{N}>"))?;
    Ok(buf)
}

fn write_data<W: Write + Seek>(data: &[u8], writer: &mut W) -> StreamResult<()> {
    writer
        .write_all(data)
        .with_context(writer.stream_position()?, || "write_data")
}

pub trait ReadableNoOptions: Sized {
    fn read_no_opts<R: Read + Seek>(reader: &mut R) -> StreamResult<Self>;
}

pub trait WriteableNoOptions {
    fn write_no_opts<W: Write + Seek>(self, writer: &mut W) -> StreamResult<()>;
}

impl<T: Readable<Args = ()>> ReadableNoOptions for T {
    fn read_no_opts<R: Read + Seek>(reader: &mut R) -> StreamResult<Self> {
        T::read(reader, ())
    }
}

impl<T: Writeable<Args = ()>> WriteableNoOptions for T {
    fn write_no_opts<W: Write + Seek>(self, writer: &mut W) -> StreamResult<()> {
        self.write(writer, ())
    }
}

impl Readable for u8 {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self::from_le_bytes(
            read_data(reader).add_context(|| "read u8")?,
        ))
    }
}

impl Readable for u16 {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self::from_le_bytes(
            read_data(reader).add_context(|| "read u16")?,
        ))
    }
}

impl Readable for u32 {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self::from_le_bytes(
            read_data(reader).add_context(|| "read u32")?,
        ))
    }
}

impl Readable for u64 {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self::from_le_bytes(
            read_data(reader).add_context(|| "read u64")?,
        ))
    }
}

impl Readable for f32 {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self::from_le_bytes(
            read_data(reader).add_context(|| "read f32")?,
        ))
    }
}
impl Readable for f64 {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        Ok(Self::from_le_bytes(
            read_data(reader).add_context(|| "read f64")?,
        ))
    }
}

impl Writeable for u8 {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        write_data(&self.to_le_bytes(), writer).add_context(|| "write u8")
    }
}

impl Writeable for u16 {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        write_data(&self.to_le_bytes(), writer).add_context(|| "write u16")
    }
}

impl Writeable for u32 {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        write_data(&self.to_le_bytes(), writer).add_context(|| "write u32")
    }
}
impl Writeable for u64 {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        write_data(&self.to_le_bytes(), writer).add_context(|| "write u64")
    }
}
impl Writeable for f32 {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        write_data(&self.to_le_bytes(), writer).add_context(|| "write f32")
    }
}
impl Writeable for f64 {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        write_data(&self.to_le_bytes(), writer).add_context(|| "write f64")
    }
}

pub trait VecReadable: Sized {
    fn read_vec<R: Read + Seek>(reader: &mut R, count: usize) -> StreamResult<Self>;
}

pub trait VecWritable {
    fn write_vec<W: Write + Seek>(self, writer: &mut W) -> StreamResult<()>;
}

impl<T: ReadableNoOptions> VecReadable for Vec<T> {
    fn read_vec<R: Read + Seek>(reader: &mut R, count: usize) -> StreamResult<Self> {
        let mut data = Vec::with_capacity(count);

        for i in 0..count {
            data.push(T::read_no_opts(reader).add_context(|| {
                format!("reading item {} of vec of type {}", i, type_name::<T>())
            })?);
        }

        Ok(data)
    }
}

impl<T: WriteableNoOptions> VecWritable for Vec<T> {
    fn write_vec<W: Write + Seek>(self, writer: &mut W) -> StreamResult<()> {
        for (i, val) in self.into_iter().enumerate() {
            val.write_no_opts(writer).add_context(|| {
                format!("writing item {} of vec of type {}", i, type_name::<T>())
            })?;
        }

        Ok(())
    }
}

impl Readable for bool {
    type Args = ();
    fn read<R: Read + Seek>(reader: &mut R, _args: Self::Args) -> StreamResult<Self> {
        let data: u8 = u8::read_no_opts(reader).add_context(|| "read u8 for bool")?;

        Ok(data != 0)
    }
}

impl Writeable for bool {
    type Args = ();
    fn write<W: Write + Seek>(self, writer: &mut W, _args: Self::Args) -> StreamResult<()> {
        let data: u8 = match self {
            true => 1,
            false => 0,
        };
        data.write_no_opts(writer)
            .add_context(|| "write u8 for bool")
    }
}
