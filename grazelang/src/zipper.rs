use std::{
    fs::{File, OpenOptions},
    io::{self, Read, Seek, Write, copy},
    path::{Path, PathBuf},
};

use zip::{CompressionMethod, ZipWriter, result::ZipError, write::SimpleFileOptions};

use crate::codegen::core::{CURRENT_DIRECTORY_STR, GrazeSb3GeneratorContext};

#[derive(Debug, thiserror::Error)]
pub enum WriteIntoZipError {
    #[error(transparent)]
    ZipError(#[from] ZipError),
    #[error(transparent)]
    IoError(#[from] io::Error),
    #[error(transparent)]
    SerdeJsonError(#[from] serde_json::Error),
    #[error("path {path:?} tries to escape the resource directory")]
    PathTriesToEscapeResourceDirectory { path: PathBuf },
}

pub fn write_to_zip_path(
    zip_path: &Path,
    codegen_context: &GrazeSb3GeneratorContext,
) -> Result<(), WriteIntoZipError> {
    let zip_file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(zip_path)?;
    write_to_zip_file(zip_file, codegen_context)
}

pub fn write_to_zip_file<W>(
    zip_file: W,
    codegen_context: &GrazeSb3GeneratorContext,
) -> Result<(), WriteIntoZipError>
where
    W: Write + Seek,
{
    let mut zip = ZipWriter::new(zip_file);
    write_into_zip(&mut zip, codegen_context)
}

pub fn append_to_zip_path(
    zip_path: &Path,
    codegen_context: &GrazeSb3GeneratorContext,
) -> Result<(), WriteIntoZipError> {
    let zip_file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(zip_path)?;
    append_to_zip_file(zip_file, codegen_context)
}

pub fn append_to_zip_file<A>(
    zip_file: A,
    codegen_context: &GrazeSb3GeneratorContext,
) -> Result<(), WriteIntoZipError>
where
    A: Read + Write + Seek,
{
    let mut zip = ZipWriter::new_append(zip_file)?;
    write_into_zip(&mut zip, codegen_context)
}

pub fn write_into_zip<W>(
    zip: &mut ZipWriter<W>,
    codegen_context: &GrazeSb3GeneratorContext,
) -> Result<(), WriteIntoZipError>
where
    W: Write + Seek,
{
    pub fn extend_path_safely(
        buf: &mut PathBuf,
        base: &Path,
        path: &str,
    ) -> Result<PathBuf, WriteIntoZipError> {
        buf.clear();
        buf.push(base);
        buf.push(path);
        let buf = buf.canonicalize()?;
        if !buf.starts_with(base) {
            return Err(WriteIntoZipError::PathTriesToEscapeResourceDirectory { path: buf });
        }
        Ok(buf)
    }
    let resources_directory = codegen_context
        .settings
        .resources_path
        .as_deref()
        .unwrap_or(Path::new(CURRENT_DIRECTORY_STR));
    let mut path_buf = PathBuf::new();
    let options = SimpleFileOptions::default()
        .compression_method(CompressionMethod::Stored)
        .unix_permissions(0o644);
    zip.start_file("project.json", options)?;
    zip.write_all(serde_json::to_string(&codegen_context.sb3)?.as_bytes())?;
    for (name, path) in &codegen_context.asset_files {
        zip.start_file(name, options)?;
        copy(
            &mut File::open(extend_path_safely(
                &mut path_buf,
                resources_directory,
                path.as_str(),
            )?)?,
            zip,
        )?;
    }
    Ok(())
}
