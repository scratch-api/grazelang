use std::{
    fs::{File, OpenOptions},
    io::{self, Read, Seek, Write, copy},
    path::{Path, PathBuf},
    rc::Rc,
};

use zip::{CompressionMethod, ZipWriter, result::ZipError, write::SimpleFileOptions};

use crate::{
    codegen::core::{CURRENT_DIRECTORY_STR, GrazeSb3GeneratorContext},
    messages::types::GetLintId,
};

#[derive(Debug, Clone, thiserror::Error, enum_assoc::Assoc)]
#[func(const fn internal_lint_id(&self) -> &'static str)]
#[func(pub const fn get_primary_message(&self) -> &'static str)]
pub enum WriteIntoZipError {
    #[assoc(internal_lint_id = "zip_error")]
    #[assoc(get_primary_message = "could not zip the project")]
    #[error(transparent)]
    ZipError(#[from] std::rc::Rc<ZipError>),
    #[assoc(internal_lint_id = "io_error")]
    #[assoc(get_primary_message = "an io error occurred")]
    #[error(transparent)]
    IoError(#[from] std::rc::Rc<io::Error>),
    #[assoc(internal_lint_id = "json_error")]
    #[assoc(get_primary_message = "could not encode the project json")]
    #[error(transparent)]
    SerdeJsonError(#[from] std::rc::Rc<serde_json::Error>),
    #[assoc(internal_lint_id = "zip_parent_directory_not_found")]
    #[assoc(get_primary_message = "parent directory of sb3 file not found")]
    #[error("path {path:?} not found")]
    ZipParentDirectoryNotFound { path: PathBuf },
    #[assoc(internal_lint_id = "zip_permission_denied")]
    #[assoc(get_primary_message = "permission to sb3 file denied")]
    #[error("permission for path {path:?} denied")]
    ZipPermissionDenied { path: PathBuf },
    #[assoc(internal_lint_id = "resource_file_not_found")]
    #[assoc(get_primary_message = "resource file not found")]
    #[error("path {path:?} not found")]
    ResourceFileNotFound { path: PathBuf },
    #[assoc(internal_lint_id = "resource_permission_denied")]
    #[assoc(get_primary_message = "permission to resource file denied")]
    #[error("permission for path {path:?} denied")]
    ResourceFilePermissionDenied { path: PathBuf },
    #[assoc(internal_lint_id = "path_tries_to_escape_resource_directory")]
    #[assoc(get_primary_message = "asset file path tries to escape resource directory")]
    #[error("path {path:?} tries to escape the resource directory")]
    PathTriesToEscapeResourceDirectory { path: PathBuf },
}

impl GetLintId for WriteIntoZipError {
    fn get_lint_id(&self) -> &'static str {
        self.internal_lint_id()
    }
}

impl PartialEq for WriteIntoZipError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::ZipError(l0), Self::ZipError(r0)) => Rc::as_ptr(l0) == Rc::as_ptr(r0),
            (Self::IoError(l0), Self::IoError(r0)) => Rc::as_ptr(l0) == Rc::as_ptr(r0),
            (Self::SerdeJsonError(l0), Self::SerdeJsonError(r0)) => {
                Rc::as_ptr(l0) == Rc::as_ptr(r0)
            }
            (
                Self::PathTriesToEscapeResourceDirectory { path: l_path },
                Self::PathTriesToEscapeResourceDirectory { path: r_path },
            ) => l_path == r_path,
            _ => false,
        }
    }
}

impl From<io::Error> for WriteIntoZipError {
    fn from(value: io::Error) -> Self {
        std::rc::Rc::new(value).into()
    }
}

impl From<ZipError> for WriteIntoZipError {
    fn from(value: ZipError) -> Self {
        std::rc::Rc::new(value).into()
    }
}

impl From<serde_json::Error> for WriteIntoZipError {
    fn from(value: serde_json::Error) -> Self {
        std::rc::Rc::new(value).into()
    }
}

pub fn write_to_zip_path(
    zip_path: &Path,
    codegen_context: &GrazeSb3GeneratorContext,
) -> Result<(), WriteIntoZipError> {
    use std::io::ErrorKind;
    let zip_file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(zip_path)
        .map_err(|err| match err.kind() {
            ErrorKind::NotFound => WriteIntoZipError::ZipParentDirectoryNotFound {
                path: zip_path.to_path_buf(),
            },
            ErrorKind::PermissionDenied => WriteIntoZipError::ZipPermissionDenied {
                path: zip_path.to_path_buf(),
            },
            _ => err.into(),
        })?;
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
    pub fn extend_resource_file_path_safely(
        buf: &mut PathBuf,
        base: &Path,
        path: &str,
    ) -> Result<PathBuf, WriteIntoZipError> {
        buf.clear();
        buf.push(base);
        buf.push(path);
        let buf = buf
            .canonicalize()
            .map_err(|_| WriteIntoZipError::ResourceFileNotFound {
                path: buf.to_path_buf(),
            })?;
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
        use std::io::ErrorKind;
        let resource_path =
            extend_resource_file_path_safely(&mut path_buf, resources_directory, path.as_str())?;
        copy(
            &mut File::open(&resource_path).map_err(|err| match err.kind() {
                ErrorKind::NotFound => WriteIntoZipError::ResourceFileNotFound {
                    path: resource_path,
                },
                ErrorKind::PermissionDenied => WriteIntoZipError::ResourceFilePermissionDenied {
                    path: resource_path,
                },
                _ => err.into(),
            })?,
            zip,
        )?;
    }
    Ok(())
}
