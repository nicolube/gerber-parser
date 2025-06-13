use crate::{ContentError, GerberDoc};
use gerber_types::{
    CoordinateFormat, CoordinateNumber, CoordinateOffset, Coordinates, GerberCode, GerberError,
};
use std::io::BufReader;
use std::str;
use std::str::Chars;

#[must_use]
pub fn gerber_to_reader(gerber_string: &str) -> BufReader<&[u8]> {
    let bytes = gerber_string.as_bytes();
    BufReader::new(bytes)
}

#[must_use]
pub fn gerber_doc_to_str(gerber_doc: GerberDoc) -> String {
    let mut filevec = Vec::<u8>::new();
    // we use the serialisation methods of the gerber-types crate
    gerber_doc.into_commands().serialize(&mut filevec).unwrap();
    str::from_utf8(&filevec).unwrap().to_string()
}

#[must_use]
pub fn gerber_doc_as_str(gerber_doc: &GerberDoc) -> String {
    let mut filevec = Vec::<u8>::new();
    // we use the serialisation methods of the gerber-types crate
    gerber_doc.commands().iter().for_each(|command| {
        command.serialize(&mut filevec).unwrap();
    });
    str::from_utf8(&filevec).unwrap().to_string()
}

/// Extract the individual elements (AttributeName and Fields) from Chars
///
/// Gerber spec 2024.05 5.1 Attributes overview:
/// "In accordance with the general rule in 3.4.3 standard attribute names must begin with a dot ‘.’
/// while user attribute names cannot begin with a dot."
///
/// The arguments of the attribute statement can have whitespace as this will be trimmed.
/// `attribute_chars` argument must be the **partial line** from the gerber file
/// with the **first three characters removed**, but with the end-of-line characters still in place.
/// E.g. ".Part,single*%" not "%TF.Part,single*%" or ".Part,single"
///
/// If the end-of-line characters '*%' are not present an error is returned.
/// ```
/// use gerber_parser::util::attr_args;
/// let attribute_chars = ".DrillTolerance, 0.02, 0.01 *%".chars();
///
/// let arguments = attr_args(attribute_chars).unwrap();
/// assert_eq!(arguments, vec![".DrillTolerance","0.02","0.01"])
/// ```
pub fn attr_args(mut partial_line: Chars) -> Result<Vec<&str>, ContentError> {
    let last = partial_line.next_back();
    let second_last = partial_line.next_back();

    match (second_last, last) {
        (Some('*'), Some('%')) => Ok(partial_line
            .as_str()
            .split(',')
            .map(|el| el.trim())
            .collect()),
        _ => Err(ContentError::InvalidFileAttribute {
            file_attribute: partial_line.as_str().to_string(),
        }),
    }
}

#[cfg(test)]
mod attr_args_tests {
    use super::*;

    #[test]
    pub fn test_attr_args() {
        let attribute_chars = ".DrillTolerance, 0.02, 0.01 *%".chars();
        let arguments = attr_args(attribute_chars).unwrap();
        println!("arguments: {:?}", arguments);
        assert_eq!(arguments, vec![".DrillTolerance", "0.02", "0.01"])
    }

    #[test]
    pub fn test_no_trailing_eol_chars() {
        let attribute_chars = ".DrillTolerance, 0.02, 0.01 ".chars();
        let result = attr_args(attribute_chars);
        println!("result: {:?}", result);
        assert!(result.is_err());
    }
}

pub fn coordinates_from_gerber(
    mut x_as_int: i64,
    mut y_as_int: i64,
    fs: CoordinateFormat,
) -> Result<Coordinates, GerberError> {
    // we have the raw gerber string as int but now have to convert it to nano precision format
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    x_as_int *= 10i64.pow(factor);
    y_as_int *= 10i64.pow(factor);
    Ok(Coordinates::new(
        CoordinateNumber::new(x_as_int).validate(&fs)?,
        CoordinateNumber::new(y_as_int).validate(&fs)?,
        fs,
    ))
}

pub fn partial_coordinates_from_gerber(
    x_as_int: Option<i64>,
    y_as_int: Option<i64>,
    fs: CoordinateFormat,
) -> Result<Coordinates, GerberError> {
    // we have the raw gerber string as int but now have to convert it to nano precision format
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    let x = x_as_int
        .map(|value| CoordinateNumber::new(value * 10i64.pow(factor)).validate(&fs))
        .transpose()?;
    let y = y_as_int
        .map(|value| CoordinateNumber::new(value * 10i64.pow(factor)).validate(&fs))
        .transpose()?;

    Ok(Coordinates::new(x, y, fs))
}

pub fn coordinates_offset_from_gerber(
    mut x_as_int: i64,
    mut y_as_int: i64,
    fs: CoordinateFormat,
) -> Result<CoordinateOffset, GerberError> {
    // we have the raw gerber string as int but now have to convert it to nano precision format
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    x_as_int *= 10i64.pow(factor);
    y_as_int *= 10i64.pow(factor);
    Ok(CoordinateOffset::new(
        CoordinateNumber::new(x_as_int).validate(&fs)?,
        CoordinateNumber::new(y_as_int).validate(&fs)?,
        fs,
    ))
}
