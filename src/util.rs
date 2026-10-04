use crate::GerberDoc;
use gerber_types::{
    CoordinateFormat, CoordinateNumber, CoordinateOffset, Coordinates, GerberCode, GerberError,
};
use std::io::BufReader;
use std::str;

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

/// Converts a raw Gerber integer in format `fs` to the nano (6-decimal)
/// precision `CoordinateNumber` uses, rejecting formats with more than 6
/// decimals and values that overflow instead of panicking.
fn to_nano(value: i64, fs: &CoordinateFormat) -> Result<CoordinateNumber, GerberError> {
    let factor = 6u8.checked_sub(fs.decimal).ok_or_else(|| {
        GerberError::CoordinateFormatError(format!(
            "{} decimal places are more than the supported 6",
            fs.decimal
        ))
    })?;
    let nano = value
        .checked_mul(10i64.pow(factor as u32))
        .ok_or_else(|| GerberError::RangeError(format!("coordinate {value} is too large")))?;
    CoordinateNumber::new(nano).validate(fs)
}

pub fn coordinates_from_gerber(
    x_as_int: i64,
    y_as_int: i64,
    fs: CoordinateFormat,
) -> Result<Option<Coordinates>, GerberError> {
    Ok(Some(Coordinates::new(
        to_nano(x_as_int, &fs)?,
        to_nano(y_as_int, &fs)?,
        fs,
    )))
}

pub fn partial_coordinates_from_gerber(
    x_as_int: Option<i64>,
    y_as_int: Option<i64>,
    fs: CoordinateFormat,
) -> Result<Option<Coordinates>, GerberError> {
    let x = x_as_int.map(|value| to_nano(value, &fs)).transpose()?;
    let y = y_as_int.map(|value| to_nano(value, &fs)).transpose()?;

    let coordinates = match (x, y) {
        (None, None) => None,
        (x, y) => Some(Coordinates::new(x, y, fs)),
    };
    Ok(coordinates)
}

pub fn partial_coordinates_offset_from_gerber(
    x_as_int: Option<i64>,
    y_as_int: Option<i64>,
    fs: CoordinateFormat,
) -> Result<Option<CoordinateOffset>, GerberError> {
    let x = x_as_int.map(|value| to_nano(value, &fs)).transpose()?;
    let y = y_as_int.map(|value| to_nano(value, &fs)).transpose()?;

    let coordinate_offset = match (x, y) {
        (None, None) => None,
        (x, y) => Some(CoordinateOffset::new(x, y, fs)),
    };
    Ok(coordinate_offset)
}
