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

pub fn coordinates_from_gerber(
    mut x_as_int: i64,
    mut y_as_int: i64,
    fs: CoordinateFormat,
) -> Result<Option<Coordinates>, GerberError> {
    // we have the raw gerber string as int but now have to convert it to nano precision format
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    x_as_int *= 10i64.pow(factor);
    y_as_int *= 10i64.pow(factor);
    Ok(Some(Coordinates::new(
        CoordinateNumber::new(x_as_int).validate(&fs)?,
        CoordinateNumber::new(y_as_int).validate(&fs)?,
        fs,
    )))
}

pub fn partial_coordinates_from_gerber(
    x_as_int: Option<i64>,
    y_as_int: Option<i64>,
    fs: CoordinateFormat,
) -> Result<Option<Coordinates>, GerberError> {
    // we have the raw gerber string as int but now have to convert it to nano precision format
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    let x = x_as_int
        .map(|value| CoordinateNumber::new(value * 10i64.pow(factor)).validate(&fs))
        .transpose()?;
    let y = y_as_int
        .map(|value| CoordinateNumber::new(value * 10i64.pow(factor)).validate(&fs))
        .transpose()?;

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
    // we have the raw gerber string as int but now have to convert it to nano precision format
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    let x = x_as_int
        .map(|value| CoordinateNumber::new(value * 10i64.pow(factor)).validate(&fs))
        .transpose()?;
    let y = y_as_int
        .map(|value| CoordinateNumber::new(value * 10i64.pow(factor)).validate(&fs))
        .transpose()?;

    let coordinate_offset = match (x, y) {
        (None, None) => None,
        (x, y) => Some(CoordinateOffset::new(x, y, fs)),
    };
    Ok(coordinate_offset)
}
