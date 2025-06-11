use crate::GerberDoc;
use gerber_types::GerberCode;
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
    gerber_doc.as_commands().iter().for_each(|command| {
        command.serialize(&mut filevec).unwrap();
    });
    str::from_utf8(&filevec).unwrap().to_string()
}
