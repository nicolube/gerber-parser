use gerber_parser::GerberDoc;
use gerber_types::GerberCode;
use std::io::BufReader;
use std::str;
use stringreader::StringReader;

#[must_use]
pub fn gerber_to_reader(gerber_string: &str) -> BufReader<StringReader> {
    BufReader::new(StringReader::new(gerber_string))
}

#[must_use]
#[allow(dead_code)]
pub fn gerber_doc_to_str(gerber_doc: GerberDoc) -> String {
    let mut filevec = Vec::<u8>::new();
    // we use the serialisation methods of the gerber-types crate
    gerber_doc.into_commands().serialize(&mut filevec).unwrap();
    str::from_utf8(&filevec).unwrap().to_string()
}
