use gerber_parser::parse;
use gerber_parser::util::{gerber_doc_as_str, gerber_to_reader};

/// All files in these tests are taken from the Ucamco 20220409 file format examples
/// downloaded from https://www.ucamco.com/en/gerber/downloads on 20220628
///
/// Some files have some slight edits made to ensure the gbr -> rust -> gbr works
/// correctly. The changes do not change the meaning of the file.
/// The edited files are in the `edited` folder so they can be compared to the originals.
/// For example, when gerber-types serializes decimals (not co-ordinates), trailing 0's are removed.

#[test]
fn two_square_boxes_to_rust() {
    // unmodified reference file, purely to check for panics
    let gbr_string = include_str!("../assets/reference_files/two_square_boxes.gbr");
    let reader = gerber_to_reader(gbr_string);
    parse(reader).unwrap();
}

#[test]
fn two_square_boxes_to_rust_and_back() {
    let gbr_string = include_str!("../assets/reference_files/two_square_boxes.gbr");
    let reader = gerber_to_reader(gbr_string);
    let doc = parse(reader).unwrap();

    assert_eq!(
        gerber_doc_as_str(&doc),
        gbr_string,
        "unexpected differences, commands: {:?}",
        doc.commands
    )
}

#[test]
fn polarities_and_apertures_to_rust() {
    // unmodified reference file, purely to check for panics
    let gbr_string = include_str!("../assets/reference_files/polarities_and_apertures.gbr");
    let reader = gerber_to_reader(gbr_string);
    parse(reader).unwrap();
}

#[test]
fn polarities_and_apertures_to_rust_and_back() {
    let gbr_string = include_str!("../assets/reference_files/edited/polarities_and_apertures.gbr");
    let reader = gerber_to_reader(gbr_string);
    let doc = parse(reader).unwrap();

    assert_eq!(
        gerber_doc_as_str(&doc),
        gbr_string,
        "unexpected differences, commands: {:?}",
        doc.commands
    )
}

#[test]
// unmodified reference file, purely to check for panics
fn a_drill_file_to_rust() {
    let gbr_string = include_str!("../assets/reference_files/drill_file.gbr");
    let reader = gerber_to_reader(gbr_string);
    parse(reader).unwrap();
}

#[test]
fn a_drill_file_to_rust_and_back() {
    let gbr_string = include_str!("../assets/reference_files/edited/drill_file.gbr");
    let reader = gerber_to_reader(gbr_string);
    let doc = parse(reader).unwrap();

    assert_eq!(
        gerber_doc_as_str(&doc),
        gbr_string,
        "unexpected differences, commands: {:?}",
        doc.commands
    )
}
