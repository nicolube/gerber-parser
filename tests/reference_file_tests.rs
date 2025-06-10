use gerber_parser::parse;
mod utils;

/// All files in these tests are taken from the Ucamco 20220409 file format examples
/// downloaded from https://www.ucamco.com/en/gerber/downloads on 20220628
///
/// Some files have some slight edits made to ensure the gbr -> rust -> gbr works
/// But these are ones that do not change the meaning of the file.

#[test]
fn two_square_boxes_to_rust() {
    // unmodified reference file, purely to check for panics
    let gbr_string = include_str!("../assets/reference_files/two_square_boxes.gbr");
    let reader = utils::gerber_to_reader(gbr_string);
    parse(reader).unwrap();
}

#[test]
fn two_square_boxes_to_rust_and_back() {
    let gbr_string = include_str!("../assets/reference_files/two_square_boxes.gbr");
    let reader = utils::gerber_to_reader(gbr_string);
    let doc = parse(reader).unwrap();

    assert_eq!(
        utils::gerber_doc_as_str(&doc),
        gbr_string,
        "unexpected differences, commands: {:?}",
        doc.commands
    )
}

#[test]
fn polarities_and_apertures_to_rust() {
    // unmodified reference file, purely to check for panics
    let gbr_string = include_str!("../assets/reference_files/polarities_and_apertures.gbr");
    let reader = utils::gerber_to_reader(gbr_string);
    parse(reader).unwrap();
}

// Remaining issues:
// 1. Not parsed:
// ```
// %AMTHERMAL80*
// 7,0,0,0.800,0.550,0.125,45*%
// ```
// 2. Trailing/Leading zeros not retained
#[test]
#[ignore]
fn polarities_and_apertures_to_rust_and_back() {
    let gbr_string = include_str!("../assets/reference_files/polarities_and_apertures.gbr");
    let reader = utils::gerber_to_reader(gbr_string);
    let doc = parse(reader).unwrap();

    assert_eq!(
        utils::gerber_doc_as_str(&doc),
        gbr_string,
        "unexpected differences, commands: {:?}",
        doc.commands
    )
}

#[test]
// unmodified reference file, purely to check for panics
fn a_drill_file_to_rust() {
    let gbr_string = "   
    ";
    let reader = utils::gerber_to_reader(gbr_string);
    parse(reader).unwrap();
}

// Remaining issues:
// 1. Trailing/Leading zeros not retained
#[test]
#[ignore]
fn a_drill_file_to_rust_and_back() {
    let gbr_string = include_str!("../assets/reference_files/drill_file.gbr");
    let reader = utils::gerber_to_reader(gbr_string);
    let doc = parse(reader).unwrap();

    assert_eq!(
        utils::gerber_doc_as_str(&doc),
        gbr_string,
        "unexpected differences, commands: {:?}",
        doc.commands
    )
}
