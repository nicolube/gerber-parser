use gerber_types::{GCode, DCode, FunctionCode, Command, Unit, CoordinateFormat, Aperture, ApertureMacro, Circle, Rectangular, Polygon, Operation, ExtendedCode, ApertureAttribute, ApertureFunction, FileAttribute, Part, FileFunction, FilePolarity, StepAndRepeat, MacroContent, PolygonPrimitive, OutlinePrimitive, MacroDecimal, ApertureDefinition, CirclePrimitive, VectorLinePrimitive, MacroBoolean, MacroInteger};
use::std::collections::HashMap;
use gerber_parser::error::{GerberParserErrorWithContext, };
use gerber_parser::parser::{parse_gerber, coordinates_from_gerber, coordinates_offset_from_gerber, partial_coordinates_from_gerber};

mod utils;

fn dump_commands(commands: &Vec<Result<Command, GerberParserErrorWithContext>>) {
    println!("{}", commands_to_string(commands));
    println!();
}

fn commands_to_string(commands: &Vec<Result<Command, GerberParserErrorWithContext>>) -> String {
    commands
        .iter()
        .map(|it| format!("{:?}", it))
        .collect::<Vec<_>>()
        .join("\n")
}

/// This macro can be used like assert_eq!(), except that is converts the commands to a string, with newlines between
/// each command and compares those first.
///
/// Doing this makes it easier to see the differences in IDEs like RustRover which provide a 'diff' view
/// for analyzing failures.  Different or missing commands are *much* easier to spot.
///
/// It does two asserts, one for the string (debug) representation of the commands, and one for the commands' values.
macro_rules! assert_eq_commands {
    ($left:expr, $right:expr) => {
        {
            let left = $left;
            let right = $right;
            // ensures the debug representations are the same
            assert_eq!(commands_to_string(&left), commands_to_string(&right));
            // ensures the values are the same
            assert_eq!(left, right);
        }
    };
}

// #[test]
// fn test_full_gerber() {
//     let gerber_reader = utils::gerber_to_reader(&SAMPLE_GERBER_1);
//     let gbr = parse_gerber(gerber_reader);
//     println!("{}",&gbr);
//     assert_eq!(gbr, GerberDoc::new());
// }

#[test]
fn format_specification() {
    let reader_fs_1 = utils::gerber_to_reader("
    %FSLAX15Y15*%
    %MOMM*%
    M02*        
    ");

    let reader_fs_2 = utils::gerber_to_reader("
    %FSLAX36Y36*%
    %MOIN*%
    G04 Actual apertures and draw commands go here*
    M02*        
    ");

    assert_eq!(parse_gerber(reader_fs_1).format_specification, Some(CoordinateFormat::new(1, 5)));

    assert_eq!(parse_gerber(reader_fs_2).format_specification, Some(CoordinateFormat::new(3, 6)));
}

#[test]
fn units() {
    let reader_mm = utils::gerber_to_reader("
    G04 The next line specifies the precision of the units*
    %FSLAX23Y23*%
    G04 The next line specifies the units (inches or mm)*
    %MOMM*%

    G04 Actual apertures and draw commands go here*
    M02*        
    ");

    let reader_in = utils::gerber_to_reader("
    G04 The next line specifies the precision of the units*
    %FSLAX23Y23*%
    G04 The next line specifies the units (inches or mm)*
    %MOIN*%

    G04 Actual apertures and draw commands go here*
    M02*        
    ");

    assert_eq!(parse_gerber(reader_mm).units , Some(Unit::Millimeters));
    assert_eq!(parse_gerber(reader_in).units , Some(Unit::Inches));
}

#[test]
#[allow(non_snake_case)]
fn G04_comments() {
    let reader = utils::gerber_to_reader("
    G04 Comment before typical configuration lines*
    %FSLAX23Y23*%
    %MOMM*%
    G04 And now a comment after them*
    M02*        
    ");

    let filter_commands = |cmds: Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
            Ok(Command::FunctionCode(FunctionCode::GCode(GCode::Comment(_)))) => true,
            _ => false
        }).collect()
    };

    let test_vec: Vec<Result<Command, GerberParserErrorWithContext>> = vec![
        Ok(Command::FunctionCode(FunctionCode::GCode(GCode::Comment("Comment before typical configuration lines".to_string())))),
        Ok(Command::FunctionCode(FunctionCode::GCode(GCode::Comment("And now a comment after them".to_string()))))
    ];

    assert_eq!(filter_commands(parse_gerber(reader).commands), test_vec)
}

#[test]
fn aperture_selection() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%

    %ADD999C, 0.01*%
    %ADD22R, 0.01X0.15*%

    G04 Select some valid apertures*
    D22*
    D999*
    D22*

    M02*        
    ");

    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
            Ok(Command::FunctionCode(FunctionCode::DCode(DCode::SelectAperture(_)))) => true, _ => false}).collect()};

    assert_eq!(filter_commands(parse_gerber(reader).commands), vec![
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::SelectAperture(22.into())))),
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::SelectAperture(999.into())))),
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::SelectAperture(22.into()))))])
}

/// Test the D01* statements (linear)
#[test]
#[allow(non_snake_case)]
fn D01_interpolation_linear() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%

    %ADD999C, 0.01*%
    D999*

    X4000Y5000D01*
    X0Y0D01*
    X-1000Y-30000D01*

    M02*        
    ");

    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
            Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(_, _))))) => true, _ => false}).collect()};

    let fs =  CoordinateFormat::new(2,3);
    assert_eq!(filter_commands(parse_gerber(reader).commands), vec![
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(
            coordinates_from_gerber(4000, 5000, fs), None))))),
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(
            coordinates_from_gerber(0, 0, fs), None))))),
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(
            coordinates_from_gerber(-1000, -30000, fs), None)))))])
}


/// Test the D01* statements (circular)
#[test]
#[allow(non_snake_case)]
fn D01_interpolation_circular() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%

    %ADD999C, 0.01*%
    D999*

    X0Y0D01*
    X-1000Y-30000I200J-5000D01*

    M02*        
    ");

    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
                Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(_, _))))) => true, _ => false}).collect()};

    let fs =  CoordinateFormat::new(2,3);
    assert_eq!(filter_commands(parse_gerber(reader).commands), vec![
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(
            coordinates_from_gerber(0, 0, fs), None))))),
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(
            coordinates_from_gerber(-1000, -30000, fs),
            Some(coordinates_offset_from_gerber(200, -5000, fs)))))))])
}

/// Test the D02* statements 
#[test]
#[allow(non_snake_case)]
fn DO2_move_to_command() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%

    %ADD999C, 0.01*%
    D999*

    X0Y-333D02*
    X300Y300D01*

    X5555Y-12D02*
    X-300Y-300D01*

    M02*        
    ");

    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
                Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Move(_))))) => true, _ => false}).collect()};

    let fs =  CoordinateFormat::new(2,3);
    assert_eq!(filter_commands(parse_gerber(reader).commands), vec![
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Move(
            coordinates_from_gerber(0, -333, fs)))))),
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Move(
            coordinates_from_gerber(5555, -12, fs))))))])
}

/// Test the D03* statements 
#[test]
#[allow(non_snake_case)]
fn DO3_flash_command() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%

    %ADD999C, 0.01*%
    D999*

    X4000Y-5000D03*
    X0Y0D03*

    M02*        
    ");

    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
                Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Flash(_))))) => true, _ => false}).collect()};

    let fs =  CoordinateFormat::new(2,3);
    assert_eq!(filter_commands(parse_gerber(reader).commands), vec![
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Flash(
            coordinates_from_gerber(4000, -5000, fs)))))),
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Flash(
            coordinates_from_gerber(0, 0, fs))))))])
}

/// Gerber spec allows for omitted coordinates. This means that 'X100D03*' and 'Y100D03*' are
/// valid statements.
#[test]
fn omitted_coordinate() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%
    %ADD999C, 0.01*%    
    D999*

    G04 here the last coordinate is (0,0) - by construction*
    Y-3000D03*
    G04 Now we set X=1234, but the client needs to maintain the last Y coordinate, namely -3000*
    X1234D03*

    M02*        
    ");

    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
                Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Flash(_))))) => true, _ => false}).collect()};

    let fs =  CoordinateFormat::new(2,3);
    assert_eq!(filter_commands(parse_gerber(reader).commands), vec![
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Flash(
            partial_coordinates_from_gerber(None, Some(-3000), fs)))))),
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Flash(
            partial_coordinates_from_gerber(Some(1234), None, fs))))))])
}


/// Test Step and Repeat command (%SR*%)
#[test]
fn step_and_repeat() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%

    %ADD999C, 0.01*%
    D999*

    %SRX12Y6I3.33J8.120*%
    X4000Y5000D01*
    X0Y0D01*
    X-1000Y-30000D01*
    %SR*%

    M02*        
    ");

    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
                Ok(Command::ExtendedCode(ExtendedCode::StepAndRepeat(_))) => true, _ => false}).collect()};

    assert_eq!(filter_commands(parse_gerber(reader).commands), vec![
        Ok(Command::ExtendedCode(ExtendedCode::StepAndRepeat(StepAndRepeat::Open{
            repeat_x: 12,
            repeat_y: 6,
            distance_x: 3.33,
            distance_y: 8.12,
        }))),
        Ok(Command::ExtendedCode(ExtendedCode::StepAndRepeat(StepAndRepeat::Close))),
    ])
}


#[test]
fn aperture_definitions() {
    let reader = utils::gerber_to_reader("
    %FSLAX26Y26*%
    %MOMM*%

    G04 Aperture Definitions*
    %ADD999C, 0.01*%
    %ADD22R, 0.01X0.15*%
    %ADD23O, 0.01X0.15*%
    %ADD21P, 0.7X10*%
    %ADD24P, 0.7X10X16.5*%

    G04 Apertures with holes*
    %ADD123C, 0.01X0.003*%
    %ADD124R, 0.1X0.15X0.00001*%
    %ADD125O, 0.1X0.15X0.019*%
    %ADD126P, 1X7X5.5X0.7*%

    M02*        
    ");

    // when
    let doc = parse_gerber(reader);
    println!("{:?}", doc.commands);
    let aperture_definitions = doc.apertures;
    
    // then
    assert_eq!(aperture_definitions,  HashMap::from([
        (999, Aperture::Circle(Circle {diameter: 0.01, hole_diameter: None})),
        (22, Aperture::Rectangle(Rectangular{x: 0.01,y: 0.15,hole_diameter: None})),
        (23, Aperture::Obround(Rectangular{x: 0.01,y: 0.15,hole_diameter: None})),
        (21, Aperture::Polygon(Polygon{diameter: 0.7,vertices: 10,rotation: None, hole_diameter: None})),
        (24, Aperture::Polygon(Polygon{diameter: 0.7,vertices: 10,rotation: Some(16.5),hole_diameter: None})),
        (123, Aperture::Circle(Circle {diameter: 0.01,hole_diameter: Some(0.003)})),
        (124, Aperture::Rectangle(Rectangular {x: 0.1,y: 0.15,hole_diameter: Some(0.00001)})),
        (125, Aperture::Obround(Rectangular {x: 0.1,y: 0.15,hole_diameter: Some(0.019)})),
        (126, Aperture::Polygon(Polygon{diameter: 1.0,vertices: 7,rotation: Some(5.5),hole_diameter: Some(0.7)})),
        ]))
}

// TODO: make more exhaustive
#[test]
#[allow(non_snake_case)]
fn TA_aperture_attributes() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%

    %ADD999C, 0.01*%
    %TA.AperFunction, WasherPad*%
    %TA.AperFunction,Profile*%
    %TA.AperFunction,   Other,   teststring*%

    %TA.DrillTolerance, 0.032, 0.022*%

    M02*        
    ");

    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
            Ok(Command::ExtendedCode(ExtendedCode::ApertureAttribute(_))) => true, _ => false}).collect()};

    assert_eq!(filter_commands(parse_gerber(reader).commands), vec![
        Ok(Command::ExtendedCode(ExtendedCode::ApertureAttribute(ApertureAttribute::ApertureFunction(ApertureFunction::WasherPad)))),
        Ok(Command::ExtendedCode(ExtendedCode::ApertureAttribute(ApertureAttribute::ApertureFunction(ApertureFunction::Profile)))),
        Ok(Command::ExtendedCode(ExtendedCode::ApertureAttribute(ApertureAttribute::ApertureFunction(ApertureFunction::Other("teststring".to_string()))))),
        Ok(Command::ExtendedCode(ExtendedCode::ApertureAttribute(ApertureAttribute::DrillTolerance{ plus: 0.032, minus: 0.022 })))
        ])
}

// TODO: make more exhaustive
#[test]
#[allow(non_snake_case)]
fn TF_file_attributes() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%

    %ADD999C, 0.01*%

    %TF.Part, Array*%
    %TF.Part, Other, funnypartname*%
    %TF.FileFunction, test part*%
    %TF.FilePolarity, Negative*%

    M02*        
    ");

    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
            Ok(Command::ExtendedCode(ExtendedCode::FileAttribute(_))) => true, _ => false}).collect()};

    assert_eq!(filter_commands(parse_gerber(reader).commands), vec![
        Ok(Command::ExtendedCode(ExtendedCode::FileAttribute(FileAttribute::Part(Part::Array)))),
        Ok(Command::ExtendedCode(ExtendedCode::FileAttribute(FileAttribute::Part(Part::Other("funnypartname".to_string()))))),
        Ok(Command::ExtendedCode(ExtendedCode::FileAttribute(FileAttribute::FileFunction(FileFunction::Other("test part".to_string()))))),
        Ok(Command::ExtendedCode(ExtendedCode::FileAttribute(FileAttribute::FilePolarity(FilePolarity::Negative)))),
        ])
}

// #[test]
// // TODO: make more exhaustive
// fn TO_object_attributes() {
//     let reader = utils::gerber_to_reader("
//     %FSLAX23Y23*%
//     %MOMM*%
// 
//     %ADD999C, 0.01*%
// 
//     %TO.DoNotForget, 32, fragile*%
//     %TO.N, 0.22*%
// 
//     M02*        
//     ");
// 
//     let filter_commands = |cmds:Vec<Command>| -> Vec<Command> {
//         cmds.into_iter().filter(|cmd| match cmd {
//                 Command::ExtendedCode(ExtendedCode::ObjectAttribute(_)) => true, _ => false}).collect()};
// 
//     let fs =  CoordinateFormat::new(2,3);
//     assert_eq!(filter_commands(parse_gerber(reader).commands), vec![
//         Command::ExtendedCode(ExtendedCode::ObjectAttribute(ObjectAttribute{
//             attribute_name: "DoNotForget".to_string(),
//             values: vec!["32".to_string(), "fragile".to_string()]
//         })),
//         Command::ExtendedCode(ExtendedCode::ObjectAttribute(ObjectAttribute{
//             attribute_name: "N".to_string(),
//             values: vec!["0.22".to_string()]
//         }))])
// }

#[test]
#[should_panic]
fn conflicting_aperture_codes() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%        
    %ADD24P, 0.7X10X16.5*%
    %ADD39P, 0.7X10X16.5*%
    G04 We cannot use the same code (24) again in the same document*
    %ADD24P, 1X10X20.0*%

    M02*      
    ");
    let guy = parse_gerber(reader);
    assert!(guy.get_errors().is_empty());
}

#[test]
#[should_panic]
fn missing_eof() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%-

    G04 We should have a MO2 at the end, but what if we forget it?*      
    ");
    let guy = parse_gerber(reader);
    assert!(guy.get_errors().is_empty());
}

#[test]
#[should_panic]
fn multiple_unit_statements() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%
    G04 We can only declare the unit type once in a document* 
    %MOIN*%
        
    M02*  
    ");
    let guy = parse_gerber(reader);
    assert!(guy.get_errors().is_empty());
}

#[test]
#[should_panic]
fn multiple_fs_statements() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    G04 We can only declare the format specification once in a document* 
    %FSLAX46Y46*%
    %MOMM*%
        
    M02*  
    ");
    let guy = parse_gerber(reader);
    assert!(guy.get_errors().is_empty());
}

#[test]
#[should_panic]
fn nonexistent_aperture_selection() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%        
    %MOMM*%

    %ADD100P, 0.7X10X16.5*%

    G04 We should not be able to select apertures that are not defined* 
    D
        
    M02*  
    ");
    let guy = parse_gerber(reader);
    assert!(guy.get_errors().is_empty());
}

/// This statement should fail as this is not within the format specification (2 integer, 3 decimal)
#[test]
#[ignore]
#[should_panic]
fn coordinates_not_within_format() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%

    %ADD999C, 0.01*%
    D999*

    X100000Y0D01*

    M02*        
    ");

    let guy = parse_gerber(reader);
    assert!(guy.get_errors().is_empty());
}


/// Test the D* statements, diptrace exports gerber files without the leading `0` on the `D0*` commands. 
#[test]
#[allow(non_snake_case)]
fn diptrace_Dxx_statements() {
    let reader = utils::gerber_to_reader(r#"
    %TF.GenerationSoftware,Novarm,DipTrace,4.3.0.6*%
    %TF.CreationDate,2025-04-22T21:52:19+00:00*%
    %FSLAX35Y35*%
    %MOMM*%
    %TF.FileFunction,Copper,L1,Top*%
    %TF.Part,Single*%
    
    G37*
    G36*
    X2928500Y1670000D2*
    X2998500D1*
    Y1530000D1*
    G37*
    M02*

    "#);

    // when
    let commands = parse_gerber(reader).commands;
    dump_commands(&commands);

    // then
    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
            Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Move(_))))) => true,
            Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(_, _))))) => true,
            _ => false}
        ).collect()};

    let filtered_commands = filter_commands(commands);
    dump_commands(&filtered_commands);

    let fs =  CoordinateFormat::new(3,5);

    assert_eq_commands!(filtered_commands, vec![
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Move(coordinates_from_gerber(2928500, 1670000, fs)))))),
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(partial_coordinates_from_gerber(Some(2998500), None, fs), None))))),
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(partial_coordinates_from_gerber(None, Some(1530000), fs), None))))),
    ]);
}

// See gerber spec 2021-02, section 4.5
#[test]
fn test_outline_macro_and_aperture_definition() {
    // given
    let reader = utils::gerber_to_reader(r#"
    %TF.GenerationSoftware,Novarm,DipTrace,4.3.0.6*%
    %TF.CreationDate,2025-04-24T12:32:15+00:00*%
    %FSLAX35Y35*%
    %MOMM*%
    %TF.FileFunction,Copper,L1,Top*%
    %TF.Part,Single*%
    %AMOUTLINE0*
    4,1,5,
    -0.40659,0.19445,
    -0.26517,0.33588,
    -0.12374,0.33588,
    0.40659,-0.19445,
    0.19445,-0.40659,
    -0.40659,0.19445,
    0*%
    %ADD17OUTLINE0*%
    "#);
    
    // Note, it's important that the macro definition isn't the only command in the file because it's important that
    // the macro parser finds the end of the macro definition correctly and that it doesn't consume additional lines.
    
    let expected_macro: ApertureMacro = ApertureMacro {
        name: "OUTLINE0".to_string(),
        content: vec![MacroContent::Outline(OutlinePrimitive {
            exposure: MacroBoolean::Value(true),  // 1 indicates exposure on
            points: vec![
                (MacroDecimal::Value(-0.40659), MacroDecimal::Value(0.19445)),
                (MacroDecimal::Value(-0.26517), MacroDecimal::Value(0.33588)),
                (MacroDecimal::Value(-0.12374), MacroDecimal::Value(0.33588)),
                (MacroDecimal::Value(0.40659), MacroDecimal::Value(-0.19445)),
                (MacroDecimal::Value(0.19445), MacroDecimal::Value(-0.40659)),
                (MacroDecimal::Value(-0.40659), MacroDecimal::Value(0.19445)),
            ],
            angle: MacroDecimal::Value(0.0),
        })],
    };

    // when
    let commands = parse_gerber(reader).commands;
    dump_commands(&commands);

    // then
    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
            Ok(Command::ExtendedCode(ExtendedCode::ApertureMacro(_))) => true,
            Ok(Command::ExtendedCode(ExtendedCode::ApertureDefinition(_))) => true,
            _ => false}
        ).collect()};

    let filtered_commands = filter_commands(commands);
    dump_commands(&filtered_commands);

    assert_eq_commands!(filtered_commands, vec![
        Ok(Command::ExtendedCode(ExtendedCode::ApertureMacro(expected_macro))),
        Ok(Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition::new(
            17,
            Aperture::Macro("OUTLINE0".to_string(), None)
        )))),
    ]);
}

// See gerber spec 2021-02, section 4.5
#[test]
fn test_polygon_macro_and_aperture_definition() {
    // given
    let reader = utils::gerber_to_reader(r#"
    %FSLAX36Y36*%
    %MOMM*%
    %AMDIAMOND*
    5,1,4,0,0,1,45*%
    %ADD10DIAMOND*%
    %LPD*%
    G75*
    G54D10*
    X0Y0D03*
    X5000000Y0D03*
    X2500000Y2500000D03*
    M02*
    "#);
    
    // Note, it's important that the macro definition isn't the only command in the file because it's important that
    // the macro parser finds the end of the macro definition correctly and that it doesn't consume additional lines.

    let expected_macro: ApertureMacro = ApertureMacro {
        name: "DIAMOND".to_string(),
        content: vec![MacroContent::Polygon(PolygonPrimitive {
            exposure: MacroBoolean::Value(true),  // 1 indicates exposure on
            vertices: MacroInteger::Value(4),     // diamond has 4 vertices
            center: (MacroDecimal::Value(0.0), MacroDecimal::Value(0.0)),  // centered at origin
            diameter: MacroDecimal::Value(1.0),  // diameter of circumscribed circle
            angle: MacroDecimal::Value(45.0),  // 45 degree rotation
        })],
    };

    // when
    let commands = parse_gerber(reader).commands;
    dump_commands(&commands);

    // then
    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
            Ok(Command::ExtendedCode(ExtendedCode::ApertureMacro(_))) => true,
            Ok(Command::ExtendedCode(ExtendedCode::ApertureDefinition(_))) => true,
            _ => false}
        ).collect()};

    let filtered_commands = filter_commands(commands);
    dump_commands(&filtered_commands);

    assert_eq_commands!(filtered_commands, vec![
        Ok(Command::ExtendedCode(ExtendedCode::ApertureMacro(expected_macro))),
        Ok(Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition::new(
            10,
            Aperture::Macro("DIAMOND".to_string(), None)
        )))),
    ]);
}


/// Diptrace 4.3 generates operation commands without a leading `0` on the `D0*` commands. 
#[test]
fn test_standalone_d_commands() {
    let reader = utils::gerber_to_reader(r#"
%FSLAX35Y35*%
%MOMM*%
%ADD10C,0.25*%
D10*
G01*

X10000Y10000D2*
D1*
X20000D1*
D3*
Y20000D1*
D3*
X10000D1*
D3*
Y10000D1*
D3*
M02*
"#);

    // when
    let commands = parse_gerber(reader).commands;
    dump_commands(&commands);

    // then
    let filter_commands = |cmds: Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
            Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(_)))) => true,
            _ => false
        }).collect()
    };

    let filtered_commands = filter_commands(commands);
    dump_commands(&filtered_commands);

    let fs = CoordinateFormat::new(3, 5);

    assert_eq_commands!(filtered_commands, vec![
        // Initial move
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Move(coordinates_from_gerber(10000, 10000, fs)))))),
        // D1 with no coordinates - uses previous position
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(partial_coordinates_from_gerber(None, None, fs), None))))),
        // X20000D1 - keeps previous Y coordinate
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(partial_coordinates_from_gerber(Some(20000), None, fs), None))))),
        // D3 with no coordinates - uses previous position
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Flash(partial_coordinates_from_gerber(None, None, fs)))))),
        // Y20000D1 - keeps previous X coordinate
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(partial_coordinates_from_gerber(None, Some(20000), fs), None))))),
        // D3 with no coordinates - uses previous position
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Flash(partial_coordinates_from_gerber(None, None, fs)))))),
        // X10000D1 - keeps previous Y coordinate
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(partial_coordinates_from_gerber(Some(10000), None, fs), None))))),
        // D3 with no coordinates - uses previous position
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Flash(partial_coordinates_from_gerber(None, None, fs)))))),
        // Y10000D1 - keeps previous X coordinate
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Interpolate(partial_coordinates_from_gerber(None, Some(10000), fs), None))))),
        // D3 with no coordinates - uses previous position
        Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(Operation::Flash(partial_coordinates_from_gerber(None, None, fs))))))
    ]);
}

#[test]
#[allow(non_snake_case)]
fn test_kicad_macro() {
    let reader = utils::gerber_to_reader(r#"
%TF.GenerationSoftware,KiCad,Pcbnew,8.0.3*%
%TF.CreationDate,2025-04-28T16:25:44+02:00*%
%TF.ProjectId,SPRacingRXN1-RevB-20240507-1510,53505261-6369-46e6-9752-584e312d5265,rev?*%
%TF.SameCoordinates,Original*%
%TF.FileFunction,Copper,L1,Top*%
%TF.FilePolarity,Positive*%
%FSLAX46Y46*%
G04 Gerber Fmt 4.6, Leading zero omitted, Abs format (unit mm)*
G04 Created by KiCad (PCBNEW 8.0.3) date 2025-04-28 16:25:44*
%MOMM*%
%LPD*%
G01*
G04 APERTURE LIST*
G04 Aperture macros list*
%AMRoundRect*
0 Rectangle with rounded corners*
0 $1 Rounding radius*
0 $2 $3 $4 $5 $6 $7 $8 $9 X,Y pos of 4 corners*
0 Add a 4 corners polygon primitive as box body*
4,1,4,$2,$3,$4,$5,$6,$7,$8,$9,$2,$3,0*
0 Add four circle primitives for the rounded corners*
1,1,$1+$1,$2,$3*
1,1,$1+$1,$4,$5*
1,1,$1+$1,$6,$7*
1,1,$1+$1,$8,$9*
0 Add four rect primitives between the rounded corners*
20,1,$1+$1,$2,$3,$4,$5,0*
20,1,$1+$1,$4,$5,$6,$7,0*
20,1,$1+$1,$6,$7,$8,$9,0*
20,1,$1+$1,$8,$9,$2,$3,0*%

G04 Aperture macros list end*
%ADD36RoundRect,0.110250X0.114771X-0.214739X0.114729X0.214761X-0.114771X0.214739X-0.114729X-0.214761X0*%
D36*
X155251142Y-100803551D03*
M02*
    "#);

    // Note the comment "0 Add a 4 corners polygon primitive as box body" is wrong (Kicad 8.0)
    // and should read "0 Add a 4-corner outline primitive as box body" (code 4 = outline)

    // Note the comment "Add four rect primitives between the rounded corners" is wrong (Kicad 8.0)
    // and should read "Add four vector-line primitives between the rounded corners" (code 20 = vector-line)

    // FUTURE raise the above 2 issues with this in the KiCad issue tracker, add a link here.

    // Note, it's important that the macro definition isn't the only command in the file because it's important that
    // the macro parser finds the end of the macro definition correctly and that it doesn't consume additional lines.

    let expected_macro_1: ApertureMacro = ApertureMacro {
        name: "RoundRect".to_string(),
        content: vec![
            MacroContent::Comment("Rectangle with rounded corners".to_string()),
            MacroContent::Comment("$1 Rounding radius".to_string()),
            MacroContent::Comment("$2 $3 $4 $5 $6 $7 $8 $9 X,Y pos of 4 corners".to_string()),
            MacroContent::Comment("Add a 4 corners polygon primitive as box body".to_string()),
            MacroContent::Outline(OutlinePrimitive {
                exposure: MacroBoolean::Value(true),
                points: vec![
                    (MacroDecimal::Variable(2), MacroDecimal::Variable(3)),
                    (MacroDecimal::Variable(4), MacroDecimal::Variable(5)),
                    (MacroDecimal::Variable(6), MacroDecimal::Variable(7)),
                    (MacroDecimal::Variable(8), MacroDecimal::Variable(9)),
                    (MacroDecimal::Variable(2), MacroDecimal::Variable(3)),
                ],
                angle: MacroDecimal::Value(0.0),
            }),
            MacroContent::Comment("Add four circle primitives for the rounded corners".to_string()),
            MacroContent::Circle(CirclePrimitive {
                exposure: MacroBoolean::Value(true),
                diameter: MacroDecimal::Expression("$1+$1".to_string()),
                center: (MacroDecimal::Variable(2), MacroDecimal::Variable (3)),
                angle: None,
            }),
            MacroContent::Circle(CirclePrimitive {
                exposure: MacroBoolean::Value(true),
                diameter: MacroDecimal::Expression("$1+$1".to_string()),
                center: (MacroDecimal::Variable(4), MacroDecimal::Variable (5)),
                angle: None,
            }),
            MacroContent::Circle(CirclePrimitive {
                exposure: MacroBoolean::Value(true),
                diameter: MacroDecimal::Expression("$1+$1".to_string()),
                center: (MacroDecimal::Variable(6), MacroDecimal::Variable (7)),
                angle: None,
            }),
            MacroContent::Circle(CirclePrimitive {
                exposure: MacroBoolean::Value(true),
                diameter: MacroDecimal::Expression("$1+$1".to_string()),
                center: (MacroDecimal::Variable(8), MacroDecimal::Variable (9)),
                angle: None,
            }),
            MacroContent::Comment("Add four rect primitives between the rounded corners".to_string()),
            MacroContent::VectorLine(VectorLinePrimitive {
                exposure: MacroBoolean::Value(true),
                width: MacroDecimal::Expression("$1+$1".to_string()),
                start: (MacroDecimal::Variable(2), MacroDecimal::Variable (3)),
                end: (MacroDecimal::Variable(4), MacroDecimal::Variable (5)),
                angle: MacroDecimal::Value(0.0),
            }),
            MacroContent::VectorLine(VectorLinePrimitive {
                exposure: MacroBoolean::Value(true),
                width: MacroDecimal::Expression("$1+$1".to_string()),
                start: (MacroDecimal::Variable(4), MacroDecimal::Variable (5)),
                end: (MacroDecimal::Variable(6), MacroDecimal::Variable (7)),
                angle: MacroDecimal::Value(0.0),
            }),
            MacroContent::VectorLine(VectorLinePrimitive {
                exposure: MacroBoolean::Value(true),
                width: MacroDecimal::Expression("$1+$1".to_string()),
                start: (MacroDecimal::Variable(6), MacroDecimal::Variable (7)),
                end: (MacroDecimal::Variable(8), MacroDecimal::Variable (9)),
                angle: MacroDecimal::Value(0.0),
            }),
            MacroContent::VectorLine(VectorLinePrimitive {
                exposure: MacroBoolean::Value(true),
                width: MacroDecimal::Expression("$1+$1".to_string()),
                start: (MacroDecimal::Variable(8), MacroDecimal::Variable (9)),
                end: (MacroDecimal::Variable(2), MacroDecimal::Variable (3)),
                angle: MacroDecimal::Value(0.0),
            }),
        ],
    };

    // when
    let commands = parse_gerber(reader).commands;
    dump_commands(&commands);

    // then
    let filter_commands = |cmds:Vec<Result<Command, GerberParserErrorWithContext>>| -> Vec<Result<Command, GerberParserErrorWithContext>> {
        cmds.into_iter().filter(|cmd| match cmd {
            Ok(Command::ExtendedCode(ExtendedCode::ApertureMacro(_))) => true,
            Ok(Command::ExtendedCode(ExtendedCode::ApertureDefinition(_))) => true,
            _ => false}
        ).collect()};

    let filtered_commands = filter_commands(commands);
    dump_commands(&filtered_commands);

    assert_eq_commands!(filtered_commands, vec![
        Ok(Command::ExtendedCode(ExtendedCode::ApertureMacro(expected_macro_1))),
        Ok(Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition::new(
            36,
            Aperture::Macro("RoundRect".to_string(), Some(vec![
                MacroDecimal::Value(0.110250),
                MacroDecimal::Value(0.114771),
                MacroDecimal::Value(-0.214739),
                MacroDecimal::Value(0.114729),
                MacroDecimal::Value(0.214761),
                MacroDecimal::Value(-0.114771),
                MacroDecimal::Value(0.214739),
                MacroDecimal::Value(-0.114729),
                MacroDecimal::Value(-0.214761),
                MacroDecimal::Value(0.0),
            ]))
        )))),
    ]);
}
