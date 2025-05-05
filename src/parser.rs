use std::io::{Read, BufReader, BufRead, Lines};

use crate::gerber_types::{Command, ExtendedCode, Unit, FunctionCode, GCode, CoordinateFormat, Aperture, ApertureMacro, Circle, Rectangular, Polygon, MCode, DCode, Polarity, InterpolationMode, QuadrantMode, Operation, Coordinates, CoordinateNumber, CoordinateOffset, ApertureAttribute, ApertureFunction, FiducialScope, SmdPadType, FileAttribute, FilePolarity, Part, FileFunction, StepAndRepeat, MacroDecimal, OutlinePrimitive, MacroContent, PolygonPrimitive, MacroBoolean, MacroInteger, CirclePrimitive, VectorLinePrimitive, CenterLinePrimitive};
use regex::Regex;
use std::str::Chars;
use crate::error::GerberParserError;
use crate::gerber_doc::GerberDoc;
use lazy_regex::*;


// naively define some regex terms
// FUTURE investigate which ones can be done without regex for better performance.
// FUTURE using named captures in all regular expressions would be nice, this would enable the use of string
//        constants for the capture names, this would improve the error messages too.
static RE_UNITS: Lazy<Regex> = lazy_regex!(r"%MO(.*)\*%");
static RE_COMMENT: Lazy<Regex> = lazy_regex!(r"G04 (.*)\*");
static RE_FORMAT_SPEC: Lazy<Regex> = lazy_regex!(r"%FSLAX(.*)Y(.*)\*%");

/// https://regex101.com/r/YNnrmK/1
static RE_APERTURE: Lazy<Regex> = lazy_regex!(r"%ADD([0-9]+)([._$a-zA-Z][._$a-zA-Z0-9]{0,126})(?:,\s?(.*))?\*%");
static RE_INTERPOLATION: Lazy<Regex> = lazy_regex!(r"X?(-?[0-9]+)?Y?(-?[0-9]+)?I?(-?[0-9]+)?J?(-?[0-9]+)?D(0)?1\*");
static RE_MOVE_OR_FLASH: Lazy<Regex> = lazy_regex!(r"X?(-?[0-9]+)?Y?(-?[0-9]+)?D(0)?[2-3]*");
static RE_IMAGE_NAME: Lazy<Regex> = lazy_regex!(r"%IN(.*)\*%");
static RE_STEP_REPEAT: Lazy<Regex> = lazy_regex!(r"%SRX([0-9]+)Y([0-9]+)I(\d+\.?\d*)J(\d+\.?\d*)\*%");
static RE_MACRO_UNSIGNED_INTEGER: Lazy<Regex> = lazy_regex!(r"(?P<value>[0-9]+)|(?P<variable>^\$[0-9]+$)|(?P<expression>.*)");
static RE_MACRO_BOOLEAN: Lazy<Regex> = lazy_regex!(r"(?P<value>0|1)|(?P<variable>^\$[0-9]+$)|(?P<expression>.*)");
static RE_MACRO_DECIMAL: Lazy<Regex> = lazy_regex!(r"(?P<value>[+-]?[0-9]+(?:\.[0-9]*)?)|(?P<variable>^\$[0-9]+$)|(?P<expression>.*)");

struct ParserContext<T: Read> {
    line_number: usize,
    lines: Lines<BufReader<T>>,
}

impl<T: Read> ParserContext<T> {
    pub fn new(lines: Lines<BufReader<T>>) -> ParserContext<T> {
        ParserContext {
            line_number: 0,
            lines,
        }
    }

    pub fn next(&mut self) -> Option<Result<String, GerberParserError>> {
        let line = self.lines.next();
        if line.is_some() {
            self.line_number += 1;
        }
        line
            .map(|result|{
                result.map_err(|e|GerberParserError::IoError(format!("IO error on line: {}, error: {}", self.line_number, e).to_string()))
            })
    }
}

// TODO change the API so it doesn't panic on IO errors, i.e. replace this with `parse_gerber_inner`
pub fn parse_gerber<T: Read>(reader: BufReader<T>) -> GerberDoc {
    parse_gerber_inner(reader).unwrap()
}

// Parse a gerber string (in BufReader) to a GerberDoc
///
/// Take the contents of a Gerber (.gbr) file and parse it to a GerberDoc struct. The parsing does
/// some semantic checking, but is certainly not exhaustive - so don't rely on it to check if
/// your Gerber file is valid according to the spec. Some of the parsing steps are greedy - they may
/// match something unexpected (rather than panicking) if there is a typo/fault in your file.
fn parse_gerber_inner<T: Read>(reader: BufReader<T>) -> Result<GerberDoc, GerberParserError> {
    let mut gerber_doc = GerberDoc::new();

    let mut parser_context = ParserContext::new(reader.lines());

    loop {
        let Some(line_result) = parser_context.next() else {
            break
        };
        
        let line_number = parser_context.line_number;
        
        let raw_line = line_result?;
        // TODO combine this with line above
        let line = raw_line.trim();

        // Show the line 
        //log::debug!("{}. {}", index + 1, &line);
        
        if !line.is_empty() {
            let line_result = match parse_line(line, &mut gerber_doc, &mut parser_context) {
                Ok(command) => {
                    log::debug!("Found command: {:?}", command);
                    Ok(command)
                }
                Err(error_without_context) => {
                    let contexted_error = error_without_context
                        .to_with_context(Some(line.to_string()), Some(line_number));
                    log::warn!("{}", contexted_error);
                    Err(contexted_error)
                }
            };
            gerber_doc.commands.push(line_result);
        }
    }
    
    match gerber_doc.commands.last(){
        None => {gerber_doc.commands.push(Err(GerberParserError::NoEndOfFile.to_with_context(None, None)))}
        Some(command) => {
            match command{
                Ok(Command::FunctionCode(FunctionCode::MCode(MCode::EndOfFile))) => {}
                _ => {gerber_doc.commands.push(Err(GerberParserError::NoEndOfFile.to_with_context(None, None)))}
            }
        }
    }
    return Ok(gerber_doc)
}


fn parse_line<T: Read>(line: &str,
              gerber_doc: &mut GerberDoc,
              parser_context: &mut ParserContext<T>
) -> Result<Command, GerberParserError> {
    let mut linechars = line.chars();

    match linechars.next().unwrap() { // Safety: already explicitly checked that the line is not empty
        'G' => {
            match linechars.next().ok_or(GerberParserError::UnknownCommand{})? {
                '0' =>  match linechars.next().ok_or(GerberParserError::UnknownCommand{})? {
                    '1' => {// G01
                        Ok(FunctionCode::GCode(
                            GCode::InterpolationMode(InterpolationMode::Linear)
                        ).into())
                    },
                    '2' => {// G02
                        Ok(FunctionCode::GCode(
                            GCode::InterpolationMode(InterpolationMode::ClockwiseCircular)
                        ).into())
                    },
                    '3' => {// G03 
                        Ok(FunctionCode::GCode(
                            GCode::InterpolationMode(InterpolationMode::CounterclockwiseCircular)
                        ).into())
                    },
                    '4' => {// G04
                        parse_comment(line)
                    },
                    _ => Err(GerberParserError::UnknownCommand {}),
                },
                '3'=> match linechars.next().ok_or(GerberParserError::UnknownCommand{})? {
                    '6' => Ok(FunctionCode::GCode(GCode::RegionMode(true)).into()), // G36
                    '7' => Ok(FunctionCode::GCode(GCode::RegionMode(false)).into()), // G37
                    _ => Err(GerberParserError::UnknownCommand {}),
                },
                '7' => match linechars.next().ok_or(GerberParserError::UnknownCommand{})? {
                    // the G74 command is technically part of the Deprecated commands
                    '4' => Ok(FunctionCode::GCode(GCode::QuadrantMode(QuadrantMode::Single)).into()), // G74
                    '5' => Ok(FunctionCode::GCode(GCode::QuadrantMode(QuadrantMode::Multi)).into()), // G75
                    _ => Err(GerberParserError::UnknownCommand {}),
                }, 
                _ => Err(GerberParserError::UnknownCommand {}),
            }
        },
        '%' => {
            match linechars.next().ok_or(GerberParserError::UnknownCommand{})? {
                'M' => {
                    match parse_units(line, &gerber_doc){
                        Ok(units) => {
                            gerber_doc.units = Some(units);
                            Ok(Command::ExtendedCode(ExtendedCode::Unit(units)))
                        }
                        Err(e) => Err(e)
                    }
                },
                'F' => {
                    match parse_format_spec(line, &gerber_doc){
                        Ok(format_spec) => {
                            gerber_doc.format_specification = Some(format_spec);
                            Ok(ExtendedCode::CoordinateFormat(format_spec).into())
                        }
                        Err(e) => Err(e)
                    }
                },
                'A' => match linechars.next().ok_or(GerberParserError::UnknownCommand{})? {
                    'D' => {// AD
                        match parse_aperture_defs(line, &gerber_doc){
                            Ok((code, ap)) => {
                                gerber_doc.apertures.insert(code, ap.clone());
                                // Safety: While insert can 'fail' (misbehave) if the key 
                                // already exists, 
                                // `parse_aperture_defs` explicitly checks for this
                                Ok(ExtendedCode::ApertureDefinition(
                                    gerber_types::ApertureDefinition::new(code, ap)
                                ).into())
                            }
                            Err(err) => Err(err)
                        }
                    }, 
                    'M' => {
                        match parse_aperture_macro_definition(line, parser_context) {
                            Ok(macro_def) => {
                                Ok(Command::ExtendedCode(ExtendedCode::ApertureMacro(macro_def)))
                            },
                            Err(err) => Err(err)
                        }
                    }, // AM
                    _ => Err(GerberParserError::UnknownCommand {})
                },
                'L' => match linechars.next().ok_or(GerberParserError::UnknownCommand{})? {
                    'P' => match linechars.next().ok_or(GerberParserError::UnknownCommand{})? {// LP
                        'D' => Ok(ExtendedCode::LoadPolarity(Polarity::Dark).into()), // LPD
                        'C' => Ok(ExtendedCode::LoadPolarity(Polarity::Clear).into()), // LPC
                        _ => Err(GerberParserError::UnknownCommand {})
                    }, 
                    'M' => Err(GerberParserError::UnsupportedCommand {}), // LM 
                    'R' => Err(GerberParserError::UnsupportedCommand {}), // LR
                    'S' => Err(GerberParserError::UnsupportedCommand {}), // LS
                    _ => Err(GerberParserError::UnknownCommand {})
                },
                'T' => match linechars.next().ok_or(GerberParserError::UnknownCommand{})? {
                    'F' => parse_file_attribute(linechars).map(|file_attr| {
                                ExtendedCode::FileAttribute(file_attr).into()
                            }),
                    'A' => parse_aperture_attribute(linechars),
                    'D' => parse_delete_attribute(linechars),
                    _ => Err(GerberParserError::UnknownCommand {})
                },
                'S' => match linechars.next().ok_or(GerberParserError::UnknownCommand{})? {
                    'R' => match linechars.next().ok_or(GerberParserError::UnknownCommand{})? {
                        'X' => parse_step_repeat_open(line),
                        // a statement %SR*% closes a step repeat command, which has no parameters
                        '*' => Ok(ExtendedCode::StepAndRepeat(StepAndRepeat::Close).into()),
                        _ => Err(GerberParserError::UnknownCommand {})
                    },
                    _ => Err(GerberParserError::UnknownCommand {})
                },
                'I' => match linechars.next().ok_or(GerberParserError::UnknownCommand{})? {
                    'N' => { // Image Name, 8.1.3. Deprecated, but still used by fusion 360.
                        match parse_image_name(line, &gerber_doc) {
                            Ok(name) => {
                                gerber_doc.image_name = Some(name.clone());
                                // Because `gerber-types` does not support image name, 
                                // we save it in the doc and list it as a comment. 
                                // The gerber spec also says it can be treated as a comment.
                                Ok(FunctionCode::GCode(GCode::Comment(format!("Image Name: {name}"))).into())
                            }
                            Err(e) => Err(e) 
                        }
                    },
                    'P' => Err(GerberParserError::UnsupportedCommand {}), 
                    // Image Polarity, basically useless, but used by fusion
                    _ => Err(GerberParserError::UnknownCommand {})
                }
                _ => Err(GerberParserError::UnknownCommand {})
            }
        },
        'X' | 'Y' => {
            linechars.next_back(); 
            match linechars.next_back().ok_or(GerberParserError::UnknownCommand{})? {
                '1' => parse_interpolation(line, &gerber_doc), // D01
                '2' => parse_move_or_flash(line, &gerber_doc, false), // D02
                '3' => parse_move_or_flash(line, &gerber_doc, true), // D03
                _ => Err(GerberParserError::UnknownCommand{})
            }
        },
        'D' => { // select aperture D<num>* (where num >= 10) or command where num < 10
            linechars.next_back(); // remove the trailing '*'
            parse_aperture_selection_or_command(line, linechars, &gerber_doc)
        },
        'M' => Ok(FunctionCode::MCode(MCode::EndOfFile).into()),
        _ => Err(GerberParserError::UnknownCommand {})
    }
}


/// parse a Gerber Comment (e.g. 'G04 This is a comment*')
fn parse_comment(line: &str) -> Result<Command, GerberParserError> {
    match RE_COMMENT.captures(line) {
        Some(regmatch) => {
            let comment = regmatch.get(1)
                .ok_or(GerberParserError::MissingRegexCapture{
                    regex: RE_COMMENT.clone(),
                    capture_index: 1,
                })?
                .as_str();
            Ok(FunctionCode::GCode(GCode::Comment(comment.to_string())).into())
        }
        None => { Err(GerberParserError::NoRegexMatch{
            regex: RE_COMMENT.clone(),
        }) }
    }
    
}

/// parse an image name. This is optional and deprecated, but included in all exports from Fusion 360
fn parse_image_name(line: &str, gerber_doc: &GerberDoc) -> Result<String, GerberParserError> {
    if gerber_doc.image_name.is_some(){
        Err(GerberParserError::TriedToSetImageNameTwice{})
    } else {
        match RE_IMAGE_NAME.captures(line) {
            Some(regmatch) => {
                let image_name = regmatch.get(1)
                    .ok_or(GerberParserError::MissingRegexCapture{regex: RE_IMAGE_NAME.clone(), capture_index: 1})?
                    .as_str();
                Ok(String::from(image_name))
            }
            None => { 
                Err(GerberParserError::NoRegexMatch {regex: RE_IMAGE_NAME.clone()})
            }
        }
    }
}

/// gerber spec (2021.02) 4.5.1.1 "Except for the comment all the parameters can be a decimal, integer, macro variables or an
/// arithmetic expression."
///
/// Safety: the method should only be called if the line starts with %AM
fn parse_aperture_macro_definition<T: Read>(first_line: &str, parser_context: &mut ParserContext<T>) -> Result<ApertureMacro, GerberParserError> {

    // Extract the macro name from the AM command
    let re_macro = Regex::new(r"%AM([^*%]*)").unwrap();
    let macro_name = re_macro.captures(first_line)
        .and_then(|cap| cap.get(1))
        .map(|m| m.as_str().trim())
        .ok_or(GerberParserError::InvalidMacroName)?
        .to_string();

    let mut content = Vec::new();
    
    // Read and parse the macro content lines until we find the end marker ("%")
    while let Some(line_result) = parser_context.next() {
        let line = line_result?.trim().to_string();
        if line.is_empty() {
            continue;
        }

        struct LineState {
            is_last_line: bool, 
            has_continuation_line: bool,
        }
        
        fn update_line_state(line_state: &mut LineState, line: &str) {
            if line.ends_with("*%") {
                line_state.is_last_line = true;
                line_state.has_continuation_line = false;
            } else {
                line_state.is_last_line = false;
                line_state.has_continuation_line = !line.ends_with("*");
            }
        }
        
        fn trim_line(line: &str) -> &str {
            line.trim_end_matches(&['*','%'])
        }
        
        fn read_params<T: Read>(params: &mut Vec<String>, parser_context: &mut ParserContext<T>, line_state: &mut LineState) -> Result<(), GerberParserError> {
            // read all the parameters, which could be split over multiple lines
            while line_state.has_continuation_line {
                if let Some(continuation_line) = parser_context.next() {
                    let continuation_line = continuation_line?;
                    update_line_state(line_state, &continuation_line);

                    let continuation_line = trim_line(&continuation_line);

                    let extra_params = line_to_params(continuation_line);
                    params.extend(extra_params);
                } else {
                    break
                }
            }

            Ok(())
        }

        let mut line_state = LineState {
            is_last_line: false,
            has_continuation_line: false,
        };
        
        update_line_state(&mut line_state, &line);
        
        let trimmed_line = trim_line(&line);
        if trimmed_line.starts_with("0 ") {
            // Handle the special-case comment primitive
            
            // Gerber spec: 4.5.1.2 "The comment primitive starts with the ‘0’ code followed by a space and then a
            // single-line text string"
            content.push(MacroContent::Comment(trimmed_line[2..].trim().to_string()));
            continue;
        }
        
        let mut params: Vec<String> = line_to_params(trimmed_line);

        if params.is_empty() {
            continue;
        }
        
        
        // Parse outline primitive (type 4)
        // TODO would be nice to have constants for the values in `gerber-types`. e.g. const APERTURE_MACRO_TYPE_OUTLINE: u8 = 4;
        match params[0].parse::<u8>() {
            Ok(1) => {
                // Handle circle primitive
                read_params(&mut params, parser_context, &mut line_state)?;
                let param_count_excluding_code = params.len() - 1;

                if !(4..=5).contains(&param_count_excluding_code) {
                    // exposure + diameter + center x + center y [ + rotation]
                    return Err(GerberParserError::InvalidMacroDefinition("expected 4-5 parameters for circle".to_string()));
                }

                // reverse the params, so we can pop them one at a time.
                params.reverse();
                let _primitive_code = params.pop();

                let exposure_str = params.pop().unwrap().trim().to_string();
                let exposure = parse_macro_boolean(&exposure_str)?;
                
                let diameter_str = params.pop().unwrap().trim().to_string();
                let diameter = parse_macro_decimal(&diameter_str)?;
                
                let center_x_str = params.pop().unwrap().trim().to_string();
                let center_x = parse_macro_decimal(&center_x_str)?;
                
                let center_y_str = params.pop().unwrap().trim().to_string();
                let center_y = parse_macro_decimal(&center_y_str)?;


                // Parse rotation angle from the last line
                let angle = params
                    .pop()
                    .map(|angle_str|{
                        parse_macro_decimal(&angle_str)
                    })
                    .transpose()?;

                let circle = CirclePrimitive {
                    exposure,

                    diameter,
                    center: (center_x, center_y),
                    angle,
                };
                content.push(MacroContent::Circle(circle));
            }
            Ok(4) => {
                // Handle outline primitive
                read_params(&mut params, parser_context, &mut line_state)?;
                let param_count_excluding_code = params.len() - 1;
                
                if param_count_excluding_code < 6 {
                    // exposure + #vertices + start x + start y [+ (x,y)] + end x + end y [ + rotation]
                    return Err(GerberParserError::InvalidMacroDefinition("expected minimum of 6 parameters for outline".to_string()));
                }
                
                // reverse the params, so we can pop them one at a time.
                params.reverse();
                let _primitive_code = params.pop();
               
                let exposure_str = params.pop().unwrap().trim().to_string();
                let exposure = parse_macro_boolean(&exposure_str)?;

                let num_vertices  = params.pop().unwrap().trim().parse::<u32>().map_err(|_|
                    GerberParserError::ApertureDefinitionParseFailed {
                        aperture_definition_str: line.clone()
                    })?;

                // Collect points from remaining parameters
                let mut points = Vec::new();
                
                // `num_points` is actually one fewer than the number of points in the macro definition, so we use `<=` here
                while points.len() <= num_vertices as usize {
                    if params.len() >= 2 {
                        let x_str = params.pop().unwrap().trim().to_string();
                        let x = parse_macro_decimal(&x_str)?;
                        let y_str = params.pop().unwrap().trim().to_string();
                        let y = parse_macro_decimal(&y_str)?;

                        points.push((x, y));
                    } else {
                        return Err(GerberParserError::InvalidMacroDefinition("Missing outline point line.".to_string()));
                    }
                }
                
                // Parse rotation angle from the last line
                let angle = params
                    .pop()
                    .map(|angle_str|{
                        angle_str.trim().parse::<f64>()
                            .map_err(|_|GerberParserError::InvalidMacroDefinition("Invalid angle parameter".to_string()))
                    })
                    .transpose()?
                    .unwrap_or(0.0);

                let outline = OutlinePrimitive {
                    exposure,
                    points,
                    angle: MacroDecimal::Value(angle),
                };

                content.push(MacroContent::Outline(outline));
            }
            Ok(5) => {
                // Handle polygon primitive
                read_params(&mut params, parser_context, &mut line_state)?;
                let param_count_excluding_code = params.len() - 1;

                if !(5..=6).contains(&param_count_excluding_code) {
                    // exposure + #vertices + center x + center y + diameter [ + rotation]
                    return Err(GerberParserError::InvalidMacroDefinition("Expected 5-6 parameters for polygon".to_string()));
                }

                // reverse the params, so we can pop them one at a time.
                params.reverse();
                let _primitive_code = params.pop();

                let exposure_str = params.pop().unwrap().trim().to_string();
                let exposure = parse_macro_boolean(&exposure_str)?;

                let vertices_str = params.pop().unwrap().trim().to_string();
                let vertices = parse_macro_integer(&vertices_str)?;
                
                let center_x_str = params.pop().unwrap().trim().to_string();
                let center_x = parse_macro_decimal(&center_x_str)?;

                let center_y_str = params.pop().unwrap().trim().to_string();
                let center_y = parse_macro_decimal(&center_y_str)?;
                
                let diameter_str = params.pop().unwrap().trim().to_string();
                let diameter = parse_macro_decimal(&diameter_str)?;
                
                // Parse rotation angle from the last parameter
                let angle = params
                    .pop()
                    .map(|angle_str|{
                        parse_macro_decimal(&angle_str)
                    })
                    .transpose()?
                    .unwrap_or(MacroDecimal::Value(0.0));

                let polygon_primitive = PolygonPrimitive {
                    exposure,
                    vertices,
                    center: (center_x, center_y),
                    diameter,
                    angle,
                };

                content.push(MacroContent::Polygon(polygon_primitive));
            },
            Ok(20) => {
                // Vector-line primitive
                read_params(&mut params, parser_context, &mut line_state)?;
                let param_count_excluding_code = params.len() - 1;

                if !(6..=7).contains(&param_count_excluding_code) {
                    // exposure + width + start x + start y + end x + end y [ + rotation]
                    return Err(GerberParserError::InvalidMacroDefinition("Expected 6-7 parameters for vector-line".to_string()));
                }
                
                // reverse the params, so we can pop them one at a time.
                params.reverse();
                let _primitive_code = params.pop();

                let exposure_str = params.pop().unwrap().trim().to_string();
                let exposure = parse_macro_boolean(&exposure_str)?;

                let width_str = params.pop().unwrap().trim().to_string();
                let width = parse_macro_decimal(&width_str)?;

                let start_x_str = params.pop().unwrap().trim().to_string();
                let start_x = parse_macro_decimal(&start_x_str)?;

                let start_y_str = params.pop().unwrap().trim().to_string();
                let start_y = parse_macro_decimal(&start_y_str)?;

                let end_x_str = params.pop().unwrap().trim().to_string();
                let end_x = parse_macro_decimal(&end_x_str)?;

                let end_y_str = params.pop().unwrap().trim().to_string();
                let end_y = parse_macro_decimal(&end_y_str)?;
                
                // Parse rotation angle from the last parameter
                let angle = params
                    .pop()
                    .map(|angle_str|{
                        parse_macro_decimal(&angle_str)
                    })
                    .transpose()?
                    .unwrap_or(MacroDecimal::Value(0.0));

                let vector_line = VectorLinePrimitive {
                    exposure,
                    width,
                    start: (start_x, start_y),
                    end: (end_x, end_y),
                    angle,
                };
                content.push(MacroContent::VectorLine(vector_line));
            }
            Ok(21) => {
                // Center-line primitive
                read_params(&mut params, parser_context, &mut line_state)?;
                let param_count_excluding_code = params.len() - 1;

                if !(5..=6).contains(&param_count_excluding_code) {
                    // exposure + width + height + center x + center y [ + rotation]
                    return Err(GerberParserError::InvalidMacroDefinition("Expected 5-6 parameters for center-line".to_string()));
                }

                // reverse the params, so we can pop them one at a time.
                params.reverse();
                let _primitive_code = params.pop();

                let exposure_str = params.pop().unwrap().trim().to_string();
                let exposure = parse_macro_boolean(&exposure_str)?;

                let width_str = params.pop().unwrap().trim().to_string();
                let width = parse_macro_decimal(&width_str)?;

                let height_str = params.pop().unwrap().trim().to_string();
                let height = parse_macro_decimal(&height_str)?;

                let center_x_str = params.pop().unwrap().trim().to_string();
                let center_x = parse_macro_decimal(&center_x_str)?;

                let center_y_str = params.pop().unwrap().trim().to_string();
                let center_y = parse_macro_decimal(&center_y_str)?;

                // Parse rotation angle from the last parameter
                let angle = params
                    .pop()
                    .map(|angle_str|{
                        parse_macro_decimal(&angle_str)
                    })
                    .transpose()?
                    .unwrap_or(MacroDecimal::Value(0.0));

                let center_line = CenterLinePrimitive {
                    exposure,
                    dimensions: (width, height),
                    center: (center_x, center_y),
                    angle,
                };
                content.push(MacroContent::CenterLine(center_line));
            }
            _ => {
                read_params(&mut params, parser_context, &mut line_state)?;
                log::error!("Unsupported primitive type: {}, line: {}, params: {}", params[0], line, params[1..].join(", "));

                return Err(GerberParserError::UnsupportedMacroDefinition)
            },
        }
        
        if line_state.is_last_line {
            break;
        }
    }

    Ok(ApertureMacro { name: macro_name, content })
}

/// parse a Gerber unit statement (e.g. '%MOMM*%')
fn parse_units(line: &str, gerber_doc: &GerberDoc) -> Result<Unit, GerberParserError> {
    // Check that the units are not set yet (that would imply the set unit command is present twice)
    if gerber_doc.units.is_some() { 
        Err(GerberParserError::TriedToSetUnitsTwice{})
    } else {
        match RE_UNITS.captures(line) {
            Some(regmatch) => {
                let units_str = regmatch.get(1)
                    .ok_or(GerberParserError::MissingRegexCapture{regex: RE_UNITS.clone(), capture_index: 1})?
                    .as_str();
                match units_str {
                    "MM" => Ok(Unit::Millimeters),
                    "IN" => Ok(Unit::Inches),
                    _ => Err(GerberParserError::InvalidUnitFormat{units_str: line.to_string()})
                }
            }
            None => Err(GerberParserError::NoRegexMatch{regex: RE_UNITS.clone()})
        }
    }
}


/// parse a Gerber format spec statement (e.g. '%FSLAX23Y23*%')
fn parse_format_spec(line: &str, gerber_doc: &GerberDoc) -> Result<CoordinateFormat, GerberParserError> {
    // Ensure that FS was not set before, which would imply two FS statements in the same doc
    if gerber_doc.format_specification.is_some() { 
        Err(GerberParserError::TriedToFormatTwice {}) 
    } else {
        match RE_FORMAT_SPEC.captures(line) {
            Some(regmatch) => {
                let mut fs_chars = regmatch.get(1)
                    .ok_or(GerberParserError::MissingRegexCapture{regex: RE_FORMAT_SPEC.clone(), capture_index: 1})?
                    .as_str().chars();
                let integer:u8 = parse_char(fs_chars.next()
                    .ok_or(GerberParserError::ParseFormatErrorWrongNumDigits {})?)?;
                let decimal:u8 = parse_char(fs_chars.next()
                    .ok_or(GerberParserError::ParseFormatErrorWrongNumDigits{})?)?;

                // the gerber spec states that the integer value can be at most 6
                if integer < 1 || integer > 6 {
                    return Err(GerberParserError::ParseFormatErrorInvalidDigit{digit_found: integer})
                }

                Ok(CoordinateFormat::new(integer, decimal))
            }
            None => Err(GerberParserError::NoRegexMatch{regex: RE_FORMAT_SPEC.clone()})
        }
    }
}

/// helper function to move some ugly repeated .ok_or().unwrap().Arc<Mutex<Future>>
fn parse_char(char_in: char) -> Result<u8, GerberParserError> {
    Ok(char_in.to_digit(10)
        .ok_or(GerberParserError::ParseDigitError {char_found: char_in})
        ? as u8)
}


// parse a Gerber aperture definition e.g. '%ADD44R, 2.0X3.0*%')
fn parse_aperture_defs(line: &str, gerber_doc: &GerberDoc) -> Result<(i32, Aperture), GerberParserError> {
    
    let Some(captures) = RE_APERTURE.captures(line) else {
        return Err(GerberParserError::NoRegexMatch{regex: RE_APERTURE.clone()})
    };

    // Sync captures with [`RE_APERTURE`] definition.
    // `%ADD([0-9]+)([._$a-zA-Z][._$a-zA-Z0-9]{0,126})(?:,\s?(.*))?\*%`
    const CAPTURE_APERTURE_CODE: usize = 1;
    const CAPTURE_APERTURE_NAME: usize = 2;
    const CAPTURE_APERTURE_ARGS: usize = 3;

    // Parse aperture code
    let code_str = captures.get(CAPTURE_APERTURE_CODE)
        .ok_or(GerberParserError::MissingRegexCapture{ regex: RE_APERTURE.clone(), capture_index: CAPTURE_APERTURE_CODE})?
        .as_str();
    let code = parse_aperture_code(code_str)?;

    let aperture_name = captures.get(CAPTURE_APERTURE_NAME)
        .ok_or(GerberParserError::MissingRegexCapture{regex: RE_APERTURE.clone(), capture_index: CAPTURE_APERTURE_NAME})?
        .as_str();

    if gerber_doc.apertures.contains_key(&code){
        return Err(GerberParserError::ApertureDefinedTwice{aperture_code: code});
    }

    let aperture_args_str: Option<&str> = captures.get(CAPTURE_APERTURE_ARGS).map(|m| m.as_str());

    let is_macro = aperture_name.len() > 1;
    if is_macro {
        
        let optional_params: Option<Vec<MacroDecimal>> = aperture_args_str.map(|line|{
            let parse_results = line_to_args(line)
                .iter()
                .map(|param| {
                    let arg_str = param.trim().to_string();
                    parse_macro_decimal(&arg_str)
                })
                .collect::<Vec<_>>();
            
            parse_results.into_iter().try_fold(vec![], |mut args, foo| {
                let arg = foo?;
                args.push(arg);
                Ok(args)
            })
        })
            .transpose()?;
        
        return Ok((code, Aperture::Macro(aperture_name.to_string(), optional_params)));
    }

    let aperture_args_split: Option<Vec<&str>> = aperture_args_str
        .map(|m| m.split('X').collect());

    match (aperture_name, aperture_args_split) {
        ("C", Some(args)) => Ok((code, Aperture::Circle(Circle {
            diameter: args[0].trim().parse::<f64>()
                .map_err(|_| {
                    GerberParserError::ParseApertureDefinitionBodyError{aperture_code: code}
                })?,
            hole_diameter: if args.len() > 1 {
                Some(args[1].trim().parse::<f64>()
                    .map_err(|_| {
                        GerberParserError::ParseApertureDefinitionBodyError{aperture_code: code}
                    })?
                )
            } else { None }
        }))),
        ("R", Some(args)) => Ok((code, Aperture::Rectangle(Rectangular {
            x: parse_coord::<f64>(args[0])?,
            y: parse_coord::<f64>(args[1])?,
            hole_diameter: if args.len() > 2 {
                Some(args[2].trim().parse::<f64>()
                    .map_err(|_| {
                        GerberParserError::ParseApertureDefinitionBodyError{aperture_code: code}
                    })?
                )
            } else { None }
        }))),
        ("O", Some(args)) => Ok((code, Aperture::Obround(Rectangular {
            x: parse_coord::<f64>(args[0])?,
            y: parse_coord::<f64>(args[1])?,
            hole_diameter: if args.len() > 2 {
                Some(args[2].trim().parse::<f64>()
                    .map_err(|_| {
                        GerberParserError::ParseApertureDefinitionBodyError{aperture_code: code}
                    })?)
            } else { None }
        }))),
        // note that for polygon we HAVE TO specify rotation if we want to add a hole
        ("P", Some(args)) => Ok((code, Aperture::Polygon(Polygon {
            diameter: args[0].trim().parse::<f64>()
                .map_err(|_| {
                    GerberParserError::ParseApertureDefinitionBodyError{aperture_code: code}
                })?,
            vertices: args[1].trim().parse::<u8>()
                .map_err(|_| {
                    GerberParserError::ParseApertureDefinitionBodyError{aperture_code: code}
                })?,
            rotation: if args.len() > 2 {
                Some(args[2].trim().parse::<f64>()
                    .map_err(|_| {
                        GerberParserError::ParseApertureDefinitionBodyError{aperture_code: code}
                    })?)
            } else { None },
            hole_diameter: if args.len() > 3 {
                Some(args[3].trim().parse::<f64>()
                    .map_err(|_| {
                        GerberParserError::ParseApertureDefinitionBodyError{aperture_code: code}
                    })?)
            } else { None }
        }))),
        (aperture_name, None) if ["C", "R", "O", "P"].contains(&aperture_name) => {
            Err(GerberParserError::MissingApertureDefinitionArgs{aperture_code: code, aperture_name: aperture_name.to_string()})
        }
        (unknown_type, _args) => {
            Err(GerberParserError::UnknownApertureType{type_str: unknown_type.to_string()})
        }
    }
}

fn parse_coord<T: std::str::FromStr>(coord_str: &str) -> Result<T, GerberParserError> {
    coord_str.trim().parse::<T>()
        .map_err(|_| {GerberParserError::FailedToParseCoordinate{coord_num_str: coord_str.to_string()}})
}


fn parse_aperture_code(code_str: &str) -> Result<i32, GerberParserError> {
    match code_str.parse::<i32>(){
        Ok(v) if v >= 10 => {
            Ok(v)
        }
        Ok(_v) => {
            Err(GerberParserError::ApertureCodeParseFailed{ aperture_code_str: code_str.to_string() })
        }
        Err(_) => {
            Err(GerberParserError::ApertureCodeParseFailed { aperture_code_str: code_str.to_string() })
        }
    }
}
fn parse_aperture_selection_or_command(
    line: &str,
    linechars: Chars,
    gerber_doc: &GerberDoc
)
    -> Result<Command, GerberParserError>
{
    let aperture_str = linechars.as_str();
    if let Ok(aperture_code) = parse_aperture_code(aperture_str) {
        match gerber_doc.apertures.contains_key(&aperture_code) {
            true => {
                Ok(FunctionCode::DCode(DCode::SelectAperture(aperture_code)).into())
            }
            false => {
                Err(GerberParserError::ApertureNotDefined{aperture_code})
            }
        }
    } else {
        parse_command(line, gerber_doc)
    }
}

fn parse_command(command_str: &str, gerber_doc: &GerberDoc) -> Result<Command, GerberParserError> {
    static RE_STANDALONE_D: Lazy<Regex> = lazy_regex!(r"D(0)?([1-3])\*");
    if let Some(captures) = RE_STANDALONE_D.captures(command_str) {
        let command_code = captures.get(2).unwrap().as_str(); {

            let format = gerber_doc.format_specification.ok_or(
                GerberParserError::OperationBeforeFormat{}
            )?;
            
            let coords = Coordinates{
                x: None,
                y: None,
                format,
            };

            let operation = match command_code {
                "1" => Operation::Interpolate(coords, None),
                "2" => Operation::Move(coords),
                "3" => Operation::Flash(coords),
                _ => unreachable!()
            };
            Ok(Command::FunctionCode(FunctionCode::DCode(DCode::Operation(operation))))
        }
    } else {
        Err(GerberParserError::UnknownCommand {})
    }
}

// parse a Gerber interpolation command (e.g. 'X2000Y40000I300J50000D01*')
fn parse_interpolation(
    line: &str,
    gerber_doc: &GerberDoc, 
)
    -> Result<Command, GerberParserError> 
{
    match RE_INTERPOLATION.captures(line) {
        Some(regmatch) => {
            let x_coord = regmatch.get(1).map(|x| parse_coord::<i64>(x.as_str())).transpose()?;
            let y_coord = regmatch.get(2).map(|y| parse_coord::<i64>(y.as_str())).transpose()?;
    
            if let Some((i_offset_raw, j_offset_raw)) = regmatch
                .get(3)
                .zip(regmatch.get(4))
            {  //  we have X,Y,I,J parameters and we are doing circular interpolation
                let i_offset = parse_coord::<i64>(i_offset_raw.as_str())?;
                let j_offset = parse_coord::<i64>(j_offset_raw.as_str())?;
    
                Ok(FunctionCode::DCode(DCode::Operation(
                    Operation::Interpolate(
                        partial_coordinates_from_gerber(
                            x_coord, 
                            y_coord, 
                            gerber_doc.format_specification.ok_or(
                                GerberParserError::OperationBeforeFormat{}
                            )?
                        ), 
                        Some(coordinates_offset_from_gerber(
                            i_offset, 
                            j_offset, 
                            gerber_doc.format_specification.unwrap(/*Already checked above*/)
                        ))
                    )
                )).into())
            } else { // linear interpolation, only X,Y parameters
                Ok(FunctionCode::DCode(DCode::Operation(
                    Operation::Interpolate(
                        partial_coordinates_from_gerber(
                            x_coord, 
                            y_coord, 
                            gerber_doc.format_specification.ok_or(
                                GerberParserError::OperationBeforeFormat{}
                            )?
                        ), 
                        None
                    )
                )).into())
            }
        }
        None => Err(GerberParserError::NoRegexMatch{regex: RE_INTERPOLATION.clone()})
    }
}


// parse a Gerber move or flash command (e.g. 'X2000Y40000D02*')
fn parse_move_or_flash(
    line: &str,
    gerber_doc: &GerberDoc, 
    flash: bool
) 
    -> Result<Command, GerberParserError> 
{
    match RE_MOVE_OR_FLASH.captures(line) {
        Some(regmatch) => {
            let x_coord = regmatch.get(1).map(|x| parse_coord::<i64>(x.as_str())).transpose()?;
            let y_coord = regmatch.get(2).map(|y| parse_coord::<i64>(y.as_str())).transpose()?;
            
            let coords = partial_coordinates_from_gerber(
                x_coord,
                y_coord,
                gerber_doc.format_specification
                    .ok_or(GerberParserError::OperationBeforeFormat {})?,
            );
    
            if flash {
                Ok(FunctionCode::DCode(DCode::Operation(Operation::Flash(coords))).into())
            } else {
                Ok(FunctionCode::DCode(DCode::Operation(Operation::Move(coords))).into())
            }
        }
        None => Err(GerberParserError::NoRegexMatch{regex: RE_MOVE_OR_FLASH.clone()})
    }   
}

// a step and repeat open statement has four (required) parameters that we need to extract
// X (pos int) Y (pos int), I (decimal), J (decimal)
fn parse_step_repeat_open(line: &str) -> Result<Command, GerberParserError> {
    match RE_STEP_REPEAT.captures(line) {
        Some(regmatch) => {
            Ok(ExtendedCode::StepAndRepeat(StepAndRepeat::Open{
                repeat_x: parse_coord::<u32>(regmatch.get(1).ok_or(
                    GerberParserError::MissingRegexCapture{regex: RE_STEP_REPEAT.clone(), capture_index: 1}
                )?.as_str())?,
                repeat_y: parse_coord::<u32>(regmatch.get(2).ok_or(
                    GerberParserError::MissingRegexCapture{regex: RE_STEP_REPEAT.clone(), capture_index: 2}
                )?.as_str())?,
                distance_x: parse_coord::<f64>(regmatch.get(3).ok_or(
                    GerberParserError::MissingRegexCapture{regex: RE_STEP_REPEAT.clone(), capture_index: 3}
                )?.as_str())?,
                distance_y: parse_coord::<f64>(regmatch.get(4).ok_or(
                    GerberParserError::MissingRegexCapture{regex: RE_STEP_REPEAT.clone(), capture_index: 4}
                )?.as_str())?,
            }).into())
        }
        None => Err(GerberParserError::NoRegexMatch{regex: RE_STEP_REPEAT.clone()})
    }
}


/// Parse an Aperture Attribute (%TF.<AttributeName>[,<AttributeValue>]*%) into Command
/// 
/// For now we consider two types of TA statements:
/// 1. Aperture Function (AperFunction) with field: String
/// 2. Drill tolerance (DrillTolerance) with fields: [1] num [2] num 
/// 
/// ⚠️ Any other Attributes (which seem to be valid within the gerber spec) we will **fail** to parse!
/// 
/// ⚠️ This parsing statement needs a lot of tests and validation at the current stage!
fn parse_file_attribute(line: Chars) -> Result<FileAttribute, GerberParserError> {

    let attr_args = get_attr_args(line)?;
    if attr_args.len() >= 2 {  // we must have at least 1 field
        //log::debug!("TF args are: {:?}", attr_args);
        match attr_args[0] {
            "Part" => match attr_args[1]{
                "Single" => Ok(FileAttribute::Part(Part::Single)),
                "Array" => Ok(FileAttribute::Part(Part::Array)),
                "FabricationPanel" => Ok(FileAttribute::Part(Part::FabricationPanel)),
                "Coupon" => Ok(FileAttribute::Part(Part::Coupon)),
                "Other" => Ok(FileAttribute::Part(Part::Other(attr_args[2].to_string()))),
                _ => Err(GerberParserError::UnsupportedPartType{part_type: attr_args[1].to_string()})
            },
            // TODO do FileFunction properly, but needs changes in gerber-types
            "FileFunction" => Ok(FileAttribute::FileFunction(FileFunction::Other(
                attr_args[1].to_string()
            ))),  
            "FilePolarity" => match attr_args[1]{
                "Positive" => Ok(FileAttribute::FilePolarity(FilePolarity::Positive)),
                "Negative" => Ok(FileAttribute::FilePolarity(FilePolarity::Negative)),
                _ => Err(GerberParserError::UnsupportedPolarityType{polarity_type: attr_args[1].to_string()})
            },
            "Md5" => Ok(FileAttribute::Md5(attr_args[1].to_string())),
            _ => Err(GerberParserError::UnsupportedFileAttribute{attribute_name: attr_args[0].to_string()})
        }
    }
    else {
        Err(GerberParserError::FileAttributeParseError{})
    }
}


/// Parse an Aperture Attribute (%TA.<AttributeName>[,<AttributeValue>]*%) into Command
/// 
/// For now we consider two types of TA statements:
/// 1. Aperture Function (AperFunction) with field: String
/// 2. Drill tolerance (DrillTolerance) with fields: [1] num [2] num 
/// 
/// ⚠️ Any other Attributes (which seem to be valid within the gerber spec) we will **fail** to parse!
/// 
/// ⚠️ This parsing statement needs a lot of tests and validation at the current stage!
fn parse_aperture_attribute(line: Chars) -> Result<Command, GerberParserError> {
    
    use GerberParserError::UnsupportedApertureAttribute;
    
    let raw_line = line.as_str().to_string();
    let attr_args = get_attr_args(line)?;
    // log::debug!("TA ARGS: {:?}", attr_args);
    if attr_args.len() >= 2 {  // we must have at least 1 field
        match attr_args[0] {
            "AperFunction" => {
                Ok(ExtendedCode::ApertureAttribute(ApertureAttribute::ApertureFunction(match attr_args[1] {
                    "ViaDrill" => ApertureFunction::ViaDrill,
                    "BackDrill" => ApertureFunction::BackDrill,
                    "ComponentDrill" => ApertureFunction::ComponentDrill{ press_fit: None },  // TODO parse this
                    "CastellatedDrill" => ApertureFunction::CastellatedDrill,
                    "MechanicalDrill" => ApertureFunction::MechanicalDrill { function: None }, // TODO parse this
                    "Slot" => ApertureFunction::Slot,
                    "CutOut" => ApertureFunction::CutOut,
                    "Cavity" => ApertureFunction::Cavity,
                    "OtherDrill" => ApertureFunction::OtherDrill(attr_args[2].to_string()),
                    "ComponentPad " => ApertureFunction::ComponentPad{ press_fit: None }, // TODO parse this
                    "SmdPad" => match attr_args[2] {
                        "CopperDefined" => ApertureFunction::SmdPad(SmdPadType::CopperDefined),
                        "SoldermaskDefined" => ApertureFunction::SmdPad(SmdPadType::SoldermaskDefined),
                        _ => return Err(UnsupportedApertureAttribute{aperture_attribute: raw_line})
                    },
                    "BgaPad" => match attr_args[2] {
                        "CopperDefined" => ApertureFunction::BgaPad(SmdPadType::CopperDefined),
                        "SoldermaskDefined" => ApertureFunction::BgaPad(SmdPadType::SoldermaskDefined),
                        _ => return Err(UnsupportedApertureAttribute{aperture_attribute: raw_line})
                    },
                    "HeatsinkPad" => ApertureFunction::HeatsinkPad,
                    "TestPad" => ApertureFunction::TestPad,
                    "CastellatedPad" => ApertureFunction::CastellatedPad,
                    "FiducialPad" => match attr_args[2]{
                        "Global" => ApertureFunction::FiducialPad(FiducialScope::Global),
                        "Local" => ApertureFunction::FiducialPad(FiducialScope::Local),
                        _ => return Err(UnsupportedApertureAttribute{aperture_attribute: raw_line})
                    },
                    "ThermalReliefPad" => ApertureFunction::ThermalReliefPad,
                    "WasherPad" => ApertureFunction::WasherPad,
                    "AntiPad" => ApertureFunction::AntiPad,
                    "OtherPad" => ApertureFunction::OtherPad(attr_args[2].to_string()),
                    "Conductor" => ApertureFunction::Conductor,
                    "NonConductor" => ApertureFunction::NonConductor,
                    "CopperBalancing" => ApertureFunction::CopperBalancing,
                    "Border" => ApertureFunction::Border,
                    "OtherCopper" => ApertureFunction::OtherCopper(attr_args[2].to_string()),
                    "Profile" => ApertureFunction::Profile,
                    "NonMaterial" => ApertureFunction::NonMaterial,
                    "Material" => ApertureFunction::Material,
                    "Other" => ApertureFunction::Other(attr_args[2].to_string()),
                    _ => return Err(UnsupportedApertureAttribute{aperture_attribute: raw_line})
                })).into())
            },
            "DrillTolerance" => {
                Ok(ExtendedCode::ApertureAttribute(ApertureAttribute::DrillTolerance{
                     plus: attr_args[1].parse::<f64>().map_err(|_|{
                         GerberParserError::DrillToleranceParseNumError {
                             number_str: attr_args[1].to_string(),
                         }
                     })?,
                     minus: attr_args[2].parse::<f64>().map_err(|_|{
                         GerberParserError::DrillToleranceParseNumError {
                             number_str: attr_args[2].to_string(),
                         }
                     })?
                     }).into())
            }
            _ => Err(UnsupportedApertureAttribute{aperture_attribute: raw_line})
        }
    }
    else { 
        Err(GerberParserError::InvalidApertureAttribute{aperture_attribute: raw_line}) 
    }
}


fn parse_delete_attribute(line: Chars) -> Result<Command, GerberParserError>{
    let raw_line = line.as_str().to_string();
    let attr_args = get_attr_args(line)?;
    if attr_args.len() == 1 {
        Ok(ExtendedCode::DeleteAttribute(attr_args[0].to_string()).into())
    } else {
        Err(GerberParserError::InvalidDeleteAttribute{delete_attribute: raw_line})
    }
}


/// Extract the individual elements (AttributeName and Fields) from Chars
/// 
/// The arguments of the attribute statement can have whitespace as this will be trimmed. 
/// `attribute_chars` argument must be the **trimmed line** from the gerber file
/// with the **first three characters removed**. E.g. ".Part,single*%" not "%TF.Part,single*%"
/// ```
/// use gerber_parser::parser::get_attr_args;
/// let attribute_chars = ".DrillTolerance, 0.02, 0.01 *%".chars();
/// 
/// let arguments = get_attr_args(attribute_chars).unwrap();
/// assert_eq!(arguments, vec!["DrillTolerance","0.02","0.01"])
/// ```
pub fn get_attr_args(mut attribute_chars: Chars) -> Result<Vec<&str>, GerberParserError> {
    
    attribute_chars.next_back()
        .ok_or(GerberParserError::InvalidFileAttribute{file_attribute: attribute_chars.as_str().to_string()})?;
    attribute_chars.next_back()
        .ok_or(GerberParserError::InvalidFileAttribute{file_attribute: attribute_chars.as_str().to_string()})?;
    attribute_chars.next()
        .ok_or(GerberParserError::InvalidFileAttribute{file_attribute: attribute_chars.as_str().to_string()})?;
    Ok(attribute_chars.as_str().split(",").map(|el| el.trim()).collect())
} 


pub fn coordinates_from_gerber(
    mut x_as_int: i64, 
    mut y_as_int: i64, 
    fs: CoordinateFormat
) 
    -> Coordinates 
{
    // we have the raw gerber string as int but now have to convert it to nano precision format 
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    x_as_int *= 10i64.pow(factor);
    y_as_int *= 10i64.pow(factor);
    Coordinates::new(CoordinateNumber::new(x_as_int), CoordinateNumber::new(y_as_int), fs)
}

pub fn partial_coordinates_from_gerber(
    x_as_int: Option<i64>,
    y_as_int: Option<i64>,
    fs: CoordinateFormat
)
    -> Coordinates
{
    // we have the raw gerber string as int but now have to convert it to nano precision format 
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    let x = x_as_int.map(|value| value * 10i64.pow(factor));
    let y = y_as_int.map(|value| value * 10i64.pow(factor));
    
    match (x,y)  {
        (Some(x), Some(y)) => Coordinates::new(CoordinateNumber::new(x), CoordinateNumber::new(y), fs),
        (None, Some(y)) => Coordinates::at_y(CoordinateNumber::new(y), fs),
        (Some(x), None) => Coordinates::at_x(CoordinateNumber::new(x), fs),
        (None, None) => Coordinates {
            x: None,
            y: None,
            format: fs,
        }
    }
}


pub fn coordinates_offset_from_gerber(
    mut x_as_int: i64, 
    mut y_as_int: i64, 
    fs: CoordinateFormat
) 
    -> CoordinateOffset 
{
    // we have the raw gerber string as int but now have to convert it to nano precision format 
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    x_as_int *= 10i64.pow(factor);
    y_as_int *= 10i64.pow(factor);
    CoordinateOffset::new(CoordinateNumber::new(x_as_int), CoordinateNumber::new(y_as_int), fs)
}

/// Split the line by commas and convert to a vector of strings
/// trimming whitespace from each parameter
fn line_to_params(line: &str) -> Vec<String> {
    line
        .split(',')
        .map(|param| param.trim().into())
        .filter(|param: &String|!param.is_empty())
        .collect()
}

/// Split the line by X and convert to a vector of strings
/// trimming whitespace from each parameter
fn line_to_args(line: &str) -> Vec<String> {
    line
        .split('X')
        .map(|param| param.trim().into())
        .filter(|param: &String|!param.is_empty())
        .collect()
}

/// value should be a pre-trimmed string.
fn parse_macro_decimal(value: &str) -> Result<MacroDecimal, GerberParserError> {
    let captures = RE_MACRO_DECIMAL.captures(&value).ok_or(GerberParserError::InvalidMacroDefinition("Invalid parameter".to_string()))?;
    if let Some(value_match) = captures.name("value") {
        let value = value_match.as_str().parse::<f64>().map_err(|_| GerberParserError::InvalidMacroDefinition("Invalid decimal".to_string()))?;
        Ok(MacroDecimal::Value(value))
    } else if let Some(value_match) = captures.name("variable") {
        let variable = value_match.as_str().trim_start_matches('$').parse::<u32>().map_err(|_|GerberParserError::InvalidMacroDefinition("Invalid variable".to_string()))?;
        Ok(MacroDecimal::Variable(variable))
    } else if let Some(value_match) = captures.name("expression") {
        let variable = value_match.as_str().to_string();
        Ok(MacroDecimal::Expression(variable))
    } else {
        // it has to match one of the named captures.
        unreachable!()
    }
}

/// value should be a pre-trimmed string.
fn parse_macro_boolean(value: &str) -> Result<MacroBoolean, GerberParserError> {
    let captures = RE_MACRO_BOOLEAN.captures(&value).ok_or(GerberParserError::InvalidMacroDefinition("Invalid parameter".to_string()))?;
    if let Some(value_match) = captures.name("value") {
        let value = value_match.as_str().parse::<u8>().map_err(|_| GerberParserError::InvalidMacroDefinition("Invalid boolean".to_string()))?;
        Ok(MacroBoolean::Value(value == 1))
    } else if let Some(value_match) = captures.name("variable") {
        let variable = value_match.as_str().trim_start_matches('$').parse::<u32>().map_err(|_|GerberParserError::InvalidMacroDefinition("Invalid variable".to_string()))?;
        Ok(MacroBoolean::Variable(variable))
    } else if let Some(value_match) = captures.name("expression") {
        let variable = value_match.as_str().to_string();
        Ok(MacroBoolean::Expression(variable))
    } else {
        // it has to match one of the named captures.
        unreachable!()
    }
}

/// value should be a pre-trimmed string.
fn parse_macro_integer(value: &str) -> Result<MacroInteger, GerberParserError> {
    let captures = RE_MACRO_UNSIGNED_INTEGER.captures(&value).ok_or(GerberParserError::InvalidMacroDefinition("Invalid parameter".to_string()))?;
    if let Some(value_match) = captures.name("value") {
        let value = value_match.as_str().parse::<u32>().map_err(|_| GerberParserError::InvalidMacroDefinition("Invalid integer".to_string()))?;
        Ok(MacroInteger::Value(value))
    } else if let Some(value_match) = captures.name("variable") {
        let variable = value_match.as_str().trim_start_matches('$').parse::<u32>().map_err(|_|GerberParserError::InvalidMacroDefinition("Invalid variable".to_string()))?;
        Ok(MacroInteger::Variable(variable))
    } else if let Some(value_match) = captures.name("expression") {
        let variable = value_match.as_str().to_string();
        Ok(MacroInteger::Expression(variable))
    } else {
        // it has to match one of the named captures.
        unreachable!()
    }
}
