extern crate gerber_parser;

use anyhow::anyhow;
use gerber_types::Command;

pub fn main() -> anyhow::Result<()> {
    use std::fs::File;
    use std::io::BufReader;

    let path = "assets/reference_files/two_square_boxes.gbr";
    
    // open a .gbr file from system
    let file = File::open(path).unwrap();
    let reader = BufReader::new(file);

    // Now we parse the file to a GerberDoc, if io errors occur a partial gerber_doc will be returned along with the error
    let gerber_doc = gerber_parser::parse(reader)
        .map_err(|(_doc, parse_error)| anyhow!("Error parsing file: {:?}", parse_error))?;

    let commands: Vec<&Command> = gerber_doc.as_commands();
    
    // Now you can use the commands as you wish
    println!("Parsed document. command_count: {} ", commands.len());
    dump_commands(&commands);

    // there are other methods that consume the document to yield an 'atomic' representation purely
    // in terms of types defined in the gerber-types crate
    let _commands: Vec<Command> = gerber_doc.into_commands();
    
    Ok(())
}

pub fn dump_commands(commands: &Vec<&Command>) {
    for command in commands {
        println!("{:?}", command);
    }
}