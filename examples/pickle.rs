// Copyright (c) 2015-2021 Georg Brandl.  Licensed under the Apache License,
// Version 2.0 <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0>
// or the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at
// your option. This file may not be copied, modified, or distributed except
// according to those terms.

//! A simple example executable that manipulates pickle streams to demonstrate
//! the library's features.

use std::env;
use std::error::Error;
use std::fs::File;
use std::io::{stdin, stdout, Read};
use std::process::exit;

use serde_json as json;
use serde_pickle as pickle;

fn main() -> Result<(), Box<dyn Error>> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        println!("Usage: pickle (decode | transcode | to_json | from_json) [filename]");
        println!("");
        println!("Input is either given file or stdin.");
        println!("decode:    decode and display pickle");
        println!("transcode: decode and re-encode pickle");
        println!("to_json:   decode and jsonify pickle");
        println!("from_json: encode pickle from json");
        exit(1);
    }

    let reader: Box<dyn Read> = if args.len() == 3 {
        Box::new(File::open(&args[2])?)
    } else {
        Box::new(stdin())
    };

    match &*args[1] {
        "decode" => {
            let decoded: pickle::Value = pickle::value_from_reader(reader, Default::default())?;
            println!("{:#?}", decoded);
        },
        "transcode" => {
            let decoded: pickle::Value = pickle::value_from_reader(reader, Default::default())?;
            pickle::value_to_writer(&mut stdout(), &decoded, Default::default())?;
        },
        "to_json" => {
            let decoded: json::Value = pickle::from_reader(reader, Default::default())?;
            println!("{:#?}", decoded);
        },
        "from_json" => {
            let decoded: json::Value = json::from_reader(reader)?;
            pickle::to_writer(&mut stdout(), &decoded, pickle::SerOptions::new().proto_v2())?;
        }
        _ => {
            println!("No such subcommand.");
            exit(1);
        }
    }
    Ok(())
}
