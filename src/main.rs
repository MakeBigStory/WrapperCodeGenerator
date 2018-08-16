extern crate pad;
extern crate syntex_syntax as syntax;
extern crate regex;

use std::collections::HashMap;
use std::env;
use std::path::Path;

use pad::PadStr;
use syntax::codemap::FilePathMapping;

use syntax::ast;
use syntax::codemap::{CodeMap, Loc, Span};
use syntax::errors::DiagnosticBuilder;
use syntax::parse::{self, ParseSess};
use syntax::visit::{self, FnKind, Visitor};

use regex::Regex;

mod ffi_declaration_extractor;
mod ffi_func_aop_generator;
mod code_wrapper;

use code_wrapper::CodeWrapper;

const ES20 : &'static str = "es20";
const ES30 : &'static str = "es30";
const ES31 : &'static str = "es31";
const ES32 : &'static str = "es32";

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Error !!!, some arg is missed !!!");

        return;
    }

    let root_dir = args[1].clone();
    let es20_dir = root_dir.clone() + "/" + ES20;
    let es30_dir = root_dir.clone() + "/" + ES30;
    let es31_dir = root_dir.clone() + "/" + ES31;
    let es32_dir = root_dir.clone() + "/" + ES32;

    let mut es20_generator = ffi_func_aop_generator::FFIFuncAopGenerator::new();
    es20_generator.add_cfg = false;
    es20_generator.add_aop = true;
    let mut es20_funcs = es20_generator.generate_aop_funcs(&es20_dir);

    let mut es30_generator = ffi_func_aop_generator::FFIFuncAopGenerator::new();
    es30_generator.add_cfg = true;
    es30_generator.add_aop = true;
    let mut es30_funcs = es30_generator.generate_aop_funcs(&es30_dir);

    let mut es31_generator = ffi_func_aop_generator::FFIFuncAopGenerator::new();
    es31_generator.add_cfg = true;
    es31_generator.add_aop = true;
    let mut es31_funcs = es31_generator.generate_aop_funcs(&es31_dir);

    let mut es32_generator = ffi_func_aop_generator::FFIFuncAopGenerator::new();
    es32_generator.add_cfg = true;
    es32_generator.add_aop = true;
    let mut es32_funcs = es32_generator.generate_aop_funcs(&es32_dir);

    let mut code_wrapper = code_wrapper::CodeWrapper::new();

    code_wrapper.generate_ffi_ptr_field_declarations(&mut es30_funcs);
    code_wrapper.generate_ffi_ptr_field_declarations(&mut es31_funcs);
    code_wrapper.generate_ffi_ptr_field_declarations(&mut es32_funcs);

    let mut final_code = code_wrapper.get_prelude_code().to_string();

    for func in &es20_funcs {
        final_code = final_code + &func.func_code;
    }

    for func in &es30_funcs {
        final_code = final_code + &func.func_code;
    }

    for func in &es31_funcs {
        final_code = final_code + &func.func_code;
    }

    for func in &es32_funcs {
        final_code = final_code + &func.func_code;
    }

    final_code = final_code + code_wrapper.get_finale_code();

    println!("{}", final_code);
}


