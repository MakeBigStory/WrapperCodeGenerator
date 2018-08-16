use std::collections::HashMap;
extern crate regex;
use std::fs::File;
use std::io::prelude::*;

use regex::Regex;

#[derive(Debug, Clone)]
pub struct FFIParam {
    pub param_name: String,
    pub param_type: String
}

impl FFIParam {
    pub fn new() -> Self {
        FFIParam {
            param_name: "".to_string(),
            param_type: "".to_string()
        }
    }
}

#[derive(Debug, Clone)]
pub struct FFIDeclaration {
    pub func_name: String,
    pub params: Vec<FFIParam>,
    pub return_type: String
}

impl FFIDeclaration {
    pub fn new() -> Self {
        FFIDeclaration {
            func_name: "".to_string(),
            params: vec![],
            return_type: "".to_string()
        }
    }
}

pub struct FFIDeclarationExtractor {
    ffi_declaration_regex: Regex,
    ffi_name_regex: Regex,
    ffi_params_regex: Regex,
    ffi_return_type_regex: Regex
}

impl FFIDeclarationExtractor {
    pub fn new() -> Self {
        // TODO: my regex is ugly .....
        let ffi_declaration_regex =  regex::Regex::new(r"pub fn gl([^(])*\(([^)])*\)([^;])*;").unwrap();

        let ffi_name_regex =  regex::Regex::new(r"gl([^(])*").unwrap();

        let ffi_params_regex =  regex::Regex::new(r"\((\n|.)*\)").unwrap();

        let ffi_return_type_regex =  regex::Regex::new(r"->(.|\n)*;").unwrap();

        FFIDeclarationExtractor {
            ffi_declaration_regex,
            ffi_name_regex,
            ffi_params_regex,
            ffi_return_type_regex
        }
    }

    pub fn extract_from_file(&self, file_path : &str) -> HashMap<String, FFIDeclaration> {
        let mut file = File::open(file_path);

        if file.is_err() {
            println!("Error !!! file {} open fail !!!", file_path);
            return HashMap::new();
        }

        let mut contents = String::new();
        let read_res = file.unwrap().read_to_string(&mut contents);

        if read_res.is_err() {
            println!("Error !!! file {} read fail !!!", file_path);
            return HashMap::new();
        }

        self.extract(&contents)
    }

    fn extract_func_name(&self, code: &str) -> Option<String> {
        let name_matches: Vec<&str> = self.ffi_name_regex.find_iter(code).map(
            |match_| match_.as_str()).collect();

        if name_matches.is_empty() {
            return None;
        } else {
            Some(name_matches.get(0).unwrap().to_string())
        }
    }

    fn extract_func_return_type(&self, code: &str) -> String {
        let return_type_matches: Vec<&str> = self.ffi_return_type_regex.find_iter(code).map(
            |match_| match_.as_str()).collect();

        if return_type_matches.is_empty() {
            return "()".to_string();
        } else {
           let mut return_type_str = return_type_matches.get(0).unwrap().to_string();

            return_type_str = return_type_str.replace("->", "");
            return_type_str = return_type_str.replace(";", "");
            return_type_str = return_type_str.replace("\n", "");
            return_type_str.trim();

            return return_type_str;
        }
    }

    fn extract_func_params(&self, code: &str) -> Option<Vec<FFIParam>> {
        let mut res= vec![];

        let params_matches: Vec<&str> = self.ffi_params_regex.find_iter(code).map(
            |match_| match_.as_str()).collect();

        if params_matches.is_empty() {
            return None;
        } else {
            let mut params_str = params_matches.get(0).unwrap().to_string();

            params_str.trim();
            params_str = params_str.replace("\n", "");
            params_str.pop();
            params_str.remove(0);

            let param_str_parts: Vec<&str> = params_str.split(",").collect();

            for param_str in &param_str_parts {
                let param_name_type: Vec<&str> = param_str.split(":").collect();

                if param_name_type.len() < 2 {
                    continue
                } else {
                    let mut param_info = FFIParam::new();
                    param_info.param_name = param_name_type.get(0).unwrap().clone().to_string();
                    param_info.param_type = param_name_type.get(1).unwrap().clone().to_string();

                    param_info.param_name = param_info.param_name.replace("\n", "");
                    param_info.param_name.trim();
                    param_info.param_type = param_info.param_type.replace("\n", "");
                    param_info.param_type.trim();

//                    println!("Param {} {}", param_info.param_name, param_info.param_type);

                    res.push(param_info);
                }
            }
        }

        Some(res)
    }

    fn extract_func_declaration(&self, decl : &str) -> Option<FFIDeclaration> {
       let mut declaration = FFIDeclaration::new();

        let mut func_name = self.extract_func_name(decl);

        if func_name.is_none() {
            return None;
        }

        declaration.func_name = func_name.unwrap().clone();

        let mut param_info = self.extract_func_params(decl);
        if param_info.is_none() {
            return None;
        }

        declaration.params = param_info.unwrap();

        let mut return_type = self.extract_func_return_type(decl);

        declaration.return_type = return_type;

        Some(declaration)
    }

    pub fn extract(&self, code : &str) -> HashMap<String, FFIDeclaration> {
        let matches: Vec<&str> = self.ffi_declaration_regex.find_iter(code).map(
            |match_| match_.as_str()).collect();

        let mut result = HashMap::new();

        for decl in matches {

            let mut declaration_info = self.extract_func_declaration(decl);

            match declaration_info {
                Some(declaration) => {
                    let tmp = declaration.clone();
                    result.insert(declaration.func_name, tmp);
                },
                None => {
                    println!("Error !!!, parse {} decl fail !!!", decl);
                }
            }
        }

        result
    }
}

