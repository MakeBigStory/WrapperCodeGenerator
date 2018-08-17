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

use super::ffi_declaration_extractor::FFIDeclarationExtractor;
use super::ffi_declaration_extractor::FFIDeclaration;
use super::ffi_declaration_extractor::FFIParam;

#[derive(Debug, Clone)]
pub struct GeneratedFunc {
    pub func_name: String,
    pub func_code: String,
    pub ffi_ptr_name: String,
    pub ffi_declaration: FFIDeclaration,
    pub success: bool,
    pub error: Option<FuncProcessError>
}

impl GeneratedFunc {
    fn new() -> Self {
        GeneratedFunc {
            func_name: "".to_string(),
            func_code: "".to_string(),
            ffi_ptr_name: "".to_string(),
            ffi_declaration: FFIDeclaration::new(),
            success: false,
            error: None
        }
    }
}

const FFI_FILE_PATH : &'static str = "ffi.rs";
const WRAPPER_FILE_PATH : &'static str = "wrapper.rs";

#[derive(Debug)]
struct ParamInfo {
    param_type: String,
    param_name: String
}

#[derive(Debug)]
struct FuncInfo {
    func_name: String,
    func_params: Vec<ParamInfo>
}

#[derive(Debug, Clone)]
pub struct FuncProcessError {
    error_desc: String
}

pub struct FFIFuncAopGenerator<'a> {
    pub add_cfg: bool,
    pub add_aop: bool,
    ffi_call_regex : Regex,
    ffi_gl_func_regex: Regex,
    ffi_gl_func_name_regex: Regex,
    func_body_regex : Regex,
    func_decl_regex : Regex,
    func_param_regex : Regex,

    func_codemap: Option<&'a CodeMap>,
    generate_result: Vec<GeneratedFunc>,
    parse_session: Option<ParseSess>,
    declaration_extractor: FFIDeclarationExtractor,
    declaration_infos: HashMap<String, FFIDeclaration>
}

impl <'a>FFIFuncAopGenerator<'a> {
    pub fn generate_aop_funcs(&mut self, dir_path: &str) -> Vec<GeneratedFunc> {

        let ffi_file_path = dir_path.clone().to_owned() + "/" + FFI_FILE_PATH;

        self.declaration_infos =
            self.declaration_extractor.extract_from_file(ffi_file_path.as_str());

        let wrapper_file_path = dir_path.clone().to_owned() + "/" + WRAPPER_FILE_PATH;

        self.parse_session = Some(ParseSess::new(FilePathMapping::empty()));

        let krate = parse(wrapper_file_path.as_str(), self.parse_session.as_ref().unwrap()).unwrap();

        self.visit_mod(&krate.module, krate.span, &krate.attrs, krate.module.items[0].id);

        self.generate_result.clone()
    }

    pub fn new() -> Self {
        // TODO: my regex is ugly .....
        let ffi_call_regex =  regex::Regex::new(r"[\n]?.*ffi::gl([^;]|\n)*\);").unwrap();

        // TODO: my regex is ugly .....
        let ffi_gl_func_regex = regex::Regex::new(r"ffi::gl([^(])*\(").unwrap();

        // TODO: my regex is ugly .....
        let ffi_gl_func_name_regex = regex::Regex::new(r"gl[^(]*").unwrap();

        let func_body_regex = Regex::new(r"\{\n(.*|\n)*}").unwrap();

        // TODO: my regex is ugly .....
        let func_decl_regex = Regex::new(r"pub ([^)])*\)([^-])* -> ([^{])*\{").unwrap();

        // TODO: my regex is ugly .....
        let func_param_regex = Regex::new(r"\((\n|.)*&(mut )*self(\n|[^-])*\)").unwrap();

        FFIFuncAopGenerator {
            add_cfg: false,
            add_aop: false,
            ffi_call_regex,
            ffi_gl_func_regex,
            ffi_gl_func_name_regex,
            func_body_regex,
            func_decl_regex,
            func_param_regex,
            func_codemap: None,
            generate_result: vec![],
            parse_session: None,
            declaration_extractor: FFIDeclarationExtractor::new(),
            declaration_infos: HashMap::new()
        }
    }
}

const PTR_SUFFIX : &'static str = r#"_ptr"#;
const PTR_CALL_SUFFIX : &'static str = r#"_ptr)("#;

const PTR_CALL_PATTERN : &'static str = r#"std::mem::transmute::<_, extern "system" fn({param}) -> {result}> (self.{ptr_name}"#;

const ANDROID_CFG : &'static str = r#"#[cfg(target_os="android")]"#;
const IOS_CFG : &'static str = r#"#[cfg(target_os="ios")]"#;

impl <'a>FFIFuncAopGenerator<'a> {
    fn generate_ffi_ptr_call(&self, mut ffi_declaration: &FFIDeclaration, ptr_name: &str) -> String {
        let mut params : Vec<String> = vec![];

        for ffi_param in &ffi_declaration.params {
            params.push(ffi_param.param_type.clone());
        }

        let params = params.join(",");

        let mut result = PTR_CALL_PATTERN.replace("{param}", &params);
        result = result.replace("{result}", &ffi_declaration.return_type);
        result = result.replace("{ptr_name}", &ptr_name);

        result
    }

    fn add_cfg_control(&self, mut generated_func: GeneratedFunc) -> Result<GeneratedFunc, FuncProcessError> {

        let func_code = generated_func.func_code.clone();

        let code_parts: Vec<&str> = self.ffi_call_regex.split(&func_code).collect();

        let matches: Vec<&str> = self.ffi_call_regex.find_iter(&func_code).map(
            |match_| match_.as_str()).collect();

        if matches.is_empty() {
            return Err(FuncProcessError {
                error_desc: "Can not find ffi call !!!".to_string()
            });
        }

        let ffi_call_code = matches.get(0).unwrap();

//        println!("{} -> {}", generated_func.func_name, ffi_call_code);

        let gl_func_name_matches: Vec<&str> = self.ffi_gl_func_name_regex.find_iter(ffi_call_code).map(
            |match_| match_.as_str()).collect();

        if gl_func_name_matches.is_empty() {
            return Err(FuncProcessError {
                error_desc: "Can not find gl func name !!!".to_string()
            });
        }

        let ffi_gl_func_name = gl_func_name_matches.get(0).unwrap().to_string();

//        println!("{} -> {}", generated_func.func_name, ffi_gl_func_name);

        let ffi_func_info = self.declaration_infos.get(&ffi_gl_func_name);

        if ffi_func_info.is_none() {
            return Err(FuncProcessError {
                error_desc: generated_func.func_name.to_string() + "Can not find ffi func in ffi func infos !!!"
                        + &ffi_gl_func_name
            });
        }

        let mut ffi_func_info = ffi_func_info.unwrap();
        generated_func.ffi_declaration = ffi_func_info.clone();
        generated_func.ffi_ptr_name = ffi_func_info.func_name.clone() + PTR_SUFFIX;

        let ffi_ptr_call_prefix = ffi_func_info.func_name.clone() + PTR_CALL_SUFFIX;

        let ffi_ptr_call = self.generate_ffi_ptr_call(ffi_func_info, &ffi_ptr_call_prefix);

//        println!("{} -> {}", generated_func.func_name, ffi_ptr_call);

        let replaced_ffi_call_code = self.ffi_gl_func_regex.replace(ffi_call_code,
                                                                    |caps: &regex::Captures| {
                                                                        format!("{}", ffi_ptr_call)
                                                                    }).to_string();

//        println!("{} -> {}", generated_func.func_name, replaced_ffi_call_code);

        let mut res = code_parts[0].to_string() + "\n" +
            IOS_CFG +
            &ffi_call_code + "\n" +
            ANDROID_CFG +
            &replaced_ffi_call_code + "\n" +
            code_parts[1] + "\n";

        generated_func.func_code = res;

        Ok(generated_func)
    }
}

const PRE_PROCESS_CALL : &'static str = r#"
            func_info.func_param_infos = param_infos;
            func_info.func_param_values = param_values;
            self.pre_process(&func_info)?;
"#;

const POST_PROCESS_CALL : &'static str = r#"
            let res_desc = format!("{:?}", res);

            self.post_process(&func_info, &res_desc)?;
"#;

impl <'a>FFIFuncAopGenerator<'a> {

    fn extract_func_body(&self, func_code : &str) -> Result<String, FuncProcessError> {
        let matches: Vec<&str> = self.func_body_regex.find_iter(func_code).map(|match_| match_.as_str())
            .collect();

        if matches.is_empty() {
            return Err(
                FuncProcessError {
                    error_desc : "no func body find !!!".to_string()
                }
            )
        } else {
            let mut func_body = matches.get(0).unwrap().to_string();
            func_body.trim();
            // TODO: ugly logic, remove "{"
            func_body.remove(0);

            // TODO: ugly logic, remove "}"
            func_body.pop();

            return Ok(func_body)
        }
    }

    fn extract_func_decl(&self, func_code: &str) -> Result<String, FuncProcessError> {
        let matches: Vec<&str> = self.func_decl_regex.find_iter(func_code).map(|match_| match_.as_str())
            .collect();

        if matches.is_empty() {
            return Err(
                FuncProcessError {
                    error_desc : "no func decl find !!!".to_string()
                }
            )
        } else {
            let mut func_decl = matches.get(0).unwrap().to_string();

//            println!("extract_func_decl {} -> {}", func_code, func_decl);

            // TODO: ugly logic, remove "{"
            func_decl.pop();

            return Ok(func_decl)
        }
    }

    fn extract_func_params(&self, func_code: &str) -> Vec<ParamInfo> {
        let matches: Vec<&str> = self.func_param_regex.find_iter(func_code).map(|match_| match_.as_str())
            .collect();

        let mut param_infos : Vec<ParamInfo> = vec![];

        if matches.is_empty() {
            return param_infos
        } else {
            let mut func_param_code = matches.get(0).unwrap().to_string();

            func_param_code.replace("\n", "");

            func_param_code.trim();

            // TODO: ugly logic, remove "("
            func_param_code.remove(0);
            // TODO: ugly logic, remove ")"
            func_param_code.pop();

            let func_param_decls : Vec<&str> = func_param_code.split(',').collect();

            for func_param_decl in func_param_decls {
                let f: Vec<&str> = func_param_decl.split(":").collect();
                if f.len() > 1 {
                    let param_info = ParamInfo {
                        param_name: f[0].trim().to_string(),
                        param_type: f[1].trim().to_string(),
                    };
                    param_infos.push(param_info);
                }
            }

            return param_infos
        }
    }

    // TODO: in fact, I prefer Option + Closure to make pre_process as input, but fucking Rust lifetime ....
    fn pre_process(&self, func_info: &mut FuncInfo) -> Result<String, FuncProcessError> {

        let mut res = r#"
            if self.is_debug() {
        "#.to_string();

        for param in &func_info.func_params {
            if param.param_type == "&str" {
                res = res + &format!(r#"
                                        let mut param_info_{} = ParamInfo::new("{}", "{}");
                                        let param_value_{} = {}.to_string();
                                    "#,
                                     param.param_name,
                                     param.param_type, param.param_name, param.param_name, param.param_name);
            } else if param.param_type.contains("[") { // slice
                res = res + &format!(r#"
                                        let mut param_info_{} = ParamInfo::new("{}", "{}");
                                        let param_value_{} = {}.to_vec();
                                    "#,
                                     param.param_name,
                                     param.param_type, param.param_name, param.param_name, param.param_name);
            } else {
                res = res + &format!(r#"
                                        let mut param_info_{} = ParamInfo::new("{}", "{}");
                                    "#,
                                     param.param_name, param.param_type, param.param_name);
            }
        }


        // TODO: foolish format! marco, I can not make these pattern code as const static &str ....
        res =  res + &format!(
            r#"
                let mut param_values: Vec<&Param> = vec![];
                let mut param_infos: Vec<&ParamInfo> = vec![];

                let mut func_info = FuncInfo::new();
                func_info.func_name = "{}".to_string();
        "#, func_info.func_name);

        for param in &func_info.func_params {
            if param.param_type == "&str" {
                res = res + &format!(r#"
                                        param_infos.push(&param_info_{});
                                        param_values.push(&param_value_{});
                                    "#,
                                     param.param_name, param.param_name);
            } else if param.param_type.contains("[") { // slice
                res = res + &format!(r#"
                                        param_infos.push(&param_info_{});
                                        param_values.push(&param_value_{});
                                    "#,
                                     param.param_name, param.param_name);
            } else {
                res = res + &format!(r#"
                                        param_infos.push(&param_info_{});
                                        param_values.push(&{});
                                    "#,
                                     param.param_name, param.param_name);
            }
        }

        res = res + "";
        res = res + PRE_PROCESS_CALL;

        Ok(res)
    }

    // TODO: in fact, I prefer Option + Closure to make post_process as input,, but fucking Rust lifetime ....
    fn post_process(&self, func_info: &mut FuncInfo) -> Result<String, FuncProcessError> {
        Ok(POST_PROCESS_CALL.to_string())
    }

    fn add_aop_code(&self, mut generated_func: GeneratedFunc) -> Result<GeneratedFunc, FuncProcessError> {
        let func_body = self.extract_func_body(&generated_func.func_code)?;

        let func_decl = self.extract_func_decl(&generated_func.func_code)?;

        let func_param_infos = self.extract_func_params(&generated_func.func_code);

        let mut func_info = FuncInfo {
            func_name: generated_func.func_name.to_string(),
            func_params: func_param_infos
        };


        let mut composed_func = func_decl + "{\n";

        match self.pre_process(&mut func_info) {
            Ok(pre_process_code) => {
                composed_func = composed_func + "\n" + &pre_process_code;
                composed_func = composed_func + "\n";
            },
            Err(error) => return Err(error)
        }

        composed_func = composed_func + "let res = {\n" + &func_body + "}";


        match self.post_process(&mut func_info) {
            Ok(post_process_code) => {
                composed_func = composed_func + ";\n" + &post_process_code;
                composed_func = composed_func + "\n";
            },
            Err(error) => return Err(error)
        }

        composed_func = composed_func + "res\n";

        composed_func = composed_func + "}\n";

        composed_func = composed_func + "else {\n" + &func_body + "\n}\n \n}\n";

        generated_func.func_code = composed_func;

        Ok(generated_func)
    }

    fn process_func(&self, mut generated_func: GeneratedFunc) -> Result<GeneratedFunc, FuncProcessError> {

        let mut res_func = generated_func;

        if self.add_cfg {
            res_func  = self.add_cfg_control(res_func)?;
        }

        if self.add_aop {
            res_func  = self.add_aop_code(res_func)?;
        }

        Ok(res_func)
    }
}

fn parse<'a, T: ?Sized + AsRef<Path>>(path: &T,
                                      parse_session: &'a ParseSess)
                                      -> Result<ast::Crate, Option<DiagnosticBuilder<'a>>> {
    let path = path.as_ref();

    match parse::parse_crate_from_file(path,  parse_session) {
        // There may be parse errors that the parser recovered from, which we
        // want to treat as an error.
        Ok(_) if parse_session.span_diagnostic.has_errors() => Err(None),
        Ok(krate) => Ok(krate),
        Err(e) => Err(Some(e)),
    }
}

impl <'a>FFIFuncAopGenerator<'a> {

    fn format_loc(loc: &Loc) -> String {
        format!("{}:{}", loc.line, loc.col.0)
    }

    fn format_span(&self, span: Span) -> String {
        format!("{}-{}",
                FFIFuncAopGenerator::format_loc(&self.parse_session.as_ref().unwrap().codemap().lookup_char_pos(span.lo)),
                FFIFuncAopGenerator::format_loc(&self.parse_session.as_ref().unwrap().codemap().lookup_char_pos(span.hi)))
    }
}

fn func_filter(func_name : &str) -> bool {
    return func_name != "gl_get_error";
//        && func_name != "gl_tex_image_3d"
//        && func_name != "gl_tex_sub_image_3d"
//        && func_name != "gl_get_uniform_indices"
//        && func_name != "gl_map_buffer_range"
//        && func_name != "gl_transform_feedback_varyings"
//        && func_name != "gl_debug_message_callback";
}

impl<'v, 'a> Visitor<'v> for FFIFuncAopGenerator<'a> {
    fn visit_fn(&mut self,
                fn_kind: FnKind<'v>,
                fn_decl: &'v ast::FnDecl,
                span: Span,
                _id: ast::NodeId) {
        let fn_name = match fn_kind {
            FnKind::ItemFn(id, _, _, _, _, _, _) |
            FnKind::Method(id, _, _, _) => id.name.as_str().to_string(),
            FnKind::Closure(_) => format!("<closure at {}>", self.format_span(span)),
        };

        let mut generated_func= GeneratedFunc::new();
        generated_func.func_name = fn_name.clone();

        let mut func_code = self.parse_session.as_ref().unwrap().codemap().span_to_snippet(span);
        match func_code {
            Ok(desc) =>
                {
                    generated_func.func_code = desc.clone();

                    match self.process_func(generated_func) {
                        Ok(mut success_generated_func) => {
                            success_generated_func.success = true;

                            match func_filter(&fn_name) {
                                true => {},
                                false => {
                                    success_generated_func.func_code = desc.clone();
                                }
                            }

                            self.generate_result.push(success_generated_func);
                        },
                        Err(error) => {
                            panic!("Process {} error {:?} !!!", fn_name, error);
                        }
                    }
                }

            Err(_) => {
                generated_func.error = Some(FuncProcessError{
                    error_desc: "AST Parse func code fail !!!".to_string()
                })
            }
        }



        // Continue walking the rest of the funciton so we pick up any functions
        // or closures defined in its body.
        visit::walk_fn(self, fn_kind, fn_decl, span);
    }

    // The default implementation panics, so this is needed to work on files
    // with macro invocations, eg calls to `format!()` above. A better solution
    // would be to expand macros before walking the AST, but I haven't looked at
    // how to do that. We will miss any functions defined via a macro, but
    // that's fine for this example.
    fn visit_mac(&mut self, _mac: &'v ast::Mac) {}
}


