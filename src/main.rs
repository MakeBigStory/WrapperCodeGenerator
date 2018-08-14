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

#[derive(Debug)]
struct FuncProcessError {
    error_desc: String
}

trait FuncHandler {
    fn compose_func(&self, func_name: &str, func_code: &str) -> Result<String, FuncProcessError>;

    fn get_prelude_code(&self) -> &str;
}

struct FuncFfiCfgProcessor {
    ffi_call_regex : Regex,
    ffi_gl_func_regex: Regex
}

impl FuncFfiCfgProcessor {
    fn new() -> Self {
        // TODO: my regex is ugly .....
        let ffi_call_regex =  regex::Regex::new(r"[\n]?.*ffi::gl([^;]|\n)*\);").unwrap();

        // TODO: my regex is ugly .....
        let ffi_gl_func_regex = regex::Regex::new(r"ffi::gl.*\(").unwrap();

        FuncFfiCfgProcessor {
            ffi_call_regex,
            ffi_gl_func_regex
        }
    }
}

const GET_PROC_ADDRESS_CALL_CODE : &'static str = r#"ptr("#;

const ANDROID_CFG : &'static str = r#"[cfg(target_os="android")]"#;
const IOS_CFG : &'static str = r#"[cfg(target_os="ios")]"#;

impl FuncHandler for FuncFfiCfgProcessor {
    fn compose_func(&self, func_name: &str, func_code: &str) -> Result<String, FuncProcessError> {

        let code_parts: Vec<&str> = self.ffi_call_regex.split(func_code).collect();

        let matches: Vec<&str> = self.ffi_call_regex.find_iter(func_code).map(
            |match_| match_.as_str()).collect();

        if matches.is_empty() {
            return Err(FuncProcessError {
                error_desc: "Can not find ffi call !!!".to_string()
            });
        }

        let ffi_call_code = matches.get(0).unwrap();
        // println!("matches: {:?} \n", ffi_call_code);

        let replaced_ffi_call_code = self.ffi_gl_func_regex.replace(ffi_call_code,
                                                                    GET_PROC_ADDRESS_CALL_CODE).to_string();

        // println!("replaced {:?}", replaced_ffi_call_code);

        let res = code_parts[0].to_string() + "\n" +
            IOS_CFG +
            &ffi_call_code + "\n" +
            ANDROID_CFG +
            &replaced_ffi_call_code + "\n" +
            code_parts[1] + "\n";

        // println!("res {:?}", res);

        Ok(res)
    }

    fn get_prelude_code(&self) -> &str {
        return "";
    }
}

struct FuncAopProcessor {
    func_body_regex : Regex,
    func_decl_regex : Regex,
    func_param_regex : Regex
}

const PRELUDE_CODE: &'static str = r#"
    use super::data_struct::*;
    use super::ffi::*;
    use super::*;
    use types::TextureUnit;
    use types::BufferTarget;
    use types::FrameBufferTarget;
    use types::RenderBufferTarget;
    use types::TextureBindTarget;
    use types::BlendEquationMode;
    use types::BlendFactor;
    use types::BufferUsage;
    use types::FrameBufferStatus;
    use types::TextureTarget;
    use types::ShaderType;
    use types::FaceMode;
    use types::FuncType;
    use types::FeatureType;
    use types::BeginMode;
    use types::FrameBufferAttachmentType;
    use types::FrontFaceDirection;
    use types::StateType;
    use types::BufferParamName;
    use types::ErrorType;
    use types::FrameBufferAttachmentParamType;
    use types::ProgramParamType;
    use types::RenderBufferParamType;
    use types::ShaderParamType;
    use types::ShaderPrecisionType;
    use types::ConstantType;
    use types::TextureParamType;
    use types::VertexAttributeParamType;
    use types::HintTargetType;
    use types::HintBehaviorType;
    use types::PackParamType;
    use types::PixelFormat;
    use types::PixelDataType;
    use types::ActionType;
    use types::DataType;

    #[derive(Debug)]
    pub struct Active {
        pub name: String,
        pub size: i32,
        pub type_: DataType,
        pub length: i32,
    }

    #[derive(Debug)]
    pub struct ShaderPrecisionFormat {
        pub precision: i32,
        pub range: [i32; 2],
    }

    #[derive(Debug)]
    pub struct Error {

    }

    pub struct Wrapper {
    }

    trait Interceptor {
        fn intercept(&mut self, fun_name: &str) -> Result<(), Error>;
    }

    trait Param: std::fmt::Debug {
    }

    impl Param for i32 {
    }

    impl Param for u32 {
    }

    impl Param for f32 {
    }

    impl Param for bool {
    }

    impl Param for String {
    }

    impl Param for TextureUnit {
    }

    impl Param for BufferTarget {
    }

    impl Param for FrameBufferTarget {
    }

    impl Param for RenderBufferTarget {
    }

    impl Param for TextureBindTarget {
    }

    impl Param for BlendEquationMode {
    }

    impl Param for BlendFactor {
    }

    impl Param for BufferUsage {
    }

    impl Param for FrameBufferStatus {
    }

    impl Param for TextureTarget {
    }

    impl Param for ShaderType {
    }

    impl Param for FaceMode {
    }

    impl Param for FuncType {
    }

    impl Param for FeatureType {
    }

    impl Param for BeginMode {
    }

    impl Param for FrameBufferAttachmentType {
    }

    impl Param for FrontFaceDirection {
    }

    impl Param for StateType {
    }

    impl Param for BufferParamName {
    }

    impl Param for ErrorType {
    }

    impl Param for FrameBufferAttachmentParamType {
    }

    impl Param for ProgramParamType {
    }

    impl Param for RenderBufferParamType {
    }

    impl Param for ShaderParamType {
    }

    impl Param for ShaderPrecisionType {
    }

    impl Param for ConstantType {
    }

    impl Param for TextureParamType {
    }

    impl Param for VertexAttributeParamType {
    }

    impl Param for HintTargetType {
    }

    impl Param for HintBehaviorType {
    }

    impl Param for PackParamType {
    }

    impl Param for PixelFormat {
    }

    impl Param for PixelDataType {
    }

    impl Param for ActionType {
    }

    impl Param for DataType {
    }

    impl <T> Param for Vec<T> where T: std::fmt::Debug {
    }

    #[derive(Debug)]
    struct ParamInfo {
        param_type: String,
        param_name: String,
    }

    impl ParamInfo {
        fn new(param_name: &str, param_type: &str) -> Self {
            ParamInfo {
                param_type: param_type.to_string(),
                param_name: param_name.to_string()
            }
        }
    }

    #[derive(Debug)]
    struct FuncInfo<'a> {
        func_name: String,
        func_param_infos: Vec<&'a ParamInfo>,
        func_param_values: Vec<&'a Param>
    }

    impl<'a> FuncInfo<'a> {
        fn new() -> Self {
            FuncInfo {
                func_name: "".to_string(),
                func_param_infos: vec![],
                func_param_values: vec![]
            }
        }
    }

    impl Wrapper {

            fn pre_process(&mut self, func_info: &FuncInfo) -> Result<(), Error> {
                Ok(())
            }

            fn post_process(&mut self, func_info: &FuncInfo, error_desc: &str) -> Result<(), Error> {
                Ok(())
            }

            fn is_debug(&self) -> bool {
                true
            }

"#;

const PRE_PROCESS_CALL : &'static str = r#"
            func_info.func_param_infos = param_infos;
            func_info.func_param_values = param_values;
            self.pre_process(&func_info)?;
"#;

const POST_PROCESS_CALL : &'static str = r#"
            let res_desc = format!("{:?}", res);

            self.post_process(&func_info, &res_desc)?;
"#;

impl FuncAopProcessor {
    fn new() -> Self {
        // TODO: my regex is ugly .....
        let func_body_regex = Regex::new(r"\{\n(.*\n)*").unwrap();

        // TODO: my regex is ugly .....
        let func_decl_regex = Regex::new(r"pub (.|\n)* -> .* \{").unwrap();

        // TODO: my regex is ugly .....
        let func_param_regex = Regex::new(r"\((\n|.)*&mut self(\n|[^>])*\)").unwrap();

        FuncAopProcessor {
            func_body_regex,
            func_decl_regex,
            func_param_regex
        }
    }

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

    fn add_cfg_on_ffi_call(&self, func_name: &str, func_code: &str) -> Result<String, FuncProcessError> {
        let v: Vec<&str> = func_code.split("^ffi::gl.*);").collect();

        println!("cfg {:?}", v);

        Ok("".to_string())
    }

    // TODO: in fact, I prefer Option + Closure to make pre_process as input, but fucking Rust lifetime ....
    fn pre_process(&self, func_info: &mut FuncInfo) -> Result<String, FuncProcessError> {

        // TODO: foolish format! marco, I can not make these pattern code as const static &str ....
        let mut res = format!(
            r#"
                if self.is_debug() {{
                    let mut param_values: Vec<&Param> = vec![];
                    let mut param_infos: Vec<&ParamInfo> = vec![];

                    let mut func_info = FuncInfo::new();
                    func_info.func_name = "{}".to_string();
        "#, func_info.func_name);

        for param in &func_info.func_params {
            if param.param_type == "&str" {
                res = res + &format!(r#"
                                        let mut param_info = ParamInfo::new("{}", "{}");
                                        param_infos.push(&param_info);
                                        let param_value = {}.to_string();
                                        param_values.push(&param_value);
                                    "#,
                                     param.param_name,
                                     param.param_type, param.param_name);
            } else if param.param_type.contains("[") { // slice
                res = res + &format!(r#"
                                        let mut param_info = ParamInfo::new("{}", "{}");
                                        param_infos.push(&param_info);
                                        let param_value = {}.to_vec();
                                        param_values.push(&param_value);
                                    "#,
                                     param.param_name,
                                     param.param_type, param.param_name);
            } else {
                res = res + &format!(r#"
                                        let mut param_info = ParamInfo::new("{}", "{}");
                                        param_infos.push(&param_info);
                                        param_values.push(&{});
                                    "#,
                                     param.param_name, param.param_type, param.param_name);
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
}

impl FuncHandler for FuncAopProcessor {
    fn compose_func(&self, func_name: &str, func_code: &str) -> Result<String, FuncProcessError> {
        let func_body = self.extract_func_body(func_code)?;

        let func_decl = self.extract_func_decl(func_code)?;

        let func_param_infos = self.extract_func_params(func_code);

        let mut func_info = FuncInfo {
            func_name: func_name.to_string(),
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

        Ok(composed_func)
    }

    fn get_prelude_code(&self) -> &str {
        return PRELUDE_CODE;
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

const FFI_MODE : &'static str = r#"ffi"#;
const AOP_MODE : &'static str = r#"aop"#;

fn get_func_handler(mode: &str) -> Option<Box<FuncHandler>>{
    if FFI_MODE == mode {
        Some(Box::new(FuncFfiCfgProcessor::new()))
    } else if AOP_MODE == mode {
        Some(Box::new(FuncAopProcessor::new()))
    } else {
        None
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Error !!!, some arg is missed !!!");

        return;
    }

    let parse_session = ParseSess::new(FilePathMapping::empty());
    let krate = parse(args[2].as_str(), &parse_session).unwrap();

    let func_handler = get_func_handler(args[1].as_str());

    if func_handler.is_none() {
        println!("Error !!!, mode arg {} is invalid !!!", args[1]);

        return;
    }

    let mut func_composer = FuncComposer::new(parse_session.codemap(),
                                              func_handler.unwrap());

    func_composer.compose(&krate);

    println!("{}", &func_composer.output());
}

struct FuncComposeResult {
    func_name: String,
    func_code: String,
    success: bool,
    error: Option<FuncProcessError>
}

impl FuncComposeResult {
    fn new() -> Self {
        FuncComposeResult {
            func_name: "".to_string(),
            func_code: "".to_string(),
            success: false,
            error: None
        }
    }
}

struct FuncComposer<'a> {
    func_handler: Box<FuncHandler>,
    func_codemap: &'a CodeMap,
    func_compose_result: Vec<FuncComposeResult>
}

impl<'a> FuncComposer<'a> {
    pub fn new(codemap: &'a CodeMap, handler: Box<FuncHandler>) -> Self {
        FuncComposer {
            func_handler: handler,
            func_codemap: codemap,
            func_compose_result: vec![]
        }
    }

    pub fn compose(&mut self, krate: &ast::Crate) {
        self.visit_mod(&krate.module, krate.span, &krate.attrs, krate.module.items[0].id);
    }

    pub fn output(&self) -> String {
        let mut output = String::new();

        for compose_result in &self.func_compose_result {
            if (compose_result.success) {
                output = output + &compose_result.func_code;
            } else {
                println!("Error!!! {} compose fail !!! {:?}", compose_result.func_name, compose_result.error)
            }
        }

        output = self.func_handler.get_prelude_code().to_string() + &output;

        output
    }

    fn format_loc(loc: &Loc) -> String {
        format!("{}:{}", loc.line, loc.col.0)
    }

    fn format_span(&self, span: Span) -> String {
        format!("{}-{}",
                FuncComposer::format_loc(&self.func_codemap.lookup_char_pos(span.lo)),
                FuncComposer::format_loc(&self.func_codemap.lookup_char_pos(span.hi)))
    }
}

fn func_filter(func_name : &str) -> bool {
    return func_name != "gl_get_error";
}

impl<'v, 'a> Visitor<'v> for FuncComposer<'a> {
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

        let mut compose_result = FuncComposeResult::new();
        compose_result.func_name = fn_name.clone();

        let mut func_code = self.func_codemap.span_to_snippet(span);
        match func_code {
            Ok(desc) =>
                match func_filter(&fn_name) {
                        true => match self.func_handler.compose_func(&fn_name, &desc) {
                        Ok(composed_func) => {
                            compose_result.func_code = composed_func.clone();
                            compose_result.success = true;
                        },
                        Err(error) => {
                            compose_result.error = Some(error);
                        }
                    },
                    false => {
                        compose_result.func_code = desc.clone();
                        compose_result.success = true;
                    }

                }

            Err(_) => {
                compose_result.error = Some(FuncProcessError{
                    error_desc: "AST Parse func code fail !!!".to_string()
                })
            }
        }

        self.func_compose_result.push(compose_result);

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
