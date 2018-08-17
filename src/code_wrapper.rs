use super::ffi_declaration_extractor::*;
use super::ffi_func_aop_generator::*;
use std::collections::HashSet;

const PRELUDE_CODE: &'static str = r#"
    use es30::ffi::*;
    use es31::ffi::*;
    use es32::ffi::*;

    use types::*;
    use consts::*;
    use enums::*;
    use std::mem::size_of;
    use std::str::from_utf8;

    use super::ffi::*;
    use super::*;
    use egl::ffi::eglGetProcAddress;

    use types::GLDEBUGPROC;

    use std::mem;

    use std::ptr;
    use std::slice;

    use libc::{c_char, c_int, c_short, c_uchar, c_uint, c_ushort, c_void};

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

    #[derive(Clone, Debug, PartialEq)]
    pub struct ProgramBinary{
        pub length: GLsizei,
        pub binary_format: GLenum,
        pub binary: Vec<u8>
    }

    #[derive(Debug)]
    pub struct Error {
    }

    pub struct Wrapper {
        debug: bool,
    {struct_decl}
    }

    trait Interceptor {
        fn pre_intercept(&mut self, func_info: &FuncInfo) -> Result<(), Error>;

        fn post_intercept(&mut self, func_info: &FuncInfo, res_desc: &str) -> Result<(), Error>;
    }

    trait Param: std::fmt::Debug {
    }

    impl Param for i32 {}

    impl Param for u32 {}

    impl Param for f32 {}

    impl Param for bool {}

    impl Param for String {}

    impl Param for TextureUnit {}

    impl Param for BufferTarget {}

    impl Param for FrameBufferTarget {}

    impl Param for RenderBufferTarget {}

    impl Param for TextureBindTarget {}

    impl Param for BlendEquationMode {}

    impl Param for BlendFactor {}

    impl Param for BufferUsage {}

    impl Param for FrameBufferStatus {}

    impl Param for TextureTarget {}

    impl Param for ShaderType {}

    impl Param for FaceMode {}

    impl Param for FuncType {}

    impl Param for FeatureType {}

    impl Param for BeginMode {}

    impl Param for FrameBufferAttachmentType {}

    impl Param for FrontFaceDirection {}

    impl Param for StateType {}

    impl Param for BufferParamName {}

    impl Param for ErrorType {}

    impl Param for FrameBufferAttachmentParamType {}

    impl Param for ProgramParamType {}

    impl Param for RenderBufferParamType {}

    impl Param for ShaderParamType {}

    impl Param for ShaderPrecisionType {}

    impl Param for ConstantType {}

    impl Param for TextureParamType {}

    impl Param for VertexAttributeParamType {}

    impl Param for HintTargetType {}

    impl Param for HintBehaviorType {}

    impl Param for PackParamType {}

    impl Param for PixelFormat {}

    impl Param for PixelDataType {}

    impl Param for ActionType {}

    impl Param for DataType {}

    impl Param for SamplerParameter {}

    impl Param for BufferMask {}

    impl Param for FilterMode {}

    impl Param for FramebufferTarget {}

    impl Param for AttachmentTarget {}

    impl Param for ColorBufferMode {}

    impl Param for BufferObjectTarget {}

    impl Param for BufferMapTarget {}

    impl Param for MappingBit {}

    impl Param for TransformFeedbackMode {}

    impl Param for PixelDataFormat {}

    impl Param for TransformFeedbackObjectTarget {}

    impl Param for u64 {}

    impl Param for *const types::__GLsync {}

    impl Param for *mut i32 {}

    impl Param for *mut f32 {}

    impl Param for *mut u32 {}

    impl Param for *mut i8 {}

    impl Param for *mut u8 {}

    impl Param for *const i8 {}

    impl Param for *const i32 {}

    impl Param for *const u32 {}

    impl Param for *const f32 {}

    impl Param for *const u8 {}

    impl Param for *const *const u8 {}

    impl Param for isize {}

    impl Param for u8 {}

    impl Param for *const std::os::raw::c_void {}

    impl Param for *mut std::os::raw::c_void {}

    impl Param for *mut *mut std::os::raw::c_void {}

    impl Param for *const *const i8 {}

    impl Param for *const libc::c_void {}

    impl Param for *mut *mut libc::c_void {}

    impl Param for *mut libc::c_void {}

    impl <T> Param for Vec<T> where T: std::fmt::Debug {
    }

    #[derive(Debug)]
    struct ParamInfo {
        param_type: String,
        param_name: String,
    }

    impl ParamInfo {
        fn new(param_type: &str, param_name: &str) -> Self {
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

    struct InitError {
        desc: String
    }

    impl Wrapper {

            fn pre_process(&mut self, func_info: &FuncInfo) -> Result<(), Error> {
                Ok(())
            }

            fn post_process(&mut self, func_info: &FuncInfo, res_desc: &str) -> Result<(), Error> {
                Ok(())
            }

            fn is_debug(&self) -> bool {
                self.debug
            }

            fn set_debug(&mut self, debug: bool) {
                self.debug = debug;
            }

            fn new() -> Self {
                Wrapper {
                    debug: false,
                    {ptr_null}
                }
            }

            fn init(&mut self) -> Result<(), InitError> {
                {ptr_init}

                Ok(())
            }

            fn get_proc_address(&mut self, proc_name: &str) -> Result<*const c_void, InitError> {
                unsafe {
                    let string = CString::new(proc_name).unwrap();

                    let address = eglGetProcAddress(string.as_ptr());

                    if address.is_null() {
                        return Err(InitError{ desc: proc_name.to_string()});
                    } else {
                        return Ok(address);
                    }
                }
            }

"#;

const FINALE_CODE: &'static str = r#"
}
"#;

pub struct CodeWrapper {
    ffi_ptr_field_declarations: String,
    ffi_ptr_zero_declarations: String,
    ffi_ptr_init_declarations: String
}

impl CodeWrapper {
    pub fn new() -> Self {
        CodeWrapper{
            ffi_ptr_field_declarations: "".to_string(),
            ffi_ptr_zero_declarations: "".to_string(),
            ffi_ptr_init_declarations: "".to_string()
        }
    }

    pub fn get_prelude_code(&mut self) -> String {
        let mut preclude = PRELUDE_CODE.replace("{struct_decl}", &self.ffi_ptr_field_declarations);
        preclude = preclude.replace("{ptr_null}", &self.ffi_ptr_zero_declarations);
        preclude = preclude.replace("{ptr_init}", &self.ffi_ptr_init_declarations);

        return preclude;
    }

    pub fn get_finale_code(&mut self) -> &str {
        return FINALE_CODE;
    }

    pub fn reset(&mut self) {
        self.ffi_ptr_field_declarations.clear();
        self.ffi_ptr_zero_declarations.clear();
        self.ffi_ptr_init_declarations.clear();
    }

    pub fn generate_ffi_ptr_field_declarations(&mut self, generated_funcs: &mut Vec<GeneratedFunc>) {

        let mut ffi_func_set = HashSet::new();

        for generated_func in generated_funcs {
            if ffi_func_set.contains(&generated_func.ffi_ptr_name) {
                continue;
            } else {
                ffi_func_set.insert(&generated_func.ffi_ptr_name);
            }

            self.ffi_ptr_field_declarations += &generated_func.ffi_ptr_name;
            self.ffi_ptr_field_declarations += " : *const c_void,\n";

            self.ffi_ptr_zero_declarations += &generated_func.ffi_ptr_name;
            self.ffi_ptr_zero_declarations += ": 0 as *const c_void,\n";

            self.ffi_ptr_init_declarations += &format!("self.{} = self.get_proc_address(\"{}\")?;\n",
                                                      &generated_func.ffi_ptr_name,
                                                      &generated_func.ffi_declaration.func_name);
        }
    }
}