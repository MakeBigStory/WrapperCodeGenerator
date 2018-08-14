# WrapperCodeGeneratorCode generator for OpenGL wrapper.

### usage
1. To use it, just "cargo run [mode] [the original Opengl wrapper code file path] > [the generated code's saving file path]".
2. Currently, two modes supported:
  1. aop: generate aop code for wrapper func.
  2. ffi: generate cfg control for ffi call in wrapper func.
3. Of course, you can try to integrate it with build.rs.

### generate whats?
1. some use/declaration.
2. collect each func's param type and value, fill them in a vector, then insert a func "pre_process" before the original func body, and pass the vector to "pre_process".
3. append a func "post_process" after the original func body, pass the vector with param type/value and the func body's executing result.

