/// Prints a string to stdout
pub fn print(s: &str) {
    println!("{}", s);
}

/// C-compatible puts function
#[no_mangle]
pub extern "C" fn puts(s: *const i8) -> i32 {
    let c_str = unsafe { std::ffi::CStr::from_ptr(s) };
    match c_str.to_str() {
        Ok(str_slice) => {
            print(str_slice);
            0
        }
        Err(_) => -1,
    }
}

/// Power function for floating point numbers
#[no_mangle]
pub extern "C" fn pow_func(base: f64, exp: f64) -> f64 {
    base.powf(exp)
}