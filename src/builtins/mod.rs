


pub fn print(s: &str) {
    println!("{}", s);
}

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
