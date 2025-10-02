// Auto-generated Rust code from Fystan program
fn calculate(a, b) -> i64 { let result = a * b;;
        return result;
         }
fn main() {
    let x = 10;;
    let y = 5;;
    let sum_result = x + y;;
    println!("{}", "Sum: ");
    println!("{}", sum_result);
    let product = calculate(6, 7);;
    println!("{}", "Product: ");
    println!("{}", product);
    let text = "Testing Fystan";;
    let text_len = text.len() as i64;;
    println!("{}", "Text length: ");
    println!("{}", text_len);
    for i in (1..3) { println!("{}", i);
         };
    let value = 15;;
    if value > 10 { println!("{}", "Value is greater than 10");
         } else { println!("{}", "Value is 10 or less");
         };
    let numbers = vec![100, 200, 300];;
    let count = numbers.len() as i64;;
    println!("{}", "Array count: ");
    println!("{}", count);
    let final_result = x - y + product / 2;;
    println!("{}", "Final calculation: ");
    println!("{}", final_result);
    println!("{}", "Basic test completed successfully!");
}
