// Auto-generated Rust code from Fystan program
fn multiply(a, b) -> i64 { return a * b;
         }
fn main() {
    let x = 10;;
    let y = 3;;
    let sum_result = x + y;;
    println!("{}", "Sum: ");
    println!("{}", sum_result);
    let product = multiply(4, 5);;
    println!("{}", "Product: ");
    println!("{}", product);
    let text = "Fystan Language";;
    let text_length = text.len() as i64;;
    println!("{}", "Text length: ");
    println!("{}", text_length);
    println!("{}", "Range test:");
    for num in (1..4) { println!("{}", num);
         };
    let value = 7;;
    if value % 2 == 0 { println!("{}", "Value is even");
         } else { println!("{}", "Value is odd");
         };
    let numbers = vec![10, 20, 30];;
    let count = numbers.len() as i64;;
    println!("{}", "Array count: ");
    println!("{}", count);
    let result = x + y * 2 - 5;;
    println!("{}", "Complex calculation: ");
    println!("{}", result);
    let a = 1;;
    let b = 0;;
    println!("{}", "Boolean tests:");
    println!("{}", a && b);
    println!("{}", a || b);
    println!("{}", (! b));
    println!("{}", "All tests completed successfully!");
}
