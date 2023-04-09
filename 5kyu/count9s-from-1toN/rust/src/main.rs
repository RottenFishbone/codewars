// https://www.codewars.com/kata/55143152820d22cdf00001bb

fn main() {        
    println!("Count: {}", count_nines(542453499));
}

/// Counts the number of 9's found from [1..n]
/// Computation is O(d) where d is the number of base 10 digits in n
fn count_nines(n: u64) -> u64 {
    let mut n = n;
    let mut count: u64 = 0;
    while n > 0 {
        let sig = (n as f64).log10().floor() as u64;    // Digit significance
        let exp = 10_u64.pow(sig as u32);               // Digit magnitude
        let digit = n / exp;                // The digit [0-9]
        let isolated = digit * exp;         // The digit to the correct magnitude
        let tail = n - isolated;            // The value to the right of the digit
        
        count += sig * exp / 10_u64 * digit;    // Number of 9's for digits [0-8]
        if digit == 9 { count += tail+1; }      // Extra 9's when digit is 9 as well
                                                
        n = tail;                           // Drop the digit
    }

    return count;
}
