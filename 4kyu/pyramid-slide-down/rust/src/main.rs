fn main() {
}

// Starting from the row above the base, for each value calculate the sum of each value
// with the items below to the left and right, taking the max sum as the new accumulated value.
// That is:
//   1          1       10
//  5 3    ->  8 9 -> 
// 3 2 6
// Repeat this until the tip is calculated.
fn longest_slide_down(pyramid: &[Vec<u16>]) -> u16 {
    // Base cases
    if pyramid.is_empty() { return 0; }
    if pyramid.len() == 1 { return *pyramid[0].first().unwrap(); }
    
    let mut accum = pyramid.last().unwrap().clone();
    for row in pyramid.iter().rev().skip(1) {
        let prev_accum = accum.clone();
        accum.clear();
        for (n, num) in row.iter().enumerate() {
            accum.push( num + std::cmp::max(prev_accum[n], prev_accum[n+1])); 
        }
    }

    // The final accum row will have one value, the highest sum
    *accum.first().unwrap()
}
