fn main() {}

fn order_weight(s: &str) -> String {
    // Map the string of weights into a vec of tuples, containing the person's
    // weight and the sum weighting.
    let mut weights = s
        .split_whitespace()
        .map(|w_str| {
            let weight = w_str.parse::<usize>().unwrap();
            
            // Get sum of digits
            let mut n = weight;
            let mut sum = 0_usize;
            while n > 0 {
                sum += n % 10;
                n /= 10;
            }

            (weight, sum)
        })
        .collect::<Vec<(usize, usize)>>();
    
    // Sort by sum weighting if not equal, otherwise order alphabetically
    weights.sort_by(|a, b| {
        if a.1 != b.1 {
            (a.1).cmp(&b.1)
        }
        else {
            (a.0.to_string()).cmp(&(b.0).to_string())
        }
    });
    
    // Join sorted weights into a string, dropping the sum
    weights.iter()
           .map(|w| w.0.to_string())
           .collect::<Vec<String>>()
           .join(" ")
}
