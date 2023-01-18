// https://www.codewars.com/kata/526d84b98f428f14a60008da
fn main() {
    let n = 13000;
    println!("{:#?}", hamming(n));
}

fn hamming(n: usize) -> u64 {
    let mut hammings: Vec<usize> = Vec::new();
    if n == 0 { return 0; }
    
    hammings.push(1);
    let mut i = [0,0,0];
    let primes = [2,3,5];
    let mut next_multiple = primes.clone();
    while hammings.len() < n as usize {
        let next = *next_multiple.iter().min().unwrap();
        hammings.push(next);
        for (j, p) in primes.iter().enumerate() {
            if next != next_multiple[j] { continue; }

            i[j]+=1;
            next_multiple[j] = hammings[i[j]] * p;
        }
    }
    
    *hammings.last().unwrap() as u64
}
