use std::io::{stdin, stdout, Write};

/*
echo ./sieve.rs | entr -c bash -c 'rustc ./sieve.rs -o /tmp/tmp.rs.exe && echo OK'

rustc ./sieve.rs -o /tmp/tmp.rs.exe && /tmp/tmp.rs.exe
*/

fn get_max() -> usize {
    print!("Enter a number: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).expect("Failed to read line");
    input.trim().parse().unwrap()
}

fn compute(max: usize, prime: &mut [bool]) {
    let limit = (max as f64).sqrt() as usize;
    prime[0] = false;
    prime[1] = false;
    for n in 2..=limit {
        if prime[n] {
            let mut m = n * n;
            while m <= max {
                prime[m] = false;
                m += n;
            }
        }
    }
}

fn main() {
    let max = get_max();
    let mut prime = vec![true; max + 1];
    compute(max, &mut prime);
    for (n, &is_prime) in prime.iter().enumerate().skip(2) {
        if is_prime {
            println!("{}", n);
        }
    }
}
