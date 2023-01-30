use std::collections::HashSet;

fn int_to_reverse_digits(n: u128) -> Vec<u8> {
    // convert an integer to a vector of digits in reverse order
    let mut digits = Vec::new();
    let mut n = n;
    while n > 0 {
        digits.push((n % 10) as u8);
        n /= 10;
    }
    digits
}
fn int_to_reverse_digits_dropping_last(n: u128) -> Vec<u8> {
    // convert an integer to a vector of digits in reverse order
    let mut digits = Vec::new();
    let mut n = n;
    n /= 10;
    while n > 0 {
        digits.push((n % 10) as u8);
        n /= 10;
    }
    digits
}

fn digits_to_int(digits: Vec<u8>) -> u128 {
    // convert a vector of digits in to an integer
    let mut n = 0;
    for digit in digits {
        n = n * 10 + digit as u128;
    }
    n
}

fn reverse_digits(n: u128) -> u128 {
    digits_to_int(int_to_reverse_digits(n))
}

fn is_palindrome(n: u128) -> bool {
    n == reverse_digits(n)
}

pub fn odd_length_palindrome(n: u128) -> u128 {
    let reversed_digits = int_to_reverse_digits_dropping_last(n);
    n * 10u128.pow(reversed_digits.len() as u32) + digits_to_int(reversed_digits)
}

pub fn even_length_palindrome(n: u128) -> u128 {
    let reversed_digits = int_to_reverse_digits(n);
    n * 10u128.pow(reversed_digits.len() as u32) + digits_to_int(reversed_digits)
}

fn prediction(n: u32) -> u32 {
    let k = if n % 2 == 0 { n / 2 } else { (n - 1) / 2 };
    4 * 9u32.pow(k - 1)
}

fn count_palindromes(n: u32) -> u32 {
    let mut count = 0;
    let even = n % 2 == 0;
    let k = if even { n / 2 } else { (n - 1) / 2 };
    for i in 10u128.pow(k - 1)..10u128.pow(k) {
        for j in i..10u128.pow(k) {
            let ip = if even {
                odd_length_palindrome(i)
            } else {
                even_length_palindrome(i)
            };
            let jp = if even {
                odd_length_palindrome(j)
            } else {
                even_length_palindrome(j)
            };
            //            println!("{n}\t{k}:\t{} + {} = {}", ip, jp, ip + jp);
            let sum = if even {
                odd_length_palindrome(i) + odd_length_palindrome(j)
            } else {
                even_length_palindrome(i) + even_length_palindrome(j)
            };
            if sum >= 10u128.pow(n - 1) && is_palindrome(sum) {
                count += 1;
                //println!("{n}\t{k}:\t{} + {} = {}", ip, jp, sum);
            }
        }
    }
    count
}

pub fn list_unique_sums() {
    for i in 2..20 {
        println!(
            "{}: {}\t{}",
            i,
            count_palindromes_unique_sum(i),
            prediction_unique_sum(i)
        );
    }
}

pub fn prediction_unique_sum(n: u32) -> u32 {
    let k = if n % 2 == 0 { n / 2 } else { (n - 1) / 2 };
    2u32.pow(k - 1)
}

pub fn count_palindromes_unique_sum(n: u32) -> u32 {
    let mut sums = HashSet::new();
    let even = n % 2 == 0;
    let k = if even { n / 2 } else { (n - 1) / 2 };
    for i in 10u128.pow(k - 1)..10u128.pow(k) {
        for j in i..10u128.pow(k) {
            let ip = if even {
                odd_length_palindrome(i)
            } else {
                even_length_palindrome(i)
            };
            let jp = if even {
                odd_length_palindrome(j)
            } else {
                even_length_palindrome(j)
            };
            //            println!("{n}\t{k}:\t{} + {} = {}", ip, jp, ip + jp);
            let sum = if even {
                odd_length_palindrome(i) + odd_length_palindrome(j)
            } else {
                even_length_palindrome(i) + even_length_palindrome(j)
            };
            if sum >= 10u128.pow(n - 1) && is_palindrome(sum) {
                sums.insert(sum);
                //println!("{n}\t{k}:\t{} + {} = {}", ip, jp, sum);
            }
        }
    }
    println!("{:?}", sums);
    sums.len() as u32
}

fn check_for_carry(a: u128, b: u128) -> bool {
    assert!(b > a);
    let a_digits = int_to_reverse_digits(a);
    let b_digits = int_to_reverse_digits(b);
    for i in 0..a_digits.len() {
        if a_digits[i] + b_digits[i] > 9 {
            return true;
        }
    }
    false
}
pub fn count_palindromes_unique_sum_different_lengths(n: u32) -> u32 {
    let mut sums = HashSet::new();
    let even = n % 2 == 0;
    let k = if even { n / 2 } else { (n - 1) / 2 };
    for i in 10u128.pow(k - 1)..10u128.pow(k) {
        for j in 10u128.pow(k)..10u128.pow(k + 1) {
            let ip = if even {
                odd_length_palindrome(i)
            } else {
                even_length_palindrome(i)
            };
            let jp = if even {
                even_length_palindrome(j)
            } else {
                odd_length_palindrome(j)
            };
            //            println!("{n}\t{k}:\t{} + {} = {}", ip, jp, ip + jp);
            let sum = ip + jp;
            if sum >= 10u128.pow(n - 1) && check_for_carry(ip, jp) && is_palindrome(sum) {
                sums.insert(sum);
                println!("{n}\t{k}:\t{} + {} = {}", ip, jp, sum);
            }
        }
    }
    println!("{:?}", sums);
    sums.len() as u32
}

pub fn list_unique_sums_different_lengths() {
    for i in 2..7 {
        println!(
            "{}: {}\t{}\t{}",
            i,
            count_palindromes_unique_sum(i),
            count_palindromes_unique_sum_different_lengths(i),
            prediction_unique_sum(i)
        );
    }
}
// create a test module and functions to test the functions
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_to_reverse_digits() {
        assert_eq!(int_to_reverse_digits(123), vec![3, 2, 1]);
        assert_eq!(int_to_reverse_digits(10), vec![0, 1]);
    }

    #[test]
    fn test_int_to_reverse_digits_dropping_last() {
        assert_eq!(int_to_reverse_digits_dropping_last(123), vec![2, 1]);
        assert_eq!(int_to_reverse_digits_dropping_last(10), vec![1]);
    }

    #[test]
    fn test_digits_to_int() {
        assert_eq!(digits_to_int(vec![3, 2, 1]), 321);
        assert_eq!(digits_to_int(vec![0, 1]), 1);
    }

    #[test]
    fn test_converstions() {
        for n in 0..1000 {
            if n % 10 != 0 {
                assert_eq!(reverse_digits(reverse_digits(n)), n);
            }
        }
    }

    #[test]
    fn test_odd_length_palindrome() {
        assert_eq!(odd_length_palindrome(1), 1);
        assert_eq!(odd_length_palindrome(123), 12321);
        assert_eq!(odd_length_palindrome(10), 101);
    }

    #[test]
    fn test_even_length_palindrome() {
        assert_eq!(even_length_palindrome(123), 123321);
        assert_eq!(even_length_palindrome(10), 1001);
    }

    #[test]
    fn test_is_palindrome() {
        assert_eq!(is_palindrome(1), true);
        assert_eq!(is_palindrome(11), true);
        assert_eq!(is_palindrome(123), false);
        assert_eq!(is_palindrome(12321), true);
        assert_eq!(is_palindrome(123321), true);
        assert_eq!(is_palindrome(1001), true);
    }

    #[test]
    fn test_count() {
        for i in 2..10 {
            println!("{}: {}\t{}", i, count_palindromes(i), prediction(i));
            assert_eq!(count_palindromes(i), prediction(i));
        }
    }

    #[test]
    fn test_count_unique_sum() {
        for i in 2..10 {
            println!(
                "{}: {}\t{}",
                i,
                count_palindromes_unique_sum(i),
                prediction_unique_sum(i)
            );
            assert_eq!(count_palindromes(i), prediction(i));
        }
    }
}
