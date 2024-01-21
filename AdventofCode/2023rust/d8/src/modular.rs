// a module to calculate inverses in a modular ring, and to solve Bézout's identity
// for the modular ring



use std::fmt::{self, Display, Formatter};
use std::ops::{Add, Mul, Sub};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Modular {
    pub value: i64,
    pub modulus: i64,
}

impl Display for Modular {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} (mod {})", self.value, self.modulus)
    }
}
impl Add for Modular {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Modular::new(self.value + rhs.value, self.modulus)
    }
}

impl Sub for Modular {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Modular::new(self.value - rhs.value, self.modulus)
    }
}

impl Mul for Modular {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Modular::new(self.value * rhs.value, self.modulus)
    }
}

impl Modular {
    pub fn new(value: i64, modulus: i64) -> Self {
        Modular {
            value: value.rem_euclid(modulus),
            modulus,
        }
    }

    pub fn pow(&self, exponent: i64) -> Self {
        let mut result = Modular::new(1, self.modulus);
        let mut base = *self;
        let mut exp = exponent;
        while exp > 0 {
            if exp % 2 == 1 {
                result = result * base;
            }
            base = base * base;
            exp /= 2;
        }
        result
    }

    pub fn inverse(&self) -> Option<Self> {
        let (gcd, x, _) = Modular::bezout(self.value, self.modulus);
        if gcd == 1 {
            Some(Modular::new(x, self.modulus))
        } else {
            None
        }
    }

    pub fn bezout(a: i64, b: i64) -> (i64, i64, i64) {
        let mut old_r = a;
        let mut r = b;
        let mut old_s = 1;
        let mut s = 0;
        let mut old_t = 0;
        let mut t = 1;
        while r != 0 {
            let quotient = old_r / r;
            let (old_r_tmp, r_tmp) = (r, old_r - quotient * r);
            old_r = old_r_tmp;
            r = r_tmp;
            let (old_s_tmp, s_tmp) = (s, old_s - quotient * s);
            old_s = old_s_tmp;
            s = s_tmp;
            let (old_t_tmp, t_tmp) = (t, old_t - quotient * t);
            old_t = old_t_tmp;
            t = t_tmp;
        }
        (old_r, old_s, old_t)
    }

    // a function to solve a system of linear congruences
    // https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Existence_(direct_construction)
    // https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Computation
    pub fn solve_system(congruences: &[(i64, i64)]) -> Option<Self> {
        let mut result = Modular::new(0, 1);
        let mut product = 1;
        for (residue, modulus) in congruences {
            let modulus = *modulus;
            let residue = *residue;
            let p = product;
            let q = modulus;
            let (gcd, x, _) = Modular::bezout(p, q);
            if (residue - result.value) % gcd != 0 {
                return None;
            }
            result = Modular::new(
                result.value + (residue - result.value) / gcd * x * p,
                product * modulus,
            );
            product *= modulus;
        }
        Some(result)
    }
}

mod tests {
    

    #[test]
    fn test_modular() {
        let m = Modular::new(3, 7);
        assert_eq!(m.value, 3);
        assert_eq!(m.modulus, 7);
        assert_eq!(m.pow(3), Modular::new(6, 7));
        assert_eq!(m.pow(4), Modular::new(4, 7));
        assert_eq!(m.pow(5), Modular::new(5, 7));
        assert_eq!(m.pow(6), Modular::new(1, 7));
        assert_eq!(m.pow(7), Modular::new(3, 7));
        assert_eq!(m.pow(8), Modular::new(2, 7));
    }

    #[test]
    fn test_bezout() {
        assert_eq!(Modular::bezout(3, 7), (1, -2, 1));
        assert_eq!(Modular::bezout(7, 3), (1, 1, -2));
        assert_eq!(Modular::bezout(3, 6), (3, 1, 0));
        assert_eq!(Modular::bezout(6, 3), (3, 0, 1));
        assert_eq!(Modular::bezout(3, 5), (1, 2, -1));
        assert_eq!(Modular::bezout(5, 3), (1, -1, 2));
        assert_eq!(Modular::bezout(3, 4), (1, -1, 1));
        assert_eq!(Modular::bezout(4, 3), (1, 1, -1));
        assert_eq!(Modular::bezout(3, 3), (3, 0, 1));
        assert_eq!(Modular::bezout(3, 2), (1, 1, -1));
        assert_eq!(Modular::bezout(2, 3), (1, -1, 1));
        assert_eq!(Modular::bezout(3, 1), (1, 0, 1));
        assert_eq!(Modular::bezout(1, 3), (1, 1, 0));
        assert_eq!(Modular::bezout(3, 0), (3, 1, 0));
        assert_eq!(Modular::bezout(0, 3), (3, 0, 1));
        assert_eq!(Modular::bezout(0, 0), (0, 1, 0));

        assert_eq!(Modular::bezout(6, 9), (3, -1, 1));
        assert_eq!(Modular::bezout(0, 0), (0, 1, 0));
        assert_eq!(Modular::bezout(0, 0), (0, 1, 0));
        assert_eq!(Modular::bezout(0, 0), (0, 1, 0));
        assert_eq!(Modular::bezout(0, 0), (0, 1, 0));
        assert_eq!(Modular::bezout(0, 0), (0, 1, 0));
        assert_eq!(Modular::bezout(0, 0), (0, 1, 0));
        assert_eq!(Modular::bezout(0, 0), (0, 1, 0));
    }
}
