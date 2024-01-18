use modular::Modular;
use std::{
    iter::Product,
    ops::{Add, Mul},
};

struct ModularConstraint {
    minimum_value: i32,
    modulus: i32,
}

impl ModularConstraint {
    fn new(minimum_value: i32, modulus: i32) -> Self {
        ModularConstraint {
            minimum_value,
            modulus,
        }
    }

    fn satisfy(&self, value: i32) -> Option<i32> {
        if value < self.minimum_value {
            return None;
        }
        let residue = value - self.minimum_value;
        if residue % self.modulus != 0 {
            return None;
        }
        Some(residue / self.modulus)
    }
}

impl Mul for ModularConstraint {
    type Output = Self;

    /*
    Given two modular constraints, return a new modular constraint that
    is satisfied by the intersection of the two constraints.  If the
    intersection is empty, return None.
    If the two moduli are coprime, then the intersection is the
    intersection of the two residues mod the product of the two moduli.
    If the two moduli are not coprime, then the intersection is the
    intersection of the two residues mod the gcd of the two moduli.
    The new minimal value is the smallest value that satisfies both
    constraints and is greater than both minimum values.
     */

}
