use std::{
    iter::Product,
    ops::{Add, Mul},
};

use crate::modular;

/*
Constraints are sets of integers that limit the values of a solution.  Constraints can be combined by taking a union or intersection.  Constraints can be satisfied by a value, and can be used to generate a sequence of values that satisfy the constraint.
Types of constraints are:
    specific values - a finite list of integers
    modular - a modulus and a minimum value
    modular set - a modulus and a set of modular minimum values, essentially a union of modular constraints with the same modulus
    complex - a combination of a modular contraint and a specific value constraint
*/
#[derive(Debug, Clone, PartialEq, Eq)]
struct ModularConstraint {
    minimum_value: i64,
    modulus: i64,
}

impl ModularConstraint {
    fn new(minimum_value: i64, modulus: i64) -> Self {
        ModularConstraint {
            minimum_value,
            modulus,
        }
    }

    fn satisfy(&self, value: i64) -> Option<i64> {
        if value < self.minimum_value {
            return None;
        }
        let residue = value - self.minimum_value;
        if residue % self.modulus != 0 {
            return None;
        }
        Some(residue / self.modulus)
    }

    fn intersection(&self, other: &Self) -> Option<Self> {
        let (g, x, y) = modular::Modular::bezout(self.modulus, other.modulus);
        let diff = (other.minimum_value - self.minimum_value);
        if diff % g != 0 {
            return None;
        }
        let new_modulus = self.modulus * other.modulus / g;
        let mut new_minimum_value = (self.minimum_value
            + ((x * (diff / g)).rem_euclid(other.modulus / g)) * self.modulus)
            % new_modulus;
        while new_minimum_value < self.minimum_value || new_minimum_value < other.minimum_value {
            new_minimum_value += new_modulus;
        }
        Some(ModularConstraint::new(new_minimum_value, new_modulus))
    }

    fn union(&self, other: &Self) -> Self
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_modular_constraint_intersection() {
        assert_eq!(
            ModularConstraint::new(0, 3).intersection(&ModularConstraint::new(0, 3)),
            Some(ModularConstraint::new(0, 3))
        );
        assert_eq!(
            ModularConstraint::new(0, 3).intersection(&ModularConstraint::new(0, 5)),
            Some(ModularConstraint::new(0, 15))
        );
        assert_eq!(
            ModularConstraint::new(1, 3).intersection(&ModularConstraint::new(2, 5)),
            Some(ModularConstraint::new(7, 15))
        );
        assert_eq!(
            ModularConstraint::new(4, 3).intersection(&ModularConstraint::new(2, 5)),
            Some(ModularConstraint::new(7, 15))
        );
        assert_eq!(
            ModularConstraint::new(7, 3).intersection(&ModularConstraint::new(2, 5)),
            Some(ModularConstraint::new(7, 15))
        );
        assert_eq!(
            ModularConstraint::new(10, 3).intersection(&ModularConstraint::new(2, 5)),
            Some(ModularConstraint::new(22, 15))
        );

        assert_eq!(
            ModularConstraint::new(1, 3).intersection(&ModularConstraint::new(7, 5)),
            Some(ModularConstraint::new(7, 15))
        );
        assert_eq!(
            ModularConstraint::new(1, 3).intersection(&ModularConstraint::new(12, 5)),
            Some(ModularConstraint::new(22, 15))
        );

        assert_eq!(
            ModularConstraint::new(2, 3).intersection(&ModularConstraint::new(1, 5)),
            Some(ModularConstraint::new(11, 15))
        );

        assert_eq!(
            ModularConstraint::new(0, 24).intersection(&ModularConstraint::new(0, 18)),
            Some(ModularConstraint::new(0, 72))
        );
        assert_eq!(
            ModularConstraint::new(3, 24).intersection(&ModularConstraint::new(9, 18)),
            Some(ModularConstraint::new(27, 72))
        );
    }
}
