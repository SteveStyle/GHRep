use std::{
    collections::HashSet,
    iter::Product,
    ops::{Add, Mul},
};

use crate::modular;

/*
Constraints are sets of integers that limit the values of a solution.  Constraints can be combined by taking a union or intersection.  Constraints can be satisfied by a value, and can be used to generate a sequence of values that satisfy the constraint.
Types of constraints are:
    specific value - a single value
    modular - a modulus and a minimum value
    complex - a set of modular contraints and a set of specific value constraint
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModularConstraint {
    pub minimum_value: i64,
    pub modulus: i64,
}

impl ModularConstraint {
    pub fn new(minimum_value: i64, modulus: i64) -> Self {
        ModularConstraint {
            minimum_value,
            modulus,
        }
    }

    fn contains(&self, value: i64) -> bool {
        if value < self.minimum_value {
            return false;
        }
        (value - self.minimum_value) % self.modulus == 0
    }

    fn least_value(&self) -> i64 {
        self.minimum_value
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
}

type SpecificValueConstraint = i64;

pub struct ComplexConstraint {
    pub modular_constraints: Vec<ModularConstraint>,
    pub specific_value_constraints: Vec<SpecificValueConstraint>,
}

impl ComplexConstraint {
    pub fn new() -> Self {
        ComplexConstraint {
            modular_constraints: Vec::new(),
            specific_value_constraints: Vec::new(),
        }
    }

    pub fn least_value(&self) -> i64 {
        let mut least_value = i64::MAX;
        for modular_constraint in &self.modular_constraints {
            least_value = least_value.min(modular_constraint.least_value());
        }
        for specific_value_constraint in &self.specific_value_constraints {
            least_value = least_value.min(*specific_value_constraint);
        }
        least_value
    }

    pub fn intersection(&self, other: &Self) -> Self {
        let mut new_modular_constraints = Vec::new();
        for self_modular_constraint in &self.modular_constraints {
            for other_modular_constraint in &other.modular_constraints {
                if let Some(intersection) =
                    self_modular_constraint.intersection(other_modular_constraint)
                {
                    new_modular_constraints.push(intersection);
                }
            }
        }
        let mut new_specific_value_constraints = HashSet::new();
        for self_specific_value_constraint in &self.specific_value_constraints {
            if other
                .specific_value_constraints
                .contains(self_specific_value_constraint)
            {
                new_specific_value_constraints.insert(*self_specific_value_constraint);
                continue;
            }
            for other_modular_constraint in &other.modular_constraints {
                if other_modular_constraint.contains(*self_specific_value_constraint) {
                    new_specific_value_constraints.insert(*self_specific_value_constraint);
                    break;
                }
            }
        }

        for other_specific_value_constraint in &other.specific_value_constraints {
            if !new_specific_value_constraints.contains(other_specific_value_constraint) {
                for self_modular_constraint in &self.modular_constraints {
                    if self_modular_constraint.contains(*other_specific_value_constraint) {
                        new_specific_value_constraints.insert(*other_specific_value_constraint);
                        break;
                    }
                }
            }
        }

        let mut new_specific_value_constraints = new_specific_value_constraints
            .into_iter()
            .collect::<Vec<_>>();
        ComplexConstraint {
            modular_constraints: new_modular_constraints,
            specific_value_constraints: new_specific_value_constraints,
        }
    }
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
