use std::{fmt::Display, ops};

use crate::utils::integer_utils::gcd;

#[derive(Debug, Clone)]
pub struct Rationals {
    pub under: i64,
    pub over: i64,
}

impl Rationals {
    pub fn put_to_denominator(&self, n: i64) -> Self {
        Rationals {
            under: self.under * n,
            over: self.over * n,
        }
    }
    pub fn new(under: i64, over: i64) -> Self {
        Rationals { under, over }
    }

    pub fn approx(self) -> f64 {
        return self.over as f64 / self.under as f64;
    }

    pub fn rationalize(f: f64) -> Self {
        let r = (f * (10.0_f64.powf(10.0))).round() as i64;
        Rationals::new(10_i64.pow(10), r).reduce()
    }

    pub fn is_null(self) -> bool {
        return self.over == 0;
    }

    pub fn opposite(self) -> Self {
        Rationals::new(self.under, -1 * self.over)
    }

    pub fn reduce(self) -> Self {
        let minus;
        let i1;
        let i2;
        if self.over < 0 && self.under > 0 {
            minus = true;
            i1 = -self.over;
            i2 = self.under;
        } else if self.over > 0 && self.under < 0 {
            minus = true;
            i1 = self.over;
            i2 = -self.under;
        } else if self.over < 0 && self.under < 0 {
            minus = false;
            i1 = -self.over;
            i2 = -self.under;
        } else {
            minus = false;
            i1 = self.over;
            i2 = self.under;
        }

        if i1 == 0 && i2 == 0 {
            return Rationals { under: 0, over: 0 };
        } else if i1 == 0 {
            return Rationals { under: 1, over: 0 };
        } else if i2 == 0 {
            return Rationals {
                under: 1,
                over: i64::MAX,
            };
        } else {
            let gcd = gcd(i1, i2);
            let new_under = self.under.abs() / gcd;
            let new_over = if minus {
                -self.over.abs() / gcd
            } else {
                self.over.abs() / gcd
            };
            return Rationals {
                under: new_under,
                over: new_over,
            };
        }
    }
    pub fn abs(self) -> Self {
        Rationals::new(self.under.abs(), self.over.abs())
    }
}

impl Display for Rationals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fs = self.clone().reduce();
        if fs.under == 1 {
            write!(f, "{}", fs.over)
        } else {
            write!(f, "{}/{}", fs.over, fs.under)
        }
    }
}

impl PartialEq for Rationals {
    fn eq(&self, other: &Self) -> bool {
        if self.under == other.under {
            return self.over == other.over;
        } else {
            let i1 = self.put_to_denominator(other.under);
            let i2 = other.put_to_denominator(self.under);
            return i1.over == i2.over;
        }
    }
    fn ne(&self, other: &Self) -> bool {
        return !self.eq(other);
    }
}

impl PartialOrd for Rationals {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if !(self.under == other.under) {
            if self.over == other.over {
                return Some(std::cmp::Ordering::Equal);
            }
            if self.over > other.over {
                return Some(std::cmp::Ordering::Greater);
            }
            if self.over < other.over {
                return Some(std::cmp::Ordering::Less);
            }
            None
        } else {
            let i1 = self.put_to_denominator(other.under);
            let i2 = other.put_to_denominator(self.under);
            if i1.over == i2.over {
                return Some(std::cmp::Ordering::Equal);
            }
            if i1.over > i2.over {
                return Some(std::cmp::Ordering::Greater);
            }
            if i1.over < i2.over {
                return Some(std::cmp::Ordering::Less);
            }
            None
        }
    }

    fn le(&self, other: &Self) -> bool {
        return self < other || self == other;
    }
    fn ge(&self, other: &Self) -> bool {
        return self > other || self == other;
    }
}

impl ops::Add for Rationals {
    type Output = Rationals;
    fn add(self, rhs: Self) -> Self::Output {
        if self.under == rhs.under {
            Rationals::new(self.under, self.over + rhs.over).reduce()
        } else {
            let f1 = self.put_to_denominator(rhs.under);
            let f2 = rhs.put_to_denominator(self.under);
            Rationals::new(f1.under, f1.over + f2.over).reduce()
        }
    }
}

impl ops::Sub for Rationals {
    type Output = Rationals;
    fn sub(self, rhs: Self) -> Self::Output {
        return self + Rationals::new(rhs.under, -rhs.over);
    }
}

impl ops::Mul for Rationals {
    type Output = Rationals;
    fn mul(self, rhs: Self) -> Self::Output {
        return Rationals::new(self.under * rhs.under, self.over * rhs.over).reduce();
    }
}

impl ops::Div for Rationals {
    type Output = Rationals;
    fn div(self, rhs: Self) -> Self::Output {
        let l = self.under * rhs.over;
        let rs = self.over * rhs.under;
        let r = Rationals::new(l, rs);
        return r.reduce();
    }
}

#[cfg(test)]
mod test {
    use super::Rationals;

    #[test]
    pub fn test_denominator() {
        let expected = Rationals::new(10, 30);
        let result = Rationals::new(2, 6).put_to_denominator(5);
        assert_eq!(expected, result);
    }

    #[test]
    pub fn test_equality_simple() {
        let r1 = Rationals::new(2, 5);
        let r2 = Rationals::new(2, 3);
        assert_eq!(r1 > r2, true);
    }

    #[test]
    pub fn test_equality_hard() {
        let f1 = Rationals::new(2, 3);
        let f2 = Rationals::new(3, 1);
        assert_eq!(f2 < f1, true);
    }

    #[test]
    pub fn test_reduce_one() {
        let f1 = Rationals::new(10, 30);
        assert_eq!(Rationals::new(1, 3), f1.reduce());
    }

    #[test]
    pub fn test_reduce_two() {
        let f1 = Rationals::new(15, 9);
        assert_eq!(Rationals::new(5, 3), f1.reduce());
    }

    #[test]
    pub fn add_easy() {
        let expected = Rationals::new(3, 5);
        let value = Rationals::new(3, 1) + Rationals::new(3, 4);
        assert_eq!(value, expected)
    }

    #[test]
    pub fn add() {
        let expected = Rationals::new(26, 71);
        let value = Rationals::new(13, 3) + Rationals::new(2, 5);
        assert_eq!(value, expected);
    }

    #[test]
    pub fn add_negative() {
        let expected = Rationals::new(3, 1);
        let value = Rationals::new(3, 2) + Rationals::new(3, -1);
        assert_eq!(value, expected)
    }

    #[test]
    pub fn test_sub() {
        let expected = Rationals::new(3, 1);
        let value = Rationals::new(3, 2) - Rationals::new(3, 1);
        assert_eq!(value, expected)
    }

    #[test]
    pub fn test_minus() {
        let expected = Rationals::new(3, -1);
        let value = Rationals::new(1, 0) - Rationals::new(3, 1);
        assert_eq!(value, expected)
    }

    #[test]
    pub fn test_mult_simple() {
        let expected = Rationals::new(15, 4);
        let value = Rationals::new(3, 2) * Rationals::new(5, 2);
        assert_eq!(value, expected);
    }

    #[test]
    pub fn test_mult_hard() {
        let expected = Rationals::new(475, 336);
        let value = Rationals::new(25, 32) * Rationals::new(76, 42);
        assert_eq!(value, expected);
    }

    #[test]
    pub fn test_div_simpl() {
        let expected = Rationals::new(9, 2);
        let value = Rationals::new(3, 2) / Rationals::new(1, 3);
        assert_eq!(value, expected);
    }

    #[test]
    pub fn test_div_hard() {
        let expected = Rationals::new(525, 1216);
        let value = Rationals::new(25, 32) / Rationals::new(76, 42);
        assert_eq!(value, expected);
    }
}
