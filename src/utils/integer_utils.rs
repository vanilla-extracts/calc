/// # GCD
/// Computes the GCD netween two mutable sixty four bits integers.
/// The algorithm is the classic euclid algorithm.
/// Returns the GCD
pub fn gcd(mut n: i64, mut m: i64) -> i64 {
    while m != 0 {
        if m < n {
            std::mem::swap(&mut m, &mut n);
        }
        m %= n;
    }
    n
}
