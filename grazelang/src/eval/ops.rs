use crate::{
    eval::cast::{
        JsPrimitive, ScratchVmCompare, ScratchVmToBoolean, ScratchVmToNumber, ScratchVmToString,
        ToLowercaseU16,
    },
    parser::cst::{BinOp, UnOp},
};

pub trait ConstantExprUnOp {
    fn apply_operation(&self, value: JsPrimitive) -> JsPrimitive;
}

impl ConstantExprUnOp for UnOp {
    fn apply_operation(&self, value: JsPrimitive) -> JsPrimitive {
        match self {
            UnOp::Minus(_) => MinusUnOp.apply_operation(value),
            UnOp::Not(_) => NotUnOp.apply_operation(value),
            UnOp::Exp(_) => ExpUnOp.apply_operation(value),
            UnOp::Pow(_) => PowUnOp.apply_operation(value),
        }
    }
}

pub struct MinusUnOp;

impl ConstantExprUnOp for MinusUnOp {
    fn apply_operation(&self, value: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Number(-value.to_number())
    }
}
pub struct NotUnOp;

impl ConstantExprUnOp for NotUnOp {
    fn apply_operation(&self, value: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Bool(!value.to_boolean())
    }
}
pub struct ExpUnOp;

impl ConstantExprUnOp for ExpUnOp {
    fn apply_operation(&self, value: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Number(value.to_number().exp())
    }
}
pub struct PowUnOp;

impl ConstantExprUnOp for PowUnOp {
    fn apply_operation(&self, value: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Number(10_f64.powf(value.to_number()))
    }
}

pub trait ConstantExprBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive;
}

impl ConstantExprBinOp for BinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        match self {
            BinOp::Plus(_) => PlusBinOp.apply_operation(value_a, value_b),
            BinOp::Minus(_) => MinusBinOp.apply_operation(value_a, value_b),
            BinOp::Times(_) => TimesBinOp.apply_operation(value_a, value_b),
            BinOp::Div(_) => DivBinOp.apply_operation(value_a, value_b),
            BinOp::Mod(_) => ModBinOp.apply_operation(value_a, value_b),
            BinOp::Join(_) => JoinBinOp.apply_operation(value_a, value_b),
            BinOp::Contains(_) => ContainsBinOp.apply_operation(value_a, value_b),
            BinOp::And(_) => AndBinOp.apply_operation(value_a, value_b),
            BinOp::Or(_) => OrBinOp.apply_operation(value_a, value_b),
            BinOp::Equals(_) => EqualsBinOp.apply_operation(value_a, value_b),
            BinOp::NotEquals(_) => NotEqualsBinOp.apply_operation(value_a, value_b),
            BinOp::LessThan(_) => LessThanBinOp.apply_operation(value_a, value_b),
            BinOp::GreaterThan(_) => GreaterThanBinOp.apply_operation(value_a, value_b),
            BinOp::LessThanOrEqual(_) => LessThanOrEqualBinOp.apply_operation(value_a, value_b),
            BinOp::GreaterThanOrEqual(_) => {
                GreaterThanOrEqualBinOp.apply_operation(value_a, value_b)
            }
        }
    }
}

pub struct PlusBinOp;

impl ConstantExprBinOp for PlusBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Number(value_a.to_number() + value_b.to_number())
    }
}

pub struct MinusBinOp;

impl ConstantExprBinOp for MinusBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Number(value_a.to_number() - value_b.to_number())
    }
}

pub struct TimesBinOp;

impl ConstantExprBinOp for TimesBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Number(value_a.to_number() * value_b.to_number())
    }
}

pub struct DivBinOp;

impl ConstantExprBinOp for DivBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Number(value_a.to_number() / value_b.to_number())
    }
}

pub struct ModBinOp;

impl ConstantExprBinOp for ModBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        {
            let n = value_a.to_number();
            let modulus = value_b.to_number();
            let result = n % modulus;
            JsPrimitive::Number(if result / modulus < 0.0 {
                result + modulus
            } else {
                result
            })
        }
    }
}

pub struct JoinBinOp;

impl ConstantExprBinOp for JoinBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::JsString({
            let mut expr_a_string = value_a.to_js_string();
            value_b.write_to_js_string(&mut expr_a_string);
            expr_a_string
        })
    }
}

pub struct ContainsBinOp;

impl ConstantExprBinOp for ContainsBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        let (haystack, needle) = (value_a.to_js_cow_str(), value_b.to_js_cow_str());
        let haystack = char::decode_utf16(haystack.iter().copied())
            .to_lowercase()
            .collect::<Vec<_>>();
        let needle = char::decode_utf16(needle.iter().copied())
            .to_lowercase()
            .collect::<Vec<_>>();
        JsPrimitive::Bool(str_contains::<16>(&haystack, &needle))
    }
}

pub struct AndBinOp;

impl ConstantExprBinOp for AndBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Bool(value_a.to_boolean() && value_b.to_boolean())
    }
}

pub struct OrBinOp;

impl ConstantExprBinOp for OrBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Bool(value_a.to_boolean() || value_b.to_boolean())
    }
}

pub struct EqualsBinOp;

impl ConstantExprBinOp for EqualsBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Bool(value_a.compare(&value_b) == 0.0)
    }
}

pub struct NotEqualsBinOp;

impl ConstantExprBinOp for NotEqualsBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Bool(value_a.compare(&value_b) != 0.0)
    }
}

pub struct LessThanBinOp;

impl ConstantExprBinOp for LessThanBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Bool(value_a.compare(&value_b) < 0.0)
    }
}

pub struct GreaterThanBinOp;

impl ConstantExprBinOp for GreaterThanBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Bool(value_a.compare(&value_b) > 0.0)
    }
}

pub struct LessThanOrEqualBinOp;

impl ConstantExprBinOp for LessThanOrEqualBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Bool(!matches!(
            value_a.compare(&value_b).partial_cmp(&0.0),
            Some(std::cmp::Ordering::Greater)
        ))
    }
}

pub struct GreaterThanOrEqualBinOp;

impl ConstantExprBinOp for GreaterThanOrEqualBinOp {
    fn apply_operation(&self, value_a: JsPrimitive, value_b: JsPrimitive) -> JsPrimitive {
        JsPrimitive::Bool(!matches!(
            value_a.compare(&value_b).partial_cmp(&0.0),
            Some(std::cmp::Ordering::Less)
        ))
    }
}

pub fn str_contains<const N: usize>(haystack: &[u16], needle: &[u16]) -> bool {
    fn compute_table<'a>(needle: &[u16], buf: &'a mut [isize]) -> &'a [isize] {
        let m = needle.len();
        let t = &mut buf[..m];
        t[0] = -1;
        let mut pos = 1;
        let mut cnd = 0;
        while pos < m {
            if needle[pos] == needle[cnd as usize] {
                t[pos] = t[cnd as usize];
            } else {
                t[pos] = cnd;
                while cnd >= 0 && needle[pos] != needle[cnd as usize] {
                    cnd = t[cnd as usize];
                }
            }
            pos += 1;
            cnd += 1;
        }
        t
    }
    if needle.is_empty() {
        return true;
    }
    if haystack.len() < needle.len() {
        return false;
    }
    if needle.len() > N {
        return any_str_contains(haystack, needle);
    }
    let mut buf = [0; N];
    let t = compute_table(needle, &mut buf);
    let mut i = 0;
    let mut j = 0;
    while i < haystack.len() {
        if j == -1 || haystack[i] == needle[j as usize] {
            i += 1;
            j += 1;
            if j == needle.len() as isize {
                return true;
            }
        } else {
            j = t[j as usize];
        }
    }
    false
}

pub fn any_str_contains(haystack: &[u16], needle: &[u16]) -> bool {
    fn compute_table(needle: &[u16]) -> Vec<isize> {
        let m = needle.len();
        let mut t = vec![0; m];
        t[0] = -1;
        let mut pos = 1;
        let mut cnd = 0;
        while pos < m {
            if needle[pos] == needle[cnd as usize] {
                t[pos] = t[cnd as usize];
            } else {
                t[pos] = cnd;
                while cnd >= 0 && needle[pos] != needle[cnd as usize] {
                    cnd = t[cnd as usize];
                }
            }
            pos += 1;
            cnd += 1;
        }
        t
    }
    if needle.is_empty() {
        return true;
    }
    if haystack.len() < needle.len() {
        return false;
    }
    let t = compute_table(needle);
    let mut i = 0;
    let mut j = 0;
    while i < haystack.len() {
        if j == -1 || haystack[i] == needle[j as usize] {
            i += 1;
            j += 1;
            if j == needle.len() as isize {
                return true;
            }
        } else {
            j = t[j as usize];
        }
    }
    false
}
