use std::{
    mem::MaybeUninit,
    sync::{LazyLock, Mutex, MutexGuard},
};

use num_enum::{IntoPrimitive, TryFromPrimitive};
use rand::{
    RngExt, SeedableRng,
    rngs::{StdRng, SysRng},
};
use serde::{Deserialize, Serialize};

use crate::{
    eval::cast::{
        JsPrimitive, ScratchVmCompare, ScratchVmIsInt, ScratchVmToBoolean, ScratchVmToNumber,
        ScratchVmToString, ToLowercaseU16, try_convert_f64_into_i128,
    },
    lexer::SourceSpan,
    messages::ConstantExprEvaluationError,
    parser::cst::{EMPTY_ISTRING_REF, Expression},
};

#[repr(u32)]
#[derive(
    Debug, Clone, Copy, PartialEq, Serialize, Deserialize, TryFromPrimitive, IntoPrimitive,
)]
pub enum ConstantExprFunction {
    OperatorAdd = 0,
    OperatorSubtract = 1,
    OperatorMultiply = 2,
    OperatorDivide = 3,
    OperatorRandom = 4,
    OperatorGreaterThan = 5,
    OperatorLessThan = 6,
    OperatorEquals = 7,
    OperatorAnd = 8,
    OperatorOr = 9,
    OperatorNot = 10,
    OperatorJoin = 11,
    OperatorLetterOf = 12,
    OperatorLength = 13,
    OperatorContains = 14,
    OperatorMod = 15,
    OperatorRound = 16,
    OperatorMathop = 17,
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

impl ConstantExprFunction {
    pub fn apply<'a, I>(
        self,
        args: I,
        source_span: SourceSpan,
    ) -> Result<JsPrimitive, ConstantExprEvaluationError>
    where
        I: Iterator<Item = &'a Expression>,
    {
        // TODO: Replace Option<JsPrimitive> with Result<JsPrimitive, E> and decide on E
        // Issue: #66
        fn bin_op_args<'a, I>(
            mut args: I,
            source_span: SourceSpan,
        ) -> Result<(&'a Expression, &'a Expression), ConstantExprEvaluationError>
        where
            I: Iterator<Item = &'a Expression>,
        {
            let Some(expr_a) = args.next() else {
                return Err(ConstantExprEvaluationError::IncorrectParamCount {
                    unexpected: 0,
                    expected: 2,
                    source_span,
                });
            };
            let Some(expr_b) = args.next() else {
                return Err(ConstantExprEvaluationError::IncorrectParamCount {
                    unexpected: 1,
                    expected: 2,
                    source_span,
                });
            };
            if args.next().is_some() {
                return Err(ConstantExprEvaluationError::IncorrectParamCount {
                    unexpected: args.count() + 3,
                    expected: 2,
                    source_span,
                });
            }
            Ok((expr_a, expr_b))
        }
        fn un_op_args<'a, I>(
            mut args: I,
            source_span: SourceSpan,
        ) -> Result<&'a Expression, ConstantExprEvaluationError>
        where
            I: Iterator<Item = &'a Expression>,
        {
            let Some(expr) = args.next() else {
                return Err(ConstantExprEvaluationError::IncorrectParamCount {
                    unexpected: 0,
                    expected: 1,
                    source_span,
                });
            };
            let rest = args.count();
            if rest > 0 {
                return Err(ConstantExprEvaluationError::IncorrectParamCount {
                    unexpected: 1 + rest,
                    expected: 1,
                    source_span,
                });
            }
            Ok(expr)
        }
        fn n_ary_args<'a, I, const N: usize>(
            mut args: I,
            source_span: SourceSpan,
        ) -> Result<[&'a Expression; N], ConstantExprEvaluationError>
        where
            I: Iterator<Item = &'a Expression>,
        {
            let mut maybe_expressions = [MaybeUninit::<&Expression>::uninit(); N];
            let mut i = 0;
            while i < N {
                maybe_expressions[i].write(args.next().ok_or_else(|| {
                    ConstantExprEvaluationError::IncorrectParamCount {
                        unexpected: i,
                        expected: N,
                        source_span,
                    }
                })?);
                i += 1;
            }
            let rest = args.count();
            if rest > 0 {
                Err(ConstantExprEvaluationError::IncorrectParamCount {
                    unexpected: N + rest,
                    expected: N,
                    source_span,
                })
            } else {
                let maybe_expressions_ptr = &maybe_expressions as *const _;
                let expressions_ptr = maybe_expressions_ptr as *const [&Expression; N];
                // SAFETY: when i >= N, all array indices in 0..N have been written to and therefore it is safe
                // to assume all MaybeUninit are init
                let expressions = unsafe { expressions_ptr.read() };
                Ok(expressions)
            }
        }
        match self {
            ConstantExprFunction::OperatorAdd => {
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                Ok(JsPrimitive::Number(
                    expr_a.calculate_value_js()?.to_number()
                        + expr_b.calculate_value_js()?.to_number(),
                ))
            }
            ConstantExprFunction::OperatorSubtract => {
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                Ok(JsPrimitive::Number(
                    expr_a.calculate_value_js()?.to_number()
                        - expr_b.calculate_value_js()?.to_number(),
                ))
            }
            ConstantExprFunction::OperatorMultiply => {
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                Ok(JsPrimitive::Number(
                    expr_a.calculate_value_js()?.to_number()
                        * expr_b.calculate_value_js()?.to_number(),
                ))
            }
            ConstantExprFunction::OperatorDivide => {
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                Ok(JsPrimitive::Number(
                    expr_a.calculate_value_js()?.to_number()
                        / expr_b.calculate_value_js()?.to_number(),
                ))
            }
            ConstantExprFunction::OperatorRandom => {
                static RNG: LazyLock<Mutex<StdRng>> =
                    LazyLock::new(|| Mutex::new(StdRng::try_from_rng(&mut SysRng).unwrap()));
                fn acquire_rng() -> MutexGuard<'static, StdRng> {
                    RNG.lock().unwrap_or_else(|err| err.into_inner())
                }
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                let (expr_a, expr_b) = (expr_a.calculate_value_js()?, expr_b.calculate_value_js()?);
                let (from, to) = (expr_a.to_number(), expr_b.to_number());
                let (from, to) = if from <= to { (from, to) } else { (to, from) };
                if expr_a.is_int()
                    && expr_b.is_int()
                    && let Some(from) = try_convert_f64_into_i128(from)
                    && let Some(to) = try_convert_f64_into_i128(to)
                {
                    return Ok(JsPrimitive::Number(
                        acquire_rng().random_range(from..=to) as f64
                    ));
                }
                Ok(JsPrimitive::Number(acquire_rng().random_range(from..=to)))
            }
            ConstantExprFunction::OperatorGreaterThan => {
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                let (expr_a, expr_b) = (expr_a.calculate_value_js()?, expr_b.calculate_value_js()?);
                Ok(JsPrimitive::Boolean(expr_a.compare(&expr_b) > 0.0))
            }
            ConstantExprFunction::OperatorLessThan => {
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                let (expr_a, expr_b) = (expr_a.calculate_value_js()?, expr_b.calculate_value_js()?);
                Ok(JsPrimitive::Boolean(expr_a.compare(&expr_b) < 0.0))
            }
            ConstantExprFunction::OperatorEquals => {
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                let (expr_a, expr_b) = (expr_a.calculate_value_js()?, expr_b.calculate_value_js()?);
                Ok(JsPrimitive::Boolean(expr_a.compare(&expr_b) == 0.0))
            }
            ConstantExprFunction::OperatorAnd => {
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                let (expr_a, expr_b) = (expr_a.calculate_value_js()?, expr_b.calculate_value_js()?);
                Ok(JsPrimitive::Boolean(
                    expr_a.to_boolean() && expr_b.to_boolean(),
                ))
            }
            ConstantExprFunction::OperatorOr => {
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                let (expr_a, expr_b) = (expr_a.calculate_value_js()?, expr_b.calculate_value_js()?);
                Ok(JsPrimitive::Boolean(
                    expr_a.to_boolean() || expr_b.to_boolean(),
                ))
            }
            ConstantExprFunction::OperatorNot => {
                let expr = un_op_args(args, source_span)?;
                let expr = expr.calculate_value_js()?;
                Ok(JsPrimitive::Boolean(!expr.to_boolean()))
            },
            ConstantExprFunction::OperatorJoin => {
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                let (expr_a, expr_b) = (expr_a.calculate_value_js()?, expr_b.calculate_value_js()?);
                Ok(JsPrimitive::JsString({
                    let mut expr_a_string = expr_a.to_js_string();
                    expr_b.write_to_js_string(&mut expr_a_string);
                    expr_a_string
                }))
            }
            ConstantExprFunction::OperatorLetterOf => {
                let (index, string) = bin_op_args(args, source_span)?;
                let (index, string) = (index.calculate_value_js()?, string.calculate_value_js()?);
                let string = string.to_js_cow_str();
                let index = index.to_number() - 1.0;
                let Ok(index) =
                    usize::try_from(if let Some(it) = try_convert_f64_into_i128(index.floor()) {
                        it
                    } else {
                        return Ok(JsPrimitive::IString(EMPTY_ISTRING_REF.clone()));
                    })
                else {
                    return Ok(JsPrimitive::IString(EMPTY_ISTRING_REF.clone()));
                };
                Ok(string
                    .get(index)
                    .map(|value| JsPrimitive::JsString(vec![*value]))
                    .unwrap_or_else(|| JsPrimitive::IString(EMPTY_ISTRING_REF.clone())))
            }
            ConstantExprFunction::OperatorLength => {
                let expr = un_op_args(args, source_span)?;
                let expr = expr.calculate_value_js()?;
                Ok(JsPrimitive::Number(expr.to_js_cow_str().len() as f64))
            },
            ConstantExprFunction::OperatorContains => {
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                let (expr_a, expr_b) = (expr_a.calculate_value_js()?, expr_b.calculate_value_js()?);
                let (haystack, needle) = (expr_a.to_js_cow_str(), expr_b.to_js_cow_str());
                let haystack = char::decode_utf16(haystack.iter().copied())
                    .to_lowercase()
                    .collect::<Vec<_>>();
                let needle = char::decode_utf16(needle.iter().copied())
                    .to_lowercase()
                    .collect::<Vec<_>>();
                Ok(JsPrimitive::Boolean(str_contains::<16>(&haystack, &needle)))
            }
            ConstantExprFunction::OperatorMod => {
                let (expr_a, expr_b) = bin_op_args(args, source_span)?;
                let (expr_a, expr_b) = (expr_a.calculate_value_js()?, expr_b.calculate_value_js()?);
                let n = expr_a.to_number();
                let modulus = expr_b.to_number();
                let result = n % modulus;
                Ok(JsPrimitive::Number(if result / modulus < 0.0 {
                    result + modulus
                } else {
                    result
                }))
            }
            ConstantExprFunction::OperatorRound => todo!(),
            ConstantExprFunction::OperatorMathop => todo!(),
        }
    }
}

#[repr(u32)]
#[derive(
    Debug, Clone, Copy, PartialEq, Serialize, Deserialize, TryFromPrimitive, IntoPrimitive,
)]
pub enum ConstantExprValue {
    Abs = 0,
    Floor = 1,
    Ceiling = 2,
    Sqrt = 3,
    Sin = 4,
    Cos = 5,
    Tan = 6,
    Asin = 7,
    Acos = 8,
    Atan = 9,
    Ln = 10,
    Log = 11,
    Exp = 12,
    Pow = 13,
}
