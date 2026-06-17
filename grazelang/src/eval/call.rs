use num_enum::{IntoPrimitive, TryFromPrimitive};
use serde::{Deserialize, Serialize};

use crate::{
    eval::cast::{
        JsPrimitive, ScratchVmCompare, ScratchVmToBoolean, ScratchVmToNumber, ScratchVmToString,
    },
    parser::cst::Expression,
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

impl ConstantExprFunction {
    pub fn apply<'a, I>(self, mut args: I) -> Option<JsPrimitive>
    where
        I: Iterator<Item = &'a Expression>,
    {
        // TODO: Replace Option<JsPrimitive> with Result<JsPrimitive, E> and decide on E
        // Issue: #66
        fn bin_op_args<'a, I>(mut args: I) -> Option<(&'a Expression, &'a Expression)>
        where
            I: Iterator<Item = &'a Expression>,
        {
            let Some(expr_a) = args.next() else {
                return None;
            };
            let Some(expr_b) = args.next() else {
                return None;
            };
            if args.next().is_some() {
                return None;
            }
            Some((expr_a, expr_b))
        }
        fn un_op_args<'a, I>(mut args: I) -> Option<&'a Expression>
        where
            I: Iterator<Item = &'a Expression>,
        {
            let Some(expr) = args.next() else {
                return None;
            };
            if args.next().is_some() {
                return None;
            }
            Some(expr)
        }
        match self {
            ConstantExprFunction::OperatorAdd => {
                let (expr_a, expr_b) = bin_op_args(args)?;
                Some(JsPrimitive::Number(
                    expr_a.calculate_value_js()?.to_number()
                        + expr_b.calculate_value_js()?.to_number(),
                ))
            }
            ConstantExprFunction::OperatorSubtract => {
                let (expr_a, expr_b) = bin_op_args(args)?;
                Some(JsPrimitive::Number(
                    expr_a.calculate_value_js()?.to_number()
                        - expr_b.calculate_value_js()?.to_number(),
                ))
            }
            ConstantExprFunction::OperatorMultiply => {
                let (expr_a, expr_b) = bin_op_args(args)?;
                Some(JsPrimitive::Number(
                    expr_a.calculate_value_js()?.to_number()
                        * expr_b.calculate_value_js()?.to_number(),
                ))
            }
            ConstantExprFunction::OperatorDivide => {
                let (expr_a, expr_b) = bin_op_args(args)?;
                Some(JsPrimitive::Number(
                    expr_a.calculate_value_js()?.to_number()
                        / expr_b.calculate_value_js()?.to_number(),
                ))
            }
            ConstantExprFunction::OperatorRandom => todo!(),
            ConstantExprFunction::OperatorGreaterThan => todo!(),
            ConstantExprFunction::OperatorLessThan => todo!(),
            ConstantExprFunction::OperatorEquals => todo!(),
            ConstantExprFunction::OperatorAnd => todo!(),
            ConstantExprFunction::OperatorOr => todo!(),
            ConstantExprFunction::OperatorNot => todo!(),
            ConstantExprFunction::OperatorJoin => todo!(),
            ConstantExprFunction::OperatorLetterOf => todo!(),
            ConstantExprFunction::OperatorLength => todo!(),
            ConstantExprFunction::OperatorContains => todo!(),
            ConstantExprFunction::OperatorMod => todo!(),
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
