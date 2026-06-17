use crate::{
    eval::cast::JsPrimitive,
    parser::cst::{BinOp, UnOp},
};

impl UnOp {
    pub fn apply_operation(&self, expr: JsPrimitive) -> JsPrimitive {
        use crate::eval::cast::{ScratchVmToBoolean, ScratchVmToNumber};
        match self {
            UnOp::Minus(_) => JsPrimitive::Number(-expr.to_number()),
            UnOp::Not(_) => JsPrimitive::Boolean(!expr.to_boolean()),
            UnOp::Exp(_) => JsPrimitive::Number(expr.to_number().exp()),
            UnOp::Pow(_) => JsPrimitive::Number(10_f64.powf(expr.to_number())),
        }
    }
}

impl BinOp {
    pub fn apply_operation(&self, expr_a: JsPrimitive, expr_b: JsPrimitive) -> JsPrimitive {
        use super::cast::{
            ScratchVmCompare, ScratchVmToBoolean, ScratchVmToNumber, ScratchVmToString,
        };
        match self {
            BinOp::Plus(_) => JsPrimitive::Number(expr_a.to_number() + expr_b.to_number()),
            BinOp::Minus(_) => JsPrimitive::Number(expr_a.to_number() - expr_b.to_number()),
            BinOp::Times(_) => JsPrimitive::Number(expr_a.to_number() * expr_b.to_number()),
            BinOp::Div(_) => JsPrimitive::Number(expr_a.to_number() / expr_b.to_number()),
            BinOp::Mod(_) => {
                let n = expr_a.to_number();
                let modulus = expr_b.to_number();
                let result = n % modulus;
                JsPrimitive::Number(if result / modulus < 0.0 {
                    result + modulus
                } else {
                    result
                })
            }
            BinOp::Join(_) => JsPrimitive::JsString({
                let mut expr_a_string = expr_a.to_js_string();
                expr_b.write_to_js_string(&mut expr_a_string);
                expr_a_string
            }),
            BinOp::And(_) => JsPrimitive::Boolean(expr_a.to_boolean() && expr_b.to_boolean()),
            BinOp::Or(_) => JsPrimitive::Boolean(expr_a.to_boolean() || expr_b.to_boolean()),
            BinOp::Equals(_) => JsPrimitive::Boolean(expr_a.compare(&expr_b) == 0.0),
            BinOp::NotEquals(_) => JsPrimitive::Boolean(expr_a.compare(&expr_b) != 0.0),
            BinOp::LessThan(_) => JsPrimitive::Boolean(expr_a.compare(&expr_b) < 0.0),
            BinOp::GreaterThan(_) => JsPrimitive::Boolean(expr_a.compare(&expr_b) > 0.0),
            BinOp::LessThanOrEqual(_) => JsPrimitive::Boolean(!matches!(
                expr_a.compare(&expr_b).partial_cmp(&0.0),
                Some(std::cmp::Ordering::Greater)
            )),
            BinOp::GreaterThanOrEqual(_) => JsPrimitive::Boolean(!matches!(
                expr_a.compare(&expr_b).partial_cmp(&0.0),
                Some(std::cmp::Ordering::Less)
            )),
        }
    }
}
