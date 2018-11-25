use std::ops::*;

use syn::{
    BinOp, Expr, ExprBinary, ExprCast, ExprLit, ExprPath, ExprUnary, Ident, Lit, Type, UnOp,
};

pub trait Const {
    fn ident(&self) -> &Ident;
    fn value(&self) -> u64;
}

pub fn int_eval(expr: &Expr, consts: &[impl Const]) -> Result<u64, String> {
    Ok(match expr_eval(expr, consts)? {
        Value::U64(u) => u,
        Value::I64(i) => i as u64,
        Value::F64(f) => f as u64,
    })
}

fn expr_eval(expr: &Expr, consts: &[impl Const]) -> Result<Value, String> {
    match expr {
        Expr::Lit(expr) => lit_eval(expr),
        Expr::Path(expr) => path_eval(expr, consts),
        Expr::Binary(expr) => bin_eval(expr, consts),
        Expr::Unary(expr) => un_eval(expr, consts),
        Expr::Cast(expr) => cast_eval(expr, consts),
        Expr::Paren(expr) => expr_eval(&expr.expr, consts),
        Expr::Group(expr) => expr_eval(&expr.expr, consts),
        _ => return Err("Unsupported expression in flag value".into()),
    }
}

fn lit_eval(expr: &ExprLit) -> Result<Value, String> {
    match &expr.lit {
        Lit::Int(i) => Ok(Value::U64(i.value())),
        Lit::Float(f) => Ok(Value::F64(f.value())),
        Lit::Byte(b) => Ok(Value::U64(b.value() as u64)),
        _ => return Err("Unsupported literal type".into()),
    }
}

fn path_eval(expr: &ExprPath, consts: &[impl Const]) -> Result<Value, String> {
    if expr.qself != None || expr.path.segments.len() != 1 {
        return Err("Unsupported path item in constant expression".into());
    }

    let const_name = &expr.path.segments[0].ident;
    for item in consts {
        if item.ident() == const_name {
            return Ok(Value::U64(item.value()));
        }
    }
    
    Err(format!(
        "Could not find constant `{}` during const evaluation. Only constants defined \
         above in the enum flags may be used.",
        const_name
    ))
}

fn bin_eval(expr: &ExprBinary, consts: &[impl Const]) -> Result<Value, String> {
    let left = expr_eval(&expr.left, consts)?;
    let right = expr_eval(&expr.right, consts)?;

    match expr.op {
        BinOp::Add(_) => left + right,
        BinOp::Sub(_) => left - right,
        BinOp::Mul(_) => left * right,
        BinOp::Div(_) => left / right,
        BinOp::Rem(_) => left % right,
        BinOp::BitXor(_) => left ^ right,
        BinOp::BitAnd(_) => left & right,
        BinOp::BitOr(_) => left | right,
        BinOp::Shl(_) => left << right,
        BinOp::Shr(_) => left >> right,
        _ => Err("Unsupported binary operation".into()),
    }
}

fn un_eval(expr: &ExprUnary, consts: &[impl Const]) -> Result<Value, String> {
    let value = expr_eval(&expr.expr, consts)?;

    match expr.op {
        UnOp::Neg(_) => Ok(-value),
        _ => Err("Unsupported unary operation".into()),
    }
}

fn cast_eval(expr: &ExprCast, consts: &[impl Const]) -> Result<Value, String> {
    let value = expr_eval(&expr.expr, consts)?;
    Ok(match &*expr.ty {
        Type::Path(path) => {
            if path.path.segments.len() != 1 {
                return Err("Invalid cast type".into());
            }

            let seg = &path.path.segments[0];
            match (value, &seg.ident.to_string()[..]) {
                (Value::U64(v), "u8") => Value::U64(v as u8 as u64),
                (Value::U64(v), "i8") => Value::I64(v as i8 as i64),
                (Value::U64(v), "u16") => Value::U64(v as u16 as u64),
                (Value::U64(v), "i16") => Value::I64(v as i16 as i64),
                (Value::U64(v), "u32") => Value::U64(v as u32 as u64),
                (Value::U64(v), "i32") => Value::I64(v as i32 as i64),
                (Value::U64(v), "u64") => Value::U64(v as u64),
                (Value::U64(v), "i64") => Value::I64(v as i64),
                (Value::U64(v), "f64") => Value::F64(v as f64),
                (Value::U64(v), "usize") => Value::U64(v as usize as u64),
                (Value::U64(v), "isize") => Value::I64(v as isize as i64),

                (Value::I64(v), "u8") => Value::U64(v as u8 as u64),
                (Value::I64(v), "i8") => Value::I64(v as i8 as i64),
                (Value::I64(v), "u16") => Value::U64(v as u16 as u64),
                (Value::I64(v), "i16") => Value::I64(v as i16 as i64),
                (Value::I64(v), "u32") => Value::U64(v as u32 as u64),
                (Value::I64(v), "i32") => Value::I64(v as i32 as i64),
                (Value::I64(v), "u64") => Value::U64(v as u64),
                (Value::I64(v), "i64") => Value::I64(v as i64),
                (Value::I64(v), "f64") => Value::F64(v as f64),
                (Value::I64(v), "usize") => Value::U64(v as usize as u64),
                (Value::I64(v), "isize") => Value::I64(v as isize as i64),

                (Value::F64(v), "u8") => Value::U64(v as u8 as u64),
                (Value::F64(v), "i8") => Value::I64(v as i8 as i64),
                (Value::F64(v), "u16") => Value::U64(v as u16 as u64),
                (Value::F64(v), "i16") => Value::I64(v as i16 as i64),
                (Value::F64(v), "u32") => Value::U64(v as u32 as u64),
                (Value::F64(v), "i32") => Value::I64(v as i32 as i64),
                (Value::F64(v), "u64") => Value::U64(v as u64),
                (Value::F64(v), "i64") => Value::I64(v as i64),
                (Value::F64(v), "f64") => Value::F64(v as f64),
                (Value::F64(v), "usize") => Value::U64(v as usize as u64),
                (Value::F64(v), "isize") => Value::I64(v as isize as i64),

                _ => return Err("Invalid cast type".into()),
            }
        }
        _ => return Err("Invalid cast type".into()),
    })
}

#[derive(Copy, Clone)]
enum Value {
    U64(u64),
    I64(i64),
    F64(f64),
}

impl Value {
    fn name(&self) -> &'static str {
        match self {
            Value::U64(_) => "u64",
            Value::I64(_) => "i64",
            Value::F64(_) => "f64",
        }
    }

    fn mismatch(op: &str, lhs: &Value, rhs: &Value) -> String {
        format!("Cannot {} values of {} and {}", op, lhs.name(), rhs.name())
    }
}

impl Add for Value {
    type Output = Result<Value, String>;
    fn add(self, rhs: Self) -> Result<Value, String> {
        match (self, rhs) {
            (Value::U64(lhs), Value::U64(rhs)) => Ok(Value::U64(lhs + rhs)),
            (Value::I64(lhs), Value::I64(rhs)) => Ok(Value::I64(lhs + rhs)),
            (Value::F64(lhs), Value::F64(rhs)) => Ok(Value::F64(lhs + rhs)),
            (lhs, rhs) => Err(Value::mismatch("add", &lhs, &rhs)),
        }
    }
}

impl Sub for Value {
    type Output = Result<Value, String>;
    fn sub(self, rhs: Self) -> Result<Value, String> {
        match (self, rhs) {
            (Value::U64(lhs), Value::U64(rhs)) => Ok(Value::U64(lhs - rhs)),
            (Value::I64(lhs), Value::I64(rhs)) => Ok(Value::I64(lhs - rhs)),
            (Value::F64(lhs), Value::F64(rhs)) => Ok(Value::F64(lhs - rhs)),
            (lhs, rhs) => Err(Value::mismatch("sub", &lhs, &rhs)),
        }
    }
}

impl Mul for Value {
    type Output = Result<Value, String>;
    fn mul(self, rhs: Self) -> Result<Value, String> {
        match (self, rhs) {
            (Value::U64(lhs), Value::U64(rhs)) => Ok(Value::U64(lhs * rhs)),
            (Value::I64(lhs), Value::I64(rhs)) => Ok(Value::I64(lhs * rhs)),
            (Value::F64(lhs), Value::F64(rhs)) => Ok(Value::F64(lhs * rhs)),
            (lhs, rhs) => Err(Value::mismatch("mul", &lhs, &rhs)),
        }
    }
}

impl Div for Value {
    type Output = Result<Value, String>;
    fn div(self, rhs: Self) -> Result<Value, String> {
        match (self, rhs) {
            (Value::U64(lhs), Value::U64(rhs)) => Ok(Value::U64(lhs / rhs)),
            (Value::I64(lhs), Value::I64(rhs)) => Ok(Value::I64(lhs / rhs)),
            (Value::F64(lhs), Value::F64(rhs)) => Ok(Value::F64(lhs / rhs)),
            (lhs, rhs) => Err(Value::mismatch("div", &lhs, &rhs)),
        }
    }
}

impl Rem for Value {
    type Output = Result<Value, String>;
    fn rem(self, rhs: Self) -> Result<Value, String> {
        match (self, rhs) {
            (Value::U64(lhs), Value::U64(rhs)) => Ok(Value::U64(lhs % rhs)),
            (Value::I64(lhs), Value::I64(rhs)) => Ok(Value::I64(lhs % rhs)),
            (lhs, rhs) => Err(Value::mismatch("rem", &lhs, &rhs)),
        }
    }
}

impl BitXor for Value {
    type Output = Result<Value, String>;
    fn bitxor(self, rhs: Self) -> Result<Value, String> {
        match (self, rhs) {
            (Value::U64(lhs), Value::U64(rhs)) => Ok(Value::U64(lhs ^ rhs)),
            (Value::I64(lhs), Value::I64(rhs)) => Ok(Value::I64(lhs ^ rhs)),
            (lhs, rhs) => Err(Value::mismatch("rem", &lhs, &rhs)),
        }
    }
}

impl BitAnd for Value {
    type Output = Result<Value, String>;
    fn bitand(self, rhs: Self) -> Result<Value, String> {
        match (self, rhs) {
            (Value::U64(lhs), Value::U64(rhs)) => Ok(Value::U64(lhs & rhs)),
            (Value::I64(lhs), Value::I64(rhs)) => Ok(Value::I64(lhs & rhs)),
            (lhs, rhs) => Err(Value::mismatch("rem", &lhs, &rhs)),
        }
    }
}

impl BitOr for Value {
    type Output = Result<Value, String>;
    fn bitor(self, rhs: Self) -> Result<Value, String> {
        match (self, rhs) {
            (Value::U64(lhs), Value::U64(rhs)) => Ok(Value::U64(lhs | rhs)),
            (Value::I64(lhs), Value::I64(rhs)) => Ok(Value::I64(lhs | rhs)),
            (lhs, rhs) => Err(Value::mismatch("rem", &lhs, &rhs)),
        }
    }
}

impl Shl for Value {
    type Output = Result<Value, String>;
    fn shl(self, rhs: Self) -> Result<Value, String> {
        match (self, rhs) {
            (Value::U64(lhs), Value::U64(rhs)) => Ok(Value::U64(lhs << rhs)),
            (Value::I64(lhs), Value::U64(rhs)) => Ok(Value::I64(lhs << rhs)),
            (Value::I64(lhs), Value::I64(rhs)) => Ok(Value::I64(lhs << rhs)),
            (lhs, rhs) => Err(Value::mismatch("shl", &lhs, &rhs)),
        }
    }
}

impl Shr for Value {
    type Output = Result<Value, String>;
    fn shr(self, rhs: Self) -> Result<Value, String> {
        match (self, rhs) {
            (Value::U64(lhs), Value::U64(rhs)) => Ok(Value::U64(lhs >> rhs)),
            (Value::I64(lhs), Value::U64(rhs)) => Ok(Value::I64(lhs >> rhs)),
            (Value::I64(lhs), Value::I64(rhs)) => Ok(Value::I64(lhs >> rhs)),
            (lhs, rhs) => Err(Value::mismatch("shr", &lhs, &rhs)),
        }
    }
}

impl Neg for Value {
    type Output = Value;
    fn neg(self) -> Value {
        match self {
            Value::U64(u) => Value::I64(-(u as i64)),
            Value::I64(i) => Value::I64(-i),
            Value::F64(f) => Value::F64(-f),
        }
    }
}
