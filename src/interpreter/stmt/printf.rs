use crate::{
    errors::EvaluationError,
    interpreter::{stmt::StmtResult, value::Value, Eval, RuntimeMut},
    parser::ast::{ExprList, OutputRedirection},
};
use std::io::Write;

#[derive(Debug, Default)]
struct Conversion {
    left_justified: bool,
    signed: bool,
    space: bool,
    alternative_form: bool,
    leading_zeros: bool,
    width: usize,
    precision: usize,
    specifier: ConversionSpecifier,
}

impl Conversion {
    fn new<I: Iterator<Item = char>>(iter: &mut I) -> Result<Conversion, String> {
        let mut conv: Conversion = Default::default();
        let mut err = String::new();
        let mut iter = iter.peekable();

        // flags
        while let Some(c) = iter.peek() {
            match c {
                '-' => conv.left_justified = true,
                '+' => conv.signed = true,
                ' ' => conv.space = true,
                '#' => conv.alternative_form = true,
                '0' => conv.leading_zeros = true,
                _ => break,
            };
            err.push(iter.next().unwrap());
        }
        // width
        if let Some(&c) = iter.peek() {
            if c.is_digit(10) {
                let mut width = String::new();
                while let Some(&p) = iter.peek() {
                    if !p.is_digit(10) {
                        break;
                    }
                    width.push(p);
                    err.push(iter.next().unwrap());
                }
                conv.width = if let Ok(width) = width.parse() {
                    width
                } else {
                    return Err(err);
                };
            }
        }
        // precision
        if let Some('.') = iter.peek() {
            err.push(iter.next().unwrap());
            let mut precision = String::new();
            while let Some(&p) = iter.peek() {
                if !p.is_digit(10) {
                    conv.precision = if let Ok(precision) = precision.parse() {
                        precision
                    } else {
                        return Err(err);
                    };
                    break;
                }
                precision.push(p);
                err.push(iter.next().unwrap());
            }
        } else {
            conv.precision = 6;
        }
        // specifier
        if let Some(c) = iter.next() {
            err.push(c);
            conv.specifier = if let Some(specifier) = ConversionSpecifier::new(c) {
                specifier
            } else {
                return Err(err);
            };
            return Ok(conv);
        }
        Err(err)
    }

    fn convert<Output: Write>(
        &self,
        output: &mut Output,
        value: Value,
    ) -> Result<(), EvaluationError> {
        match self.specifier {
            ConversionSpecifier::Float => {
                let value = value.as_number();
                let trunc = value.trunc();
                write!(output, "{}", trunc)?;
                if self.precision != 0 {
                    let fract = (value.fract() * 10_f64.powi(self.precision as i32)).round();
                    if fract == 0_f64 {
                        write!(output, ".{}", "0".repeat(self.precision))?;
                    } else {
                        write!(output, ".{}", fract)?;
                    }
                }
            },
            _ => unimplemented!("{:?}", self.specifier),
        };
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
enum ConversionSpecifier {
    SignedDecimal,
    UnsignedOctal,
    UnsignedDecimal,
    UnsignedHexadecimalLower,
    UnsignedHexadecimalUpper,
    Float,
    UnsignedChar,
    String,
    Percent,
}

impl Default for ConversionSpecifier {
    fn default() -> Self {
        ConversionSpecifier::Percent
    }
}

impl ConversionSpecifier {
    fn new(c: char) -> Option<Self> {
        match c {
            'd' | 'i' => Some(ConversionSpecifier::SignedDecimal),
            'o' => Some(ConversionSpecifier::UnsignedOctal),
            'u' => Some(ConversionSpecifier::UnsignedDecimal),
            'x' => Some(ConversionSpecifier::UnsignedHexadecimalLower),
            'X' => Some(ConversionSpecifier::UnsignedHexadecimalUpper),
            'f' | 'e' | 'E' | 'g' | 'G' => Some(ConversionSpecifier::Float),
            'c' => Some(ConversionSpecifier::UnsignedChar),
            's' => Some(ConversionSpecifier::String),
            '%' => Some(ConversionSpecifier::Percent),
            _ => None,
        }
    }
}

pub fn execute<Output>(
    rt: &mut RuntimeMut<'_, Output>,
    exprs: &ExprList,
    redir: &Option<OutputRedirection>,
) -> Result<Option<StmtResult>, EvaluationError>
where
    Output: Write,
{
    let mut iter = exprs.0.iter();
    let format = iter.next().unwrap().eval(rt)?.as_string();
    let mut format_iter = format.chars();

    while let Some(c) = format_iter.next() {
        match c {
            '%' => match Conversion::new(&mut format_iter) {
                Ok(conv) => match iter.next() {
                    Some(value) => {
                        let value = value.eval(rt)?;
                        conv.convert(rt.output, value)?
                    },
                    None => return Err(EvaluationError::MissingFormatStringArgs(format)),
                },
                Err(err) => write!(rt.output, "{}", err)?,
            },
            '\\' => match format_iter.next().unwrap() {
                'n' => write!(rt.output, "{}", '\n')?,
                _ => unimplemented!(),
            },
            _ => write!(rt.output, "{}", c)?,
        }
    }
    Ok(None)
}

mod tests {
    use super::*;

    #[test]
    fn conversions() {
        let conv = Conversion::new(&mut "d".chars()).unwrap();
        assert_eq!(conv.specifier, ConversionSpecifier::SignedDecimal);

        let conv = Conversion::new(&mut ".5f".chars()).unwrap();
        assert_eq!(conv.precision, 5);
        assert_eq!(conv.specifier, ConversionSpecifier::Float);

        let conv = Conversion::new(&mut "42s".chars()).unwrap();
        assert_eq!(conv.width, 42);
        assert_eq!(conv.specifier, ConversionSpecifier::String);

        let conv = Conversion::new(&mut "%".chars()).unwrap();
        assert_eq!(conv.specifier, ConversionSpecifier::Percent);

        let conv = Conversion::new(&mut ".42%".chars()).unwrap();
        assert_eq!(conv.specifier, ConversionSpecifier::Percent);
    }
}
