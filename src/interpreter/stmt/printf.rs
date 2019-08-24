use crate::{
    errors::EvaluationError,
    interpreter::{
        stmt::{redirections::file_name, StmtResult},
        value::Value,
        Eval, RuntimeMut,
    },
    parser::ast::{ExprList, OutputRedirection},
};
use std::{
    cmp::max,
    io::{Cursor, Write},
};

#[derive(Debug, Default)]
struct Conversion {
    left_justified: bool,
    signed: bool,
    space: bool,
    alternative_form: bool,
    leading_zeros: bool,
    width: Option<usize>,
    precision: Option<usize>,
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
                    Some(width)
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
                        Some(precision)
                    } else {
                        return Err(err);
                    };
                    break;
                }
                precision.push(p);
                err.push(iter.next().unwrap());
            }
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
        value: &Value,
    ) -> Result<(), EvaluationError> {
        match self.specifier {
            ConversionSpecifier::Float => self.convert_float(output, value),
            ConversionSpecifier::ExponentialLower => self.convert_exponential(output, value, true),
            ConversionSpecifier::ExponentialUpper => self.convert_exponential(output, value, false),
            ConversionSpecifier::ExponentialOrFloatLower => {
                self.convert_exponential_or_float(output, value, true)
            },
            ConversionSpecifier::ExponentialOrFloatUpper => {
                self.convert_exponential_or_float(output, value, false)
            },
            ConversionSpecifier::String => self.convert_string(output, value),
            ConversionSpecifier::SignedDecimal => self.convert_signed_decimal(output, value),
            _ => unimplemented!("{:?}", self.specifier),
        }
    }

    fn convert_signed_decimal<Output: Write>(
        &self,
        output: &mut Output,
        value: &Value,
    ) -> Result<(), EvaluationError> {
        let precision = self.precision.unwrap_or(1);
        let value = value.as_number().trunc();

        if value == 0_f64 && precision == 0 {
            if let Some(width) = self.width {
                write!(output, "{}", " ".repeat(width))?;
            }
            return Ok(());
        }

        let value_str = value.abs().to_string();
        let padding = |width: usize| {
            width.saturating_sub(
                if value.is_sign_negative() || self.signed || self.space {
                    1
                } else {
                    0
                } + max(precision, value_str.len()),
            )
        };
        let (spaces, zeros) = match (self.width, self.precision) {
            (Some(width), Some(precision)) => {
                (padding(width), precision.saturating_sub(value_str.len()))
            },
            (Some(width), None) => {
                if self.leading_zeros {
                    (0, padding(width))
                } else {
                    (padding(width), 0)
                }
            },
            (None, Some(precision)) => (0, precision.saturating_sub(value_str.len())),
            (None, None) => (0, 0),
        };

        if spaces != 0 {
            write!(output, "{}", " ".repeat(spaces))?;
        }
        self.sign(output, value)?;
        if zeros != 0 {
            write!(output, "{}", "0".repeat(zeros))?;
        }

        write!(output, "{}", value.abs())?;

        Ok(())
    }

    fn convert_exponential_or_float<Output: Write>(
        &self,
        output: &mut Output,
        value: &Value,
        lower: bool,
    ) -> Result<(), EvaluationError> {
        let value = value.as_number();
        let p = match self.precision {
            Some(0) => 1,
            Some(p) => p,
            None => 6,
        } as isize;
        let (value_exp, exp) = Conversion::exponent(value);

        let signum = value.signum();

        let (value, exp_notation, precision) = if p > exp && exp >= -4 {
            // decimal notation
            (value, false, (p - 1 - exp) as i32)
        } else {
            // decimal exponent notation
            (value_exp, true, (p - 1) as i32)
        };

        let value = value.abs();
        let trunc = value.trunc().to_string();
        // remove trailing zeros
        let mut fract = (value.fract() * 10_f64.powi(precision)).round() as usize;
        while fract != 0 && fract % 10 == 0 {
            fract /= 10;
        }
        let fract = fract.to_string();

        let padding = if exp_notation {
            self.width.map(|width| {
                width.saturating_sub(
                    if signum.is_sign_negative() || self.signed || self.space { 1 } else { 0 }
                    // 5 = mantissa + exponent
                    + 5
                    // 1 = '.'
                    + if precision != 0 && fract != "0" { fract.len() + 1 } else { 0 },
                )
            })
        } else {
            self.width.map(|width| {
                width.saturating_sub(
                    if signum.is_sign_negative() || self.signed || self.space { 1 } else { 0 }
                    + trunc.len()
                    // 1 = '.'
                    + if precision != 0 && fract != "0" { fract.len() + 1 } else { 0 },
                )
            })
        }
        .unwrap_or(0);

        if padding != 0 && !self.leading_zeros {
            write!(output, "{}", " ".repeat(padding))?;
        }
        self.sign(output, signum)?;
        if padding != 0 && self.leading_zeros {
            write!(output, "{}", "0".repeat(padding))?;
        }

        write!(output, "{}", trunc)?;
        if fract != "0" {
            write!(output, ".{}", fract)?;
        }

        if exp_notation {
            write!(output, "{}{:+03}", if lower { 'e' } else { 'E' }, exp)?;
        }
        Ok(())
    }

    fn convert_exponential<Output: Write>(
        &self,
        output: &mut Output,
        value: &Value,
        lower: bool,
    ) -> Result<(), EvaluationError> {
        let value = value.as_number();
        let precision = self.precision.unwrap_or(6);
        let padding = if let Some(width) = self.width {
            width.saturating_sub(
                if value.is_sign_negative() || self.signed || self.space { 1 } else { 0 }
                // 5 = mantissa + exponent
                + 5
                // 1 = '.'
                + if precision != 0 { precision + 1 } else { 0 },
            )
        } else {
            0
        };

        if padding != 0 && !self.leading_zeros {
            write!(output, "{}", " ".repeat(padding))?;
        }
        self.sign(output, value)?;
        if padding != 0 && self.leading_zeros {
            write!(output, "{}", "0".repeat(padding))?;
        }

        // exponent
        let (value, exp) = Conversion::exponent(value);

        // mantissa
        write!(output, "{}", value.trunc().abs())?;

        // fract
        Conversion::fract(output, precision, value.fract())?;
        write!(output, "{}{:+03}", if lower { 'e' } else { 'E' }, exp)?;

        Ok(())
    }

    fn exponent(value: f64) -> (f64, isize) {
        let mut value = value.abs();

        if value == 0_f64 {
            (0_f64, 0)
        } else if value >= 1_f64 {
            let mut exp = 0;
            while value > 10_f64 {
                value = value / 10_f64;
                exp += 1;
            }
            (value, exp)
        } else {
            let mut exp = 0;
            while value < 1_f64 {
                value = value * 10_f64;
                exp += 1;
            }
            (value, -exp)
        }
    }

    fn convert_float<Output: Write>(
        &self,
        output: &mut Output,
        value: &Value,
    ) -> Result<(), EvaluationError> {
        let value = value.as_number();
        let signum = value.signum();
        let value = value.abs();
        let trunc = value.trunc().to_string();

        let precision = self.precision.unwrap_or(6);
        let padding = if let Some(width) = self.width {
            width.saturating_sub(
                if signum.is_sign_negative() || self.signed || self.space { 1 } else { 0 }
                + trunc.len()
                // 1 = '.'
                + if precision != 0 { precision + 1 } else { 0 },
            )
        } else {
            0
        };

        if padding != 0 && !self.leading_zeros {
            write!(output, "{}", " ".repeat(padding))?;
        }
        self.sign(output, signum)?;
        if padding != 0 && self.leading_zeros {
            write!(output, "{}", "0".repeat(padding))?;
        }

        write!(output, "{}", trunc)?;
        Conversion::fract(output, precision, value.fract())
    }

    fn sign<Output: Write>(&self, output: &mut Output, value: f64) -> Result<(), EvaluationError> {
        if value.is_sign_positive() {
            if self.signed {
                write!(output, "+")?;
            } else if self.space {
                write!(output, " ")?;
            }
        } else {
            write!(output, "-")?;
        }
        Ok(())
    }

    fn fract<Output: Write>(
        output: &mut Output,
        precision: usize,
        value: f64,
    ) -> Result<(), EvaluationError> {
        if precision != 0 {
            let fract = (value * 10_f64.powi(precision as i32)).round();
            if fract == 0_f64 {
                write!(output, ".{}", "0".repeat(precision))?;
            } else {
                write!(output, ".{}", fract)?;
            }
        }
        Ok(())
    }

    fn convert_string<Output: Write>(
        &self,
        output: &mut Output,
        value: &Value,
    ) -> Result<(), EvaluationError> {
        let value = if let Some(precision) = self.precision {
            value.as_string().get(0..precision).unwrap().to_string()
        } else {
            value.as_string()
        };
        if let Some(width) = self.width {
            write!(output, "{}", " ".repeat(width.saturating_sub(value.len())))?;
        }
        write!(output, "{}", value)?;
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
    ExponentialLower,
    ExponentialUpper,
    ExponentialOrFloatLower,
    ExponentialOrFloatUpper,
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
            'f' | 'F' => Some(ConversionSpecifier::Float),
            'e' => Some(ConversionSpecifier::ExponentialLower),
            'E' => Some(ConversionSpecifier::ExponentialUpper),
            'g' => Some(ConversionSpecifier::ExponentialOrFloatLower),
            'G' => Some(ConversionSpecifier::ExponentialOrFloatUpper),
            'c' => Some(ConversionSpecifier::UnsignedChar),
            's' => Some(ConversionSpecifier::String),
            '%' => Some(ConversionSpecifier::Percent),
            _ => None,
        }
    }
}

pub fn convert_values<
    FormatIterator: Iterator<Item = char>,
    ValueIterator: Iterator<Item = Value>,
    Output: Write,
>(
    format: &str,
    mut format_iter: FormatIterator,
    mut value: ValueIterator,
    output: &mut Output,
) -> Result<Option<StmtResult>, EvaluationError> {
    while let Some(c) = format_iter.next() {
        match c {
            '%' => match Conversion::new(&mut format_iter) {
                Ok(conv) => match value.next() {
                    Some(value) => conv.convert(output, &value)?,
                    None => {
                        return Err(EvaluationError::MissingFormatStringArgs(format.to_owned()));
                    },
                },
                Err(err) => write!(output, "{}", err)?,
            },
            '\\' => match format_iter.next().unwrap() {
                '\\' => write!(output, "{}", '\\')?,
                'a' => write!(output, "{}", '\x07')?,
                'b' => write!(output, "{}", '\x08')?,
                'f' => write!(output, "{}", '\x0C')?,
                'n' => write!(output, "{}", '\n')?,
                'r' => write!(output, "{}", '\r')?,
                't' => write!(output, "{}", '\t')?,
                'v' => write!(output, "{}", '\x0B')?,
                _ => unimplemented!(),
            },
            _ => write!(output, "{}", c)?,
        }
    }
    Ok(None)
}

pub fn sprintf<Output: Write>(
    args: &ExprList,
    rt: &mut RuntimeMut<'_, Output>,
) -> Result<Value, EvaluationError> {
    if args.len() == 0 {
        return Err(EvaluationError::InvalidNumberOfArguments(
            String::from("sprintf"),
            1,
            args.len(),
        ));
    }

    let mut iter = args.0.iter();
    let format = iter.next().unwrap().eval(rt)?.as_string();
    let format_iter = format.chars();

    let mut values = Vec::new();
    while let Some(expr) = iter.next() {
        values.push(expr.eval(rt)?);
    }

    let mut out = Cursor::new(Vec::new());
    convert_values(&format, format_iter, values.into_iter(), &mut out)?;

    let result = std::str::from_utf8(out.get_ref())?;
    Ok(Value::from(result))
}

pub fn execute<Output>(
    rt: &mut RuntimeMut<'_, Output>,
    exprs: &ExprList,
    redir: &Option<OutputRedirection>,
) -> Result<Option<StmtResult>, EvaluationError>
where
    Output: Write,
{
    let path = if let Some(redir) = redir {
        let path = file_name(rt, &redir)?;
        rt.redirs.add_file(&path, redir)?;
        Some(path)
    } else {
        None
    };

    let mut iter = exprs.0.iter();
    let format = iter.next().unwrap().eval(rt)?.as_string();
    let format_iter = format.chars();

    let mut values = Vec::new();
    while let Some(expr) = iter.next() {
        values.push(expr.eval(rt)?);
    }

    match &path {
        Some(path) => {
            let file = rt.redirs.get_file(path);
            convert_values(&format, format_iter, values.into_iter(), file)
        },
        None => convert_values(&format, format_iter, values.into_iter(), rt.output),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn conversions() {
        let conv = Conversion::new(&mut "d".chars()).unwrap();
        assert_eq!(conv.specifier, ConversionSpecifier::SignedDecimal);

        let conv = Conversion::new(&mut ".5f".chars()).unwrap();
        assert_eq!(conv.precision, Some(5));
        assert_eq!(conv.specifier, ConversionSpecifier::Float);

        let conv = Conversion::new(&mut "42s".chars()).unwrap();
        assert_eq!(conv.width, Some(42));
        assert_eq!(conv.specifier, ConversionSpecifier::String);

        let conv = Conversion::new(&mut "%".chars()).unwrap();
        assert_eq!(conv.specifier, ConversionSpecifier::Percent);

        let conv = Conversion::new(&mut ".42%".chars()).unwrap();
        assert_eq!(conv.specifier, ConversionSpecifier::Percent);
    }

    fn assert_conversions(data: &[(&str, Value, &str)]) {
        for (i, (fmt, arg, expected)) in data.iter().enumerate() {
            let mut out = Cursor::new(Vec::new());
            let conv = Conversion::new(&mut fmt.chars()).unwrap();
            conv.convert(&mut out, &arg).unwrap();
            assert_eq!(
                String::from_utf8(out.into_inner()).unwrap(),
                *expected,
                "failed on input[{}]: {:?}",
                i,
                conv
            );
        }
    }

    #[test]
    fn float_conversion() {
        #[rustfmt::skip]
        let data = [
            (".2f",     Value::from(4.2),   "4.20"),
            (".2f",     Value::from(-4.2),  "-4.20"),
            (".0f",     Value::from(4.2),   "4"),
            (".2f",     Value::from(42.2),  "42.20"),
            (".2f",     Value::from(10),    "10.00"),
            ("f",       Value::from(4.2),   "4.200000"),
            ("+.2f",    Value::from(4.2),   "+4.20"),
            ("+.2f",    Value::from(-4.2),  "-4.20"),
            (" .2f",    Value::from(-4.2),  "-4.20"),
            (" .2f",    Value::from(4.2),   " 4.20"),
            ("0f",      Value::from(4.2),   "4.200000"),
            ("010.2f",  Value::from(4.2),   "0000004.20"),
            ("010.0f",  Value::from(4.2),   "0000000004"),
            ("02.0f",   Value::from(4.2),   "04"),
            (" 04.1f",  Value::from(4.2),   " 4.2"),
            ("05.1f",   Value::from(4.2),   "004.2"),
            (" 05.1f",  Value::from(4.2),   " 04.2"),
            (" 05.1f",  Value::from(-4.2),  "-04.2"),
            ("+05.1f",  Value::from(4.2),   "+04.2"),
            ("+5.1f",   Value::from(4.2),   " +4.2"),
            (" 5.1f",   Value::from(4.2),   "  4.2"),
            ("5.1f",    Value::from(4.2),   "  4.2"),
        ];
        assert_conversions(&data);
    }

    #[test]
    fn string_conversion() {
        #[rustfmt::skip]
        let data = [
            ("s",      Value::from("123456789"),  "123456789"),
            ("5s",     Value::from("123456789"),  "123456789"),
            ("10s",    Value::from("123456789"),  " 123456789"),
            (".5s",    Value::from("123456789"),  "12345"),
            ("10.5s",  Value::from("123456789"),  "     12345"),
            (".0s",    Value::from("123456789"),  ""),
            ("2.0s",   Value::from("123456789"),  "  "),
        ];
        assert_conversions(&data);
    }

    #[test]
    fn exponent_conversion() {
        #[rustfmt::skip]
        let data = [
            ("e",       Value::from(0),                 "0.000000e+00"),
            ("E",       Value::from(0),                 "0.000000E+00"),
            ("e",       Value::from(4.2),               "4.200000e+00"),
            ("e",       Value::from(4999.2),            "4.999200e+03"),
            ("e",       Value::from(42),                "4.200000e+01"),
            ("e",       Value::from(4),                 "4.000000e+00"),
            (".2e",     Value::from(4),                 "4.00e+00"),
            (".2e",     Value::from(4235),              "4.24e+03"),
            (".0e",     Value::from(4235),              "4e+03"),
            ("10.0e",   Value::from(4235),              "     4e+03"),
            ("9.2e",    Value::from(4),                 " 4.00e+00"),
            ("10.2e",   Value::from(-4235),             " -4.24e+03"),
            ("010.2e",  Value::from(-4235),             "-04.24e+03"),
            ("e",       Value::from(0.0000000000045),   "4.500000e-12"),
            ("015e",    Value::from(0.0000000000045),   "0004.500000e-12"),
            ("15e",     Value::from(0.0000000000045),   "   4.500000e-12"),
            ("+15e",    Value::from(0.0000000000045),   "  +4.500000e-12"),
            (" 15e",    Value::from(0.0000000000045),   "   4.500000e-12"),
            (" 15e",    Value::from(-0.0000000000045),  "  -4.500000e-12"),
            ("+.0e",    Value::from(0),                 "+0e+00"),
        ];
        assert_conversions(&data);
    }

    #[test]
    fn exponent_or_float_conversion() {
        #[rustfmt::skip]
        let data = [
            (".2g",     Value::from(4.2),               "4.2"),
            (".2g",     Value::from(-4.2),              "-4.2"),
            (".0g",     Value::from(4.2),               "4"),
            (".2g",     Value::from(42.2),              "42"),
            (".2g",     Value::from(10),                "10"),
            ("g",       Value::from(4.2),               "4.2"),
            ("+.2g",    Value::from(4.2),               "+4.2"),
            ("+.2g",    Value::from(-4.2),              "-4.2"),
            (" .2g",    Value::from(-4.2),              "-4.2"),
            (" .2g",    Value::from(4.2),               " 4.2"),
            ("0g",      Value::from(4.2),               "4.2"),
            ("010.2g",  Value::from(4.2),               "00000004.2"),
            ("010.0g",  Value::from(4.2),               "0000000004"),
            ("02.0g",   Value::from(4.2),               "04"),
            (" 04.1g",  Value::from(4.2),               " 004"),
            ("05.1g",   Value::from(4.2),               "00004"),
            (" 05.1g",  Value::from(4.2),               " 0004"),
            (" 05.1g",  Value::from(-4.2),              "-0004"),
            ("+05.1g",  Value::from(4.2),               "+0004"),
            ("+5.1g",   Value::from(4.2),               "   +4"),
            (" 5.1g",   Value::from(4.2),               "    4"),
            ("5.1g",    Value::from(4.2),               "    4"),
            ("g",       Value::from(0),                 "0"),
            ("G",       Value::from(0),                 "0"),
            ("g",       Value::from(4.2),               "4.2"),
            ("g",       Value::from(4999.2),            "4999.2"),
            ("g",       Value::from(42),                "42"),
            ("g",       Value::from(4),                 "4"),
            (".2g",     Value::from(4),                 "4"),
            (".2g",     Value::from(4235),              "4.2e+03"),
            (".0g",     Value::from(4235),              "4e+03"),
            ("10.0g",   Value::from(-4235),             "    -4e+03"),
            ("9.2g",    Value::from(4),                 "        4"),
            ("10.2g",   Value::from(-4235),             "  -4.2e+03"),
            ("010.2g",  Value::from(-4235),             "-004.2e+03"),
            ("g",       Value::from(0.0000000000045),   "4.5e-12"),
            ("015g",    Value::from(0.0000000000045),   "000000004.5e-12"),
            ("15g",     Value::from(0.0000000000045),   "        4.5e-12"),
            ("+15g",    Value::from(0.0000000000045),   "       +4.5e-12"),
            (" 15g",    Value::from(0.0000000000045),   "        4.5e-12"),
            (" 15g",    Value::from(-0.0000000000045),  "       -4.5e-12"),
            ("+.0g",    Value::from(0),                 "+0"),
        ];
        assert_conversions(&data);
    }

    #[test]
    fn signed_decimal_conversion() {
        #[rustfmt::skip]
        let data = [
            ("d",      Value::from(0.0),  "0"),
            (".0d",    Value::from(0.0),  ""),
            (".0d",    Value::from(4),    "4"),
            ("d",      Value::from(4.2),  "4"),
            ("5d",     Value::from(4),    "    4"),
            ("5d",     Value::from(-4),   "   -4"),
            ("5.0d",   Value::from(0),    "     "),
            ("05d",    Value::from(4),    "00004"),
            ("05d",    Value::from(-4),   "-0004"),
            ("05.0d",  Value::from(4),    "    4"),
            ("05.0d",  Value::from(-4),   "   -4"),
            ("05.1d",  Value::from(4),    "    4"),
            ("05.2d",  Value::from(4),    "   04"),
            (" d",     Value::from(4),    " 4"),
            (" d",     Value::from(-4),   "-4"),
            ("+d",     Value::from(4),    "+4"),
            ("+d",     Value::from(-4),   "-4"),
            (".2d",    Value::from(4),    "04"),
            (".2d",    Value::from(-4),   "-04"),
            ("5.2d",   Value::from(4),    "   04"),
            ("5.2d",   Value::from(-4),   "  -04"),
        ];
        assert_conversions(&data);
    }
}
