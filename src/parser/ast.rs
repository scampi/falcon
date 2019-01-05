use regex::Regex;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Program {
    items: Vec<Item>,
}

impl Program {
    pub fn new(items: Vec<Item>) -> Program {
        Program { items }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for item in &self.items {
            write!(f, "{};", item)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Item {
    Action(StmtList),
    PatternAction(Pattern, StmtList),
    FunctionDef(String, Vec<String>, StmtList),
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Item::Action(stmts) => write!(f, "{}", stmts),
            Item::PatternAction(pattern, action) => write!(f, "{} {}", pattern, action),
            Item::FunctionDef(name, args, stmts) => {
                write!(f, "function {}(", name)?;
                for arg in args {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ") {}", stmts)
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Begin,
    End,
    Exprs(ExprList),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Begin => write!(f, "BEGIN"),
            Pattern::End => write!(f, "END"),
            Pattern::Exprs(exprs) => write!(f, "{}", exprs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StmtList(pub Vec<Stmt>);

impl fmt::Display for StmtList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.0 {
            write!(f, "{}, ", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Block(StmtList),
    IfElse(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    DoWhile(Expr, Box<Stmt>),
    For(
        Option<Box<Stmt>>,
        Option<Expr>,
        Option<Box<Stmt>>,
        Box<Stmt>,
    ),
    ForIn(String, String, Box<Stmt>),
    Expr(Expr),
    Break,
    Continue,
    Next,
    Exit(Option<Expr>),
    Return(Option<Expr>),
    Delete(String, ExprList),
    Print(ExprList, Option<OutputRedirection>),
    Printf(ExprList, Option<OutputRedirection>),
}

#[derive(Debug, PartialEq)]
pub enum OutputRedirection {
    Truncate(Expr),
    Append(Expr),
    Pipe(Expr),
}

impl fmt::Display for OutputRedirection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OutputRedirection::Truncate(e) => write!(f, "> {}", e),
            OutputRedirection::Append(e) => write!(f, ">> {}", e),
            OutputRedirection::Pipe(e) => write!(f, "| {}", e),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Print(exprs, None) => write!(f, "print({})", exprs),
            Stmt::Printf(exprs, None) => write!(f, "printf({})", exprs),
            Stmt::Print(exprs, Some(redir)) => write!(f, "print({}) {}", exprs, redir),
            Stmt::Printf(exprs, Some(redir)) => write!(f, "printf({}) {}", exprs, redir),
            Stmt::Block(stmts) => write!(f, "{{ {} }}", stmts),
            Stmt::IfElse(cond, ok, ko) => {
                write!(f, "if ({}) {}", cond, ok)?;
                if let Some(ko) = ko {
                    write!(f, " else {}", ko)?;
                }
                Ok(())
            },
            Stmt::For(start, until, next, body) => {
                write!(f, "for (")?;
                if let Some(start) = start {
                    write!(f, "{}", start)?;
                }
                write!(f, ";")?;
                if let Some(until) = until {
                    write!(f, " {}", until)?;
                }
                write!(f, ";")?;
                if let Some(next) = next {
                    write!(f, " {}", next)?;
                }
                write!(f, ") {}", body)
            },
            Stmt::ForIn(a, b, body) => write!(f, "for ({} in {}) {}", a, b, body),
            Stmt::While(cond, body) => write!(f, "while ({}) {}", cond, body),
            Stmt::DoWhile(cond, body) => write!(f, "do {} while ({})", body, cond),
            Stmt::Expr(e) => write!(f, "{}", e),
            Stmt::Break => write!(f, "break"),
            Stmt::Continue => write!(f, "continue"),
            Stmt::Next => write!(f, "next"),
            Stmt::Exit(e) => {
                write!(f, "exit")?;
                if let Some(e) = e {
                    write!(f, "{}", e)?;
                }
                Ok(())
            },
            Stmt::Return(e) => {
                write!(f, "return")?;
                if let Some(e) = e {
                    write!(f, "{}", e)?;
                }
                Ok(())
            },
            Stmt::Delete(name, exprs) => write!(f, "delete {}[{}]", name, exprs),
        }
    }
}

#[derive(Debug)]
pub struct RegexEq(pub Regex);

impl PartialEq for RegexEq {
    fn eq(&self, other: &RegexEq) -> bool {
        self.0.as_str() == other.0.as_str()
    }
}

impl fmt::Display for RegexEq {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl RegexEq {
    #[cfg(test)]
    pub fn new(reg: &str) -> RegexEq {
        RegexEq(Regex::new(reg).unwrap())
    }
}

#[derive(Debug, PartialEq)]
pub struct ExprList(pub Vec<Expr>);

impl fmt::Display for ExprList {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        for expr in &self.0 {
            write!(formatter, "{}, ", expr)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Grouping(Box<Expr>),
    UnaryPlus(Box<Expr>),
    UnaryMinus(Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Concat(Box<Expr>, Box<Expr>),
    Comparison(CmpOperator, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Box<Expr>),
    NonMatch(Box<Expr>, Box<Expr>),
    Array(ExprList, String),
    LogicalAnd(Box<Expr>, Box<Expr>),
    LogicalOr(Box<Expr>, Box<Expr>),
    LogicalNot(Box<Expr>),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
    Number(f64),
    String(String),
    LValue(LValueType),
    Regexp(RegexEq),
    PreIncrement(LValueType),
    PreDecrement(LValueType),
    PostIncrement(LValueType),
    PostDecrement(LValueType),
    FunctionCall(String, ExprList),
    Assign(AssignType, LValueType, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum CmpOperator {
    LessThan,
    LessThanOrEqual,
    NotEqual,
    Equal,
    GreaterThan,
    GreaterThanOrEqual,
}

impl CmpOperator {
    pub fn compare<T: PartialOrd>(&self, avalue: &T, bvalue: &T) -> bool {
        match self {
            CmpOperator::LessThan => avalue < bvalue,
            CmpOperator::LessThanOrEqual => avalue <= bvalue,
            CmpOperator::NotEqual => avalue != bvalue,
            CmpOperator::Equal => avalue == bvalue,
            CmpOperator::GreaterThan => avalue > bvalue,
            CmpOperator::GreaterThanOrEqual => avalue >= bvalue,
        }
    }
}

impl From<&str> for CmpOperator {
    fn from(s: &str) -> Self {
        match s {
            "<" => CmpOperator::LessThan,
            "<=" => CmpOperator::LessThanOrEqual,
            "!=" => CmpOperator::NotEqual,
            "==" => CmpOperator::Equal,
            ">" => CmpOperator::GreaterThan,
            ">=" => CmpOperator::GreaterThanOrEqual,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for CmpOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CmpOperator::LessThan => write!(f, "<"),
            CmpOperator::LessThanOrEqual => write!(f, "<="),
            CmpOperator::NotEqual => write!(f, "!="),
            CmpOperator::Equal => write!(f, "=="),
            CmpOperator::GreaterThan => write!(f, ">"),
            CmpOperator::GreaterThanOrEqual => write!(f, ">="),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum LValueType {
    Name(String),
    Dollar(Box<Expr>),
    Brackets(String, ExprList),
}

impl fmt::Display for LValueType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LValueType::Name(name) => write!(f, "{}", name),
            LValueType::Dollar(e) => write!(f, "{}", e),
            LValueType::Brackets(name, exprs) => write!(f, "{}[{}]", name, exprs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum AssignType {
    Normal,
    Pow,
    Mod,
    Mul,
    Div,
    Add,
    Sub,
}

impl AssignType {
    pub fn new(s: &str) -> Self {
        match s {
            "=" => AssignType::Normal,
            "^=" => AssignType::Pow,
            "%=" => AssignType::Mod,
            "*=" => AssignType::Mul,
            "/=" => AssignType::Div,
            "+=" => AssignType::Add,
            "-=" => AssignType::Sub,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for AssignType {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignType::Normal => write!(formatter, "="),
            AssignType::Pow => write!(formatter, "^="),
            AssignType::Mod => write!(formatter, "%="),
            AssignType::Mul => write!(formatter, "*="),
            AssignType::Div => write!(formatter, "/="),
            AssignType::Add => write!(formatter, "+="),
            AssignType::Sub => write!(formatter, "-="),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Grouping(e) => write!(formatter, "❨ {} ❩", e),
            Expr::UnaryPlus(e) => write!(formatter, "+( {} )", e),
            Expr::UnaryMinus(e) => write!(formatter, "-( {} )", e),
            Expr::Pow(l, r) => write!(formatter, "( {} ) ^ ( {} )", l, r),
            Expr::Mul(l, r) => write!(formatter, "( {} ) * ( {} )", l, r),
            Expr::Div(l, r) => write!(formatter, "( {} ) / ( {} )", l, r),
            Expr::Mod(l, r) => write!(formatter, "( {} ) % ( {} )", l, r),
            Expr::Add(l, r) => write!(formatter, "( {} ) + ( {} )", l, r),
            Expr::Minus(l, r) => write!(formatter, "( {} ) - ( {} )", l, r),
            Expr::Concat(l, r) => write!(formatter, "( {} )   ( {} )", l, r),
            Expr::Comparison(op, l, r) => write!(formatter, "( {} ) {} ( {} )", l, op, r),
            Expr::Match(l, r) => write!(formatter, "( {} ) ~ ( {} )", l, r),
            Expr::NonMatch(l, r) => write!(formatter, "( {} ) !~ ( {} )", l, r),
            Expr::Array(exprs, name) => write!(formatter, "({}) in {}", exprs, name),
            Expr::LogicalAnd(l, r) => write!(formatter, "( {} ) && ( {} )", l, r),
            Expr::LogicalOr(l, r) => write!(formatter, "( {} ) || ( {} )", l, r),
            Expr::LogicalNot(e) => write!(formatter, "!( {} )", e),
            Expr::Conditional(c, t, f) => write!(formatter, "( {} ) ? ( {} ) : ( {} )", c, t, f),

            Expr::Number(n) => write!(formatter, "{}", n),
            Expr::String(s) => write!(formatter, r#" "{}" "#, s),
            Expr::LValue(lvalue) => write!(formatter, "{}", lvalue),
            Expr::Regexp(ere) => write!(formatter, " /{}/ ", ere),
            Expr::PreIncrement(lvalue) => write!(formatter, " ++{} ", lvalue),
            Expr::PreDecrement(lvalue) => write!(formatter, " --{} ", lvalue),
            Expr::PostIncrement(lvalue) => write!(formatter, " {}++ ", lvalue),
            Expr::PostDecrement(lvalue) => write!(formatter, " {}-- ", lvalue),
            Expr::FunctionCall(fname, args) => write!(formatter, " {}({}) ", fname, args),
            Expr::Assign(op, l, v) => write!(formatter, " {} {} {} ", l, op, v),
        }
    }
}
