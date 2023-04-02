#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    And,
    Or,
    Xor,
}

/// A node of an abstract syntax tree representing logical expressions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression<'a> {
    Constant(bool),
    Variable(&'a str),
    Not(Box<Expression<'a>>),
    Binary(BinaryOp, Box<Expression<'a>>, Box<Expression<'a>>),
}

// --- Combinators ---
fn satisfies<P>(p: P) -> impl Fn(&str) -> Option<(&str, char)>  
where P: Fn(char) -> bool 
{
    move |s| {
        let char = s.chars().next()?;

        if p(char) {
            Some((&s[1..], char))
        } else {
            None
        }
    }
}

fn char(c: char) -> impl Fn(&str) -> Option<(&str, char)> {
    satisfies(move |x| x == c)
}

fn take_while1<P>(p: P) -> impl Fn(&str) -> Option<(&str, &str)> 
where P: Fn(char) -> bool {
    move |s| {
        let mut chars = s.char_indices();
        if !p(chars.next()?.1) {
            return None
        }

        let mut index = 1;

        for (i, c) in s.char_indices() {
            if !p(c) {
                break;
            }

            index = i + c.len_utf8();
        }

        Some((&s[index..], &s[..index]))
    }
}

fn delimited<'a, D1, P, D2, O1, O, O2>(d1: D1, parser: P, d2: D2) -> impl Fn(&'a str) -> Option<(&'a str, O)> 
where D1: Fn(&'a str) -> Option<(&'a str, O1)>,
      D2: Fn(&'a str) -> Option<(&'a str, O2)>,
      P: Fn(&'a str) -> Option<(&'a str, O)>
{
    move |s| {
        let (rest, _) = d1(s)?;
        let (rest, output) = parser(rest)?;
        let (rest, _) = d2(rest)?;

        Some((rest, output))
    }
}

fn preceded<'a, D, P, O1, O>(d: D, parser: P) -> impl Fn(&'a str) -> Option<(&'a str, O)> 
where D: Fn(&'a str) -> Option<(&'a str, O1)>,
      P: Fn(&'a str) -> Option<(&'a str, O)>
{
    move |s| {
        let (rest, _) = d(s)?;

        parser(rest)
    }
}

fn thruple<'a, P1, P2, P3, O1, O2, O3>(p1: P1, p2: P2, p3: P3) -> impl Fn(&'a str) -> Option<(&'a str, (O1, O2, O3))>
where P1: Fn(&'a str) -> Option<(&'a str, O1)>,
      P2: Fn(&'a str) -> Option<(&'a str, O2)>,
      P3: Fn(&'a str) -> Option<(&'a str, O3)>,
{
    move |s| {
        let (rest, o1) = p1(s)?;
        let (rest, o2) = p2(rest)?;
        let (rest, o3) = p3(rest)?;

        Some((rest, (o1, o2, o3)))
    }
}

// --- Basic Primitives and Predicates ---



// Parses a constant ("0" or "1")
fn constant(s: &str) -> Option<(&str, Expression)> {
    let (rest, c) = satisfies(|c| c == '0' || c == '1')(s)?;

    let b = Expression::Constant(match c {
        '0' => false,
        '1' => true,
        _ => unreachable!(),
    });

    Some((rest, b))
}

// Parses a logical variable, which is a string of 1 or more alphabetical characters.
fn variable(s: &str) -> Option<(&str, Expression)> {
    take_while1(|c: char| c.is_ascii_alphabetic())(s)
        .map(|(rest, out)| (rest, Expression::Variable(out)))
}

// Parses an infix binary operator ("&", "|" or "^").
fn binary_op(s: &str) -> Option<(&str, BinaryOp)> {
    satisfies(|c| "&|^".contains(c))(s)
        .map(|(rest, c)| 
            (rest, match c {
                '&' => BinaryOp::And,
                '|' => BinaryOp::Or,
                '^' => BinaryOp::Xor,
                _ => unreachable!(),
        }))
}

// From this point on i test out my grammar.
// god help us

// value ::= constant | variable
fn value(s: &str) -> Option<(&str, Expression)> {
    constant(s).or(variable(s))
}

// This isn't really an "atom". It's just the only word i can think of for it.
// Either a value or a bracketed expression.

// atom ::= value | '(' expression ')'
fn atom(s: &str) -> Option<(&str, Expression)> {
    value(s).or(delimited(char('('), expression, char(')'))(s))
}

fn negated_atom(s: &str) -> Option<(&str, Expression)> {
    let (rest, output) = preceded(char('~'), atom)(s)?;

    Some((rest, Expression::Not(Box::new(output))))
}

// term ::= '~' atom | atom
fn term(s: &str) -> Option<(&str, Expression)> {
    negated_atom(s).or(atom(s))
}

fn combination(s: &str) -> Option<(&str, Expression)> {
    let (rest, (t, bop, expr)) = thruple(term, binary_op, expression)(s)?;
    Some((rest, Expression::Binary(bop, Box::new(t), Box::new(expr))))
}

// expression ::= term binary_op expression | term

/// Parses a logical expression, returning the abstract syntax tree.
/// The order of operations for this is actually wrong. It's okay because the
/// spec for the problem says all intermediate steps should be bracketed
/// However for a real logical expression parser, & would have to have higher
/// precedence than |. It currently has the same.
pub fn expression(s: &str) -> Option<(&str, Expression)> {
    combination(s).or(term(s))
}

// --- Unit Tests ---
mod test {
    // This seems like a bug? I have definitely used these imports.
    #[allow(unused)]
    use super::*;

    #[test]
    fn test_value() {
        assert_eq!(value("xyz1|&"), Some(("1|&", Expression::Variable("xyz"))));
        assert_eq!(value("101010"), Some(("01010", Expression::Constant(true))));
        assert!(value("&|()^^^hello111").is_none());
    }

    #[test]
    fn test_expression() {
        let xyz = Expression::Variable("xyz");
        let fr = Expression::Constant(true);
        let de_morgan = Expression::Not(Box::new(Expression::Binary(
            BinaryOp::And,
            Box::new(Expression::Not(Box::new(xyz.clone()))),
            Box::new(Expression::Not(Box::new(fr.clone()))),
        )));
        // (Q&~P)|(P&~Q)
        let test_input = Expression::Binary(
            BinaryOp::Or,
            Box::new(Expression::Binary(
                BinaryOp::And,
                Box::new(Expression::Variable("Q")),
                Box::new(Expression::Not(Box::new(Expression::Variable("P")))),
            )),
            Box::new(Expression::Binary(
                BinaryOp::And,
                Box::new(Expression::Variable("P")),
                Box::new(Expression::Not(Box::new(Expression::Variable("Q")))),
            )),
        );
        // Simple values
        assert_eq!(expression("xyz"), Some(("", xyz.clone())));
        assert_eq!(expression("1"), Some(("", fr.clone())));

        // Negation
        assert_eq!(
            expression("~xyz"),
            Some(("", Expression::Not(Box::new(xyz.clone()))))
        );

        // De Morgan's Law
        assert_eq!(expression("~(~xyz&~1)"), Some(("", de_morgan)));

        // Test input
        assert_eq!(expression("(Q&~P)|(P&~Q)"), Some(("", test_input)));

        // invalid input?
        assert!(expression("~~P").is_none());
        assert!(expression("(A|B").is_none());
    }
}
