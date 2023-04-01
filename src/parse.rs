use nom::{
    branch::alt,
    bytes::complete::take_while1,
    character::complete::{char, one_of},
    sequence::{delimited, preceded, tuple},
    IResult, Parser,
};

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

// --- Basic Primitives and Predicates ---

// Parses a constant ("0" or "1")
fn constant(s: &str) -> IResult<&str, Expression> {
    one_of("01")
        .map(|c| match c {
            '0' => Expression::Constant(false),
            '1' => Expression::Constant(true),
            _ => unreachable!(),
        })
        .parse(s)
}

// Parses a logical variable, which is a string of 1 or more alphabetical characters.
fn variable(s: &str) -> IResult<&str, Expression> {
    take_while1(|c: char| c.is_ascii_alphabetic())
        .map(Expression::Variable)
        .parse(s)
}

// Parses an infix binary operator ("&", "|" or "^").
fn binary_op(s: &str) -> IResult<&str, BinaryOp> {
    one_of("&|^")
        .map(|c| match c {
            '&' => BinaryOp::And,
            '|' => BinaryOp::Or,
            '^' => BinaryOp::Xor,
            _ => unreachable!(),
        })
        .parse(s)
}

// From this point on i test out my grammar.
// god help us

// value ::= constant | variable
fn value(s: &str) -> IResult<&str, Expression> {
    alt((constant, variable))(s)
}

// This isn't really an "atom". It's just the only word i can think of for it.
// Either a value or a bracketed expression.

// atom ::= value | '(' expression ')'
fn atom(s: &str) -> IResult<&str, Expression> {
    alt((value, delimited(char('('), expression, char(')'))))(s)
}

fn negated_atom(s: &str) -> IResult<&str, Expression> {
    preceded(char('~'), atom)
        .map(|expr| Expression::Not(Box::new(expr)))
        .parse(s)
}

// term ::= '~' atom | atom
fn term(s: &str) -> IResult<&str, Expression> {
    alt((negated_atom, atom))(s)
}

fn combination(s: &str) -> IResult<&str, Expression> {
    tuple((term, binary_op, expression))
        .map(|(t, bop, expr)| Expression::Binary(bop, Box::new(t), Box::new(expr)))
        .parse(s)
}

// expression ::= term binary_op expression | term

/// Parses a logical expression, returning the abstract syntax tree.
/// The order of operations for this is actually wrong. It's okay because the
/// spec for the problem says all intermediate steps should be bracketed
/// However for a real logical expression parser, & would have to have higher
/// precedence than |. It currently has the same.
pub fn expression(s: &str) -> IResult<&str, Expression> {
    alt((combination, term))(s)
}

// --- Unit Tests ---
mod test {
    // This seems like a bug? I have definitely used these imports.
    #[allow(unused)]
    use super::*;

    #[test]
    fn test_value() {
        assert_eq!(value("xyz1|&"), Ok(("1|&", Expression::Variable("xyz"))));
        assert_eq!(value("101010"), Ok(("01010", Expression::Constant(true))));
        assert!(value("&|()^^^hello111").is_err());
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
        assert_eq!(expression("xyz"), Ok(("", xyz.clone())));
        assert_eq!(expression("1"), Ok(("", fr.clone())));

        // Negation
        assert_eq!(
            expression("~xyz"),
            Ok(("", Expression::Not(Box::new(xyz.clone()))))
        );

        // De Morgan's Law
        assert_eq!(expression("~(~xyz&~1)"), Ok(("", de_morgan)));

        // Test input
        assert_eq!(expression("(Q&~P)|(P&~Q)"), Ok(("", test_input)));

        // invalid input?
        assert!(expression("~~P").is_err());
        assert!(expression("(A|B").is_err());
    }
}
