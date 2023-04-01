mod parse;
use std::collections::{HashMap, HashSet};

use parse::*;

// Takes in an expression and a map binding variable names to boolean values
// the evaluation of that expression under those bindings. If it can't be
// evaluated it returns None.
fn evaluate(exp: &Expression, vars: &HashMap<&str, bool>) -> Option<bool> {
    match exp {
        Expression::Constant(b) => Some(*b),
        Expression::Variable(name) => vars.get(name).copied(),
        Expression::Binary(op, left_exp, right_exp) => {
            let left = evaluate(left_exp, vars)?;
            let right = evaluate(right_exp, vars)?;

            Some(match op {
                BinaryOp::And => left && right,
                BinaryOp::Or => left || right,
                BinaryOp::Xor => left != right,
            })
        }

        Expression::Not(new_exp) => Some(!(evaluate(new_exp, vars)?)),
    }
}

fn vars_helper<'a>(exp: &Expression<'a>, vars: &mut HashSet<&'a str>) {
    match exp {
        // If you're wondering why the braces around this next match arm
        // HashSet::insert returns a bool and not void, so without the braces & semicolon
        // the arms would have different return types.
        Expression::Variable(var) => {
            vars.insert(var);
        }
        Expression::Not(new_exp) => vars_helper(new_exp, vars),
        Expression::Binary(_, left_exp, right_exp) => {
            vars_helper(left_exp, vars);
            vars_helper(right_exp, vars);
        }
        _ => {}
    };
}

// Returns the set of variable names in an expression.
fn vars<'a>(exp: &Expression<'a>) -> HashSet<&'a str> {
    let mut res = HashSet::new();
    vars_helper(exp, &mut res);
    res
}

fn truth(x: &str) -> Vec<String> {
    let (_, expression) = expression(x).unwrap();
    let mut vars = vars(&expression).into_iter().collect::<Vec<_>>();

    vars.sort();
    let mut res = vec!["".to_string(), "".to_string()];

    for var in vars.iter() {
        // This formatting will be correct because the only variables we'll get
        // in the test cases are 1 letter long. Wouldn't work for all possible
        // expressions.
        res[0].push_str(var);
        res[0].push(' ');

        res[1].push_str("--");
    }

    res[0].push_str("| =");
    res[1].push_str("+--");

    // Funny maths trick
    for x in 0..(2_u32.pow(vars.len() as u32)) {
        // Generate the truth table
        let map: HashMap<&str, bool> = HashMap::from_iter(
            vars.iter()
                .rev()
                .enumerate()
                .map(|(i, var)| (*var, (x >> i) % 2 == 1)),
        );

        let mut line = String::new();
        // Add the line to the result
        for v in vars.iter() {
            line.push_str(&format!("{} ", if *map.get(v).unwrap() { 1 } else { 0 }));
        }

        line.push_str(&format!(
            "| {}",
            if evaluate(&expression, &map).unwrap() {
                1
            } else {
                0
            }
        ));
        res.push(line);
    }

    res
}

fn main() {
    let out = truth("(Q&~P)|(P&~Q)");
    println!("{}", out.join("\n"));
}
