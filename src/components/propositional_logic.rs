use core::fmt;

use dioxus::prelude::*;
use nom::{
    bytes::complete::tag, character::complete::multispace0, multi::many0, sequence::pair, IResult,
};

pub fn PropositionalLogic(cx: Scope) -> Element {
    let proof = use_ref(cx, Vec::<PropositionalLogic>::new);
    let input = use_state(cx, String::new);

    cx.render(rsx! {
        for line in &*proof.read() {
            div {
                dangerous_inner_html: "{line.to_string()}"
            }
        },
        div {
            input {
                value: "{input}",
                oninput: move |evt| input.set(evt.value.clone()),
            }
            button {
                onclick: move |_| {
                    let mut proof = proof.write();
                    let i = input.get();
                    let expr = PropositionalLogic::from(i.as_str());
                    proof.push(expr);
                    input.set(String::new());
                },
                "Add"
            }
            p {
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "¬");
                    },
                    "¬",
                }
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "∧");
                    },
                    "∧",
                }
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "∨");
                    },
                    "∨",
                }
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "→");
                    },
                    "→",
                }
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "↔");
                    },
                    "↔",
                }
                button {
                    onclick: move |_| {
                        let mut inp = input.get().clone();
                        inp.pop();
                        input.set(inp);
                    },
                    "Backspace",
                }
            }
            p {
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "P");
                    },
                    "P",
                }
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "Q");
                    },
                    "Q",
                }
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "R");
                    },
                    "R",
                }
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "S");
                    },
                    "S",
                }
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "T");
                    },
                    "T",
                }
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "(");
                    },
                    "(",
                }
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + ")");
                    },
                    ")",
                }
            }
        }
    })
}

#[derive(Debug, Clone, PartialEq)]
enum PropositionalLogic {
    Var(char),
    Not(Box<PropositionalLogic>),
    And(Box<PropositionalLogic>, Box<PropositionalLogic>),
    Or(Box<PropositionalLogic>, Box<PropositionalLogic>),
    Implies(Box<PropositionalLogic>, Box<PropositionalLogic>),
    Iff(Box<PropositionalLogic>, Box<PropositionalLogic>),
}

impl From<&str> for PropositionalLogic {
    fn from(input: &str) -> Self {
        parse_propositional_logic(input).unwrap().1
    }
}

impl fmt::Display for PropositionalLogic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PropositionalLogic::Var(var) => write!(f, "{}", var),
            PropositionalLogic::Not(expr) => write!(f, "¬({})", expr),
            PropositionalLogic::And(expr1, expr2) => write!(f, "({} ∧ {})", expr1, expr2),
            PropositionalLogic::Or(expr1, expr2) => write!(f, "({} ∨ {})", expr1, expr2),
            PropositionalLogic::Implies(expr1, expr2) => {
                write!(f, "({} → {})", expr1, expr2)
            }
            PropositionalLogic::Iff(expr1, expr2) => {
                write!(f, "({} ↔ {})", expr1, expr2)
            }
        }
    }
}

fn parse_propositional_logic(input: &str) -> IResult<&str, PropositionalLogic> {
    let (input, _) = multispace0(input)?;
    let (input, result) = parse_propositional_logic_or(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, result))
}

fn parse_propositional_logic_or(input: &str) -> IResult<&str, PropositionalLogic> {
    let (input, expr1) = parse_propositional_logic_and(input)?;
    let (input, operator_pairs) = many0(pair(tag("∨"), parse_propositional_logic_and))(input)?;
    Ok((
        input,
        operator_pairs
            .into_iter()
            .fold(expr1, parse_binary_operation),
    ))
}

fn parse_propositional_logic_and(input: &str) -> IResult<&str, PropositionalLogic> {
    let (input, expr1) = parse_propositional_logic_implies(input)?;
    let (input, operator_pairs) =
        many0(pair(tag("∧"), parse_propositional_logic_implies))(input)?;
    Ok((
        input,
        operator_pairs
            .into_iter()
            .fold(expr1, parse_binary_operation),
    ))
}

fn parse_propositional_logic_implies(input: &str) -> IResult<&str, PropositionalLogic> {
    let (input, expr1) = parse_propositional_logic_iff(input)?;
    let (input, operator_pairs) =
        many0(pair(tag("→"), parse_propositional_logic_iff))(input)?;
    Ok((
        input,
        operator_pairs
            .into_iter()
            .fold(expr1, parse_binary_operation),
    ))
}

fn parse_propositional_logic_iff(input: &str) -> IResult<&str, PropositionalLogic> {
    let (input, expr1) = parse_propositional_logic_not(input)?;
    let (input, operator_pairs) =
        many0(pair(tag("↔"), parse_propositional_logic_not))(input)?;
    Ok((
        input,
        operator_pairs
            .into_iter()
            .fold(expr1, parse_binary_operation),
    ))
}

fn parse_propositional_logic_not(input: &str) -> IResult<&str, PropositionalLogic> {
    let (input, result) = nom::branch::alt((
        parse_propositional_logic_atom,
        parse_propositional_logic_neg,
    ))(input)?;
    Ok((input, result))
}

fn parse_propositional_logic_neg(input: &str) -> IResult<&str, PropositionalLogic> {
    let (input, _) = tag("¬")(input)?;
    let (input, expr) = parse_propositional_logic_atom(input)?;
    Ok((input, PropositionalLogic::Not(Box::new(expr))))
}

fn parse_propositional_logic_atom(input: &str) -> IResult<&str, PropositionalLogic> {
    let (input, _) = multispace0(input)?;
    let (input, result) = nom::branch::alt((
        parse_propositional_logic_var,
        parse_propositional_logic_paren,
    ))(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, result))
}

fn parse_propositional_logic_var(input: &str) -> IResult<&str, PropositionalLogic> {
    let (input, var) = nom::character::complete::alpha1(input)?;
    Ok((input, PropositionalLogic::Var(var.chars().next().unwrap())))
}

fn parse_propositional_logic_paren(input: &str) -> IResult<&str, PropositionalLogic> {
    let (input, _) = tag("(")(input)?;
    let (input, expr) = parse_propositional_logic_or(input)?;
    let (input, _) = tag(")")(input)?;
    Ok((input, expr))
}

fn parse_binary_operation(
    expr1: PropositionalLogic,
    operator_pair: (&str, PropositionalLogic),
) -> PropositionalLogic {
    let (operator, expr2) = operator_pair;
    match operator {
        "∧" => PropositionalLogic::And(Box::new(expr1), Box::new(expr2)),
        "∨" => PropositionalLogic::Or(Box::new(expr1), Box::new(expr2)),
        "→" => PropositionalLogic::Implies(Box::new(expr1), Box::new(expr2)),
        "↔" => PropositionalLogic::Iff(Box::new(expr1), Box::new(expr2)),
        _ => panic!("Unknown operator: {}", operator),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_pl_examples() {
        let parsed1 = parse_propositional_logic("p").unwrap().1;
        assert_eq!(parsed1, PropositionalLogic::Var('p'));

        let parsed2 = parse_propositional_logic("¬ p").unwrap().1;
        assert_eq!(
            parsed2,
            PropositionalLogic::Not(Box::new(PropositionalLogic::Var('p')))
        );
        
        let parsed3 = parse_propositional_logic("p ∧ q").unwrap().1;
        assert_eq!(
            parsed3,
            PropositionalLogic::And(
                Box::new(PropositionalLogic::Var('p')),
                Box::new(PropositionalLogic::Var('q')),
            )
        );

        let parsed4 = parse_propositional_logic("p ∨ q").unwrap().1;
        assert_eq!(
            parsed4,
            PropositionalLogic::Or(
                Box::new(PropositionalLogic::Var('p')),
                Box::new(PropositionalLogic::Var('q')),
            )
        );

        let parsed5 = parse_propositional_logic("p → q").unwrap().1;
        assert_eq!(
            parsed5,
            PropositionalLogic::Implies(
                Box::new(PropositionalLogic::Var('p')),
                Box::new(PropositionalLogic::Var('q')),
            )
        );

        let parsed6 = parse_propositional_logic("p ↔ q").unwrap().1;
        assert_eq!(
            parsed6,
            PropositionalLogic::Iff(
                Box::new(PropositionalLogic::Var('p')),
                Box::new(PropositionalLogic::Var('q')),
            )
        );

        let parsed7 = parse_propositional_logic("p ∧ q ∨ r").unwrap().1;
        assert_eq!(
            parsed7,
            PropositionalLogic::Or(
                Box::new(PropositionalLogic::And(
                    Box::new(PropositionalLogic::Var('p')),
                    Box::new(PropositionalLogic::Var('q')),
                )),
                Box::new(PropositionalLogic::Var('r')),
            )
        );

        let parsed8 = parse_propositional_logic("p ∧ (q ∨ r)").unwrap().1;
        assert_eq!(
            parsed8,
            PropositionalLogic::And(
                Box::new(PropositionalLogic::Var('p')),
                Box::new(PropositionalLogic::Or(
                    Box::new(PropositionalLogic::Var('q')),
                    Box::new(PropositionalLogic::Var('r')),
                )),
            )
        );

        let parsed9 = parse_propositional_logic("(p ∧ q) ∨ r").unwrap().1;
        assert_eq!(
            parsed9,
            PropositionalLogic::Or(
                Box::new(PropositionalLogic::And(
                    Box::new(PropositionalLogic::Var('p')),
                    Box::new(PropositionalLogic::Var('q')),
                )),
                Box::new(PropositionalLogic::Var('r')),
            )
        );
    }
}
