use core::fmt;

use dioxus::{prelude::*, html::details};
use nom::{
    branch::alt, bytes::complete::tag, character::complete::multispace0, combinator::map,
    multi::many0, sequence::pair, IResult,
};
use rand::prelude::*;

pub fn PropositionalLogic(cx: Scope) -> Element {
    let mut proof = use_ref(cx, generate_argument);
    let mut input = use_state(cx, String::new);
    let mut level = use_state(cx, || 0);
    let mut line_num = use_state(cx, || proof.read().len());
    let mut started = use_state(cx, || false);
    let mut finished = use_state(cx, || false);

    cx.render(rsx! {
        aside {
            details {
                summary {
                    "Help"
                }
                p {
                    "Enter a proposition in the input box and click Add to add it to the proof, if it is possible from the current propositions. If you are stuck, here are the possible next steps, excluding assumptions:"
                }
                for line in all_possible_next(&proof.read()) {
                    p {
                        "{line.to_string()}"
                    }
                }
            }
        }
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
                    let oline = PLLine::from(i.as_str());
                    if **started {
                        if let Some(nline) = all_possible_next(&proof).iter().find(|l| {
                                if let Some(ex) = &l.expr {
                                    if let Some(exp) = &oline.expr {
                                        if ex == exp {
                                            return true;
                                        }
                                    }
                                }
                                if let Some(ru) = &l.rule {
                                    if let Some(rul) = &oline.rule {
                                        if ru == rul && ru == &PLRule::Refute {
                                            return true;
                                        }
                                    }
                                }
                                false
                            }
                        ) {
                            proof.push(PLLine { line: Some(*line_num.get()), expr: oline.expr, rule: nline.rule.clone() });
                            input.set(String::new());
                            line_num += 1;
                            match nline.rule {
                                Some(PLRule::Assume) => level += 1,
                                Some(PLRule::Contradiction(_, _)) => level -= 1,
                                Some(PLRule::Refute) => level -= 1,
                                _ => {}
                            }
                            if *level.get() == 0 {
                                finished.set(true);
                            }
                        } else if oline.rule == Some(PLRule::Assume) {
                            proof.push(PLLine { line: Some(*line_num.get()), expr: oline.expr, rule: oline.rule });
                            input.set(String::new());
                            line_num += 1;
                            level += 1;
                        }
                    } else {
                        match oline.expr {
                            Some(PropositionalLogic::Not(box a)) => {
                                if let Some(conc) = proof.iter().find(|l| {
                                    l.line.is_none()
                                }) {
                                    if conc.expr == Some(a.clone()) && oline.rule == Some(PLRule::Assume) {
                                        proof.push(PLLine { line: Some(*line_num.get()), expr: Some(PropositionalLogic::Not(Box::new(a))), rule: Some(PLRule::Assume) });
                                        input.set(String::new());
                                        line_num += 1;
                                        level += 1;
                                        started.set(true);
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
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
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "Assume ");
                    },
                    "Assume",
                }
                button {
                    onclick: move |_| {
                        input.set(input.get().clone() + "Refute");
                    },
                    "Refute",
                }
            }
            button {
                onclick: move |_| {
                    let mut proof = proof.write();
                    *proof = generate_argument();
                    input.set(String::new());
                    line_num.set(proof.len());
                    level.set(0);
                    started.set(false);
                    finished.set(false);
                },
                "New Argument",
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

#[derive(Debug, Clone, PartialEq)]
enum PLRule {
    And(usize),
    Nif(usize),
    Nor(usize),
    DN(usize),
    Iff(usize),
    Niff(usize),
    CS(usize, usize),
    DS(usize, usize),
    MP(usize, usize),
    MT(usize, usize),
    Contradiction(usize, usize),
    Assume,
    Refute,
}

#[derive(Debug, Clone, PartialEq)]
struct PLLine {
    line: Option<usize>,
    expr: Option<PropositionalLogic>,
    rule: Option<PLRule>,
}

fn generate_expr(level: u8) -> PropositionalLogic {
    let choice: u32 = random::<u32>() % 5;
    if level == 0 {
        match choice {
            0 => PropositionalLogic::Var('P'),
            1 => PropositionalLogic::Var('Q'),
            2 => PropositionalLogic::Var('R'),
            3 => PropositionalLogic::Var('S'),
            4 => PropositionalLogic::Var('T'),
            _ => unreachable!(),
        }
    } else {
        match choice {
            0 => PropositionalLogic::And(
                Box::new(generate_expr(level - 1)),
                Box::new(generate_expr(level - 1)),
            ),
            1 => PropositionalLogic::Iff(
                Box::new(generate_expr(level - 1)),
                Box::new(generate_expr(level - 1)),
            ),
            2 => PropositionalLogic::Implies(
                Box::new(generate_expr(level - 1)),
                Box::new(generate_expr(level - 1)),
            ),
            3 => PropositionalLogic::Not(Box::new(generate_expr(level - 1))),
            4 => PropositionalLogic::Or(
                Box::new(generate_expr(level - 1)),
                Box::new(generate_expr(level - 1)),
            ),
            _ => unreachable!(),
        }
    }
}

fn generate_argument() -> Vec<PLLine> {
    let num: usize = random::<usize>() % 4;
    let mut lines = Vec::new();

    for i in 0..num {
        lines.push(PLLine {
            line: Some(i + 1),
            expr: Some(generate_expr(random::<u8>() % 4)),
            rule: None,
        });
    }

    lines.push(PLLine {
        line: None,
        expr: Some(generate_expr(random::<u8>() % 4)),
        rule: None,
    });

    lines
}

// return every possible next line of the proof
fn all_possible_next(input: &Vec<PLLine>) -> Vec<PLLine> {
    let mut possiblities = Vec::new();
    let mut proof = input.clone();
    let mut useable = Vec::new();

    let mut assumption_level = 0;
    for line in proof.iter().rev() {
        if assumption_level <= 0 {
            useable.push(true);
        } else {
            useable.push(false);
        }

        if let Some(PLRule::Contradiction(_, _)) = &line.rule {
            assumption_level += 1;
        } else if let Some(PLRule::Assume) = &line.rule {
            assumption_level -= 1;
        }
    }

    useable.reverse();

    proof = proof
        .into_iter()
        .zip(useable.into_iter())
        .filter(|(_, useable)| *useable)
        .map(|(line, _)| line)
        .collect();

    // if the last line is a refutation, then we're done
    if let Some(line) = proof.last() {
        if let Some(PLRule::Refute) = &line.rule {
            return possiblities;
        }
    }

    for line in &proof {
        if line.line.is_none() {
            continue;
        }
        if let Some(expr) = &line.expr {
            match expr {
                PropositionalLogic::And(a, b) => {
                    possiblities.push(PLLine {
                        line: None,
                        expr: Some(*a.clone()),
                        rule: Some(PLRule::And(line.line.unwrap())),
                    });
                    possiblities.push(PLLine {
                        line: None,
                        expr: Some(*b.clone()),
                        rule: Some(PLRule::And(line.line.unwrap())),
                    });
                }
                PropositionalLogic::Not(box PropositionalLogic::Implies(a, b)) => {
                    possiblities.push(PLLine {
                        line: None,
                        expr: Some(*a.clone()),
                        rule: Some(PLRule::Nif(line.line.unwrap())),
                    });
                    possiblities.push(PLLine {
                        line: None,
                        expr: Some(PropositionalLogic::Not(b.clone())),
                        rule: Some(PLRule::Nif(line.line.unwrap())),
                    });
                }
                PropositionalLogic::Not(box PropositionalLogic::Or(a, b)) => {
                    possiblities.push(PLLine {
                        line: None,
                        expr: Some(PropositionalLogic::Not(Box::new(*a.clone()))),
                        rule: Some(PLRule::Nor(line.line.unwrap())),
                    });
                    possiblities.push(PLLine {
                        line: None,
                        expr: Some(PropositionalLogic::Not(Box::new(*b.clone()))),
                        rule: Some(PLRule::Nor(line.line.unwrap())),
                    });
                }
                PropositionalLogic::Not(box PropositionalLogic::Not(a)) => {
                    possiblities.push(PLLine {
                        line: None,
                        expr: Some(*a.clone()),
                        rule: Some(PLRule::DN(line.line.unwrap())),
                    });
                }
                PropositionalLogic::Iff(a, b) => {
                    possiblities.push(PLLine {
                        line: None,
                        expr: Some(PropositionalLogic::Implies(
                            Box::new(*a.clone()),
                            Box::new(*b.clone()),
                        )),
                        rule: Some(PLRule::Iff(line.line.unwrap())),
                    });
                    possiblities.push(PLLine {
                        line: None,
                        expr: Some(PropositionalLogic::Implies(
                            Box::new(*b.clone()),
                            Box::new(*a.clone()),
                        )),
                        rule: Some(PLRule::Iff(line.line.unwrap())),
                    });
                }
                PropositionalLogic::Not(box PropositionalLogic::Iff(a, b)) => {
                    possiblities.push(PLLine {
                        line: None,
                        expr: Some(PropositionalLogic::Or(
                            Box::new(*a.clone()),
                            Box::new(*b.clone()),
                        )),
                        rule: Some(PLRule::Niff(line.line.unwrap())),
                    });
                    possiblities.push(PLLine {
                        line: None,
                        expr: Some(PropositionalLogic::Not(Box::new(PropositionalLogic::And(
                            Box::new(*a.clone()),
                            Box::new(*b.clone()),
                        )))),
                        rule: Some(PLRule::Niff(line.line.unwrap())),
                    });
                }
                PropositionalLogic::Not(box PropositionalLogic::And(a, b)) => {
                    for i in 0..proof.len() {
                        let a_clone = *a.clone();
                        let b_clone = *b.clone();
                        if proof[i].expr == Some(a_clone) {
                            possiblities.push(PLLine {
                                line: None,
                                expr: Some(PropositionalLogic::Not(Box::new(b_clone))),
                                rule: Some(PLRule::CS(line.line.unwrap(), i)),
                            });
                        }
                        let a_clone = *a.clone();
                        let b_clone = *b.clone();
                        if proof[i].expr == Some(b_clone) {
                            possiblities.push(PLLine {
                                line: None,
                                expr: Some(PropositionalLogic::Not(Box::new(a_clone))),
                                rule: Some(PLRule::CS(line.line.unwrap(), i)),
                            });
                        }
                    }
                }
                PropositionalLogic::Or(a, b) => {
                    for i in 0..proof.len() {
                        let a_clone = *a.clone();
                        let b_clone = *b.clone();
                        if proof[i].expr == Some(PropositionalLogic::Not(Box::new(a_clone))) {
                            possiblities.push(PLLine {
                                line: None,
                                expr: Some(b_clone),
                                rule: Some(PLRule::DS(line.line.unwrap(), i)),
                            });
                        }
                        let a_clone = *a.clone();
                        let b_clone = *b.clone();
                        if proof[i].expr == Some(PropositionalLogic::Not(Box::new(b_clone))) {
                            possiblities.push(PLLine {
                                line: None,
                                expr: Some(a_clone),
                                rule: Some(PLRule::DS(line.line.unwrap(), i)),
                            });
                        }
                    }
                }
                PropositionalLogic::Implies(a, b) => {
                    for i in 0..proof.len() {
                        let a_clone = *a.clone();
                        let b_clone = *b.clone();
                        if proof[i].expr == Some(a_clone) {
                            possiblities.push(PLLine {
                                line: None,
                                expr: Some(b_clone),
                                rule: Some(PLRule::MP(line.line.unwrap(), i)),
                            });
                        }
                        let a_clone = *a.clone();
                        let b_clone = *b.clone();
                        if proof[i].expr == Some(PropositionalLogic::Not(Box::new(b_clone))) {
                            possiblities.push(PLLine {
                                line: None,
                                expr: Some(PropositionalLogic::Not(Box::new(a_clone))),
                                rule: Some(PLRule::MT(line.line.unwrap(), i)),
                            });
                        }
                    }
                }
                _ => {}
            }
            for sli in &proof {
                if sli.expr == Some(PropositionalLogic::Not(Box::new(expr.clone())))
                    && sli.line.is_some()
                {
                    let expr = proof
                        .iter()
                        .rev()
                        .find(|x| matches!(x.rule, Some(PLRule::Assume)))
                        .unwrap()
                        .expr
                        .clone();
                    possiblities.push(PLLine {
                        line: None,
                        expr: match expr {
                            Some(PropositionalLogic::Not(a)) => Some(*a),
                            _ => None,
                        },
                        rule: Some(PLRule::Contradiction(sli.line.unwrap(), line.line.unwrap())),
                    });
                }
            }
        }
    }

    possiblities.retain(|x| {
        if proof.iter().any(|y| y.expr == x.expr && y.line.is_some() && !matches!(x.rule, Some(PLRule::Contradiction(_, _)))) {
            return false;
        }
        true
    });

    if possiblities.is_empty() {
        possiblities.push(PLLine {
            line: None,
            expr: None,
            rule: Some(PLRule::Refute),
        });
    }

    possiblities
}

impl fmt::Display for PLLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(line) = &self.line {
            write!(f, "{}. ", line)?;
        } else {
            write!(f, "  [∴")?;
        }
        if let Some(expr) = &self.expr {
            write!(f, "{}", expr)?;
        }
        if let Some(rule) = &self.rule {
            write!(f, " ({})", rule)?;
        }
        write!(f, "")
    }
}

impl From<&str> for PLLine {
    fn from(input: &str) -> Self {
        let (_, line) = parse_line(input).unwrap();
        line
    }
}

// this just recieves the expr, but it might be an assumption or a refutation
fn parse_line(input: &str) -> IResult<&str, PLLine> {
    alt((
        parse_assumption,
        parse_refutation,
        map(parse_propositional_logic, |expr| PLLine {
            line: None,
            expr: Some(expr),
            rule: None,
        }),
    ))(input)
}

fn parse_assumption(input: &str) -> IResult<&str, PLLine> {
    let (input, _) = tag("Assume")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, expr) = parse_propositional_logic(input)?;
    Ok((
        input,
        PLLine {
            line: None,
            expr: Some(expr),
            rule: Some(PLRule::Assume),
        },
    ))
}

fn parse_refutation(input: &str) -> IResult<&str, PLLine> {
    let (input, _) = tag("Refute")(input)?;
    let (input, _) = multispace0(input)?;
    Ok((
        input,
        PLLine {
            line: None,
            expr: None,
            rule: Some(PLRule::Refute),
        },
    ))
}

impl fmt::Display for PLRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PLRule::And(i) => write!(f, "And, {}", i),
            PLRule::Nif(i) => write!(f, "Nif, {}", i),
            PLRule::Nor(i) => write!(f, "Nor, {}", i),
            PLRule::DN(i) => write!(f, "DN, {}", i),
            PLRule::Iff(i) => write!(f, "Iff, {}", i),
            PLRule::Niff(i) => write!(f, "Niff, {}", i),
            PLRule::CS(i, j) => write!(f, "CS, {}, {}", i, j),
            PLRule::DS(i, j) => write!(f, "DS, {}, {}", i, j),
            PLRule::MP(i, j) => write!(f, "MP, {}, {}", i, j),
            PLRule::MT(i, j) => write!(f, "MT, {}, {}", i, j),
            PLRule::Contradiction(i, j) => write!(f, "Contradiction from {}, {}", i, j),
            PLRule::Assume => write!(f, "Assume"),
            PLRule::Refute => write!(f, "Refute"),
        }
    }
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
            PropositionalLogic::Not(expr) => match **expr {
                PropositionalLogic::Var(_) => write!(f, "¬{}", expr),
                _ => write!(f, "¬({})", expr),
            },
            PropositionalLogic::And(expr1, expr2) => match **expr1 {
                PropositionalLogic::Var(_) => match **expr2 {
                    PropositionalLogic::Var(_) => write!(f, "{} ∧ {}", expr1, expr2),
                    _ => write!(f, "{} ∧ ({})", expr1, expr2),
                },
                _ => match **expr2 {
                    PropositionalLogic::Var(_) => write!(f, "({}) ∧ {}", expr1, expr2),
                    _ => write!(f, "({}) ∧ ({})", expr1, expr2),
                },
            },
            PropositionalLogic::Or(expr1, expr2) => match **expr1 {
                PropositionalLogic::Var(_) => match **expr2 {
                    PropositionalLogic::Var(_) => write!(f, "{} ∨ {}", expr1, expr2),
                    _ => write!(f, "{} ∨ ({})", expr1, expr2),
                },
                _ => match **expr2 {
                    PropositionalLogic::Var(_) => write!(f, "({}) ∨ {}", expr1, expr2),
                    _ => write!(f, "({}) ∨ ({})", expr1, expr2),
                },
            },
            PropositionalLogic::Implies(expr1, expr2) => match **expr1 {
                PropositionalLogic::Var(_) => match **expr2 {
                    PropositionalLogic::Var(_) => write!(f, "{} → {}", expr1, expr2),
                    _ => write!(f, "{} → ({})", expr1, expr2),
                },
                _ => match **expr2 {
                    PropositionalLogic::Var(_) => write!(f, "({}) → {}", expr1, expr2),
                    _ => write!(f, "({}) → ({})", expr1, expr2),
                },
            },
            PropositionalLogic::Iff(expr1, expr2) => match **expr1 {
                PropositionalLogic::Var(_) => match **expr2 {
                    PropositionalLogic::Var(_) => write!(f, "{} ↔ {}", expr1, expr2),
                    _ => write!(f, "{} ↔ ({})", expr1, expr2),
                },
                _ => match **expr2 {
                    PropositionalLogic::Var(_) => write!(f, "({}) ↔ {}", expr1, expr2),
                    _ => write!(f, "({}) ↔ ({})", expr1, expr2),
                },
            },
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
    let (input, operator_pairs) = many0(pair(tag("→"), parse_propositional_logic_iff))(input)?;
    Ok((
        input,
        operator_pairs
            .into_iter()
            .fold(expr1, parse_binary_operation),
    ))
}

fn parse_propositional_logic_iff(input: &str) -> IResult<&str, PropositionalLogic> {
    let (input, expr1) = parse_propositional_logic_not(input)?;
    let (input, operator_pairs) = many0(pair(tag("↔"), parse_propositional_logic_not))(input)?;
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

    #[test]
    fn parse_pll() {
        let parsed = parse_line("Refute").unwrap().1;
        assert_eq!(
            parsed,
            PLLine {
                line: None,
                expr: None,
                rule: Some(PLRule::Refute),
            }
        );
    }
}
