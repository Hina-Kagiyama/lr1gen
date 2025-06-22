use std::{iter::repeat_with, ops::ControlFlow};

use lr1gen::lr1::{Env, Table, grammar, interpret, n, rule, t};

#[allow(dead_code)]
fn test1() {
    // primExp:
    //   ( exp )
    //   id
    // appExp:
    //   primExp
    //   appExp primExp
    // absExp:
    //   appExp
    //   id -> absExp
    // letExp:
    //   absExp
    //   let id = absExp ; letExp
    // prog:
    //   letExp
    //   prog ; letExp
    let grm = grammar::<&str, &str>("prog", "$")
        + rule("primExp", [vec![t("("), n("exp"), t(")")], vec![t("id")]])
        + rule(
            "appExp",
            [vec![n("primExp")], vec![n("appExp"), n("primExp")]],
        )
        + rule(
            "absExp",
            [vec![n("appExp")], vec![t("id"), t("->"), n("absExp")]],
        )
        + rule(
            "letExp",
            [
                vec![n("absExp")],
                vec![t("let"), t("id"), t("="), n("absExp"), t(";"), n("letExp")],
            ],
        )
        + rule("exp", [vec![n("letExp")]])
        + rule(
            "prog",
            [
                vec![n("letExp"), t("$")],
                vec![n("prog"), t(";"), n("letExp"), t("$")],
            ],
        );
    let grm = grm.build().unwrap();

    println!("{grm}");
    println!("{}", Table::make(&grm));
}

#[allow(dead_code)]
fn test2() {
    // expr   ::= addExp
    // addExp ::= mulExp
    //          | addExp + mulExp
    //          | addExp - mulExp
    // mulExp ::= primExp
    //          | mulExp * primExp
    //          | mulExp / primExp
    // primExp ::= ( expr )
    //           | num      // literal “num” token

    let grm = grammar::<&str, &str>("prog", "$")
        + rule("primExp", [vec![t("("), n("expr"), t(")")], vec![t("num")]])
        + rule(
            "mulExp",
            [
                vec![n("primExp")],
                vec![n("mulExp"), t("*"), n("primExp")],
                vec![n("mulExp"), t("/"), n("primExp")],
            ],
        )
        + rule(
            "addExp",
            [
                vec![n("mulExp")],
                vec![n("addExp"), t("+"), n("mulExp")],
                vec![n("addExp"), t("-"), n("mulExp")],
            ],
        )
        + rule("expr", [vec![n("addExp")]])
        + rule("prog", [vec![n("expr"), t("$")]]);

    let grm = grm.build().unwrap();
    let tbl = Table::make(&grm);

    println!("{grm}");
    println!("{tbl}");

    match repeat_with(|| ()).try_fold(
        Env::new(["num", "*", "num", "$"].iter().peekable()),
        |env, _| {
            let env = interpret(env, &tbl);
            println!("{env:#?}");
            env
        },
    ) {
        ControlFlow::Continue(env) => println!("Unfinished parse, env: {env:#?}"),
        ControlFlow::Break(Ok(ans)) => println!("Finished parse, ans: {ans:#?}"),
        ControlFlow::Break(Err(env)) => println!("Error in parsing, env: {env:#?}"),
    }
}

fn main() {
    // test1()
    test2();
}
