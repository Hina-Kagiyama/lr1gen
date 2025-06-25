use lr1gen::lr1::{Table, grammar, n, rule, t};

fn main() {
    let _test1 = || {
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
        grm.build().unwrap()
    };

    let _test2 = || {
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

        grm.build().unwrap()
    };

    let grm = _test1();
    // let grm = _test2();
    println!("/*\n{grm}*/\n");
    let tbl = Table::make(&grm);

    println!("{}", grm.dump_terminals());
    println!("{}", grm.dump_non_terminals());
    println!("{}", tbl.c_code());
}
