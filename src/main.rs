use lr1gen::lr1::{grammar, n, rule, t};

fn main() {
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
    let grm = grammar::<&str, &str>("prog")
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
        + rule(
            "prog",
            [vec![n("letExp")], vec![n("prog"), t(";"), n("letExp")]],
        );
    let grm = grm.build();

    println!("{grm}")
}
