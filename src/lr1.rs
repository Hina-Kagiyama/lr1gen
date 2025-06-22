use itertools::merge;
use std::{
    collections::{HashMap, hash_map::Entry},
    fmt::Display,
    hash::Hash,
    iter::{Peekable, repeat_with},
    ops::{Add, ControlFlow},
    usize,
};

use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol<Terminal, NonTerminal> {
    Terminal(Terminal),
    NonTerminal(NonTerminal),
    Epsilon,
}

impl<Terminal: Display, NonTerminal: Display> Display for Symbol<Terminal, NonTerminal> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Terminal(x) => write!(f, "\"{x}\""),
            Symbol::NonTerminal(x) => write!(f, "{x}"),
            Symbol::Epsilon => write!(f, "<<eps>>"),
        }
    }
}

pub type Rhs<Terminal, NonTerminal> = Vec<Symbol<Terminal, NonTerminal>>;

pub type TerminalId = usize;
pub type NonTerminalId = usize;

#[derive(Debug)]
pub struct Grammar<Terminal, NonTerminal> {
    productions: HashMap<NonTerminalId, Vec<Rhs<TerminalId, NonTerminalId>>>,
    terminals: Vec<Terminal>,
    nonterminals: Vec<NonTerminal>,
}

impl<Terminal, NonTerminal: Clone> Grammar<Terminal, NonTerminal> {
    pub fn check_undefined_nonterminals(&self) -> Vec<NonTerminal> {
        // mark-table: used[i] == true  ⇔  nonterminal i occurs in some production
        let mut defined = vec![false; self.nonterminals.len()];

        // walk every RHS once
        self.productions
            .keys()
            .for_each(|&ntid| defined[ntid] = true);

        defined
            .iter()
            .enumerate()
            .filter_map(|(ntid, def)| (!def).then_some(self.nonterminals[ntid].clone()))
            .collect_vec()
    }

    pub fn check_unterminated_start_rule(&self) -> bool {
        self.productions[&0]
            .iter()
            .map(|x| match x.last() {
                Some(Symbol::Terminal(0)) => false,
                _ => true,
            })
            .fold(false, |a, b| a || b)
    }
}

impl<Terminal: Display, NonTerminal: Display> Display for Grammar<Terminal, NonTerminal> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Terminals: {}",
            self.terminals
                .iter()
                .map(ToString::to_string)
                .collect_vec()
                .join(" ")
        )?;
        writeln!(
            f,
            "NonTerminals: {}",
            self.nonterminals
                .iter()
                .map(ToString::to_string)
                .collect_vec()
                .join(" ")
        )?;
        writeln!(f, "Start Symbol: {}", self.nonterminals[0].to_string())?;
        self.productions.iter().try_for_each(|(&lhs, rhs)| {
            writeln!(
                f,
                "{}:\n\t{}",
                self.nonterminals[lhs],
                rhs.iter()
                    .map(|x| x
                        .iter()
                        .map(|&x| match x {
                            Symbol::Terminal(id) => format!("{}", self.terminals[id]),
                            Symbol::NonTerminal(id) => format!("{}", self.nonterminals[id]),
                            x => x.to_string(),
                        })
                        .collect_vec()
                        .join(" "))
                    .collect_vec()
                    .join("\n\t")
            )
        })
    }
}

pub struct GrammarBuilder<Terminal, NonTerminal> {
    grammar: Grammar<Terminal, NonTerminal>,
}

pub fn grammar<Terminal, NonTerminal>(
    start: NonTerminal,
    end: Terminal,
) -> GrammarBuilder<Terminal, NonTerminal> {
    GrammarBuilder {
        grammar: Grammar {
            productions: HashMap::new(),
            terminals: vec![end],
            nonterminals: vec![start],
        },
    }
}

impl<Terminal: Eq + Clone, NonTerminal: Eq + Hash + Clone> Add<RuleBuilder<Terminal, NonTerminal>>
    for GrammarBuilder<Terminal, NonTerminal>
{
    type Output = GrammarBuilder<Terminal, NonTerminal>;

    fn add(self, rhs: RuleBuilder<Terminal, NonTerminal>) -> Self::Output {
        let mut ans = self;
        let lhs_id = if let Some(id) = ans
            .grammar
            .nonterminals
            .iter()
            .position(|x| x.clone() == rhs.lhs)
        {
            id
        } else {
            let id = ans.grammar.nonterminals.len();
            ans.grammar.nonterminals.push(rhs.lhs);
            id
        };
        let rhss_idss = rhs
            .rhss
            .into_iter()
            .map(|rhs| {
                rhs.iter()
                    .map(|x| match x {
                        Symbol::Terminal(x) => {
                            if let Some(id) = ans.grammar.terminals.iter().position(|y| x == y) {
                                Symbol::Terminal(id)
                            } else {
                                let id = ans.grammar.terminals.len();
                                ans.grammar.terminals.push(x.clone());
                                Symbol::Terminal(id)
                            }
                        }
                        Symbol::NonTerminal(x) => {
                            if let Some(id) = ans.grammar.nonterminals.iter().position(|y| x == y) {
                                Symbol::NonTerminal(id)
                            } else {
                                let id = ans.grammar.nonterminals.len();
                                ans.grammar.nonterminals.push(x.clone());
                                Symbol::NonTerminal(id)
                            }
                        }
                        Symbol::Epsilon => Symbol::Epsilon,
                    })
                    .collect_vec()
            })
            .collect_vec();
        ans.grammar.productions.insert(lhs_id, rhss_idss);
        ans
    }
}

#[derive(Debug, Clone)]
pub enum GrammarCheckFailed<NonTerminal> {
    UnterminatedStartRule,
    UndefinedNonterminals(Vec<NonTerminal>),
}

impl<Terminal, NonTerminal: Clone + Eq + Hash> GrammarBuilder<Terminal, NonTerminal> {
    pub fn build(self) -> Result<Grammar<Terminal, NonTerminal>, GrammarCheckFailed<NonTerminal>> {
        let undefined_nt = self.grammar.check_undefined_nonterminals();
        if undefined_nt.len() != 0 {
            Err(GrammarCheckFailed::UndefinedNonterminals(undefined_nt))
        } else if self.grammar.check_unterminated_start_rule() {
            Err(GrammarCheckFailed::UnterminatedStartRule)
        } else {
            Ok(self.grammar)
        }
    }
}

pub struct RuleBuilder<Terminal, NonTerminal> {
    lhs: NonTerminal,
    rhss: Vec<Vec<Symbol<Terminal, NonTerminal>>>,
}

pub fn rule<Terminal, NonTerminal, const N: usize, T: Into<NonTerminal>>(
    lhs: T,
    rhss: [Vec<Symbol<Terminal, NonTerminal>>; N],
) -> RuleBuilder<Terminal, NonTerminal> {
    RuleBuilder {
        lhs: lhs.into(),
        rhss: rhss.into_iter().collect_vec(),
    }
}

pub fn t<Terminal, NonTerminal, T: Into<Terminal>>(t: T) -> Symbol<Terminal, NonTerminal> {
    Symbol::Terminal(t.into())
}

pub fn n<Terminal, NonTerminal, N: Into<NonTerminal>>(n: N) -> Symbol<Terminal, NonTerminal> {
    Symbol::NonTerminal(n.into())
}

pub type RhsId = usize;
pub type ProductionId = (NonTerminalId, RhsId);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Item {
    production: ProductionId,
    dot_position: usize,
    lookhead: TerminalId,
}

pub type StateId = usize;

pub enum Action {
    Shift(StateId),
    Reduce(ProductionId),
    Error,
}

pub struct State {
    items: Vec<Item>,
    action: HashMap<TerminalId, Action>,
    goto: HashMap<NonTerminalId, StateId>,
}

pub struct Table<'grammar, Terminal, NonTerminal> {
    grammar: &'grammar Grammar<Terminal, NonTerminal>,
    // start state at index 0
    states: Vec<State>,
}

#[derive(Debug, Clone)]
pub enum AbSyn<'input, Terminal> {
    Datum(&'input Terminal),
    List(NonTerminalId, Vec<AbSyn<'input, Terminal>>),
}

#[derive(Debug, Clone)]
pub struct Env<'input, Terminal, I>
where
    I: Iterator<Item = &'input Terminal>,
{
    states: Vec<StateId>,
    peekable_terminal_iter: Peekable<I>,
    value_stack: Vec<AbSyn<'input, Terminal>>,
}

impl<'input, Terminal, I> Env<'input, Terminal, I>
where
    I: Iterator<Item = &'input Terminal>,
{
    pub fn new(peekable_terminal_iter: Peekable<I>) -> Self {
        Self {
            states: vec![0],
            peekable_terminal_iter,
            value_stack: vec![],
        }
    }
}

pub fn interpret<
    'input,
    'grammar,
    Terminal: Eq,
    NonTerminal: Eq,
    I: Iterator<Item = &'input Terminal>,
>(
    mut env: Env<'input, Terminal, I>,
    table: &'grammar Table<'grammar, Terminal, NonTerminal>,
) -> ControlFlow<Result<AbSyn<'input, Terminal>, Env<'input, Terminal, I>>, Env<'input, Terminal, I>>
{
    let &cur_state_id = env.states.last().unwrap();
    let lookahead_id = env
        .peekable_terminal_iter
        .peek()
        .map(|&x| {
            table
                .grammar
                .terminals
                .iter()
                .position(|t| x == t)
                .expect("Unknown Terminal")
        })
        .unwrap_or(0);
    let action = table.states[cur_state_id]
        .action
        .get(&lookahead_id)
        .unwrap_or(&Action::Error);

    match *action {
        Action::Shift(next_state_id) => {
            let tok_ref: &'input Terminal = env
                .peekable_terminal_iter
                .next()
                .expect("No token to shift");
            env.value_stack.push(AbSyn::Datum(tok_ref));
            env.states.push(next_state_id);
            ControlFlow::Continue(env)
        }
        Action::Reduce((lhs_id, rhs_id)) => {
            let rhs = &table.grammar.productions[&lhs_id][rhs_id];
            let k = rhs.len();
            let children = repeat_with(|| env.value_stack.pop().expect("stack balance error"))
                .take(k)
                .collect_vec()
                .into_iter()
                .rev()
                .collect_vec();

            let lhs = &table.grammar.nonterminals[lhs_id];
            let lhs_id = table
                .grammar
                .nonterminals
                .iter()
                .position(|x| x == lhs)
                .expect("Bad NonTerminal");
            env.value_stack.push(AbSyn::List(lhs_id, children));

            for _ in 0..k {
                env.states.pop();
            }
            if let Some(&ready_go) = env.states.last() {
                env.states.push(table.states[ready_go].goto[&lhs_id]);
                ControlFlow::Continue(env)
            } else {
                ControlFlow::Break(Ok(env.value_stack.pop().expect("Stack balance error")))
            }
        }
        Action::Error => ControlFlow::Break(Err(env)),
    }
}

impl<'grammar, Terminal, NonTerminal> Display for Table<'grammar, Terminal, NonTerminal>
where
    Terminal: Display,
    NonTerminal: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //------------------------------------------------------------------
        // helpers
        //------------------------------------------------------------------
        let sym_to_string = |sym: &Symbol<TerminalId, NonTerminalId>| -> String {
            match *sym {
                Symbol::Terminal(t) => format!("{}", self.grammar.terminals[t]),
                Symbol::NonTerminal(nt) => format!("{}", self.grammar.nonterminals[nt]),
                Symbol::Epsilon => "ε".into(),
            }
        };

        let item_to_string = |item: &Item| -> String {
            let (lhs, rhs_id) = item.production;
            let rhs = &self.grammar.productions[&lhs][rhs_id];
            let lhs_str = format!("{}", self.grammar.nonterminals[lhs]);

            // RHS with the • at `dot_position`
            let mut rhs_repr: Vec<String> = rhs
                .iter()
                .enumerate()
                .flat_map(|(i, sym)| {
                    if i == item.dot_position {
                        vec!["•".into(), sym_to_string(sym)]
                    } else {
                        vec![sym_to_string(sym)]
                    }
                })
                .collect();

            // dot at the very end?
            if item.dot_position == rhs.len() {
                rhs_repr.push("•".into());
            }

            format!(
                "{} → {} , {}",
                lhs_str,
                rhs_repr.join(" "),
                self.grammar.terminals[item.lookhead] // look-ahead
            )
        };

        let action_to_string = |act: &Action| -> String {
            match *act {
                Action::Shift(sid) => format!("Shift({})", sid),
                Action::Reduce((lhs, rid)) => format!(
                    "Reduce({} → {})",
                    self.grammar.nonterminals[lhs],
                    self.grammar.productions[&lhs][rid]
                        .iter()
                        .map(&sym_to_string)
                        .join(" ")
                ),
                Action::Error => "Error".into(),
            }
        };
        //------------------------------------------------------------------

        writeln!(f, "LR(1) Table — {} states\n", self.states.len())?;

        for (sid, state) in self.states.iter().enumerate() {
            writeln!(f, "State {}:", sid)?;

            // items
            if f.alternate() {
                for item in &state.items {
                    writeln!(f, "    {}", item_to_string(item))?;
                }
            }

            // ACTIONS
            if !state.action.is_empty() {
                writeln!(f, "  Actions:")?;
                let mut acts: Vec<_> = state.action.iter().collect();
                acts.sort_by_key(|(tid, _)| *tid);
                for (&tid, act) in acts {
                    writeln!(
                        f,
                        "    {:>10}  →  {}",
                        self.grammar.terminals[tid],
                        action_to_string(act)
                    )?;
                }
            }

            // GOTOS
            if !state.goto.is_empty() {
                writeln!(f, "  Gotos:")?;
                let mut gts: Vec<_> = state.goto.iter().collect();
                gts.sort_by_key(|(ntid, _)| *ntid);
                for (&ntid, &dst) in gts {
                    writeln!(f, "    {:>10}  →  {}", self.grammar.nonterminals[ntid], dst)?;
                }
            }

            writeln!(f)?; // blank line between states
        }
        Ok(())
    }
}

impl<'grammar, Terminal, NonTerminal> Table<'grammar, Terminal, NonTerminal> {
    pub fn make(grammar: &'grammar Grammar<Terminal, NonTerminal>) -> Self {
        let mut states = Vec::new();

        // ---- helper aliases & imports ------------------------------------
        use itertools::{Itertools, merge};
        use std::collections::{HashMap, HashSet};

        // ---- FIRST / NULLABLE --------------------------------------------
        let first_sets = FirstSets::make(grammar);

        /* FIRST(β a) where a is the lookahead ---------------------------- */
        // FIRST(β · a)
        let first_beta_a =
            |beta: &[Symbol<TerminalId, NonTerminalId>], a: TerminalId| -> Vec<TerminalId> {
                let mut out = Vec::<TerminalId>::new();
                let mut still_nullable = true;

                for sym in beta {
                    if !still_nullable {
                        break; // prefix no longer nullable
                    }

                    match *sym {
                        Symbol::Terminal(t) => {
                            out.push(t);
                            still_nullable = false; // terminal blocks ε
                        }
                        Symbol::NonTerminal(_nt) => {
                            // union(out, FIRST(B))
                            out = merge(out.into_iter(), first_sets.first(sym).into_iter())
                                .dedup()
                                .collect();
                            still_nullable = first_sets.nullable(sym);
                        }
                        Symbol::Epsilon => { /* keep scanning */ }
                    }
                }

                if still_nullable {
                    out.push(a); // ε ⇒ add the look-ahead
                }
                out.sort_unstable();
                out.dedup();
                out
            };

        // ---- CLOSURE(I) ---------------------------------------------------
        let closure = |mut i: HashSet<Item>| -> HashSet<Item> {
            let mut changed = true;
            while changed {
                changed = false;
                for it in i.clone() {
                    // A → α ·B β , a
                    let (lhs, rhs_id) = it.production;
                    let rhs = &grammar.productions[&lhs][rhs_id];
                    if it.dot_position >= rhs.len() {
                        continue;
                    }
                    if let Symbol::NonTerminal(b) = rhs[it.dot_position] {
                        let beta = &rhs[it.dot_position + 1..];
                        let looks = first_beta_a(beta, it.lookhead);
                        for (k, _gamma) in grammar.productions[&b].iter().enumerate() {
                            looks.iter().for_each(|&t| {
                                let itm = Item {
                                    production: (b, k),
                                    dot_position: 0,
                                    lookhead: t,
                                };
                                if i.insert(itm) {
                                    changed = true;
                                }
                            });
                        }
                    }
                }
            }
            i
        };

        // ---- GOTO(I, X) ---------------------------------------------------
        let goto = |i: &HashSet<Item>, x: &Symbol<TerminalId, NonTerminalId>| -> HashSet<Item> {
            closure(
                i.iter()
                    .filter_map(|it| {
                        let (lhs, rhs_id) = it.production;
                        let rhs = &grammar.productions[&lhs][rhs_id];
                        (it.dot_position < rhs.len() && &rhs[it.dot_position] == x).then(|| Item {
                            production: it.production,
                            dot_position: it.dot_position + 1,
                            lookhead: it.lookhead,
                        })
                    })
                    .collect(),
            )
        };

        // ---- build the canonical LR(1) collection of sets ----------------
        let start_nt: NonTerminalId = 0; // assume the first NT is the start
        let eof: TerminalId = grammar.terminals.len() - 1; // assume last terminal is $

        let i0 = closure(HashSet::from([Item {
            production: (start_nt, 0),
            dot_position: 0,
            lookhead: eof,
        }]));

        /* state registry so we only create each set once ----------------- */
        fn canon(set: &HashSet<Item>) -> Vec<Item> {
            let mut v: Vec<_> = set.iter().cloned().collect();
            // A stable total ordering: (A, i, dot, lookahead)
            v.sort_by_key(|it| {
                (
                    it.production.0,
                    it.production.1,
                    it.dot_position,
                    it.lookhead,
                )
            });
            v
        }
        let mut set2id: HashMap<Vec<Item>, StateId> = HashMap::from([(canon(&i0), 0)]);
        let mut worklist = vec![i0];
        states.push(State {
            items: Vec::new(),
            action: HashMap::new(),
            goto: HashMap::new(),
        });

        while let Some(iset) = worklist.pop() {
            let sid = set2id[&canon(&iset)];

            // ensure the Vec<State> is big enough
            if states.len() <= sid {
                states.resize_with(sid + 1, || State {
                    items: Vec::new(),
                    action: HashMap::new(),
                    goto: HashMap::new(),
                });
            }
            states[sid].items = iset.iter().cloned().collect();

            // partition symbols after dots into terminals / non-terminals
            let (terms, nterms): (HashSet<_>, HashSet<_>) = iset
                .iter()
                .filter_map(|it| {
                    let (lhs, rhs_id) = it.production;
                    let rhs = &grammar.productions[&lhs][rhs_id];
                    (it.dot_position < rhs.len()).then(|| &rhs[it.dot_position])
                })
                .fold(
                    (HashSet::new(), HashSet::new()),
                    |(mut ts, mut nts), sym| {
                        match *sym {
                            Symbol::Terminal(t) => {
                                ts.insert(t);
                            }
                            Symbol::NonTerminal(nt) => {
                                nts.insert(nt);
                            }
                            Symbol::Epsilon => {}
                        }
                        (ts, nts)
                    },
                );

            /* ------- shifts & gotos ------------------------------------- */
            terms.into_iter().for_each(|t| {
                let nxt = goto(&iset, &Symbol::Terminal(t));
                if nxt.is_empty() {
                    return;
                }
                let key = canon(&nxt);
                let tgt = match set2id.entry(key) {
                    Entry::Occupied(e) => *e.get(), // state already known
                    Entry::Vacant(v) => {
                        let id = states.len(); // next free StateId
                        v.insert(id); // record in the map
                        worklist.push(nxt); // schedule for expansion
                        states.push(State {
                            // make the placeholder
                            items: Vec::new(),
                            action: HashMap::new(),
                            goto: HashMap::new(),
                        });
                        id
                    }
                };
                states[sid].action.insert(t, Action::Shift(tgt));
            });

            nterms.into_iter().for_each(|nt| {
                let nxt = goto(&iset, &Symbol::NonTerminal(nt));
                if nxt.is_empty() {
                    return;
                }
                let key = canon(&nxt);
                let tgt = match set2id.entry(key) {
                    Entry::Occupied(e) => *e.get(), // state already known
                    Entry::Vacant(v) => {
                        let id = states.len(); // next free StateId
                        v.insert(id); // record in the map
                        worklist.push(nxt); // schedule for expansion
                        states.push(State {
                            // make the placeholder
                            items: Vec::new(),
                            action: HashMap::new(),
                            goto: HashMap::new(),
                        });
                        id
                    }
                };
                states[sid].goto.insert(nt, tgt);
            });

            /* ------- reduces / accept ------------------------------------ */
            iset.iter().for_each(|it| {
                let (lhs, rhs_id) = it.production;
                let rhs = &grammar.productions[&lhs][rhs_id];
                if it.dot_position == rhs.len() {
                    // if lhs == start_nt && rhs_id == 0 && it.lookhead == eof {
                    //     states[sid].action.insert(it.lookhead, Action::Accept);
                    // } else {
                    states[sid]
                        .action
                        .insert(it.lookhead, Action::Reduce(it.production));
                    // }
                }
            });
        }

        Self { grammar, states }
    }
}

#[derive(Debug)]
pub struct FirstSets<'grammar, Terminal, NonTerminal> {
    nonterminal_first_sets: Vec<Vec<TerminalId>>,
    nonterminal_nullable: Vec<bool>,
    grammar: &'grammar Grammar<Terminal, NonTerminal>,
}

impl<'grammar, Terminal, NonTerminal> FirstSets<'grammar, Terminal, NonTerminal> {
    pub fn make(grammar: &'grammar Grammar<Terminal, NonTerminal>) -> Self {
        /* ---- initialise -------------------------------------------------- */
        let n = grammar.nonterminals.len();
        let mut nonterminal_first_sets: Vec<Vec<TerminalId>> = vec![Vec::new(); n];
        let mut nonterminal_nullable: Vec<bool> = vec![false; n];

        /* ---- small helper: A ∪= B --------------------------------------- */
        fn union<I, J>(lhs: I, rhs: J) -> Vec<usize>
        where
            I: Iterator<Item = TerminalId>,
            J: Iterator<Item = TerminalId>,
        {
            merge(lhs, rhs).dedup().collect_vec()
        }

        // -------- fixed-point computation (iterator-centric style) ---------
        let mut changed = true;
        while changed {
            changed = false;

            grammar.productions.iter().for_each(|(&lhs, rhss)| {
                rhss.iter().for_each(|rhs| {
                    // FIRST(rhs) and ε-nullability in one fold
                    let (rhs_first, rhs_nullable) = rhs.iter().fold(
                        (Vec::<TerminalId>::new(), true),
                        |(mut acc, still_nullable), sym| {
                            if !still_nullable {
                                // once we hit a non-nullable symbol we can stop
                                return (acc, false);
                            }
                            match *sym {
                                Symbol::Terminal(t) => {
                                    acc.push(t);
                                    (acc, false)
                                }
                                Symbol::NonTerminal(nt) => {
                                    acc = union(
                                        acc.into_iter(),
                                        nonterminal_first_sets[nt].iter().cloned(),
                                    );
                                    (acc, nonterminal_nullable[nt])
                                }
                                Symbol::Epsilon => (acc, true),
                            }
                        },
                    );

                    // propagate FIRST(rhs) to FIRST(lhs)
                    let new_first = union(
                        nonterminal_first_sets[lhs].iter().cloned(),
                        rhs_first.into_iter(),
                    );
                    if new_first != nonterminal_first_sets[lhs] {
                        nonterminal_first_sets[lhs] = new_first;
                        changed = true;
                    }

                    // propagate nullability
                    if rhs_nullable && !nonterminal_nullable[lhs] {
                        nonterminal_nullable[lhs] = true;
                        changed = true;
                    }
                });
            });
        }

        Self {
            nonterminal_first_sets,
            nonterminal_nullable,
            grammar,
        }
    }

    pub fn first(&self, sym: &Symbol<TerminalId, NonTerminalId>) -> Vec<TerminalId> {
        match *sym {
            Symbol::Terminal(i) => vec![i],
            Symbol::NonTerminal(i) => self.nonterminal_first_sets[i].clone(),
            Symbol::Epsilon => vec![], // just make things simpler
        }
    }

    pub fn nullable(&self, sym: &Symbol<TerminalId, NonTerminalId>) -> bool {
        match *sym {
            Symbol::Terminal(_) => false,
            Symbol::NonTerminal(i) => self.nonterminal_nullable[i],
            Symbol::Epsilon => true, // just make things simpler
        }
    }
}
