use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    ops::Add,
    usize,
};

use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol<Terminal, NonTerminal> {
    Terminal(Terminal),
    NonTerminal(NonTerminal),
    Epsilon,
    Eof,
    Start,
}

impl<Terminal: Display, NonTerminal: Display> Display for Symbol<Terminal, NonTerminal> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Terminal(x) => write!(f, "\"{x}\""),
            Symbol::NonTerminal(x) => write!(f, "{x}"),
            Symbol::Epsilon => write!(f, "<<eps>>"),
            Symbol::Eof => write!(f, "<<eof>>"),
            Symbol::Start => write!(f, "<<start>>"),
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

pub fn grammar<Terminal, NonTerminal>(start: NonTerminal) -> GrammarBuilder<Terminal, NonTerminal> {
    GrammarBuilder {
        grammar: Grammar {
            productions: HashMap::new(),
            terminals: Vec::new(),
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
                        Symbol::Eof => Symbol::Eof,
                        Symbol::Start => Symbol::Start,
                    })
                    .collect_vec()
            })
            .collect_vec();
        ans.grammar.productions.insert(lhs_id, rhss_idss);
        ans
    }
}

impl<Terminal, NonTerminal: Eq + Hash> GrammarBuilder<Terminal, NonTerminal> {
    pub fn build(self) -> Grammar<Terminal, NonTerminal> {
        self.grammar
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

pub struct ProductionRef<'grammar, Terminal, NonTerminal> {
    lhs: &'grammar NonTerminal,
    rhs: &'grammar Rhs<Terminal, NonTerminal>,
}

pub struct Item<'grammar, Terminal, NonTerminal> {
    production_ref: ProductionRef<'grammar, Terminal, NonTerminal>,
    dot_position: usize,
    lookhead: &'grammar Terminal,
}

pub type StateId = usize;

pub enum Action<'grammar, Terminal, NonTerminal> {
    Shift(StateId),
    Reduce(ProductionRef<'grammar, Terminal, NonTerminal>),
    Accept,
    Error,
}

pub struct State<'grammar, Terminal, NonTerminal> {
    items: Vec<Item<'grammar, Terminal, NonTerminal>>,
    action: HashMap<Terminal, Action<'grammar, Terminal, NonTerminal>>,
    goto: HashMap<NonTerminal, StateId>,
}

pub struct Table<'grammar, Terminal, NonTerminal> {
    grammar: &'grammar Grammar<Terminal, NonTerminal>,
    // start state at index 0
    states: Vec<State<'grammar, Terminal, NonTerminal>>,
}

pub type FirstSets<Terminal, NonTerminal> = HashMap<Symbol<Terminal, NonTerminal>, Vec<Terminal>>;

// fn compute_first_sets<'grammar, Terminal, NonTerminal>(
//     grammar: &'grammar Grammar<Terminal, NonTerminal>,
// ) -> FirstSets<Terminal, NonTerminal> {
//     let mut first = HashMap::new();
//
//     todo!()
// }
