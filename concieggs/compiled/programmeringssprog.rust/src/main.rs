use rand::seq::SliceRandom;

static PREFIXES: [&str; 12] = [
    "Visual ",
    "Microsoft ",
    "Cloud ",
    "Objective ",
    "Turbo ",
    "Object ",
    "Standard ",
    "Common ",
    "Liquid ",
    "Meta",
    "Win",
    "",
];

static ROOTS: [&str; 38] = [
    "Pascal", "Haskell", "Blaise", "Curry", "Go", "Rust", "A", "B", "C", "D", "E", "F", "Erlang",
    "Prolog", "Neumann", "Euclid", "Fermat", "ML", "Scheme", "Lisp", "Church", "Alonzo", "Futhar",
    "Java", "COBOL", "Forth", "Fortran", "Turing", "Gauss", "Riemann", "Newton", "Hilbert", "Naur",
    "Perl", "PHP", "Swift", "Python", "Basic",
];

static SUFFIXES: [(&str, u32); 18] = [
    ("#", 10),
    ("++", 10),
    (" .NET", 10),
    ("*", 2),
    ("--", 1),
    ("script", 30),
    (" with Classes", 10),
    (" 2", 1),
    (" 3", 1),
    (" 4", 1),
    (" 5", 1),
    (" 6", 1),
    (" 7", 1),
    (" 8", 1),
    (" 9", 1),
    (" 68", 1),
    (" 77", 1),
    ("", 100),
];

fn main() {
    // prepare a non-deterministic random number generator:
    let mut rng = rand::thread_rng();

    let mut result = String::new();

    result.push_str(PREFIXES.choose(&mut rng).unwrap_or(&""));

    result.push_str(ROOTS.choose(&mut rng).unwrap_or(&""));

    result.push_str(
        SUFFIXES
            .choose_weighted(&mut rng, |item| item.1)
            .map(|item| item.0)
            .unwrap_or(&""),
    );

    println!("{}", result);
}
