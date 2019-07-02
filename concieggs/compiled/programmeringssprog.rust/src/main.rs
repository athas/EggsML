use rand::seq::SliceRandom;
use rand::Rng;

static PREFIXES: [(&str, u32); 14] = [
    ("Visual ", 10),
    ("Microsoft ", 10),
    ("Cloud ", 10),
    ("Objective ", 10),
    ("Turbo ", 10),
    ("Object ", 10),
    ("Standard ", 1),
    ("Common ", 5),
    ("Liquid ", 1),
    ("Meta", 2),
    ("Win", 1),
    ("Holy", 1),
    ("Linear ", 1),
    ("", 10),
];

static ROOTS: [&str; 38] = [
    "Pascal", "Haskell", "Blaise", "Curry", "Go", "Rust", "A", "B", "C", "D", "E", "F", "Erlang",
    "Prolog", "Neumann", "Euclid", "Fermat", "ML", "Scheme", "Lisp", "Church", "Alonzo", "Futhark",
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

    result.push_str(
        PREFIXES
            .choose_weighted(&mut rng, |item| item.1)
            .map(|item| item.0)
            .unwrap_or(&""),
    );

    loop {
        if rng.gen_range(1, 10) == 9 {
            result.push_str(
                PREFIXES
                    .choose_weighted(&mut rng, |item| item.1)
                    .map(|item| item.0)
                    .unwrap_or(&""),
            );
        } else {
            break;
        }
    }

    result.push_str(ROOTS.choose(&mut rng).unwrap_or(&""));

    result.push_str(
        SUFFIXES
            .choose_weighted(&mut rng, |item| item.1)
            .map(|item| item.0)
            .unwrap_or(&""),
    );

    println!("{}", result);
}
