use std::fmt;

#[derive(Debug)]
enum S {
  Atom(char),
  Cons(char, Vec<S>),
}

impl fmt::Display for S {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      S::Atom(i) => write!(f, "{}", i),
      S::Cons(head, rest) => {
        write!(f, "({}", head)?;
        for s in rest {
          write!(f, " {}", s)?
        }

        write!(f, ")")
      }
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
  Atom(char),
  Op(char),
  Eof,
}

#[derive(Debug)]
struct Lexer {
  tokens: Vec<Token>,
}

impl Lexer {
  fn new(input: &str) -> Lexer {
    let mut tokens = input
      .chars()
      .filter(|character| !character.is_ascii_whitespace())
      .map(|character| match character {
        '0'..='9' | 'a'..='z' | 'A'..='Z' => Token::Atom(character),
        _ => Token::Op(character),
      })
      .collect::<Vec<_>>();

    tokens.reverse();

    Lexer { tokens }
  }

  fn next(&mut self) -> Token {
    self.tokens.pop().unwrap_or(Token::Eof)
  }

  fn peek(&mut self) -> Token {
    self.tokens.last().copied().unwrap_or(Token::Eof)
  }
}

fn expr(input: &str) -> S {
  let mut lexer = Lexer::new(input);

  expr_bp(&mut lexer, 0)
}

fn infix_binding_power(op: char) -> (u8, u8) {
  match op {
    '+' | '-' => (1, 2),
    '*' | '/' => (3, 4),
    _ => panic!("unexpected op: {:?}", op),
  }
}

fn expr_bp(lexer: &mut Lexer, min_bp: u8) -> S {
  let mut lhs = match lexer.next() {
    Token::Atom(character) => S::Atom(character),
    t => panic!("unexpected token: {:?}", t),
  };

  loop {
    let op = match lexer.peek() {
      Token::Eof => break,
      Token::Op(op) => op,
      t => panic!("unexpected token: {:?}", t),
    };

    let (l_bp, r_bp) = infix_binding_power(op);

    if l_bp < min_bp {
      break;
    }

    lexer.next();

    let rhs = expr_bp(lexer, r_bp);

    lhs = S::Cons(op, vec![lhs, rhs]);
  }

  lhs
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn tests() {
    assert_eq!(expr("1 + 2 * 3").to_string(), "(+ 1 (* 2 3))");

    assert_eq!(expr("1").to_string(), "1");

    assert_eq!(
      expr("a + b * c * d + e").to_string(),
      "(+ (+ a (* (* b c) d)) e)"
    );
  }
}
