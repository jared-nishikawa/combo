use std::fmt;


#[derive(Debug)]
pub enum Error {
    ParseError(String),
    Eof,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let t = match self {
            Error::ParseError(s) => s,
            Error::Eof => "EOF",
        };
        write!(f, "{}", t)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Token {
    Eof,
    Comma,
    LeftSquare,
    RightSquare,
    LeftCurly,
    RightCurly,
    String(String),
    QuotedString(String),
    ListItem(String),
    List(Vec<Token>),
    Whitespace(String),
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Token::Eof => match other {
                Token::Eof => true,
                _ => false,
            },
            Token::Comma => match other {
                Token::Comma => true,
                _ => false,
            },
            Token::LeftSquare => match other {
                Token::LeftSquare => true,
                _ => false,
            },
            Token::RightSquare => match other {
                Token::RightSquare => true,
                _ => false,
            },
            Token::LeftCurly => match other {
                Token::LeftCurly => true,
                _ => false,
            },
            Token::RightCurly => match other {
                Token::RightCurly => true,
                _ => false,
            },
            Token::String(s) => match other {
                Token::String(o) => s == o,
                _ => false,
            },
            Token::QuotedString(s) => match other {
                Token::QuotedString(o) => s == o,
                _ => false,
            },
            Token::Whitespace(s) => match other {
                Token::Whitespace(o) => s == o,
                _ => false,
            },
            Token::ListItem(s) => match other {
                Token::ListItem(o) => s == o,
                _ => false,
            },
            _ => false,
        }
    }
}

impl PartialEq<String> for Token {
    fn eq(&self, other: &String) -> bool {
        match self {
            Token::String(s) => s == other,
            Token::QuotedString(s) => s == other,
            Token::Whitespace(s) => s == other,
            Token::Comma => other == ",",
            Token::LeftSquare => other == "[",
            Token::RightSquare => other == "]",
            Token::LeftCurly => other == "{",
            Token::RightCurly => other == "}",
            _ => false,
        }
    }
}

type ParseResult<'a> = Result<(Vec<Token>, &'a str)>;
type Parser = dyn Fn(&str) -> ParseResult;

/*
 pub fn create_error(&mut self, msg: String) -> Error {
        Error::ScanError(format!("scan_error at {}: {}", self.pos, msg))
    }
*/

fn create_error(msg: &str) -> Error {
    Error::ParseError(msg.to_string())
}

/*
fn change_error<'a>(r: ParseResult<'a>, msg: &'a str) -> ParseResult<'a> {
    match r {
        Ok(_) => r,
        Err(_) => Err(create_error(msg)),
    }
}
*/

pub fn eof() -> Box<Parser> {
    Box::new(move |s: &str| {
        if s.is_empty() {
            Ok((vec![], s))
        } else {
            Err(Error::Eof)
        }
    })
}

pub fn take(n: usize) -> Box<Parser> {
    Box::new(move |s: &str| {
        if s.len() < n {
            Err(create_error("take parser failed"))
        } else {
            Ok((vec![Token::String(s[..n].to_string())], &s[n..]))
        }
    })
}

pub fn char_parser(c: char) -> Box<Parser> {
    // "move" forces closure to take ownership of borrowed 'c'
    Box::new(move |s: &str| {
        if s.chars().next() == Some(c) {
            Ok((vec![Token::String(s[..1].to_string())], &s[1..]))
        } else {
            Err(create_error("char parser failed"))
        }
    })
}

pub fn chartoken_parser(c: char, t: Token) -> Box<Parser> {
    Box::new(move |s: &str| {
        if s.chars().next() == Some(c) {
            Ok((vec![t.clone()], &s[1..]))
        } else {
            Err(create_error("special parser failed"))
        }
    })
}

pub fn comma_parser() -> Box<Parser> {
    chartoken_parser(',', Token::Comma)
}

pub fn leftsquare_parser() -> Box<Parser> {
    chartoken_parser('[', Token::LeftSquare)
}

pub fn rightsquare_parser() -> Box<Parser> {
    chartoken_parser(']', Token::RightSquare)
}

pub fn leftcurly_parser() -> Box<Parser> {
    chartoken_parser('{', Token::LeftCurly)
}

pub fn rightcurly_parser() -> Box<Parser> {
    chartoken_parser('}', Token::RightCurly)
}


pub fn literal_parser(literal: &'static str) -> Box<Parser> {
    Box::new(move |s: &str| {
        if s.starts_with(literal) {
            Ok((vec![Token::String(literal.to_string())], &s[literal.len()..]))
        } else {
            Err(create_error("literal parser failed"))
        }
    })
}

pub fn not_parser(c: char) -> Box<Parser> {
    Box::new(move |s: &str| {
        if s.is_empty() {
            return Err(create_error("not parser failed"));
        }
        if s.chars().next() != Some(c) {
            Ok((vec![Token::String(s[..1].to_string())], &s[1..]))
        } else {
            Err(create_error("not parser failed"))
        }
    })
}

pub fn combine(p: Box<Parser>) -> Box<Parser> {
    Box::new(move |s: &str| {
        let (parsed, rest) = p(s)?;
        //todo
        let mut v = Vec::new();
        for t in parsed.iter() {
            match t {
                Token::String(s) => v.push(s.clone()),
                _ => return Err(create_error("combine parser failed")),
            }
        };
        Ok((vec![Token::String(v.join(""))], rest))
    })
}

pub fn concat_parser(p1: Box<Parser>, p2: Box<Parser>) -> Box<Parser> {
    Box::new(move |s: &str| {
        let (parsed1, rest1) = p1(s)?;
        if let Ok((parsed2, rest2)) = p2(rest1) {
            let mut v = Vec::new();
            v.extend(parsed1);
            v.extend(parsed2);
            Ok((v, rest2))
        } else {
            Err(create_error("concat parser failed"))
        }
    })
}

pub fn many_concat_parser(parsers: Vec<Box<Parser>>) -> Box<Parser> {
    Box::new(move |s: &str| {
        let mut v = Vec::new();
        let mut rest = s;
        for p in parsers.iter() {
            let (pd, r) = p(rest)?;
            v.extend(pd);
            rest = r;
        }
        Ok((v, rest))
    })
}

pub fn or_parser(p1: Box<Parser>, p2: Box<Parser>) -> Box<Parser> {
    Box::new(move |s: &str| {
        if let Ok(x) = p1(s) {
            Ok(x)
        } else if let Ok(x) = p2(s) {
            Ok(x)
        } else {
            Err(create_error("or parser failed"))
        }
    })
}

pub fn many_or_parser(parsers: Vec<Box<Parser>>) -> Box<Parser> {
    Box::new(move |s: &str| {
        for p in parsers.iter() {
            if let Ok(x) = p(s) {
                return Ok(x);
            }
        }
        Err(create_error("many or parser failed"))
    })
}

pub fn zero_or_one_parser(p: Box<Parser>) -> Box<Parser> {
    Box::new(move |s: &str| {
        if let Ok((parsed, rest)) = p(s) {
            Ok((parsed, rest))
        } else {
            Ok((vec![], s))
        }
    })
}

pub fn zero_or_more_parser(p: Box<Parser>) -> Box<Parser> {
    Box::new(move |s: &str| {
        let mut v = Vec::new();
        let mut rest = s;
        while let Ok((p, r)) = p(rest) {
            v.extend(p);
            rest = r;
        }
        Ok((v, rest))
    })
}

pub fn one_or_more_parser(p: Box<Parser>) -> Box<Parser> {
    Box::new(move |s: &str| {
        let (parsed, mut rest) = p(s)?;
        let mut parsed = parsed;
        while let Ok((p, r)) = p(rest) {
            parsed.extend(p);
            rest = r;
        }
        Ok((parsed, rest))
    })
}

pub fn whitespace_parser() -> Box<Parser> {
    Box::new(move |s: &str| {
        let parsers = vec![
            char_parser(' '),
            char_parser('\t'),
            char_parser('\n'),
            char_parser('\r'),
        ];
        if let Ok((parsed, rest)) = combine(zero_or_more_parser(many_or_parser(parsers)))(s) {
            let mut v = Vec::new();
            match &parsed[0] {
                Token::String(s) => {
                    if !s.is_empty() {
                        v.push(Token::Whitespace(s.to_string()));
                    }
                }
                _ => return Err(create_error("whitespace parser failed")),
            }
            Ok((v, rest))
        } else {
            Err(create_error("whitespace parser failed"))
        }
    })
}

pub fn quoted_string_parser() -> Box<Parser> {
    Box::new(move |s: &str| {
        let parsers = vec![
            char_parser('"'),
            combine(zero_or_more_parser(not_parser('"'))),
            char_parser('"'),
        ];

        if let Ok((parsed, rest)) = many_concat_parser(parsers)(s) {
            let mut v = Vec::new();
            match &parsed[1] {
                Token::String(s) => v.push(Token::QuotedString(s.to_string())),
                _ => return Err(create_error("quoted string parser failed")),
            }
            Ok((v, rest))
        } else {
            Err(create_error("quoted string parser failed"))
        }
    })
}

pub fn list_item_parser() -> Box<Parser> {
    Box::new(move |s: &str| {
        let parsers = vec![
            quoted_string_parser(),
            whitespace_parser(),
            comma_parser(),
            whitespace_parser(),
        ];
        if let Ok((parsed, rest)) = many_concat_parser(parsers)(s) {
            match &parsed[0] {
                Token::QuotedString(s) => Ok((vec![Token::ListItem(s.to_string())], rest)),
                _ => Err(create_error("list item parser failed")),
            }
        } else {
            Err(create_error("list item parser failed"))
        }
    })
}

pub fn last_list_item_parser() -> Box<Parser> {
    Box::new(move |s: &str| {
        let parsers = vec![
            quoted_string_parser(),
            whitespace_parser(),
        ];
        if let Ok((parsed, rest)) = many_concat_parser(parsers)(s) {
            match &parsed[0] {
                Token::QuotedString(s) => Ok((vec![Token::ListItem(s.to_string())], rest)),
                _ => Err(create_error("list item parser failed")),
            }
        } else {
            Err(create_error("list item parser failed"))
        }
    })
}


pub fn list_parser() -> Box<Parser> {
    Box::new(move |s: &str| {
        let parsers = vec![
            leftsquare_parser(),
            whitespace_parser(),
            zero_or_more_parser(list_item_parser()),
            zero_or_one_parser(last_list_item_parser()),
            rightsquare_parser(),
            whitespace_parser(),
        ];
        if let Ok((parsed, rest)) = many_concat_parser(parsers)(s) {
            let mut v = Vec::new();
            for t in parsed.iter() {
                match t {
                    Token::ListItem(_) => v.push(t.clone()),
                    Token::LeftSquare | Token::RightSquare | Token::Comma | Token::Whitespace(_) => continue,
                    _ => return Err(create_error("list parser failed with unknown token")),
                }
            }
            Ok((vec![Token::List(v)], rest))
        } else {
            Err(create_error("list parser failed"))
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_eof() {
        let p = eof();
        if let Ok((parsed, rest)) = p("") {
            assert!(parsed.is_empty());
            assert_eq!(rest, "");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("hello") {
            assert!(false);
        }
    }

    #[test]
    fn parse_take() {
        let p = take(3);
        if let Ok((parsed, rest)) = p("hello") {
            assert_eq!(parsed, vec!["hel".to_string()]);
            assert_eq!(rest, "lo");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("he") {
            assert!(false);
        }
    }

    #[test]
    fn parse_char() {
        let p = char_parser('h');
        if let Ok((parsed, rest)) = p("hello") {
            assert_eq!(parsed[0], "h".to_string());
            assert_eq!(rest, "ello");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("world") {
            assert!(false);
        }
    }

    #[test]
    fn parse_literal() {
        let p = literal_parser("hello");
        if let Ok((parsed, rest)) = p("hello world") {
            assert_eq!(parsed[0], "hello".to_string());
            assert_eq!(rest, " world");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("world") {
            assert!(false);
        }
    }

    #[test]
    fn parse_not() {
        let p = not_parser('h');
        if let Ok((parsed, rest)) = p("world") {
            assert_eq!(parsed[0], "w".to_string());
            assert_eq!(rest, "orld");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("hello") {
            assert!(false);
        }

        if let Ok(_) = p("") {
            assert!(false);
        }
    }

    #[test]
    fn parse_concat() {
        let p1 = char_parser('h');
        let p2 = char_parser('e');
        let p = combine(concat_parser(p1, p2));
        if let Ok((parsed, rest)) = p("hello") {
            assert_eq!(parsed[0], "he".to_string());
            assert_eq!(rest, "llo");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("world") {
            assert!(false);
        }
    }

    #[test]
    fn parse_many_concat() {
        let p1 = char_parser('h');
        let p2 = char_parser('e');
        let p3 = char_parser('l');
        let p4 = char_parser('l');
        let p5 = char_parser('o');
        let p = combine(many_concat_parser(vec![p1, p2, p3, p4, p5]));
        if let Ok((parsed, rest)) = p("hello") {
            assert_eq!(parsed[0], "hello".to_string());
            assert_eq!(rest, "");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("world") {
            assert!(false);
        }
    }

    #[test]
    fn parse_or() {
        let p1 = char_parser('h');
        let p2 = char_parser('w');
        let p = or_parser(p1, p2);
        if let Ok((parsed, rest)) = p("hello") {
            assert_eq!(parsed[0], "h".to_string());
            assert_eq!(rest, "ello");
        } else {
            assert!(false);
        }

        if let Ok((parsed, rest)) = p("world") {
            assert_eq!(parsed[0], "w".to_string());
            assert_eq!(rest, "orld");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("foo") {
            assert!(false);
        }
    }

    #[test]
    fn parse_many_or() {
        let p1 = char_parser('h');
        let p2 = char_parser('w');
        let p3 = char_parser('f');
        let p = many_or_parser(vec![p1, p2, p3]);
        if let Ok((parsed, rest)) = p("hello") {
            assert_eq!(parsed[0], "h".to_string());
            assert_eq!(rest, "ello");
        } else {
            assert!(false);
        }

        if let Ok((parsed, rest)) = p("world") {
            assert_eq!(parsed[0], "w".to_string());
            assert_eq!(rest, "orld");
        } else {
            assert!(false);
        }

        if let Ok((parsed, rest)) = p("foo") {
            assert_eq!(parsed[0], "f".to_string());
            assert_eq!(rest, "oo");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("bar") {
            assert!(false);
        }
    }

    #[test]
    fn parse_zero_or_one() {
        let p = combine(zero_or_one_parser(char_parser('h')));
        if let Ok((parsed, rest)) = p("hello") {
            assert_eq!(parsed[0], "h".to_string());
            assert_eq!(rest, "ello");
        } else {
            assert!(false);
        }

        if let Ok((parsed, rest)) = p("world") {
            assert_eq!(parsed[0], "".to_string());
            assert_eq!(rest, "world");
        } else {
            assert!(false);
        }
    }

    #[test]
    fn parse_zero_or_more() {
        let p = combine(zero_or_more_parser(char_parser('h')));
        if let Ok((parsed, rest)) = p("hello") {
            assert_eq!(parsed[0], "h".to_string());
            assert_eq!(rest, "ello");
        } else {
            assert!(false);
        }

        if let Ok((parsed, rest)) = p("hhhello") {
            assert_eq!(parsed[0], "hhh".to_string());
            assert_eq!(rest, "ello");
        } else {
            assert!(false);
        }

        if let Ok((parsed, rest)) = p("world") {
            assert_eq!(parsed[0], "".to_string());
            assert_eq!(rest, "world");
        } else {
            assert!(false);
        }
    }

    #[test]
    fn parse_one_or_more() {
        let p = combine(one_or_more_parser(char_parser('h')));
        if let Ok((parsed, rest)) = p("hello") {
            assert_eq!(parsed[0], "h".to_string());
            assert_eq!(rest, "ello");
        } else {
            assert!(false);
        }

        if let Ok((parsed, rest)) = p("hhhello") {
            assert_eq!(parsed[0], "hhh".to_string());
            assert_eq!(rest, "ello");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("world") {
            assert!(false);
        }
    }

    #[test]
    fn parse_whitespace() {
        let p = whitespace_parser();
        if let Ok((parsed, rest)) = p(" \t\n\rhello") {
            assert_eq!(parsed[0], " \t\n\r".to_string());
            assert_eq!(rest, "hello");
        } else {
            assert!(false);
        }

        if let Ok((parsed, rest)) = p(" \t\n\r world") {
            assert_eq!(parsed[0], " \t\n\r ".to_string());
            assert_eq!(rest, "world");
        } else {
            assert!(false);
        }
    }

    #[test]
    fn parse_string() {
        let p = quoted_string_parser();
        if let Ok((parsed, rest)) = p("\"hello\" world") {
            assert_eq!(parsed[0], "hello".to_string());
            assert_eq!(rest, " world");
        } else {
            assert!(false);
        }

        if let Ok((parsed, rest)) = p("\"hello\"world") {
            assert_eq!(parsed[0], "hello".to_string());
            assert_eq!(rest, "world");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("\"hello") {
            assert!(false);
        }
    }

    #[test]
    fn parse_comma() {
        let p = comma_parser();
        if let Ok((parsed, rest)) = p(", world") {
            assert_eq!(parsed[0], Token::Comma);
            assert_eq!(rest, " world");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("hello") {
            assert!(false);
        }
    }

    #[test]
    fn parse_leftsquare() {
        let p = leftsquare_parser();
        if let Ok((parsed, rest)) = p("[ world") {
            assert_eq!(parsed[0], Token::LeftSquare);
            assert_eq!(rest, " world");
        } else {
            assert!(false);
        }

        if let Ok(_) = p("hello") {
            assert!(false);
        }
    }

    #[test]
    fn parse_list_item() {
        let p = list_item_parser();
        if let Ok((parsed, _)) = p("\"hello\", ") {
            match &parsed[0] {
                Token::ListItem(s) => assert_eq!(s, "hello"),
                _ => assert!(false),
            }
        } else {
            assert!(false);
        }

        if let Ok((parsed, _)) = p("\"hello\" , ") {
            match &parsed[0] {
                Token::ListItem(s) => assert_eq!(s, "hello"),
                _ => assert!(false),
            }
        } else {
            assert!(false);
        }

        if let Ok(_) = p("\"hello\"") {
            assert!(false);
        }
    }

    #[test]
    fn parse_list() {
        let p = list_parser();
        if let Ok((parsed, _)) = p("[]") {
            match &parsed[0] {
                Token::List(v) => assert!(v.is_empty()),
                _ => assert!(false),
            }
        } else {
            assert!(false);
        }

        if let Ok((parsed, _)) = p("[\"hello\"]") {
            match &parsed[0] {
                Token::List(v) => {
                    match &v[0] {
                        Token::ListItem(s) => assert_eq!(s, "hello"),
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            }
        } else {
            assert!(false);
        }

        if let Ok((parsed, _)) = p("[\"hello\", \"world\"]") {
            match &parsed[0] {
                Token::List(v) => {
                    match &v[0] {
                        Token::ListItem(s) => assert_eq!(s, "hello"),
                        _ => assert!(false),
                    }
                    match &v[1] {
                        Token::ListItem(s) => assert_eq!(s, "world"),
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            }
        } else {
            assert!(false);
        }
    }

}

