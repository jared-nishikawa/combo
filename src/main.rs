use combo::parser;

fn main() {
    let text = "[\"hello\", \"world\"]";
    //let text = "[\"hello\"";
    /*
    let p1 = parser::char_parser('h');
    let p2 = parser::char_parser('e');
    let p = parser::concat_parser(p1, p2);
    */

    /*
    let p1 = parser::char_parser('h');
    let p2 = parser::char_parser('x');
    let p = parser::or_parser(p1, p2);
    */

    //let p = parser::quoted_string_parser();

    /*
    let p = parser::many_concat_parser(vec![
        parser::quoted_string_parser(),
        parser::whitespace_parser(),
        parser::take(5),
        parser::eof(),
    ]);
    */

    let p = parser::list_parser();


    println!("{:?}", p(text));
}
