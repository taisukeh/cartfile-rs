use nom::*;
use nom::types::*;

type Input<'a> = CompleteByteSlice<'a>;

#[derive(Debug, PartialEq)]
pub enum OriginType {
    Binary,
    Git,
    GitHub,
}

#[derive(Debug, PartialEq)]
pub struct Library<'a> {
  pub origin_type: OriginType,
  pub origin: &'a str,
  pub version_requirement: Option<VersionRequirement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct VersionConstraint<'a> {
    pub version_op: VersionOp,
    pub version: &'a str,
}

#[derive(Debug, PartialEq)]
pub enum VersionRequirement<'a> {
    Version(VersionConstraint<'a>),
    Pin(&'a str),
}

#[derive(Debug, PartialEq)]
pub enum VersionOp {
    Equal,
    AtLeast,
    CompatibleWith,
}

named!(origin_type<Input, OriginType>,
       alt_complete!(
           map!(tag!("binary"), |_| OriginType::Binary) |
           map!(tag!("github"), |_| OriginType::GitHub) |
           map!(tag!("git"), |_| OriginType::Git)
       )
);

named!(origin<Input, &str>,
       map_res!(delimited!(tag!("\""), take_until!("\""), tag!("\"")), s2u)
);

named!(version_op<Input, VersionOp>,
       alt_complete!(
           map!(tag!("=="), |_| VersionOp::Equal) |
           map!(tag!("~>"), |_| VersionOp::CompatibleWith) |
           map!(tag!(">="), |_| VersionOp::AtLeast)
       )
);

named!(version<Input, &str>,
       map_res!(take_while!(|c| is_digit(c) || c == b'.'), s2u)
);

named!(version_constraint<Input, VersionConstraint>,
       do_parse!(
           version_op: version_op >>
           multispace0 >>
           version: version >>
           ( VersionConstraint { version_op, version } )
       )
);

named!(version_pin<Input, &str>,
  map_res!(delimited!(tag!("\""), take_until!("\""), tag!("\"")),
           s2u)
);

#[allow(dead_code)]
fn s2u(x: Input) -> Result<&str, std::str::Utf8Error> {
    std::str::from_utf8(&x)
}

named!(library<Input, Library>,
       do_parse!(
           origin_type: origin_type >>
           multispace0 >>
           origin: origin >>
           version_requirement: opt!(preceded!(multispace0, alt!(
               map!(version_constraint, |c| VersionRequirement::Version(c)) |
               map!(version_pin, |c| VersionRequirement::Pin(c))
           ))) >>
           ( Library { origin_type, origin, version_requirement } ) 
       )
);

named!(line<Input, Option<Library>>,
       do_parse!(
           multispace0 >>
           library: opt!(library) >>
           opt!(comment) >>
           take_while!( |c| c != b'\n' && c!= b'\r' ) >>
           opt!(eol) >>
           ( library )
       )
);

named!(comment<Input, Input>,
       recognize!(
           preceded!(tag!("#"),
                     take_while!( |c| c != b'\n' && c!= b'\r' ))
       )
);

named!(cartfile<Input, Vec<Library>>,
       do_parse!(
           lines: many0!( line ) >>
           ( lines.into_iter().flat_map(|e| e).collect() )
       )
);

#[test]
fn parse_library() {
    assert_eq!(library(br#"github "hoge" "foo""#[..].into()),
               Ok((b""[..].into(),
                   Library{ origin_type: OriginType::GitHub,
                            origin: "hoge",
                            version_requirement: Some(VersionRequirement::Pin("foo"))
                   })));

    assert_eq!(library(br#"github "hoge" == 2.3"#[..].into()),
               Ok((b""[..].into(),
                   Library{ origin_type: OriginType::GitHub,
                            origin: "hoge",
                            version_requirement: Some(VersionRequirement::Version(VersionConstraint{ version_op: VersionOp::Equal, version: "2.3" }))
                   })));

    assert_eq!(library(br#"github "hoge""#[..].into()),
               Ok((b""[..].into(),
                   Library{ origin_type: OriginType::GitHub,
                            origin: "hoge",
                            version_requirement: None,
                   })));
}

#[test]
fn parse_version() {
    assert_eq!(version(b"0.1 "[..].into()), Ok((b" "[..].into(), "0.1")));
}

#[test]
fn parse_origin_type() {
  assert_eq!(origin_type(b"binary"[..].into()), Ok((b""[..].into(), OriginType::Binary)));
  assert_eq!(origin_type(b"git"[..].into()), Ok((b""[..].into(), OriginType::Git)));
  assert_eq!(origin_type(b"github"[..].into()), Ok((b""[..].into(), OriginType::GitHub)));
}


#[test]
fn parse_comment() {
  assert_eq!(comment(b"# binary\n# foo"[..].into()), Ok((b"\n# foo"[..].into(), b"# binary"[..].into())));
  assert_eq!(comment(b"# binary # foo"[..].into()), Ok((b""[..].into(), b"# binary # foo"[..].into())));
}


#[test]
fn parse_cartfile() {
  let r = cartfile(br#"
# hoge

   github     "hoge"                       

  binary                  "foo"   ~>           2.10
binary"foo">=2.10
binary "foo" "bar"

"#[..].into());
    println!("====");

    if let Ok((_, libs)) = r {
        for lib in libs.iter() {
            println!("{:?}", lib);
        }
    }
}
