//! # Parser for Carthage files.
//!
//! carthage_rs provides the avility to parse Carthage's
//! `Cartfile`, `Cartfile.private` and `Carthage.resolved`.
//!
//!
//! # Examples
//! ```
//! use cartfile_rs::parse_cartfile;
//! let dependencies = parse_cartfile(
//!         r#"
//! github "antitypical/Result" ~> 3.2
//! github "Carthage/ReactiveTask" ~> 0.14
//! "#,
//! );
//!
//! assert_eq!(2, dependencies.len());
//! ```

use nom::types::*;
use nom::*;

type Input<'a> = CompleteByteSlice<'a>;

#[allow(dead_code)]
fn slice_to_str(x: Input) -> Result<&str, std::str::Utf8Error> {
    std::str::from_utf8(&x)
}

#[allow(dead_code)]
fn is_newline(c: u8) -> bool {
    c != b'\n' && c != b'\r'
}

/// Carthage dependency.
#[derive(Debug, PartialEq)]
pub struct Dependency<'a> {
    pub kind: Kind,
    pub location: &'a str,
    pub requirement: Option<Requirement<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum Kind {
    Binary,
    Git,
    GitHub,
}

#[derive(Debug, PartialEq)]
pub enum Requirement<'a> {
    Version { op: VersionOp, version: &'a str },
    Pin(&'a str),
}

#[derive(Debug, PartialEq)]
pub enum VersionOp {
    Equal,
    AtLeast,
    CompatibleWith,
}

named!(dependency<Input, Dependency>,
       do_parse!(
           kind: kind >>
           multispace0 >>
           location: location >>
           requirement: opt!(preceded!(multispace0, alt!(
               version_constraint |
               map!(version_pin, |c| Requirement::Pin(c))
           ))) >>
           ( Dependency { kind, location, requirement } )
       )
);

named!(kind<Input, Kind>,
       alt_complete!(
           map!(tag!("binary"), |_| Kind::Binary) |
           map!(tag!("github"), |_| Kind::GitHub) |
           map!(tag!("git"), |_| Kind::Git)
       )
);

#[test]
fn parse_kind() {
    assert_eq!(
        kind(b"binary"[..].into()),
        Ok((b""[..].into(), Kind::Binary))
    );

    assert_eq!(kind(b"git"[..].into()), Ok((b""[..].into(), Kind::Git)));

    assert_eq!(
        kind(b"github"[..].into()),
        Ok((b""[..].into(), Kind::GitHub))
    );
}

named!(location<Input, &str>,
       map_res!(delimited!(tag!("\""), take_until!("\""), tag!("\"")), slice_to_str)
);

named!(op<Input, VersionOp>,
       alt_complete!(
           map!(tag!("=="), |_| VersionOp::Equal) |
           map!(tag!("~>"), |_| VersionOp::CompatibleWith) |
           map!(tag!(">="), |_| VersionOp::AtLeast)
       )
);

#[test]
fn parse_op() {
    assert_eq!(
        op(br#"=="#[..].into()),
        Ok((b""[..].into(), VersionOp::Equal))
    );
    assert_eq!(
        op(br#"~>"#[..].into()),
        Ok((b""[..].into(), VersionOp::CompatibleWith))
    );
    assert_eq!(
        op(br#">="#[..].into()),
        Ok((b""[..].into(), VersionOp::AtLeast))
    );
    assert_eq!(
        op(br#"a"#[..].into()),
        Err(Err::Error(Context::Code(
            br#"a"#[..].into(),
            ErrorKind::Alt
        )))
    );
}

named!(version<Input, &str>,
       map_res!(take_while1!(|c| is_digit(c) || c == b'.'), slice_to_str)
);

#[test]
fn parse_version() {
    assert_eq!(version(br#"0"#[..].into()), Ok((b""[..].into(), "0")));
    assert_eq!(
        version(br#"0.0.0"#[..].into()),
        Ok((b""[..].into(), "0.0.0"))
    );
    assert_eq!(
        version(br#"a"#[..].into()),
        Err(Err::Error(Context::Code(
            br#"a"#[..].into(),
            ErrorKind::TakeWhile1
        )))
    );
}

named!(version_constraint<Input, Requirement>,
       do_parse!(
           op: op >>
           multispace0 >>
           version: version >>
           ( Requirement::Version { op, version } )
       )
);

#[test]
fn parse_version_constraint() {
    assert_eq!(
        version_constraint(br#"== 1.2"#[..].into()),
        Ok((
            b""[..].into(),
            Requirement::Version {
                op: VersionOp::Equal,
                version: "1.2"
            }
        ))
    );

    assert_eq!(
        version_constraint(br#"~>0"#[..].into()),
        Ok((
            b""[..].into(),
            Requirement::Version {
                op: VersionOp::CompatibleWith,
                version: "0"
            }
        ))
    );
}

named!(version_pin<Input, &str>,
  map_res!(delimited!(tag!("\""), take_until!("\""), tag!("\"")),
           slice_to_str)
);

#[test]
fn parse_version_pin() {
    assert_eq!(
        version_pin(br#""foo bar""#[..].into()),
        Ok((b""[..].into(), "foo bar"))
    )
}

named!(line<Input, Option<Dependency>>,
       do_parse!(
           multispace0 >>
           dependency: opt!(dependency) >>
           opt!(comment) >>
           take_while!( is_newline ) >>
           opt!(eol) >>
           ( dependency )
       )
);

named!(comment<Input, Input>,
       recognize!(
           preceded!(tag!("#"),
                     take_while!( is_newline ))
       )
);

named!(cartfile<Input, Vec<Dependency>>,
       do_parse!(
           lines: many0!( line ) >>
           ( lines.into_iter().flat_map(|e| e).collect() )
       )
);

#[test]
fn parse_dependency() {
    assert_eq!(
        dependency(br#"github "hoge" "foo""#[..].into()),
        Ok((
            b""[..].into(),
            Dependency {
                kind: Kind::GitHub,
                location: "hoge",
                requirement: Some(Requirement::Pin("foo"))
            }
        ))
    );

    assert_eq!(
        dependency(br#"github "hoge" == 2.3"#[..].into()),
        Ok((
            b""[..].into(),
            Dependency {
                kind: Kind::GitHub,
                location: "hoge",
                requirement: Some(Requirement::Version {
                    op: VersionOp::Equal,
                    version: "2.3"
                })
            }
        ))
    );

    assert_eq!(
        dependency(br#"github "hoge""#[..].into()),
        Ok((
            b""[..].into(),
            Dependency {
                kind: Kind::GitHub,
                location: "hoge",
                requirement: None,
            }
        ))
    );
}

#[test]
fn parse_comment() {
    assert_eq!(
        comment(b"# binary\n# foo"[..].into()),
        Ok((b"\n# foo"[..].into(), b"# binary"[..].into()))
    );

    assert_eq!(
        comment(b"# binary # foo"[..].into()),
        Ok((b""[..].into(), b"# binary # foo"[..].into()))
    );
}

/// Parse `Cartfile`.
///
/// # Examples
///
/// ```
/// use cartfile_rs::parse_cartfile;
/// let dependencies = parse_cartfile(
///         r#"
/// github "antitypical/Result" ~> 3.2
/// github "Carthage/ReactiveTask" ~> 0.14
/// "#,
/// );
///
/// assert_eq!(2, dependencies.len());
/// ```
pub fn parse_cartfile(s: &str) -> Vec<Dependency> {
    let r = cartfile(s.as_bytes().into());

    if let Ok((_, libs)) = r {
        return libs;
    } else {
        return vec![];
    }
}
