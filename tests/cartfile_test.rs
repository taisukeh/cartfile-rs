extern crate cartfile;
use cartfile::*;

#[test]
fn test_parse_cartfile() {
    let dependencies = parse_cartfile(
        r#"
# hoge

   github     "hoge"                       

  binary                  "foo"   ~>           2.10
binary"foo">=2.10
binary "foo" "bar"

"#,
    );

    assert_eq!(dependencies.len(), 4);
}

#[test]
fn test_cartfile_0() {
    let dependencies = parse_cartfile(
        r#"
github "antitypical/Result" ~> 3.2
github "Carthage/ReactiveTask" ~> 0.14
github "Carthage/Commandant" ~> 0.14
github "jdhealy/PrettyColors" ~> 5.0
github "ReactiveCocoa/ReactiveSwift" ~> 3.1
github "mdiep/Tentacle" ~> 0.11
github "thoughtbot/Curry" ~> 4.0
"#,
    );

    assert_eq!(
        dependencies[0],
        Dependency {
            kind: Kind::GitHub,
            location: "antitypical/Result",
            requirement: Some(Requirement::Version {
                op: VersionOp::CompatibleWith,
                version: "3.2"
            }),
        }
    );

    assert_eq!(dependencies.len(), 7);
}
