# Change Log

## [0.1.9](https://github.com/inaka/erlang-github/tree/0.1.9) (2015-03-09)

[Full Changelog](https://github.com/inaka/erlang-github/compare/0.1.8...0.1.9)

**Implemented enhancements:**

- Add API call to get a users organisation's membership [\#30](https://github.com/inaka/erlang-github/issues/30)

**Fixed bugs:**

- xref is not working [\#29](https://github.com/inaka/erlang-github/issues/29)

**Merged pull requests:**

- \[\#30\] Bumped version. [\#32](https://github.com/inaka/erlang-github/pull/32) ([jfacorro](https://github.com/jfacorro))

- \[Closes \#30\] User organisation membership. [\#31](https://github.com/inaka/erlang-github/pull/31) ([jfacorro](https://github.com/jfacorro))

## [0.1.8](https://github.com/inaka/erlang-github/tree/0.1.8) (2015-03-04)

[Full Changelog](https://github.com/inaka/erlang-github/compare/0.1.7...0.1.8)

**Merged pull requests:**

- Version Bump [\#28](https://github.com/inaka/erlang-github/pull/28) ([elbrujohalcon](https://github.com/elbrujohalcon))

- Handle github rate limits [\#27](https://github.com/inaka/erlang-github/pull/27) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.1.7](https://github.com/inaka/erlang-github/tree/0.1.7) (2015-02-18)

[Full Changelog](https://github.com/inaka/erlang-github/compare/0.1.6...0.1.7)

**Implemented enhancements:**

- Handle comments with position 0 as global issue comments  [\#25](https://github.com/inaka/erlang-github/issues/25)

- Add a function for creating comments in issues [\#23](https://github.com/inaka/erlang-github/issues/23)

**Merged pull requests:**

- \[Closes \#25\] Handle messages with position 0 as global issue comments. [\#26](https://github.com/inaka/erlang-github/pull/26) ([jfacorro](https://github.com/jfacorro))

## [0.1.6](https://github.com/inaka/erlang-github/tree/0.1.6) (2015-02-13)

[Full Changelog](https://github.com/inaka/erlang-github/compare/0.1.5...0.1.6)

**Merged pull requests:**

- \[\#23\] Added issue comment creation and list API calls. [\#24](https://github.com/inaka/erlang-github/pull/24) ([jfacorro](https://github.com/jfacorro))

## [0.1.5](https://github.com/inaka/erlang-github/tree/0.1.5) (2015-01-14)

[Full Changelog](https://github.com/inaka/erlang-github/compare/0.1.4...0.1.5)

**Merged pull requests:**

- Bump Version to 0.1.5 [\#22](https://github.com/inaka/erlang-github/pull/22) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.1.4](https://github.com/inaka/erlang-github/tree/0.1.4) (2015-01-12)

[Full Changelog](https://github.com/inaka/erlang-github/compare/0.1.3...0.1.4)

**Fixed bugs:**

- Typo in type name "messge" instead of "message"  [\#17](https://github.com/inaka/erlang-github/issues/17)

**Merged pull requests:**

- Fix types in egithub\_webhook [\#20](https://github.com/inaka/erlang-github/pull/20) ([elbrujohalcon](https://github.com/elbrujohalcon))

- egithub\_webhook: Provide the whole repo structure, not just the name for the callback [\#19](https://github.com/inaka/erlang-github/pull/19) ([elbrujohalcon](https://github.com/elbrujohalcon))

- \[Closes \#17\] Fix typo. [\#18](https://github.com/inaka/erlang-github/pull/18) ([jfacorro](https://github.com/jfacorro))

- Create a new behaviour: egithub\_webhook [\#16](https://github.com/inaka/erlang-github/pull/16) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.1.3](https://github.com/inaka/erlang-github/tree/0.1.3) (2014-11-13)

[Full Changelog](https://github.com/inaka/erlang-github/compare/0.1.2...0.1.3)

**Implemented enhancements:**

- Create a behaviour that allows using other library than jiffy  [\#12](https://github.com/inaka/erlang-github/issues/12)

**Fixed bugs:**

- Wrong order of arguments when checking application:get\_env/2 [\#14](https://github.com/inaka/erlang-github/issues/14)

**Merged pull requests:**

- \[Closes \#14\] Fixed bug. [\#15](https://github.com/inaka/erlang-github/pull/15) ([jfacorro](https://github.com/jfacorro))

## [0.1.2](https://github.com/inaka/erlang-github/tree/0.1.2) (2014-11-13)

[Full Changelog](https://github.com/inaka/erlang-github/compare/0.1.1...0.1.2)

**Fixed bugs:**

- Wrong credentials tuple being created for basic authentication  [\#6](https://github.com/inaka/erlang-github/issues/6)

**Merged pull requests:**

- \[\#12\] egithub\_json behavior [\#13](https://github.com/inaka/erlang-github/pull/13) ([jfacorro](https://github.com/jfacorro))

## [0.1.1](https://github.com/inaka/erlang-github/tree/0.1.1) (2014-10-16)

[Full Changelog](https://github.com/inaka/erlang-github/compare/0.1.0...0.1.1)

**Fixed bugs:**

- Handle non 404 errors when checking team membership [\#10](https://github.com/inaka/erlang-github/issues/10)

- Wrong api URI for get team membership [\#8](https://github.com/inaka/erlang-github/issues/8)

**Merged pull requests:**

- \[Fixes \#10\] Handle errors that are not 404 [\#11](https://github.com/inaka/erlang-github/pull/11) ([jfacorro](https://github.com/jfacorro))

- \[Fixes \#8\] Corrected uri [\#9](https://github.com/inaka/erlang-github/pull/9) ([jfacorro](https://github.com/jfacorro))

## [0.1.0](https://github.com/inaka/erlang-github/tree/0.1.0) (2014-09-26)

**Closed issues:**

- Basic project structure [\#2](https://github.com/inaka/erlang-github/issues/2)

- Implement GitHub API v3 [\#1](https://github.com/inaka/erlang-github/issues/1)

**Merged pull requests:**

- \[\#6\] Fix basic auth credential. [\#7](https://github.com/inaka/erlang-github/pull/7) ([jfacorro](https://github.com/jfacorro))

- \[Closes \#1\] Github API v3 [\#5](https://github.com/inaka/erlang-github/pull/5) ([jfacorro](https://github.com/jfacorro))

- \[Closes \#2\] Initial project structure. [\#3](https://github.com/inaka/erlang-github/pull/3) ([jfacorro](https://github.com/jfacorro))



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*