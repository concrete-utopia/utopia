## 1.0.0-alpha17 (March 13, 2017)

- Implemented new concept of `syntax`
    - Changed main `exports` to expose a default syntax
    - Defined initial [CSS syntax](lib/syntax/default.js)
    - Implemented `createSyntax()` method to create a new syntax from scratch
    - Implemented `fork()` method to create a new syntax based on given via extension
- Parser
    - Implemented `mediaQueryList` and `mediaQuery` parsing contexts
    - Implemented `CDO` and `CDC` node types
    - Implemented additional declaration property prefix hacks (`#` and `+`)
    - Added support for UTF-16LE BOM
    - Added support for `@font-face` at-rule
    - Added `chroma()` to legacy IE filter functions
    - Improved `HexColor` to consume hex only
    - Improved support for `\0` and `\9` hacks (#2)
    - Relaxed number check for `Ratio` terms
        - Allowed fractal values as a `Ratio` term
        - Disallowed zero number as a `Ratio` term
    - Changed important clause parsing
        - Allowed any identifier for important (to support hacks like `!ie`)
        - Store `true` for `important` field in case identifier equals to `important` and string otherwise
    - Fixed parse error formatted message rendering to take into account tabs
    - Removed exposing of `Parser` class
    - Removed `readSelectorSequence()`, `readSequenceFallback()` and `readSelectorSequenceFallback` methods
    - Used single universal sequence consumer for `AtruleExpression`, `Selector` and `Value`
- Generator
    - Reworked generator to use auto-generated functions based on syntax definition (additional work to be done in next releases)
    - Implemented `translateMarkup(ast, before, after)` method for complex cases
    - Reworked `translateWithSourceMap` to be more flexible (based on `translateMarkup`, additional work to be done in next releases)
- Walker
    - Reworked walker to use auto-generated function based on syntax definition (additional work to be done in next releases)
- Lexer
    - Prepared for better extensibility (additional work to be done in next releases)
    - Implemented `checkStructure(ast)` method to check AST structure based on syntax definition
    - Update syntax dictionaries to latest `mdn/data`
        - Add missing `<'offset-position'>` syntax
        - Extended `<position>` property with `-webkit-sticky` (@sergejmueller, #37)
    - Improved mismatch error position
- Implemented script (`gen:syntax`) to generate AST format reference page (`docs/ast.md`) using syntax definition

## 1.0.0-alpha16 (February 12, 2017)

- Exposed `Parser` class
- Added `startOffset` option to `Tokenizer` (constructor and `setSource()` method)
- Added fallback functions for default (`readSequenceFallback`) and selector (`readSelectorSequenceFallback`) sequence readers
- Fixed edge cases for `AnPlusB`
- Fixed wrong whitespace ignoring in `Selector` consumer

## 1.0.0-alpha15 (February 8, 2017)

- Fixed broken `atruleExpression` context
- Fixed vendor prefix detection in `keyword()` and `property()`
- Fixed `property()` to not lowercase custom property names
- Added `variable` boolean flag in `property()` result
- Renamed `scanner` into `tokenizer`
- Ranamed `syntax` into `lexer`
- Moved `docs/*.html` files to [csstree/docs](https://github.com/csstree/docs) repo
- Added `element()` function for `Value` context (`-moz-element()` supported as well)
- Merged `Universal` node type into `Type`
- Renamed node types:
    - `Id` -> `IdSelector`
    - `Class` -> `ClassSelector`
    - `Type` -> `TypeSelector`
    - `Attribute` -> `AttributeSelector`
    - `PseudoClass` -> `PseudoClassSelector`
    - `PseudoElement` -> `PseudoElementSelector`
    - `Hash` -> `HexColor`
    - `Space` -> `WhiteSpace`
    - `An+B` -> `AnPlusB`
- Removed `Progid` node type
- Relaxed `MediaQuery` consumer to not validate syntax on parse and to include whitespaces in children sequence as is
- Added `WhiteSpace.value` property to store whitespace sequence
- Implemented parser options to specify what should be parsed in details (when option is `false` some part of CSS represents as balanced `Raw`):
    - `parseAtruleExpression` – to parse at-rule expressions (`true` by default)
    - `parseSelector` – to parse rule's selector (`true` by default)
    - `parseValue` - to parse declaration's value (`true` by default)
    - `parseCustomProperty` – to parse value and fallback of custom property (`false` by default)
- Changed tokenization to stick leading hyphen minus to identifier token
- Changed selector parsing:
    - Don't convert spaces into descendant combinator
    - Don't validate selector structure on parsing (selectors may be checked by lexer later)
- Initial refactoring of [docs](https://github.com/csstree/csstree/blob/master/docs)
- Various improvements and fixes

## 1.0.0-alpha14 (February 3, 2017)

- Implemented `DeclarationList`, `MediaQueryList`, `MediaQuery`, `MediaFeature` and `Ratio` node types
- Implemented `declarationList` context (useful to parse HTML `style` attribute content)
- Implemented custom consumers for `@import`, `@media`, `@page` and `@supports` at-rules
- Implemented `atrule` option for `parse()` config, is used for `atruleExpession` context to specify custom consumer for at-rule if any
- Added `Scanner#skipWS()`, `Scanner#eatNonWS()`, `Scanner#consume()` and `Scanner#consumeNonWS()` helper methods
- Added custom consumers for known functional-pseudos, consume unknown functional-pseudo content as balanced `Raw`
- Allowed any `PseudoElement` to be a functional-pseudo (#33)
- Improved walker implementations to reduce GC thrashing by reusing cursors
- Changed `Atrule.block` to contain a `Block` node type only if any
- Changed `Block.loc` positions to include curly brackets
- Changed `Atrule.expression` to store a `null` if no expression
- Changed parser to use `StyleSheet` node type only for top level node (when context is `stylesheet`, that's by default)
- Changed `Parentheses`, `Brackets` and `Function` consumers to use passed sequence reader instead of its own
- Changed `Value` and `AtruleExpression` consumers to use common sequence reader (that reader was used by `Value` consumer before)
- Changed default sequence reader to exclude storage of spaces around `Comma`
- Changed processing of custom properties:
    - Consume declaration value as balanced `Raw`
    - Consume `var()` fallback value as balanced `Raw`
    - Validate first argument of `var()` starts with double dash
    - Custom property's value and fallback includes spaces around
- Fixed `Nth` to have a `loc` property
- Fixed `SelectorList.loc` and `Selector.loc` positions to exclude spaces
- Fixed issue Browserify build fail with `default-syntax.json` is not found error (#32, @philschatz)
- Disallowed `Type` selector starting with dash (parser throws an error in this case now)
- Disallowed empty selectors for `Rule` (not sure if it's correct but looks reasonable)
- Removed `>>` combinator support until any browser support (no signals about that yet)
- Removed `PseudoElement.legacy` property
- Removed special case for `:before`, `:after`, `:first-letter` and `:first-line` to represent them as `PseudoElement`, now those pseudos are represented as `PseudoClass` nodes
- Removed deprecated `Syntax#match()` method
- Parser was splitted into modules and related changes, one step closer to an extensible parser
- Various fixes and improvements, all changes have negligible impact on performance

## 1.0.0-alpha13 (January 19, 2017)

- Changed location storing in `SyntaxMatchError`
    - Changed property to store mismatch offset to `mismatchOffset`
    - Changed `offset` property to store bad node offset in source CSS if any
    - Added `loc` property that stores bad node `loc` if any

## 1.0.0-alpha12 (January 19, 2017)

- Fixed `Syntax#matchProperty()` method to always return a positive result for custom properties since syntax is never defined for them (#31)
- Implemented `fromPlainObject()` and `toPlainObject()` to convert plain object to AST or AST to plain object (currently converts `List` <-> `Array`)

## 1.0.0-alpha11 (January 18, 2017)

- Added support for `:matches(<selector-list>)` (#28)
- Added support for `:has(<relative-selector-list>)`
- Added support for `::slotted(<compound-selector>)`
- Implemented `Brackets` node type
- Implemented basic support for at-rule inside rule block (#24)
- Renamed `Selector` node type to `SelectorList`
- Renamed `SimpleSelector` node type to `Selector`
- Renamed `UnicodeRange.name` property to `UnicodeRange.value`
- Replaced `Negation` node type for regular `PseudoClass`
- Unified name of node property to store nested nodes, it always `children` now:
    - `StyleSheet.rules` -> `StyleSheet.children`
    - `SelectorList.selectors` -> `SelectorList.children`
    - `Block.declarations` -> `Block.children`
    - `*.sequence` -> `*.children`
- Fixed edge cases in parsing `Hex` and `UnicodeRange` when number not an integer
- Changed `nth-` pseudos parsing
    - Implemented `An+B` node type to represent expressions like `2n + 1` or `-3n`
    - Fixed edge cases when `a` or `b` is not an integer
    - Changed `odd` and `even` keywords processing, keywords are storing as `Identifier` node type now
    - Changed `Nth` node type format to store a `nth`-query and an optional `selector`
    - Implemented `of` clause for `nth-` pseudos (a.e. `:nth-child(2n + 1 of li, img)`)
    - Limited `Nth` parsing rules to `:nth-child()`, `:nth-last-child()`, `:nth-of-type()` and `:nth-last-of-type()` pseudos
- Changed the way to store locations
    - Renamed `info` node property to `loc`
    - Changed format of `loc` to store `start` and `end` positions

## 1.0.0-alpha10 (January 11, 2017)

- Reworked `Scanner` to be a single point to its functionality
- Exposed `Scanner` class to be useful for external projects
- Changed `walk()` function behaviour to traverse AST nodes in natural order
- Implemented `walkUp()` function to traverse AST nodes from deepest to parent (behaves as `walk()` before)

## 1.0.0-alpha9 (December 21, 2016)

- Fixed `<angle>` generic according to specs that allow a `<number>` equals to zero to be used as valid value (#30)

## 1.0.0-alpha8 (November 11, 2016)

- Fixed `Scanner#skip()` issue method when cursor is moving to the end of source
- Simplified `Progid` node
- Changed behaviour for bad selector processing, now parsing fails instead of selector ignoring
- Fixed `<id-selector>` generic syntax
- Added `q` unit for `<length>` generic syntax
- Refactored syntax parser (performance)
- Reduced startup time by implementing lazy syntax parsing (default syntax doesn't parse on module load)
- Updated syntax dictionaries and used [`mdn/data`](https://github.com/mdn/data) instead of `Template:CSSData`
- Renamed `syntax.stringify()` method to `syntax.translate()`
- Simplified generic syntax functions, those functions receive a single AST node for checking and should return `true` or `false`
- Added exception for values that contains `var()`, those values are always valid for now
- Added more tests and increase code coverage to `98.5%`

## 1.0.0-alpha7 (October 7, 2016)

- Added support for explicit descendant combinator (`>>`)
- Implemented `Type` and `Universal` type nodes
- Improved `Number` parsing by including sign and exponent (#26)
- Parse `before`, `after`, `first-letter` and `first-line` pseudos with single colon as `PseudoElement`
- Changed `FunctionalPseudo` node type to `PseudoClass`
- Fixed attribute selector name parsing (namespace edge cases)
- Fixed location calculation for specified offset when `eof` is reached
- Added more non-standard colors (#25)
- Removed obsolete `Syntax#getAll()` method
- Fixed various edge cases, code clean up and performance improvements

## 1.0.0-alpha6 (September 23, 2016)

- More accurate positions for syntax mismatch errors
- Added [`apple`](https://webkit.org/blog/3709/using-the-system-font-in-web-content/) specific font keywords (#20)
- Changed `Property` node stucture from object to string
- Renamed `Ruleset` node type to `Rule`
- Removed `Argument` node type
- Fixed `Dimension` and `Percentage` position computation
- Fixed bad selector parsing (temporary solution)
- Fixed location computation for CSS with very long lines that may lead to really long parsing with `positions:true` (even freeze)
- Fixed `line` and `column` computation for `SyntaxMatch` error
- Improved performance of parsing and translation. Now CSSTree is under 10ms in [PostCSS benchmark](https://github.com/postcss/benchmark).
