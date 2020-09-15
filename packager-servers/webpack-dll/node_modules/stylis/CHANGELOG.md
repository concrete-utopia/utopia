## 3.1.5 June 09, 2017

- improve cascade isolation mode
- improve inverse selector detection
- improve monkey-patching invalid css patterns

## 3.1.4 June 09, 2017

- support monkey-patching some common invalid css patterns

## 3.1.3 June 08, 2017

- patch inverse selector `html &:after` 

## 3.1.2 June 08, 2017

- escape user authored control characters that break browser vendor parsers `\0`, `\f`, `\v`
- improve `::placeholder` detection

## 3.1.1 June 08, 2017

- middleware, patch: allow middleware to monkey-patch return value

## 3.1.0 June 08, 2017

- middleware, patch multiple chained middlewares
- middleware, allow `post` context to affect the output
- middleware, pass stylis to plugins as `this` reference for plugins to access
- middleware, allow middleware to monkey-patch return value

## 3.0.19 June 07, 2017

- patch `backface-visibility` vendor prefix

## 3.0.18 June 07, 2017

- patch middleware property context to remove the property when an empty string is returned
- patch middleware preperation and post process context UUID

## 3.0.17 June 07, 2017

- improves auto semicolon insertion
- improve minification with compress option

## 3.0.16 June 05, 2017

- patch ReferenceError `escade` undefined

## 3.0.15 June 02, 2017

- fix a ReferenceError `flex` undefined

## 3.0.14 June 02, 2017

- patch isolation mode `:not()` and `pseudo` selectors
- patch compress option to preserve spaces in `calc(0px + 10px)`

## 3.0.13 June 02, 2017

- patch vendor prefix flexbox `inline-box`

## 3.0.12 May 31, 2017

- patch missing `use strict` directive

## 3.0.11 May 31, 2017

- plugins, patch line and column number to start at `1` instead of `0`

## 3.0.10 May 30, 2017

- patch isolation mode prefixing `nth-child()` functions

## 3.0.9 May 30, 2017

- improve invalid keyframe namespace characters

## 3.0.8 May 30, 2017

- patch keyframe namespacing to handle more generic attribute expressions

## 3.0.7 May 30, 2017

- patch keyframe namespacing to not include extra characters with valid namespaces

## 3.0.6 May 29, 2017

- patch `@media` rule namespacing in cascade isolation mode

## 3.0.5 May 28, 2017

- patch isolation mode `#id` and `.class` children
- patch one character properties with no-semicolon mode ex. `x: value`
- patch removal of universal selector `*`
- improve no-semicolon detection when the last character is a `)`

## 3.0.4 May 25, 2017

- improve `@keyframes` scoping to support `number` identifiers

## 3.0.3 May 25, 2017

- patch css isolation mode `@keyframes`

## 3.0.2 May 25, 2017

- improve `::placeholder` prefixing to patch mispatching cases ex. `:papaya`
- patch parsing cascade isolation orphaned `&` operator
- patch issue with closure minification

## 3.0.1 May 25, 2017

- patch `mangling` keyframes without a name/correct whitespace
- patch duplicate `;` in cascade-less mode
- patch `webkit` vendor prefix for `align-self`

## 3.0.0 May 24, 2017

- slightly smaller file size (3.3kb –> ~2.9kb)
- support no semi colons (on by default)
- improved plugin support
- smaller output
- improves on settings options
- added cascade isolation mode (off by default)
- added aggressive compression mode (off by default)
- added an option to disable vendor prefixing

#### API

##### Plugins

Registering plugins has changed slightly and is solely through.

```js
stylis.use(Function|Array<Function>|null)
```

##### Options

Setting options has moved from arguments passed to `stylis(selector, css, opts...)` to
a dedicated setter.

```js
stylis.set({
	global: false,
	keyframe: false
	...
})
```


## 2.0.12 May 18, 2017

- support older webkit vendor prefix `transform` when used in `transition`

## 2.0.11 May 15, 2017)

- support attribute selectors without values ex. `[attribute]`.

## 2.0.10 May 13, 2017)

- patch middleware `@media` block context

## 2.0.9 May 13, 2017)

- patch `:placeholder` in relation to `@media` order

## 2.0.8 May 11, 2017)

- patch regression in selector parsing

## 2.0.7 May 11, 2017)

- patch `regression output @media order`

## 2.0.6 May 11, 2017)

- patch `regression @media parsing`

## 2.0.5 May 10, 2017)

- patch `@supports nesting`

## 2.0.4 April 23, 2017)

- patch `@supports`

## 2.0.3 April 14, 2017)

- vendor prefix `text-size-adjust`

## 2.0.2 April 05, 2017)

- patch whitespace found in a selector namespace ` .class` -> `.class`

## 2.0.1 April 05, 2017)

- patch nested `::placeholder` vendor prefixing

## 2.0.0 April 01, 2017)

- add vendor prefix for `::placeholder`
- add vendor prefix for `justify-content`
- patch vendor prefix for `align-items`
- remove mixins, variables and block level `@global {}`
- improve disabling compact features option
- 3.67KB -> 3.12KB

## 1.2.8 March 26, 2017)

- patch `!important` in `display: flex` vendor prefixing

## 1.2.7 March 25, 2017)

- patch short hand animation name parser

## 1.2.6 March 14, 2017)

- improve vendor prefixer, adds max-content and min-content

## 1.2.5 (March 11, 2017)

- improve last semicolon omission
- improve `:global()` selector parsing

## 1.2.4 (March 9, 2017)

- patch nesting for `:global()` selectors

## 1.2.3 (March 5, 2017)

- patch `:global()` selector

## 1.2.2 (March 5, 2017)

- patch `:global()` selector

## 1.2.1 (March 4, 2017)

- patch for `animation*}` when used as a last property without a semicolon

## 1.2.0 (March 3, 2017)

- patch single apostrophes in comments
- patch nested selector `:global()`
- patch middleware param ignored when `stylis.use` is used to register middlewares
- add new middleware context `ctx = 7` to allow for better linter plugins

## 1.1.13 (February 24, 2017)

- patch nesting in a `@media` block
- patch parent reference selector `selector &`

## 1.1.12 (February 21, 2017)

- patch to better handle `semicolon` omission on the last property of a selector

## 1.1.11 (February 18, 2017)

- patch `:matches(a, b, c)` and simplify `,` token handler

## 1.1.10 (February 16, 2017)

- patch token in selector i.e `.test[title=","]`

## 1.1.9 (February 16, 2017)

- patch arguments passed to middleware current `output` character count `length`

## 1.1.8 (February 16, 2017)

- patch `&` specifity overloaded, i.e `&&, & + &...`

## 1.1.7 (February 16, 2017)

- patch `:global(:not())`, `&:global()` and `:global` in nested block

## 1.1.6 (February 16, 2017)

- patch `@media {}` in nested block
- patch column and line number in nested block

## 1.1.5 (February 16, 2017)

- patch nesting in `@global {}` block

## 1.1.4 (February 16, 2017)

- patch `@global`

## 1.1.3 (February 13, 2017)

- patch `cubic-bezier`

## 1.1.2 (February 13, 2017)

- patch `@font-face {}`

## 1.1.1 (February 11, 2017)

- prefix `cursor: grab|grabbing|zoom-in|zoom-out;`
- allow `&` to be used as specifity multiplier `&&` when used in conjunction

## 1.1.0 (February 10, 2017)

- add middleware context `ctx = 1.5` for every individual selector

## 1.0.10 (February 09, 2017)

- patch block comments

## 1.0.9 (February 09, 2017)

- patch inline-block comments
- follow a consistent property-value format for `display: flex;` prefix

## 1.0.8 (February 08, 2017)

- patch prefix `-ms-flexbox`

## 1.0.7 (February 06, 2017)

- patch selectors in nested block

## 1.0.6 (February 06, 2017)

- improve handling `,` comma tokens in attribute selectors

## 1.0.5 (February 06, 2017)

- patch block comments inlined with selectors
- patch attribute selectors

## 1.0.4 (February 02, 2017)

- patch `&` nested selectors

## 1.0.3 (February 01, 2017)

- patch `inline-flex` property being replaced with `flex`

## 1.0.2 (January 31, 2017)

- patch parsing tokens in urls

## 1.0.1 (January 31, 2017)

- patch urls and data uri's

## 1.0.0 (January 27, 2017)

- improve parsing and namespacing animations names in the `animation:` property
- avoid parsing for animation name when animation namespacing is disabled
- performance improvements
- support adding middleware/plugins with objects via `stylis.use`
- add IE 8 support

## 0.12.0 (January 25, 2017)

- more supported extensions to the `@import` parser.
- finallize middleware lifecycles, adds one before compiling
- add `stylis.use` to register multiple middlewares independently

## 0.11.0 (January 20, 2017)

- patch for @media flat css

## 0.11.0 (January 18, 2017)

- patch flat, nested and import css order
- add new context that executes middleware just before compiled output is returned
- add support for higher level middleware support with middleware objects

## 0.10.0 (January 16, 2017)

- enable compact flag to enable additional features(variables and mixins)
- support nested @media blocks
- patch `&` token useage `html & {}` to produce `html ${namespace} {}`
- seperate tokens passed to middleware into `selector, property, block, flat, imports`

## 0.9.2 (January 06, 2017)

- variables `$foo: ;` -> `~~foo: ;` and `color: var(~~foo);`
- prevent variable declarations in selectors
- improve animation name finder

## 0.9.1 (January 06, 2017)

- patch animation property namespacing

## 0.9.0 (January 06, 2017)

- add support for sass-like mixins
- add support for sass-like variables in string format `$foo: 1; color: $foo;`
- add support for middleware
- patch comments in flat css context
- patch animation property
- patch tokens in strings

## 0.8.3 (December 20, 2016)

- patch `:host` conflict with `:hover`

## 0.8.2 (December 20, 2016)

- better handle line comments
- small improvements to compiler

## 0.8.1 (December 19, 2016)

- patch 0.8.0 regression with @media blocks skipping namespaces

## 0.8.0 (December 19, 2016)

- add nested support `h1 { color: red; &:hover { color: blue; } }`
- add support for flat css in @media block
- improve flat css support
- patch `//` line comments

## 0.7.0 (December 12, 2016)

- add inline `:global()` function, change `@root` to `@global`

## 0.6.8 (December 06, 2016)

- patch `:host...` implementation conflict with `:root...`

## 0.6.7 (December 06, 2016)

- add support for shadow dom selectors `:host, :host(selector), :host-context(selector)`

## 0.6.6 (December 06, 2016)

- reduction to output payload(unused prefixes)

## 0.6.5 (December 06, 2016)

- improvements to handling of line comments
- improvements to parsing performance

## 0.6.4 (December 05, 2016)

- improvements to parsing, do away with the little regex that was used
- handle edge cases with `@keyframes` nested in `@root` block
- support complete prefix support in `@keyframes` and `@root` blocks

## 0.6.3 (December 04, 2016)

- patch `h1, &:before{}` pseudo selectors in multiple selectors

## 0.6.2 (December 04, 2016)

- patch flat css `stylis('#id', 'color:red;')` to act as a block `stylis('#id', '&{color:red;}')`

