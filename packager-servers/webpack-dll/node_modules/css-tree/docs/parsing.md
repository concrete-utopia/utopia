# Parsing CSS into AST

> NOTE: Currenly parser omits redundant separators, spaces and comments (except exclamation comments, i.e. `/*! comment */`) on AST build.

## parse(source[, options])

Parses CSS into AST.

```js
// simple parsing with no options
var ast = csstree.parse('.example { color: red }');

// parse with options
var ast = csstree.parse('.foo.bar', {
    context: 'selector',
    positions: true
});
```

Options (optional):

<!-- MarkdownTOC -->

- [context](#context)
- [atrule](#atrule)
- [property](#property)
- [positions](#positions)
- [filename](#filename)
- [offset](#offset)
- [line](#line)
- [column](#column)
- [parseAtruleExpression](#parseatruleexpression)
- [parseSelector](#parseselector)
- [parseValue](#parsevalue)
- [parseCustomProperty](#parsecustomproperty)

<!-- /MarkdownTOC -->

### context

Type: `string`  
Default: `'stylesheet'`

Defines what part of CSS is parsing.

Contexts:

- `stylesheet` (default) – regular stylesheet, should be suitable in most cases
- `atrule` – at-rule (e.g. `@media screen, print { ... }`)
- `atruleExpression` – at-rule expression (`screen, print` for example above)
- `rule` – rule (e.g. `.foo, .bar:hover { color: red; border: 1px solid black; }`)
- `selectorList` – selector group (`.foo, .bar:hover` for rule example)
- `selector` – selector (`.foo` or `.bar:hover` for rule example)
- `block` – block with curly braces (`{ color: red; border: 1px solid black; }` for rule example)
- `declarationList` – block content w/o curly braces (`color: red; border: 1px solid black;` for rule example), useful for parsing HTML `style` attribute value
- `declaration` – declaration (`color: red` or `border: 1px solid black` for rule example)
- `value` – declaration value (`red` or `1px solid black` for rule example)

### atrule

Type: `string` or `null`  
Default: `null`

Using for `atruleExpression` context to apply some atrule specific parse rules

### property

Type: `string` or `null`  
Default: `null`

Using for `value` context to apply some property specific parse rules.

### positions

Type: `boolean`  
Default: `false`

Specify to store location of node content in source. Location is storing `loc` property of nodes. `loc` property is always `null` when option is `false`.

### filename

Type: `string`  
Default: `'<unknown>'`

Filename of source. This value adds to `loc` as `source` property when `positions` option is `true`. Using for source map generation.

### offset

Type: `number`  
Default: `0`

Start offset. Useful when parsing fragment of CSS to store correct positions in node's `loc` property.

### line

Type: `number`  
Default: `1`

Start line number. Useful when parsing fragment of CSS to store correct positions in node's `loc` property.

### column

Type: `number`  
Default: `1`

Start column number. Useful when parsing fragment of CSS to store correct positions in node's `loc` property.

### parseAtruleExpression

Type: `boolean`  
Default: `true`

Defines to parse a at-rule expression in details (represents as `AtruleExpresion`, `MediaQueryList` or `SelectorList` if any). Otherwise represents expression as `Raw` node.

```js
cstree.parse('@example 1 2;');
// {
//     "type": "Atrule",
//     "expression": {
//         "type": "AtruleExpression",
//         "children": [
//             { "type": "Number", "value": "1" },
//             { "type": "WhiteSpace", "value": " " },
//             { "type": "Number", "value": "2" }
//         ]
//     },
//     "block": null
// }

cstree.parse('@example 1 2;', { parseAtruleExpression: false });
// {
//     "type": "Atrule",
//     "expression": {
//         "type": "Raw",
//         "value": "1 2"
//     },
//     "block": null
// }
```

### parseSelector

Type: `boolean`  
Default: `true`

Defines to parse a rule selector in details (represents as `SelectorList`). Otherwise represents selector as `Raw` node.

```js
cstree.parse('.foo {}');
// {
//     "type": "Rule",
//     "selector": {
//         "type": "SelectorList",
//         "children": [
//             {
//                 "type": "Selector",
//                 "children": [
//                     { "type": "ClassSelector", "name": "foo" }
//                 ]
//             }
//         ]
//     },
//     "block": {
//         "type": "Block",
//         "children": []
//     }
// }

cstree.parse('.foo {}', { parseSelector: false });
// {
//     "type": "Rule",
//     "selector": {
//         "type": "Raw",
//         "value": ".foo "
//     },
//     "block": {
//         "type": "Block",
//         "children": []
//     }
// }
```

### parseValue

Type: `boolean`  
Default: `true`

Defines to parse a declaration value in details (represents as `Value`). Otherwise represents value as `Raw` node.

```js
cstree.parse('color: #aabbcc', { context: 'declaration' });
// {
//     "type": "Declaration",
//     "important": false,
//     "property": "color",
//     "value": {
//         "type": "Value",
//         "children": [
//             {
//                 "type": "HexColor",
//                 "value": "aabbcc"
//             }
//         ]
//     }
// }

cstree.parse('color: #aabbcc', { context: 'declaration', parseValue: false });
// {
//     "type": "Declaration",
//     "important": false,
//     "property": "color",
//     "value": {
//         "type": "Raw",
//         "value": " #aabbcc"
//     }
// }
```

### parseCustomProperty

Type: `boolean`  
Default: `false`

Defines to parse a custom property value and a `var()` fallback in details (represents as `Value`). Otherwise represents value as `Raw` node.

```js
cstree.parse('--custom: #aabbcc', { context: 'declaration' });
// {
//     "type": "Declaration",
//     "important": false,
//     "property": "--custom",
//     "value": {
//         "type": "Raw",
//         "value": " #aabbcc"
//     }
// }

cstree.parse('--custom: #aabbcc', { context: 'declaration', parseCustomProperty: true });
// {
//     "type": "Declaration",
//     "important": false,
//     "property": "--custom",
//     "value": {
//         "type": "Value",
//         "children": [
//             {
//                 "type": "HexColor",
//                 "value": "aabbcc"
//             }
//         ]
//     }
// }
```
