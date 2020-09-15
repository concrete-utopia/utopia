'use strict';

// token types (note: value shouldn't intersect with used char codes)
var WHITESPACE = 1;
var IDENTIFIER = 2;
var NUMBER = 3;
var STRING = 4;
var COMMENT = 5;
var PUNCTUATOR = 6;
var CDO = 7;
var CDC = 8;

var TAB = 9;
var N = 10;
var F = 12;
var R = 13;
var SPACE = 32;

var TYPE = {
    Whitespace:   WHITESPACE,
    Identifier:   IDENTIFIER,
    Number:           NUMBER,
    String:           STRING,
    Comment:         COMMENT,
    Punctuator:   PUNCTUATOR,
    CDO:                 CDO,
    CDC:                 CDC,

    ExclamationMark:      33,  // !
    QuotationMark:        34,  // "
    NumberSign:           35,  // #
    DollarSign:           36,  // $
    PercentSign:          37,  // %
    Ampersand:            38,  // &
    Apostrophe:           39,  // '
    LeftParenthesis:      40,  // (
    RightParenthesis:     41,  // )
    Asterisk:             42,  // *
    PlusSign:             43,  // +
    Comma:                44,  // ,
    HyphenMinus:          45,  // -
    FullStop:             46,  // .
    Solidus:              47,  // /
    Colon:                58,  // :
    Semicolon:            59,  // ;
    LessThanSign:         60,  // <
    EqualsSign:           61,  // =
    GreaterThanSign:      62,  // >
    QuestionMark:         63,  // ?
    CommercialAt:         64,  // @
    LeftSquareBracket:    91,  // [
    Backslash:            92,  // \
    RightSquareBracket:   93,  // ]
    CircumflexAccent:     94,  // ^
    LowLine:              95,  // _
    GraveAccent:          96,  // `
    LeftCurlyBracket:    123,  // {
    VerticalLine:        124,  // |
    RightCurlyBracket:   125,  // }
    Tilde:               126   // ~
};

var NAME = Object.keys(TYPE).reduce(function(result, key) {
    result[TYPE[key]] = key;
    return result;
}, {});

var SafeUint32Array = typeof Uint32Array !== 'undefined' ? Uint32Array : Array; // fallback on Array when TypedArray is not supported
var SYMBOL_TYPE = new SafeUint32Array(Math.max.apply(null, Object.keys(NAME).map(Number)) + 1);
var PUNCTUATION = new SafeUint32Array(SYMBOL_TYPE.length);

for (var i = 0; i < SYMBOL_TYPE.length; i++) {
    SYMBOL_TYPE[i] = IDENTIFIER;
}

// fill categories
[
    TYPE.ExclamationMark,    // !
    TYPE.QuotationMark,      // "
    TYPE.NumberSign,         // #
    TYPE.DollarSign,         // $
    TYPE.PercentSign,        // %
    TYPE.Ampersand,          // &
    TYPE.Apostrophe,         // '
    TYPE.LeftParenthesis,    // (
    TYPE.RightParenthesis,   // )
    TYPE.Asterisk,           // *
    TYPE.PlusSign,           // +
    TYPE.Comma,              // ,
    TYPE.HyphenMinus,        // -
    TYPE.FullStop,           // .
    TYPE.Solidus,            // /
    TYPE.Colon,              // :
    TYPE.Semicolon,          // ;
    TYPE.LessThanSign,       // <
    TYPE.EqualsSign,         // =
    TYPE.GreaterThanSign,    // >
    TYPE.QuestionMark,       // ?
    TYPE.CommercialAt,       // @
    TYPE.LeftSquareBracket,  // [
    // TYPE.Backslash,          // \
    TYPE.RightSquareBracket, // ]
    TYPE.CircumflexAccent,   // ^
    // TYPE.LowLine,            // _
    TYPE.GraveAccent,        // `
    TYPE.LeftCurlyBracket,   // {
    TYPE.VerticalLine,       // |
    TYPE.RightCurlyBracket,  // }
    TYPE.Tilde               // ~
].forEach(function(key) {
    SYMBOL_TYPE[Number(key)] = PUNCTUATOR;
    PUNCTUATION[Number(key)] = PUNCTUATOR;
}, SYMBOL_TYPE);

for (var i = 48; i <= 57; i++) {
    SYMBOL_TYPE[i] = NUMBER;
}

SYMBOL_TYPE[SPACE] = WHITESPACE;
SYMBOL_TYPE[TAB] = WHITESPACE;
SYMBOL_TYPE[N] = WHITESPACE;
SYMBOL_TYPE[R] = WHITESPACE;
SYMBOL_TYPE[F] = WHITESPACE;

SYMBOL_TYPE[TYPE.Apostrophe] = STRING;
SYMBOL_TYPE[TYPE.QuotationMark] = STRING;

// whitespace is punctuation ...
PUNCTUATION[SPACE] = PUNCTUATOR;
PUNCTUATION[TAB] = PUNCTUATOR;
PUNCTUATION[N] = PUNCTUATOR;
PUNCTUATION[R] = PUNCTUATOR;
PUNCTUATION[F] = PUNCTUATOR;
// ... hyper minus is not
PUNCTUATION[TYPE.HyphenMinus] = 0;

module.exports = {
    TYPE: TYPE,
    NAME: NAME,

    SYMBOL_TYPE: SYMBOL_TYPE,
    PUNCTUATION: PUNCTUATION
};
