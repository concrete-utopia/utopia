var cmpChar = require('../../tokenizer').cmpChar;
var TYPE = require('../../tokenizer').TYPE;

var IDENTIFIER = TYPE.Identifier;
var STRING = TYPE.String;
var NUMBER = TYPE.Number;
var NUMBERSIGN = TYPE.NumberSign;
var LEFTPARENTHESIS = TYPE.LeftParenthesis;
var LEFTSQUAREBRACKET = TYPE.LeftSquareBracket;
var PLUSSIGN = TYPE.PlusSign;
var HYPHENMINUS = TYPE.HyphenMinus;
var COMMA = TYPE.Comma;
var SOLIDUS = TYPE.Solidus;
var ASTERISK = TYPE.Asterisk;
var PERCENTSIGN = TYPE.PercentSign;
var BACKSLASH = TYPE.Backslash;
var U = 117; // 'u'.charCodeAt(0)

module.exports = function defaultRecognizer(context) {
    switch (this.scanner.tokenType) {
        case NUMBERSIGN:
            return this.HexColor();

        case COMMA:
            context.space = null;
            context.ignoreWSAfter = true;
            return this.Operator();

        case SOLIDUS:
        case ASTERISK:
        case PLUSSIGN:
        case HYPHENMINUS:
            return this.Operator();

        case LEFTPARENTHESIS:
            return this.Parentheses(this.readSequence, context.recognizer);

        case LEFTSQUAREBRACKET:
            return this.Brackets(this.readSequence, context.recognizer);

        case STRING:
            return this.String();

        case NUMBER:
            switch (this.scanner.lookupType(1)) {
                case PERCENTSIGN:
                    return this.Percentage();

                case IDENTIFIER:
                    // edge case: number with folowing \0 and \9 hack shouldn't to be a Dimension
                    if (cmpChar(this.scanner.source, this.scanner.tokenEnd, BACKSLASH)) {
                        return this.Number();
                    } else {
                        return this.Dimension();
                    }

                default:
                    return this.Number();
            }

        case IDENTIFIER:
            // check for unicode range, it should start with u+ or U+
            if (cmpChar(this.scanner.source, this.scanner.tokenStart, U) &&
                cmpChar(this.scanner.source, this.scanner.tokenStart + 1, PLUSSIGN)) {
                return this.UnicodeRange();
            } else if (this.scanner.lookupType(1) === LEFTPARENTHESIS) {
                if (this.scanner.lookupValue(0, 'url')) {
                    return this.Url();
                } else {
                    return this.Function(this.readSequence, context.recognizer);
                }
            } else {
                return this.Identifier();
            }
    }
};
