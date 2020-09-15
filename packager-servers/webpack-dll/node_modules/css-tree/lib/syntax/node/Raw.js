var TYPE = require('../../tokenizer').TYPE;

var WHITESPACE = TYPE.Whitespace;
var LEFTPARENTHESIS = TYPE.LeftParenthesis;
var RIGHTPARENTHESIS = TYPE.RightParenthesis;
var LEFTCURLYBRACKET = TYPE.LeftCurlyBracket;
var RIGHTCURLYBRACKET = TYPE.RightCurlyBracket;
var LEFTSQUAREBRACKET = TYPE.LeftSquareBracket;
var RIGHTSQUAREBRACKET = TYPE.RightSquareBracket;

module.exports = {
    name: 'Raw',
    structure: {
        value: String
    },
    parse: function(balanced, endTokenType1, endTokenType2) {
        var start = this.scanner.tokenStart;
        var stack = [];
        var popType = 0;
        var type = 0;

        if (balanced) {
            scan:
            for (var i = 0; type = this.scanner.lookupType(i); i++) {
                if (popType === 0) {
                    if (type === endTokenType1 ||
                        type === endTokenType2) {
                        break scan;
                    }
                }

                switch (type) {
                    case popType:
                        popType = stack.pop();
                        break;

                    case RIGHTPARENTHESIS:
                    case RIGHTCURLYBRACKET:
                    case RIGHTSQUAREBRACKET:
                        if (popType !== 0) {
                            this.scanner.skip(i);
                            this.scanner.error();
                        }
                        break scan;

                    case LEFTPARENTHESIS:
                        stack.push(popType);
                        popType = RIGHTPARENTHESIS;
                        break;

                    case LEFTCURLYBRACKET:
                        stack.push(popType);
                        popType = RIGHTCURLYBRACKET;
                        break;

                    case LEFTSQUAREBRACKET:
                        stack.push(popType);
                        popType = RIGHTSQUAREBRACKET;
                        break;
                }
            }
        } else {
            for (var i = 0; type = this.scanner.lookupType(i); i++) {
                if (type === WHITESPACE ||
                    type === endTokenType1 ||
                    type === endTokenType2) {
                    break;
                }
            }
        }

        this.scanner.skip(i);

        if (popType !== 0) {
            this.scanner.eat(popType);
        }

        return {
            type: 'Raw',
            loc: this.getLocation(start, this.scanner.tokenStart),
            value: this.scanner.substrToCursor(start)
        };
    },
    generate: function(node) {
        return node.value;
    }
};
