var TYPE = require('../../tokenizer').TYPE;
var LEFTCURLYBRACKET = TYPE.LeftCurlyBracket;

module.exports = {
    parse: {
        expression: function() {
            if (this.scanner.lookupNonWSType(0) === LEFTCURLYBRACKET) {
                return null;
            }

            return this.SelectorList();
        },
        block: function() {
            return this.Block(this.Declaration);
        }
    }
};
