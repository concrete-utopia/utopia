var List = require('../../utils/list');
var TYPE = require('../../tokenizer').TYPE;

var STRING = TYPE.String;
var IDENTIFIER = TYPE.Identifier;
var LEFTPARENTHESIS = TYPE.LeftParenthesis;

module.exports = {
    parse: {
        expression: function() {
            var children = new List();

            this.scanner.skipSC();

            switch (this.scanner.tokenType) {
                case STRING:
                    children.appendData(this.String());
                    break;

                case IDENTIFIER:
                    children.appendData(this.Url());
                    break;

                default:
                    this.scanner.error('String or url() is expected');
            }

            if (this.scanner.lookupNonWSType(0) === IDENTIFIER ||
                this.scanner.lookupNonWSType(0) === LEFTPARENTHESIS) {
                children.appendData(this.WhiteSpace());
                children.appendData(this.MediaQueryList());
            }

            return children;
        },
        block: false
    }
};
