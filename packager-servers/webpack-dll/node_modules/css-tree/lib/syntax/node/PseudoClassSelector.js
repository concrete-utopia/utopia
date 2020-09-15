var List = require('../../utils/list');
var TYPE = require('../../tokenizer').TYPE;

var IDENTIFIER = TYPE.Identifier;
var COLON = TYPE.Colon;
var LEFTPARENTHESIS = TYPE.LeftParenthesis;
var RIGHTPARENTHESIS = TYPE.RightParenthesis;
var BALANCED = true;

// : ident [ '(' .. ')' ]?
module.exports = {
    name: 'PseudoClassSelector',
    structure: {
        name: String,
        children: [['Raw'], null]
    },
    parse: function() {
        var start = this.scanner.tokenStart;
        var name;
        var children = null;

        this.scanner.eat(COLON);

        name = this.scanner.consume(IDENTIFIER);

        if (this.scanner.tokenType === LEFTPARENTHESIS) {
            var nameLowerCase = name.toLowerCase();

            this.scanner.next();

            if (this.pseudo.hasOwnProperty(nameLowerCase)) {
                this.scanner.skipSC();
                children = this.pseudo[nameLowerCase].call(this);
                this.scanner.skipSC();
            } else {
                children = new List().appendData(this.Raw(BALANCED, 0, 0));
            }

            this.scanner.eat(RIGHTPARENTHESIS);
        }

        return {
            type: 'PseudoClassSelector',
            loc: this.getLocation(start, this.scanner.tokenStart),
            name: name,
            children: children
        };
    },
    generate: function(node) {
        return node.children !== null
            ? [].concat(':' + node.name + '(', this.each(node.children), ')')
            : ':' + node.name;
    },
    walkContext: 'function'
};
