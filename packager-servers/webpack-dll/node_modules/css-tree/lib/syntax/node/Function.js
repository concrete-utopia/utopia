var TYPE = require('../../tokenizer').TYPE;

var IDENTIFIER = TYPE.Identifier;
var LEFTPARENTHESIS = TYPE.LeftParenthesis;
var RIGHTPARENTHESIS = TYPE.RightParenthesis;

// ident '(' <sequence> ')'
module.exports = {
    name: 'Function',
    structure: {
        name: String,
        children: [[]]
    },
    parse: function(readSequence, recognizer) {
        var start = this.scanner.tokenStart;
        var name = this.scanner.consume(IDENTIFIER);
        var nameLowerCase = name.toLowerCase();
        var children;

        this.scanner.eat(LEFTPARENTHESIS);

        children = recognizer.hasOwnProperty(nameLowerCase)
            ? recognizer[nameLowerCase].call(this, recognizer)
            : readSequence.call(this, recognizer);

        this.scanner.eat(RIGHTPARENTHESIS);

        return {
            type: 'Function',
            loc: this.getLocation(start, this.scanner.tokenStart),
            name: name,
            children: children
        };
    },
    generate: function(node) {
        return [].concat(node.name, '(', this.each(node.children), ')');
    },
    walkContext: 'function'
};
