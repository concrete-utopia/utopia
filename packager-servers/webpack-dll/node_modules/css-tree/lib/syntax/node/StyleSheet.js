var List = require('../../utils/list');
var TYPE = require('../../tokenizer').TYPE;

var WHITESPACE = TYPE.Whitespace;
var COMMENT = TYPE.Comment;
var EXCLAMATIONMARK = TYPE.ExclamationMark;
var COMMERCIALAT = TYPE.CommercialAt;
var CDO = TYPE.CDO;
var CDC = TYPE.CDC;

module.exports = {
    name: 'StyleSheet',
    structure: {
        children: [['Comment', 'Atrule', 'Rule']]
    },
    parse: function() {
        var start = this.scanner.tokenStart;
        var children = new List();
        var child;

        scan:
        while (!this.scanner.eof) {
            switch (this.scanner.tokenType) {
                case WHITESPACE:
                    this.scanner.next();
                    continue;

                case COMMENT:
                    // ignore comments except exclamation comments (i.e. /*! .. */) on top level
                    if (this.scanner.source.charCodeAt(this.scanner.tokenStart + 2) !== EXCLAMATIONMARK) {
                        this.scanner.next();
                        continue;
                    }

                    child = this.Comment();
                    break;

                case COMMERCIALAT:
                    child = this.Atrule();
                    break;

                case CDO: // <!--
                    child = this.CDO();
                    break;

                case CDC: // -->
                    child = this.CDC();
                    break;

                default:
                    child = this.Rule();
            }

            children.appendData(child);
        }

        return {
            type: 'StyleSheet',
            loc: this.getLocation(start, this.scanner.tokenStart),
            children: children
        };
    },
    generate: function(node) {
        return this.each(node.children);
    },
    walkContext: 'stylesheet'
};
