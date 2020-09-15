'use strict';

var SyntaxParseError = function(message, syntaxStr, offset) {
    var error = new SyntaxError();
    error.name = 'SyntaxParseError';
    error.rawMessage = message;
    error.syntax = syntaxStr;
    error.offset = offset;
    error.message = error.rawMessage + '\n' +
        '  ' + error.syntax + '\n' +
        '--' + new Array((error.offset || error.syntax.length) + 1).join('-') + '^';

    return error;
};

module.exports = {
    SyntaxParseError: SyntaxParseError
};
