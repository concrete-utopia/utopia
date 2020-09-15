'use strict';

function isTokenType(token/*, type, type*/) {
    if (token) {
        for (var i = 1; i < arguments.length; i++) {
            if (token.type === arguments[i]) {
                return true;
            }
        }
    }

    return false;
}

function serializeMultiplier(token) {
    if (token.min === 0 && token.max === 1) {
        return '?';
    }

    if (token.min === 1 && token.max === 1) {
        return '';
    }

    if (token.min === 0 && token.max === 0) {
        return '*';
    }

    if (token.min === 1 && token.max === 0) {
        return token.comma ? '#' : '+';
    }

    return (
        (token.comma ? '#' : '') +
        '{' + token.min + (token.min !== token.max ? ',' + (token.max !== 0 ? token.max : '') : '') + '}'
    );
}

function translateSequence(token, implicitBraces) {
    var result = '';

    if (token.type === 'Group' || implicitBraces) {
        result += '[' + (!isTokenType(token.terms[0], 'Comma') ? ' ' : '');
    }

    result += token.terms.map(function(term) {
        return translate(term, implicitBraces);
    }).join(token.combinator === ' ' ? ' ' : ' ' + token.combinator + ' ');

    if (token.type === 'Group' || implicitBraces) {
        result += ' ]';
    }

    return result;
}

function translateParentheses(sequence, implicitBraces) {
    if (!sequence.terms.length) {
        return '()';
    }

    return '( ' + translateSequence(sequence, implicitBraces) + ' )';
}

function translate(token, implicitBraces) {
    if (Array.isArray(token)) {
        return token.map(function(item) {
            return translate(item, implicitBraces);
        }).join('');
    }

    switch (token.type) {
        case 'Sequence':
            return translateSequence(token, implicitBraces);

        case 'Group':
            return (
                translateSequence(token, implicitBraces) +
                (token.nonEmpty ? '!' : '') +
                serializeMultiplier(token.multiplier)
            );

        case 'Keyword':
            return token.name + serializeMultiplier(token.multiplier);

        case 'Function':
            return token.name + translateParentheses(token.sequence, implicitBraces) + serializeMultiplier(token.multiplier);

        case 'Parentheses': // replace for seq('(' seq(...token.sequence) ')')
            return translateParentheses(token.sequence, implicitBraces);

        case 'Type':
            return '<' + token.name + '>' + serializeMultiplier(token.multiplier);

        case 'Property':
            return '<\'' + token.name + '\'>' + serializeMultiplier(token.multiplier);

        case 'Combinator': // remove?
        case 'Slash':      // replace for String? '/'
        case 'Percent':    // replace for String? '%'
        case 'String':
        case 'Comma':
            return token.value;

        default:
            throw new Error('Unknown type: ' + token.type);
    }
}

module.exports = translate;
