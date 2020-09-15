var data = require('../../data');

module.exports = require('./index.js').create({
    generic: true,
    types: data.types,
    properties: data.properties,

    parseContext: {
        default: 'StyleSheet',
        stylesheet: 'StyleSheet',
        atrule: 'Atrule',
        atruleExpression: function(options) {
            return this.AtruleExpression(options.atrule ? String(options.atrule) : null);
        },
        mediaQueryList: 'MediaQueryList',
        mediaQuery: 'MediaQuery',
        rule: 'Rule',
        selectorList: 'SelectorList',
        selector: 'Selector',
        block: function() {
            return this.Block(this.Declaration);
        },
        declarationList: 'DeclarationList',
        declaration: 'Declaration',
        value: function(options) {
            return this.Value(options.property ? String(options.property) : null);
        }
    },
    scope: {
        AtruleExpression: require('./scope/atruleExpression'),
        Selector: require('./scope/selector'),
        Value: require('./scope/value')
    },
    atrule: {
        'font-face': require('./atrule/font-face'),
        'import': require('./atrule/import'),
        'media': require('./atrule/media'),
        'page': require('./atrule/page'),
        'supports': require('./atrule/supports')
    },
    pseudo: {
        'dir': require('./pseudo/dir'),
        'has': require('./pseudo/has'),
        'lang': require('./pseudo/lang'),
        'matches': require('./pseudo/matches'),
        'not': require('./pseudo/not'),
        'nth-child': require('./pseudo/nth-child'),
        'nth-last-child': require('./pseudo/nth-last-child'),
        'nth-last-of-type': require('./pseudo/nth-last-of-type'),
        'nth-of-type': require('./pseudo/nth-of-type'),
        'slotted': require('./pseudo/slotted')
    },
    node: {
        AnPlusB: require('./node/AnPlusB'),
        Atrule: require('./node/Atrule'),
        AtruleExpression: require('./node/AtruleExpression'),
        AttributeSelector: require('./node/AttributeSelector'),
        Block: require('./node/Block'),
        Brackets: require('./node/Brackets'),
        CDC: require('./node/CDC'),
        CDO: require('./node/CDO'),
        ClassSelector: require('./node/ClassSelector'),
        Combinator: require('./node/Combinator'),
        Comment: require('./node/Comment'),
        Declaration: require('./node/Declaration'),
        DeclarationList: require('./node/DeclarationList'),
        Dimension: require('./node/Dimension'),
        Function: require('./node/Function'),
        HexColor: require('./node/HexColor'),
        Identifier: require('./node/Identifier'),
        IdSelector: require('./node/IdSelector'),
        MediaFeature: require('./node/MediaFeature'),
        MediaQuery: require('./node/MediaQuery'),
        MediaQueryList: require('./node/MediaQueryList'),
        Nth: require('./node/Nth'),
        Number: require('./node/Number'),
        Operator: require('./node/Operator'),
        Parentheses: require('./node/Parentheses'),
        Percentage: require('./node/Percentage'),
        PseudoClassSelector: require('./node/PseudoClassSelector'),
        PseudoElementSelector: require('./node/PseudoElementSelector'),
        Ratio: require('./node/Ratio'),
        Raw: require('./node/Raw'),
        Rule: require('./node/Rule'),
        Selector: require('./node/Selector'),
        SelectorList: require('./node/SelectorList'),
        String: require('./node/String'),
        StyleSheet: require('./node/StyleSheet'),
        TypeSelector: require('./node/TypeSelector'),
        UnicodeRange: require('./node/UnicodeRange'),
        Url: require('./node/Url'),
        Value: require('./node/Value'),
        WhiteSpace: require('./node/WhiteSpace')
    }
});
