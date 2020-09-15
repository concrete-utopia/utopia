'use strict';

var hasOwnProperty = Object.prototype.hasOwnProperty;
var keywords = Object.create(null);
var properties = Object.create(null);
var HYPHENMINUS = 45; // '-'.charCodeAt()

function isVariable(str, offset) {
    return str.charCodeAt(offset) === HYPHENMINUS &&
           str.charCodeAt(offset + 1) === HYPHENMINUS;
}

function getVendorPrefix(str, offset) {
    if (str.charCodeAt(offset) === HYPHENMINUS) {
        // vendor should contain at least one letter
        var secondDashIndex = str.indexOf('-', offset + 2);

        if (secondDashIndex !== -1) {
            return str.substring(offset, secondDashIndex + 1);
        }
    }

    return '';
}

function getKeywordInfo(keyword) {
    if (hasOwnProperty.call(keywords, keyword)) {
        return keywords[keyword];
    }

    var name = keyword.toLowerCase();

    if (hasOwnProperty.call(keywords, name)) {
        return keywords[keyword] = keywords[name];
    }

    var vendor = !isVariable(name, 0) ? getVendorPrefix(name, 0) : '';

    return keywords[keyword] = Object.freeze({
        vendor: vendor,
        prefix: vendor,
        name: name.substr(vendor.length)
    });
}

function getPropertyInfo(property) {
    if (hasOwnProperty.call(properties, property)) {
        return properties[property];
    }

    var name = property;
    var hack = property[0];

    if (hack === '/' && property[1] === '/') {
        hack = '//';
    } else if (hack !== '_' &&
               hack !== '*' &&
               hack !== '$' &&
               hack !== '#' &&
               hack !== '+') {
        hack = '';
    }

    var variable = isVariable(name, hack.length);

    if (!variable) {
        name = name.toLowerCase();
        if (hasOwnProperty.call(properties, name)) {
            return properties[property] = properties[name];
        }
    }

    var vendor = !variable ? getVendorPrefix(name, hack.length) : '';

    return properties[property] = Object.freeze({
        hack: hack,
        vendor: vendor,
        prefix: hack + vendor,
        name: name.substr(hack.length + vendor.length),
        variable: variable
    });
}

module.exports = {
    keyword: getKeywordInfo,
    property: getPropertyInfo
};
