(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.xssFilters = f()}})(function(){var define,module,exports;return (function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
/*
Copyright (c) 2015, Yahoo! Inc. All rights reserved.
Copyrights licensed under the New BSD License.
See the accompanying LICENSE file for terms.

Authors: Nera Liu <neraliu@yahoo-inc.com>
         Adonis Fung <adon@yahoo-inc.com>
         Albert Yu <albertyu@yahoo-inc.com>
*/
/*jshint node: true */

exports._getPrivFilters = function () {

    var LT     = /</g,
        QUOT   = /"/g,
        SQUOT  = /'/g,
        AMP    = /&/g,
        NULL   = /\x00/g,
        SPECIAL_ATTR_VALUE_UNQUOTED_CHARS = /(?:^$|[\x00\x09-\x0D "'`=<>])/g,
        SPECIAL_HTML_CHARS = /[&<>"'`]/g, 
        SPECIAL_COMMENT_CHARS = /(?:\x00|^-*!?>|--!?>|--?!?$|\]>|\]$)/g;

    // CSS sensitive chars: ()"'/,!*@{}:;
    // By CSS: (Tab|NewLine|colon|semi|lpar|rpar|apos|sol|comma|excl|ast|midast);|(quot|QUOT)
    // By URI_PROTOCOL: (Tab|NewLine);
    var SENSITIVE_HTML_ENTITIES = /&(?:#([xX][0-9A-Fa-f]+|\d+);?|(Tab|NewLine|colon|semi|lpar|rpar|apos|sol|comma|excl|ast|midast|ensp|emsp|thinsp);|(nbsp|amp|AMP|lt|LT|gt|GT|quot|QUOT);?)/g,
        SENSITIVE_NAMED_REF_MAP = {Tab: '\t', NewLine: '\n', colon: ':', semi: ';', lpar: '(', rpar: ')', apos: '\'', sol: '/', comma: ',', excl: '!', ast: '*', midast: '*', ensp: '\u2002', emsp: '\u2003', thinsp: '\u2009', nbsp: '\xA0', amp: '&', lt: '<', gt: '>', quot: '"', QUOT: '"'};

    // var CSS_VALID_VALUE = 
    //     /^(?:
    //     (?!-*expression)#?[-\w]+
    //     |[+-]?(?:\d+|\d*\.\d+)(?:em|ex|ch|rem|px|mm|cm|in|pt|pc|%|vh|vw|vmin|vmax)?
    //     |!important
    //     | //empty
    //     )$/i;
    var CSS_VALID_VALUE = /^(?:(?!-*expression)#?[-\w]+|[+-]?(?:\d+|\d*\.\d+)(?:r?em|ex|ch|cm|mm|in|px|pt|pc|%|vh|vw|vmin|vmax)?|!important|)$/i,
        // TODO: prevent double css escaping by not encoding \ again, but this may require CSS decoding
        // \x7F and \x01-\x1F less \x09 are for Safari 5.0, added []{}/* for unbalanced quote
        CSS_DOUBLE_QUOTED_CHARS = /[\x00-\x1F\x7F\[\]{}\\"]/g,
        CSS_SINGLE_QUOTED_CHARS = /[\x00-\x1F\x7F\[\]{}\\']/g,
        // (, \u207D and \u208D can be used in background: 'url(...)' in IE, assumed all \ chars are encoded by QUOTED_CHARS, and null is already replaced with \uFFFD
        // otherwise, use this CSS_BLACKLIST instead (enhance it with url matching): /(?:\\?\(|[\u207D\u208D]|\\0{0,4}28 ?|\\0{0,2}20[78][Dd] ?)+/g
        CSS_BLACKLIST = /url[\(\u207D\u208D]+/g,
        // this assumes encodeURI() and encodeURIComponent() has escaped 1-32, 127 for IE8
        CSS_UNQUOTED_URL = /['\(\)]/g; // " \ treated by encodeURI()

    // Given a full URI, need to support "[" ( IPv6address ) "]" in URI as per RFC3986
    // Reference: https://tools.ietf.org/html/rfc3986
    var URL_IPV6 = /\/\/%5[Bb]([A-Fa-f0-9:]+)%5[Dd]/;


    // Reference: http://shazzer.co.uk/database/All/characters-allowd-in-html-entities
    // Reference: http://shazzer.co.uk/vector/Characters-allowed-after-ampersand-in-named-character-references
    // Reference: http://shazzer.co.uk/database/All/Characters-before-javascript-uri
    // Reference: http://shazzer.co.uk/database/All/Characters-after-javascript-uri
    // Reference: https://html.spec.whatwg.org/multipage/syntax.html#consume-a-character-reference
    // Reference for named characters: https://html.spec.whatwg.org/multipage/entities.json
    var URI_BLACKLIST_PROTOCOLS = {'javascript':1, 'data':1, 'vbscript':1, 'mhtml':1, 'x-schema':1},
        URI_PROTOCOL_COLON = /(?::|&#[xX]0*3[aA];?|&#0*58;?|&colon;)/,
        URI_PROTOCOL_WHITESPACES = /(?:^[\x00-\x20]+|[\t\n\r\x00]+)/g,
        URI_PROTOCOL_NAMED_REF_MAP = {Tab: '\t', NewLine: '\n'};

    var x, 
        strReplace = function (s, regexp, callback) {
            return s === undefined ? 'undefined'
                    : s === null            ? 'null'
                    : s.toString().replace(regexp, callback);
        },
        fromCodePoint = String.fromCodePoint || function(codePoint) {
            if (arguments.length === 0) {
                return '';
            }
            if (codePoint <= 0xFFFF) { // BMP code point
                return String.fromCharCode(codePoint);
            }

            // Astral code point; split in surrogate halves
            // http://mathiasbynens.be/notes/javascript-encoding#surrogate-formulae
            codePoint -= 0x10000;
            return String.fromCharCode((codePoint >> 10) + 0xD800, (codePoint % 0x400) + 0xDC00);
        };


    function getProtocol(str) {
        var s = str.split(URI_PROTOCOL_COLON, 2);
        // str.length !== s[0].length is for older IE (e.g., v8), where delimeter residing at last will result in length equals 1, but not 2
        return (s[0] && (s.length === 2 || str.length !== s[0].length)) ? s[0] : null;
    }

    function htmlDecode(s, namedRefMap, reNamedRef, skipReplacement) {
        
        namedRefMap = namedRefMap || SENSITIVE_NAMED_REF_MAP;
        reNamedRef = reNamedRef || SENSITIVE_HTML_ENTITIES;

        function regExpFunction(m, num, named, named1) {
            if (num) {
                num = Number(num[0] <= '9' ? num : '0' + num);
                // switch(num) {
                //     case 0x80: return '\u20AC';  // EURO SIGN (€)
                //     case 0x82: return '\u201A';  // SINGLE LOW-9 QUOTATION MARK (‚)
                //     case 0x83: return '\u0192';  // LATIN SMALL LETTER F WITH HOOK (ƒ)
                //     case 0x84: return '\u201E';  // DOUBLE LOW-9 QUOTATION MARK („)
                //     case 0x85: return '\u2026';  // HORIZONTAL ELLIPSIS (…)
                //     case 0x86: return '\u2020';  // DAGGER (†)
                //     case 0x87: return '\u2021';  // DOUBLE DAGGER (‡)
                //     case 0x88: return '\u02C6';  // MODIFIER LETTER CIRCUMFLEX ACCENT (ˆ)
                //     case 0x89: return '\u2030';  // PER MILLE SIGN (‰)
                //     case 0x8A: return '\u0160';  // LATIN CAPITAL LETTER S WITH CARON (Š)
                //     case 0x8B: return '\u2039';  // SINGLE LEFT-POINTING ANGLE QUOTATION MARK (‹)
                //     case 0x8C: return '\u0152';  // LATIN CAPITAL LIGATURE OE (Œ)
                //     case 0x8E: return '\u017D';  // LATIN CAPITAL LETTER Z WITH CARON (Ž)
                //     case 0x91: return '\u2018';  // LEFT SINGLE QUOTATION MARK (‘)
                //     case 0x92: return '\u2019';  // RIGHT SINGLE QUOTATION MARK (’)
                //     case 0x93: return '\u201C';  // LEFT DOUBLE QUOTATION MARK (“)
                //     case 0x94: return '\u201D';  // RIGHT DOUBLE QUOTATION MARK (”)
                //     case 0x95: return '\u2022';  // BULLET (•)
                //     case 0x96: return '\u2013';  // EN DASH (–)
                //     case 0x97: return '\u2014';  // EM DASH (—)
                //     case 0x98: return '\u02DC';  // SMALL TILDE (˜)
                //     case 0x99: return '\u2122';  // TRADE MARK SIGN (™)
                //     case 0x9A: return '\u0161';  // LATIN SMALL LETTER S WITH CARON (š)
                //     case 0x9B: return '\u203A';  // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK (›)
                //     case 0x9C: return '\u0153';  // LATIN SMALL LIGATURE OE (œ)
                //     case 0x9E: return '\u017E';  // LATIN SMALL LETTER Z WITH CARON (ž)
                //     case 0x9F: return '\u0178';  // LATIN CAPITAL LETTER Y WITH DIAERESIS (Ÿ)
                // }
                // // num >= 0xD800 && num <= 0xDFFF, and 0x0D is separately handled, as it doesn't fall into the range of x.pec()
                // return (num >= 0xD800 && num <= 0xDFFF) || num === 0x0D ? '\uFFFD' : x.frCoPt(num);

                return skipReplacement ? fromCodePoint(num)
                        : num === 0x80 ? '\u20AC'  // EURO SIGN (€)
                        : num === 0x82 ? '\u201A'  // SINGLE LOW-9 QUOTATION MARK (‚)
                        : num === 0x83 ? '\u0192'  // LATIN SMALL LETTER F WITH HOOK (ƒ)
                        : num === 0x84 ? '\u201E'  // DOUBLE LOW-9 QUOTATION MARK („)
                        : num === 0x85 ? '\u2026'  // HORIZONTAL ELLIPSIS (…)
                        : num === 0x86 ? '\u2020'  // DAGGER (†)
                        : num === 0x87 ? '\u2021'  // DOUBLE DAGGER (‡)
                        : num === 0x88 ? '\u02C6'  // MODIFIER LETTER CIRCUMFLEX ACCENT (ˆ)
                        : num === 0x89 ? '\u2030'  // PER MILLE SIGN (‰)
                        : num === 0x8A ? '\u0160'  // LATIN CAPITAL LETTER S WITH CARON (Š)
                        : num === 0x8B ? '\u2039'  // SINGLE LEFT-POINTING ANGLE QUOTATION MARK (‹)
                        : num === 0x8C ? '\u0152'  // LATIN CAPITAL LIGATURE OE (Œ)
                        : num === 0x8E ? '\u017D'  // LATIN CAPITAL LETTER Z WITH CARON (Ž)
                        : num === 0x91 ? '\u2018'  // LEFT SINGLE QUOTATION MARK (‘)
                        : num === 0x92 ? '\u2019'  // RIGHT SINGLE QUOTATION MARK (’)
                        : num === 0x93 ? '\u201C'  // LEFT DOUBLE QUOTATION MARK (“)
                        : num === 0x94 ? '\u201D'  // RIGHT DOUBLE QUOTATION MARK (”)
                        : num === 0x95 ? '\u2022'  // BULLET (•)
                        : num === 0x96 ? '\u2013'  // EN DASH (–)
                        : num === 0x97 ? '\u2014'  // EM DASH (—)
                        : num === 0x98 ? '\u02DC'  // SMALL TILDE (˜)
                        : num === 0x99 ? '\u2122'  // TRADE MARK SIGN (™)
                        : num === 0x9A ? '\u0161'  // LATIN SMALL LETTER S WITH CARON (š)
                        : num === 0x9B ? '\u203A'  // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK (›)
                        : num === 0x9C ? '\u0153'  // LATIN SMALL LIGATURE OE (œ)
                        : num === 0x9E ? '\u017E'  // LATIN SMALL LETTER Z WITH CARON (ž)
                        : num === 0x9F ? '\u0178'  // LATIN CAPITAL LETTER Y WITH DIAERESIS (Ÿ)
                        : (num >= 0xD800 && num <= 0xDFFF) || num === 0x0D ? '\uFFFD'
                        : x.frCoPt(num);
            }
            return namedRefMap[named || named1] || m;
        }

        return s === undefined  ? 'undefined'
            : s === null        ? 'null'
            : s.toString().replace(NULL, '\uFFFD').replace(reNamedRef, regExpFunction);
    }

    function cssEncode(chr) {
        // space after \\HEX is needed by spec
        return '\\' + chr.charCodeAt(0).toString(16).toLowerCase() + ' ';
    }
    function cssBlacklist(s) {
        return s.replace(CSS_BLACKLIST, function(m){ return '-x-' + m; });
    }
    function cssUrl(s) {
        // encodeURI() in yufull() will throw error for use of the CSS_UNSUPPORTED_CODE_POINT (i.e., [\uD800-\uDFFF])
        s = x.yufull(htmlDecode(s));
        var protocol = getProtocol(s);

        // prefix ## for blacklisted protocols
        // here .replace(URI_PROTOCOL_WHITESPACES, '') is not needed since yufull has already percent-encoded the whitespaces
        return (protocol && URI_BLACKLIST_PROTOCOLS[protocol.toLowerCase()]) ? '##' + s : s;
    }

    return (x = {
        // turn invalid codePoints and that of non-characters to \uFFFD, and then fromCodePoint()
        frCoPt: function(num) {
            return num === undefined || num === null ? '' :
                !isFinite(num = Number(num)) || // `NaN`, `+Infinity`, or `-Infinity`
                num <= 0 ||                     // not a valid Unicode code point
                num > 0x10FFFF ||               // not a valid Unicode code point
                // Math.floor(num) != num || 

                (num >= 0x01 && num <= 0x08) ||
                (num >= 0x0E && num <= 0x1F) ||
                (num >= 0x7F && num <= 0x9F) ||
                (num >= 0xFDD0 && num <= 0xFDEF) ||
                
                 num === 0x0B || 
                (num & 0xFFFF) === 0xFFFF || 
                (num & 0xFFFF) === 0xFFFE ? '\uFFFD' : fromCodePoint(num);
        },
        d: htmlDecode,
        /*
         * @param {string} s - An untrusted uri input
         * @returns {string} s - null if relative url, otherwise the protocol with whitespaces stripped and lower-cased
         */
        yup: function(s) {
            s = getProtocol(s.replace(NULL, ''));
            // URI_PROTOCOL_WHITESPACES is required for left trim and remove interim whitespaces
            return s ? htmlDecode(s, URI_PROTOCOL_NAMED_REF_MAP, null, true).replace(URI_PROTOCOL_WHITESPACES, '').toLowerCase() : null;
        },

        /*
         * @deprecated
         * @param {string} s - An untrusted user input
         * @returns {string} s - The original user input with & < > " ' ` encoded respectively as &amp; &lt; &gt; &quot; &#39; and &#96;.
         *
         */
        y: function(s) {
            return strReplace(s, SPECIAL_HTML_CHARS, function (m) {
                return m === '&' ? '&amp;'
                    :  m === '<' ? '&lt;'
                    :  m === '>' ? '&gt;'
                    :  m === '"' ? '&quot;'
                    :  m === "'" ? '&#39;'
                    :  /*m === '`'*/ '&#96;';       // in hex: 60
            });
        },

        // This filter is meant to introduce double-encoding, and should be used with extra care.
        ya: function(s) {
            return strReplace(s, AMP, '&amp;');
        },

        // FOR DETAILS, refer to inHTMLData()
        // Reference: https://html.spec.whatwg.org/multipage/syntax.html#data-state
        yd: function (s) {
            return strReplace(s, LT, '&lt;');
        },

        // FOR DETAILS, refer to inHTMLComment()
        // All NULL characters in s are first replaced with \uFFFD.
        // If s contains -->, --!>, or starts with -*>, insert a space right before > to stop state breaking at <!--{{{yc s}}}-->
        // If s ends with --!, --, or -, append a space to stop collaborative state breaking at {{{yc s}}}>, {{{yc s}}}!>, {{{yc s}}}-!>, {{{yc s}}}->
        // Reference: https://html.spec.whatwg.org/multipage/syntax.html#comment-state
        // Reference: http://shazzer.co.uk/vector/Characters-that-close-a-HTML-comment-3
        // Reference: http://shazzer.co.uk/vector/Characters-that-close-a-HTML-comment
        // Reference: http://shazzer.co.uk/vector/Characters-that-close-a-HTML-comment-0021
        // If s contains ]> or ends with ], append a space after ] is verified in IE to stop IE conditional comments.
        // Reference: http://msdn.microsoft.com/en-us/library/ms537512%28v=vs.85%29.aspx
        // We do not care --\s>, which can possibly be intepreted as a valid close comment tag in very old browsers (e.g., firefox 3.6), as specified in the html4 spec
        // Reference: http://www.w3.org/TR/html401/intro/sgmltut.html#h-3.2.4
        yc: function (s) {
            return strReplace(s, SPECIAL_COMMENT_CHARS, function(m){
                return m === '\x00' ? '\uFFFD'
                    : m === '--!' || m === '--' || m === '-' || m === ']' ? m + ' '
                    :/*
                    :  m === ']>'   ? '] >'
                    :  m === '-->'  ? '-- >'
                    :  m === '--!>' ? '--! >'
                    : /-*!?>/.test(m) ? */ m.slice(0, -1) + ' >';
            });
        },

        // FOR DETAILS, refer to inDoubleQuotedAttr()
        // Reference: https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(double-quoted)-state
        yavd: function (s) {
            return strReplace(s, QUOT, '&quot;');
        },

        // FOR DETAILS, refer to inSingleQuotedAttr()
        // Reference: https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(single-quoted)-state
        yavs: function (s) {
            return strReplace(s, SQUOT, '&#39;');
        },

        // FOR DETAILS, refer to inUnQuotedAttr()
        // PART A.
        // if s contains any state breaking chars (\t, \n, \v, \f, \r, space, and >),
        // they are escaped and encoded into their equivalent HTML entity representations. 
        // Reference: http://shazzer.co.uk/database/All/Characters-which-break-attributes-without-quotes
        // Reference: https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(unquoted)-state
        //
        // PART B. 
        // if s starts with ', " or `, encode it resp. as &#39;, &quot;, or &#96; to 
        // enforce the attr value (unquoted) state
        // Reference: https://html.spec.whatwg.org/multipage/syntax.html#before-attribute-value-state
        // Reference: http://shazzer.co.uk/vector/Characters-allowed-attribute-quote
        // 
        // PART C.
        // Inject a \uFFFD character if an empty or all null string is encountered in 
        // unquoted attribute value state.
        // 
        // Rationale 1: our belief is that developers wouldn't expect an 
        //   empty string would result in ' name="passwd"' rendered as 
        //   attribute value, even though this is how HTML5 is specified.
        // Rationale 2: an empty or all null string (for IE) can 
        //   effectively alter its immediate subsequent state, we choose
        //   \uFFFD to end the unquoted attr 
        //   state, which therefore will not mess up later contexts.
        // Rationale 3: Since IE 6, it is verified that NULL chars are stripped.
        // Reference: https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(unquoted)-state
        // 
        // Example:
        // <input value={{{yavu s}}} name="passwd"/>
        yavu: function (s) {
            return strReplace(s, SPECIAL_ATTR_VALUE_UNQUOTED_CHARS, function (m) {
                return m === '\t'   ? '&#9;'  // in hex: 09
                    :  m === '\n'   ? '&#10;' // in hex: 0A
                    :  m === '\x0B' ? '&#11;' // in hex: 0B  for IE. IE<9 \v equals v, so use \x0B instead
                    :  m === '\f'   ? '&#12;' // in hex: 0C
                    :  m === '\r'   ? '&#13;' // in hex: 0D
                    :  m === ' '    ? '&#32;' // in hex: 20
                    :  m === '='    ? '&#61;' // in hex: 3D
                    :  m === '<'    ? '&lt;'
                    :  m === '>'    ? '&gt;'
                    :  m === '"'    ? '&quot;'
                    :  m === "'"    ? '&#39;'
                    :  m === '`'    ? '&#96;'
                    : /*empty or null*/ '\uFFFD';
            });
        },

        yu: encodeURI,
        yuc: encodeURIComponent,

        // Notice that yubl MUST BE APPLIED LAST, and will not be used independently (expected output from encodeURI/encodeURIComponent and yavd/yavs/yavu)
        // This is used to disable JS execution capabilities by prefixing x- to ^javascript:, ^vbscript: or ^data: that possibly could trigger script execution in URI attribute context
        yubl: function (s) {
            return URI_BLACKLIST_PROTOCOLS[x.yup(s)] ? 'x-' + s : s;
        },

        // This is NOT a security-critical filter.
        // Reference: https://tools.ietf.org/html/rfc3986
        yufull: function (s) {
            return x.yu(s).replace(URL_IPV6, function(m, p) {
                return '//[' + p + ']';
            });
        },

        // chain yufull() with yubl()
        yublf: function (s) {
            return x.yubl(x.yufull(s));
        },

        // The design principle of the CSS filter MUST meet the following goal(s).
        // (1) The input cannot break out of the context (expr) and this is to fulfill the just sufficient encoding principle.
        // (2) The input cannot introduce CSS parsing error and this is to address the concern of UI redressing.
        //
        // term
        //   : unary_operator?
        //     [ NUMBER S* | PERCENTAGE S* | LENGTH S* | EMS S* | EXS S* | ANGLE S* |
        //     TIME S* | FREQ S* ]
        //   | STRING S* | IDENT S* | URI S* | hexcolor | function
        // 
        // Reference:
        // * http://www.w3.org/TR/CSS21/grammar.html 
        // * http://www.w3.org/TR/css-syntax-3/
        // 
        // NOTE: delimiter in CSS -  \  _  :  ;  (  )  "  '  /  ,  %  #  !  *  @  .  {  }
        //                        2d 5c 5f 3a 3b 28 29 22 27 2f 2c 25 23 21 2a 40 2e 7b 7d

        yceu: function(s) {
            s = htmlDecode(s);
            return CSS_VALID_VALUE.test(s) ? s : ";-x:'" + cssBlacklist(s.replace(CSS_SINGLE_QUOTED_CHARS, cssEncode)) + "';-v:";
        },

        // string1 = \"([^\n\r\f\\"]|\\{nl}|\\[^\n\r\f0-9a-f]|\\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?)*\"
        yced: function(s) {
            return cssBlacklist(htmlDecode(s).replace(CSS_DOUBLE_QUOTED_CHARS, cssEncode));
        },

        // string2 = \'([^\n\r\f\\']|\\{nl}|\\[^\n\r\f0-9a-f]|\\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?)*\'
        yces: function(s) {
            return cssBlacklist(htmlDecode(s).replace(CSS_SINGLE_QUOTED_CHARS, cssEncode));
        },

        // for url({{{yceuu url}}}
        // unquoted_url = ([!#$%&*-~]|\\{h}{1,6}(\r\n|[ \t\r\n\f])?|\\[^\r\n\f0-9a-f])* (CSS 2.1 definition)
        // unquoted_url = ([^"'()\\ \t\n\r\f\v\u0000\u0008\u000b\u000e-\u001f\u007f]|\\{h}{1,6}(\r\n|[ \t\r\n\f])?|\\[^\r\n\f0-9a-f])* (CSS 3.0 definition)
        // The state machine in CSS 3.0 is more well defined - http://www.w3.org/TR/css-syntax-3/#consume-a-url-token0
        // CSS_UNQUOTED_URL = /['\(\)]/g; // " \ treated by encodeURI()   
        yceuu: function(s) {
            return cssUrl(s).replace(CSS_UNQUOTED_URL, function (chr) {
                return  chr === '\''        ? '\\27 ' :
                        chr === '('         ? '%28' :
                        /* chr === ')' ? */   '%29';
            });
        },

        // for url("{{{yceud url}}}
        yceud: function(s) { 
            return cssUrl(s);
        },

        // for url('{{{yceus url}}}
        yceus: function(s) { 
            return cssUrl(s).replace(SQUOT, '\\27 ');
        }
    });
};

// exposing privFilters
// this is an undocumented feature, and please use it with extra care
var privFilters = exports._privFilters = exports._getPrivFilters();


/* chaining filters */

// uriInAttr and literally uriPathInAttr
// yubl is always used 
// Rationale: given pattern like this: <a href="{{{uriPathInDoubleQuotedAttr s}}}">
//            developer may expect s is always prefixed with ? or /, but an attacker can abuse it with 'javascript:alert(1)'
function uriInAttr (s, yav, yu) {
    return privFilters.yubl(yav((yu || privFilters.yufull)(s)));
}

/** 
* Yahoo Secure XSS Filters - just sufficient output filtering to prevent XSS!
* @module xss-filters 
*/

/**
* @function module:xss-filters#inHTMLData
*
* @param {string} s - An untrusted user input
* @returns {string} The string s with '<' encoded as '&amp;lt;'
*
* @description
* This filter is to be placed in HTML Data context to encode all '<' characters into '&amp;lt;'
* <ul>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#data-state">HTML5 Data State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <div>{{{inHTMLData htmlData}}}</div>
*
*/
exports.inHTMLData = privFilters.yd;

/**
* @function module:xss-filters#inHTMLComment
*
* @param {string} s - An untrusted user input
* @returns {string} All NULL characters in s are first replaced with \uFFFD. If s contains -->, --!>, or starts with -*>, insert a space right before > to stop state breaking at <!--{{{yc s}}}-->. If s ends with --!, --, or -, append a space to stop collaborative state breaking at {{{yc s}}}>, {{{yc s}}}!>, {{{yc s}}}-!>, {{{yc s}}}->. If s contains ]> or ends with ], append a space after ] is verified in IE to stop IE conditional comments.
*
* @description
* This filter is to be placed in HTML Comment context
* <ul>
* <li><a href="http://shazzer.co.uk/vector/Characters-that-close-a-HTML-comment-3">Shazzer - Closing comments for -.-></a>
* <li><a href="http://shazzer.co.uk/vector/Characters-that-close-a-HTML-comment">Shazzer - Closing comments for --.></a>
* <li><a href="http://shazzer.co.uk/vector/Characters-that-close-a-HTML-comment-0021">Shazzer - Closing comments for .></a>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#comment-start-state">HTML5 Comment Start State</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#comment-start-dash-state">HTML5 Comment Start Dash State</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#comment-state">HTML5 Comment State</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#comment-end-dash-state">HTML5 Comment End Dash State</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#comment-end-state">HTML5 Comment End State</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#comment-end-bang-state">HTML5 Comment End Bang State</a></li>
* <li><a href="http://msdn.microsoft.com/en-us/library/ms537512%28v=vs.85%29.aspx">Conditional Comments in Internet Explorer</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <!-- {{{inHTMLComment html_comment}}} -->
*
*/
exports.inHTMLComment = privFilters.yc;

/**
* @function module:xss-filters#inSingleQuotedAttr
*
* @param {string} s - An untrusted user input
* @returns {string} The string s with any single-quote characters encoded into '&amp;&#39;'.
*
* @description
* <p class="warning">Warning: This is NOT designed for any onX (e.g., onclick) attributes!</p>
* <p class="warning">Warning: If you're working on URI/components, use the more specific uri___InSingleQuotedAttr filter </p>
* This filter is to be placed in HTML Attribute Value (single-quoted) state to encode all single-quote characters into '&amp;&#39;'
*
* <ul>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(single-quoted)-state">HTML5 Attribute Value (Single-Quoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <input name='firstname' value='{{{inSingleQuotedAttr firstname}}}' />
*
*/
exports.inSingleQuotedAttr = privFilters.yavs;

/**
* @function module:xss-filters#inDoubleQuotedAttr
*
* @param {string} s - An untrusted user input
* @returns {string} The string s with any single-quote characters encoded into '&amp;&quot;'.
*
* @description
* <p class="warning">Warning: This is NOT designed for any onX (e.g., onclick) attributes!</p>
* <p class="warning">Warning: If you're working on URI/components, use the more specific uri___InDoubleQuotedAttr filter </p>
* This filter is to be placed in HTML Attribute Value (double-quoted) state to encode all single-quote characters into '&amp;&quot;'
*
* <ul>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(double-quoted)-state">HTML5 Attribute Value (Double-Quoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <input name="firstname" value="{{{inDoubleQuotedAttr firstname}}}" />
*
*/
exports.inDoubleQuotedAttr = privFilters.yavd;

/**
* @function module:xss-filters#inUnQuotedAttr
*
* @param {string} s - An untrusted user input
* @returns {string} If s contains any state breaking chars (\t, \n, \v, \f, \r, space, null, ', ", `, <, >, and =), they are escaped and encoded into their equivalent HTML entity representations. If the string is empty, inject a \uFFFD character.
*
* @description
* <p class="warning">Warning: This is NOT designed for any onX (e.g., onclick) attributes!</p>
* <p class="warning">Warning: If you're working on URI/components, use the more specific uri___InUnQuotedAttr filter </p>
* <p>Regarding \uFFFD injection, given <a id={{{id}}} name="passwd">,<br/>
*        Rationale 1: our belief is that developers wouldn't expect when id equals an
*          empty string would result in ' name="passwd"' rendered as 
*          attribute value, even though this is how HTML5 is specified.<br/>
*        Rationale 2: an empty or all null string (for IE) can 
*          effectively alter its immediate subsequent state, we choose
*          \uFFFD to end the unquoted attr 
*          state, which therefore will not mess up later contexts.<br/>
*        Rationale 3: Since IE 6, it is verified that NULL chars are stripped.<br/>
*        Reference: https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(unquoted)-state</p>
* <ul>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(unquoted)-state">HTML5 Attribute Value (Unquoted) State</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#before-attribute-value-state">HTML5 Before Attribute Value State</a></li>
* <li><a href="http://shazzer.co.uk/database/All/Characters-which-break-attributes-without-quotes">Shazzer - Characters-which-break-attributes-without-quotes</a></li>
* <li><a href="http://shazzer.co.uk/vector/Characters-allowed-attribute-quote">Shazzer - Characters-allowed-attribute-quote</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <input name="firstname" value={{{inUnQuotedAttr firstname}}} />
*
*/
exports.inUnQuotedAttr = privFilters.yavu;


/**
* @function module:xss-filters#uriInSingleQuotedAttr
*
* @param {string} s - An untrusted user input, supposedly an <strong>absolute</strong> URI
* @returns {string} The string s encoded first by window.encodeURI(), then inSingleQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* @description
* This filter is to be placed in HTML Attribute Value (single-quoted) state for an <strong>absolute</strong> URI.<br/>
* The correct order of encoders is thus: first window.encodeURI(), then inSingleQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* <p>Notice: This filter is IPv6 friendly by not encoding '[' and ']'.</p>
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI">encodeURI | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(single-quoted)-state">HTML5 Attribute Value (Single-Quoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href='{{{uriInSingleQuotedAttr full_uri}}}'>link</a>
* 
*/
exports.uriInSingleQuotedAttr = function (s) {
    return uriInAttr(s, privFilters.yavs);
};

/**
* @function module:xss-filters#uriInDoubleQuotedAttr
*
* @param {string} s - An untrusted user input, supposedly an <strong>absolute</strong> URI
* @returns {string} The string s encoded first by window.encodeURI(), then inDoubleQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* @description
* This filter is to be placed in HTML Attribute Value (double-quoted) state for an <strong>absolute</strong> URI.<br/>
* The correct order of encoders is thus: first window.encodeURI(), then inDoubleQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* <p>Notice: This filter is IPv6 friendly by not encoding '[' and ']'.</p>
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI">encodeURI | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(double-quoted)-state">HTML5 Attribute Value (Double-Quoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href="{{{uriInDoubleQuotedAttr full_uri}}}">link</a>
* 
*/
exports.uriInDoubleQuotedAttr = function (s) {
    return uriInAttr(s, privFilters.yavd);
};


/**
* @function module:xss-filters#uriInUnQuotedAttr
*
* @param {string} s - An untrusted user input, supposedly an <strong>absolute</strong> URI
* @returns {string} The string s encoded first by window.encodeURI(), then inUnQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* @description
* This filter is to be placed in HTML Attribute Value (unquoted) state for an <strong>absolute</strong> URI.<br/>
* The correct order of encoders is thus: first the built-in encodeURI(), then inUnQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* <p>Notice: This filter is IPv6 friendly by not encoding '[' and ']'.</p>
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI">encodeURI | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(unquoted)-state">HTML5 Attribute Value (Unquoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href={{{uriInUnQuotedAttr full_uri}}}>link</a>
* 
*/
exports.uriInUnQuotedAttr = function (s) {
    return uriInAttr(s, privFilters.yavu);
};

/**
* @function module:xss-filters#uriInHTMLData
*
* @param {string} s - An untrusted user input, supposedly an <strong>absolute</strong> URI
* @returns {string} The string s encoded by window.encodeURI() and then inHTMLData()
*
* @description
* This filter is to be placed in HTML Data state for an <strong>absolute</strong> URI.
*
* <p>Notice: The actual implementation skips inHTMLData(), since '<' is already encoded as '%3C' by encodeURI().</p>
* <p>Notice: This filter is IPv6 friendly by not encoding '[' and ']'.</p>
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI">encodeURI | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#data-state">HTML5 Data State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href="/somewhere">{{{uriInHTMLData full_uri}}}</a>
* 
*/
exports.uriInHTMLData = privFilters.yufull;


/**
* @function module:xss-filters#uriInHTMLComment
*
* @param {string} s - An untrusted user input, supposedly an <strong>absolute</strong> URI
* @returns {string} The string s encoded by window.encodeURI(), and finally inHTMLComment()
*
* @description
* This filter is to be placed in HTML Comment state for an <strong>absolute</strong> URI.
*
* <p>Notice: This filter is IPv6 friendly by not encoding '[' and ']'.</p>
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI">encodeURI | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#data-state">HTML5 Data State</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#comment-state">HTML5 Comment State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <!-- {{{uriInHTMLComment full_uri}}} -->
* 
*/
exports.uriInHTMLComment = function (s) {
    return privFilters.yc(privFilters.yufull(s));
};




/**
* @function module:xss-filters#uriPathInSingleQuotedAttr
*
* @param {string} s - An untrusted user input, supposedly a URI Path/Query or relative URI
* @returns {string} The string s encoded first by window.encodeURI(), then inSingleQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* @description
* This filter is to be placed in HTML Attribute Value (single-quoted) state for a URI Path/Query or relative URI.<br/>
* The correct order of encoders is thus: first window.encodeURI(), then inSingleQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI">encodeURI | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(single-quoted)-state">HTML5 Attribute Value (Single-Quoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href='http://example.com/{{{uriPathInSingleQuotedAttr uri_path}}}'>link</a>
* <a href='http://example.com/?{{{uriQueryInSingleQuotedAttr uri_query}}}'>link</a>
* 
*/
exports.uriPathInSingleQuotedAttr = function (s) {
    return uriInAttr(s, privFilters.yavs, privFilters.yu);
};

/**
* @function module:xss-filters#uriPathInDoubleQuotedAttr
*
* @param {string} s - An untrusted user input, supposedly a URI Path/Query or relative URI
* @returns {string} The string s encoded first by window.encodeURI(), then inDoubleQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* @description
* This filter is to be placed in HTML Attribute Value (double-quoted) state for a URI Path/Query or relative URI.<br/>
* The correct order of encoders is thus: first window.encodeURI(), then inDoubleQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI">encodeURI | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(double-quoted)-state">HTML5 Attribute Value (Double-Quoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href="http://example.com/{{{uriPathInDoubleQuotedAttr uri_path}}}">link</a>
* <a href="http://example.com/?{{{uriQueryInDoubleQuotedAttr uri_query}}}">link</a>
* 
*/
exports.uriPathInDoubleQuotedAttr = function (s) {
    return uriInAttr(s, privFilters.yavd, privFilters.yu);
};


/**
* @function module:xss-filters#uriPathInUnQuotedAttr
*
* @param {string} s - An untrusted user input, supposedly a URI Path/Query or relative URI
* @returns {string} The string s encoded first by window.encodeURI(), then inUnQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* @description
* This filter is to be placed in HTML Attribute Value (unquoted) state for a URI Path/Query or relative URI.<br/>
* The correct order of encoders is thus: first the built-in encodeURI(), then inUnQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI">encodeURI | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(unquoted)-state">HTML5 Attribute Value (Unquoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href=http://example.com/{{{uriPathInUnQuotedAttr uri_path}}}>link</a>
* <a href=http://example.com/?{{{uriQueryInUnQuotedAttr uri_query}}}>link</a>
* 
*/
exports.uriPathInUnQuotedAttr = function (s) {
    return uriInAttr(s, privFilters.yavu, privFilters.yu);
};

/**
* @function module:xss-filters#uriPathInHTMLData
*
* @param {string} s - An untrusted user input, supposedly a URI Path/Query or relative URI
* @returns {string} The string s encoded by window.encodeURI() and then inHTMLData()
*
* @description
* This filter is to be placed in HTML Data state for a URI Path/Query or relative URI.
*
* <p>Notice: The actual implementation skips inHTMLData(), since '<' is already encoded as '%3C' by encodeURI().</p>
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI">encodeURI | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#data-state">HTML5 Data State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href="http://example.com/">http://example.com/{{{uriPathInHTMLData uri_path}}}</a>
* <a href="http://example.com/">http://example.com/?{{{uriQueryInHTMLData uri_query}}}</a>
* 
*/
exports.uriPathInHTMLData = privFilters.yu;


/**
* @function module:xss-filters#uriPathInHTMLComment
*
* @param {string} s - An untrusted user input, supposedly a URI Path/Query or relative URI
* @returns {string} The string s encoded by window.encodeURI(), and finally inHTMLComment()
*
* @description
* This filter is to be placed in HTML Comment state for a URI Path/Query or relative URI.
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI">encodeURI | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#data-state">HTML5 Data State</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#comment-state">HTML5 Comment State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <!-- http://example.com/{{{uriPathInHTMLComment uri_path}}} -->
* <!-- http://example.com/?{{{uriQueryInHTMLComment uri_query}}} -->
*/
exports.uriPathInHTMLComment = function (s) {
    return privFilters.yc(privFilters.yu(s));
};


/**
* @function module:xss-filters#uriQueryInSingleQuotedAttr
* @description This is an alias of {@link module:xss-filters#uriPathInSingleQuotedAttr}
* 
* @alias module:xss-filters#uriPathInSingleQuotedAttr
*/
exports.uriQueryInSingleQuotedAttr = exports.uriPathInSingleQuotedAttr;

/**
* @function module:xss-filters#uriQueryInDoubleQuotedAttr
* @description This is an alias of {@link module:xss-filters#uriPathInDoubleQuotedAttr}
* 
* @alias module:xss-filters#uriPathInDoubleQuotedAttr
*/
exports.uriQueryInDoubleQuotedAttr = exports.uriPathInDoubleQuotedAttr;

/**
* @function module:xss-filters#uriQueryInUnQuotedAttr
* @description This is an alias of {@link module:xss-filters#uriPathInUnQuotedAttr}
* 
* @alias module:xss-filters#uriPathInUnQuotedAttr
*/
exports.uriQueryInUnQuotedAttr = exports.uriPathInUnQuotedAttr;

/**
* @function module:xss-filters#uriQueryInHTMLData
* @description This is an alias of {@link module:xss-filters#uriPathInHTMLData}
* 
* @alias module:xss-filters#uriPathInHTMLData
*/
exports.uriQueryInHTMLData = exports.uriPathInHTMLData;

/**
* @function module:xss-filters#uriQueryInHTMLComment
* @description This is an alias of {@link module:xss-filters#uriPathInHTMLComment}
* 
* @alias module:xss-filters#uriPathInHTMLComment
*/
exports.uriQueryInHTMLComment = exports.uriPathInHTMLComment;



/**
* @function module:xss-filters#uriComponentInSingleQuotedAttr
*
* @param {string} s - An untrusted user input, supposedly a URI Component
* @returns {string} The string s encoded first by window.encodeURIComponent(), then inSingleQuotedAttr()
*
* @description
* This filter is to be placed in HTML Attribute Value (single-quoted) state for a URI Component.<br/>
* The correct order of encoders is thus: first window.encodeURIComponent(), then inSingleQuotedAttr()
*
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent">encodeURIComponent | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(single-quoted)-state">HTML5 Attribute Value (Single-Quoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href='http://example.com/?q={{{uriComponentInSingleQuotedAttr uri_component}}}'>link</a>
* 
*/
exports.uriComponentInSingleQuotedAttr = function (s) {
    return privFilters.yavs(privFilters.yuc(s));
};

/**
* @function module:xss-filters#uriComponentInDoubleQuotedAttr
*
* @param {string} s - An untrusted user input, supposedly a URI Component
* @returns {string} The string s encoded first by window.encodeURIComponent(), then inDoubleQuotedAttr()
*
* @description
* This filter is to be placed in HTML Attribute Value (double-quoted) state for a URI Component.<br/>
* The correct order of encoders is thus: first window.encodeURIComponent(), then inDoubleQuotedAttr()
*
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent">encodeURIComponent | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(double-quoted)-state">HTML5 Attribute Value (Double-Quoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href="http://example.com/?q={{{uriComponentInDoubleQuotedAttr uri_component}}}">link</a>
* 
*/
exports.uriComponentInDoubleQuotedAttr = function (s) {
    return privFilters.yavd(privFilters.yuc(s));
};


/**
* @function module:xss-filters#uriComponentInUnQuotedAttr
*
* @param {string} s - An untrusted user input, supposedly a URI Component
* @returns {string} The string s encoded first by window.encodeURIComponent(), then inUnQuotedAttr()
*
* @description
* This filter is to be placed in HTML Attribute Value (unquoted) state for a URI Component.<br/>
* The correct order of encoders is thus: first the built-in encodeURIComponent(), then inUnQuotedAttr()
*
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent">encodeURIComponent | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(unquoted)-state">HTML5 Attribute Value (Unquoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href=http://example.com/?q={{{uriComponentInUnQuotedAttr uri_component}}}>link</a>
* 
*/
exports.uriComponentInUnQuotedAttr = function (s) {
    return privFilters.yavu(privFilters.yuc(s));
};

/**
* @function module:xss-filters#uriComponentInHTMLData
*
* @param {string} s - An untrusted user input, supposedly a URI Component
* @returns {string} The string s encoded by window.encodeURIComponent() and then inHTMLData()
*
* @description
* This filter is to be placed in HTML Data state for a URI Component.
*
* <p>Notice: The actual implementation skips inHTMLData(), since '<' is already encoded as '%3C' by encodeURIComponent().</p>
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent">encodeURIComponent | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#data-state">HTML5 Data State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href="http://example.com/">http://example.com/?q={{{uriComponentInHTMLData uri_component}}}</a>
* <a href="http://example.com/">http://example.com/#{{{uriComponentInHTMLData uri_fragment}}}</a>
* 
*/
exports.uriComponentInHTMLData = privFilters.yuc;


/**
* @function module:xss-filters#uriComponentInHTMLComment
*
* @param {string} s - An untrusted user input, supposedly a URI Component
* @returns {string} The string s encoded by window.encodeURIComponent(), and finally inHTMLComment()
*
* @description
* This filter is to be placed in HTML Comment state for a URI Component.
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent">encodeURIComponent | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#data-state">HTML5 Data State</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#comment-state">HTML5 Comment State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <!-- http://example.com/?q={{{uriComponentInHTMLComment uri_component}}} -->
* <!-- http://example.com/#{{{uriComponentInHTMLComment uri_fragment}}} -->
*/
exports.uriComponentInHTMLComment = function (s) {
    return privFilters.yc(privFilters.yuc(s));
};


// uriFragmentInSingleQuotedAttr
// added yubl on top of uriComponentInAttr 
// Rationale: given pattern like this: <a href='{{{uriFragmentInSingleQuotedAttr s}}}'>
//            developer may expect s is always prefixed with #, but an attacker can abuse it with 'javascript:alert(1)'

/**
* @function module:xss-filters#uriFragmentInSingleQuotedAttr
*
* @param {string} s - An untrusted user input, supposedly a URI Fragment
* @returns {string} The string s encoded first by window.encodeURIComponent(), then inSingleQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* @description
* This filter is to be placed in HTML Attribute Value (single-quoted) state for a URI Fragment.<br/>
* The correct order of encoders is thus: first window.encodeURIComponent(), then inSingleQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent">encodeURIComponent | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(single-quoted)-state">HTML5 Attribute Value (Single-Quoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href='http://example.com/#{{{uriFragmentInSingleQuotedAttr uri_fragment}}}'>link</a>
* 
*/
exports.uriFragmentInSingleQuotedAttr = function (s) {
    return privFilters.yubl(privFilters.yavs(privFilters.yuc(s)));
};

// uriFragmentInDoubleQuotedAttr
// added yubl on top of uriComponentInAttr 
// Rationale: given pattern like this: <a href="{{{uriFragmentInDoubleQuotedAttr s}}}">
//            developer may expect s is always prefixed with #, but an attacker can abuse it with 'javascript:alert(1)'

/**
* @function module:xss-filters#uriFragmentInDoubleQuotedAttr
*
* @param {string} s - An untrusted user input, supposedly a URI Fragment
* @returns {string} The string s encoded first by window.encodeURIComponent(), then inDoubleQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* @description
* This filter is to be placed in HTML Attribute Value (double-quoted) state for a URI Fragment.<br/>
* The correct order of encoders is thus: first window.encodeURIComponent(), then inDoubleQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent">encodeURIComponent | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(double-quoted)-state">HTML5 Attribute Value (Double-Quoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href="http://example.com/#{{{uriFragmentInDoubleQuotedAttr uri_fragment}}}">link</a>
* 
*/
exports.uriFragmentInDoubleQuotedAttr = function (s) {
    return privFilters.yubl(privFilters.yavd(privFilters.yuc(s)));
};

// uriFragmentInUnQuotedAttr
// added yubl on top of uriComponentInAttr 
// Rationale: given pattern like this: <a href={{{uriFragmentInUnQuotedAttr s}}}>
//            developer may expect s is always prefixed with #, but an attacker can abuse it with 'javascript:alert(1)'

/**
* @function module:xss-filters#uriFragmentInUnQuotedAttr
*
* @param {string} s - An untrusted user input, supposedly a URI Fragment
* @returns {string} The string s encoded first by window.encodeURIComponent(), then inUnQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* @description
* This filter is to be placed in HTML Attribute Value (unquoted) state for a URI Fragment.<br/>
* The correct order of encoders is thus: first the built-in encodeURIComponent(), then inUnQuotedAttr(), and finally prefix the resulted string with 'x-' if it begins with 'javascript:' or 'vbscript:' that could possibly lead to script execution
*
* <ul>
* <li><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent">encodeURIComponent | MDN</a></li>
* <li><a href="http://tools.ietf.org/html/rfc3986">RFC 3986</a></li>
* <li><a href="https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(unquoted)-state">HTML5 Attribute Value (Unquoted) State</a></li>
* </ul>
*
* @example
* // output context to be applied by this filter.
* <a href=http://example.com/#{{{uriFragmentInUnQuotedAttr uri_fragment}}}>link</a>
* 
*/
exports.uriFragmentInUnQuotedAttr = function (s) {
    return privFilters.yubl(privFilters.yavu(privFilters.yuc(s)));
};


/**
* @function module:xss-filters#uriFragmentInHTMLData
* @description This is an alias of {@link module:xss-filters#uriComponentInHTMLData}
* 
* @alias module:xss-filters#uriComponentInHTMLData
*/
exports.uriFragmentInHTMLData = exports.uriComponentInHTMLData;

/**
* @function module:xss-filters#uriFragmentInHTMLComment
* @description This is an alias of {@link module:xss-filters#uriComponentInHTMLComment}
* 
* @alias module:xss-filters#uriComponentInHTMLComment
*/
exports.uriFragmentInHTMLComment = exports.uriComponentInHTMLComment;

},{}]},{},[1])(1)
});