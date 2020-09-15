/*
Copyright (c) 2015, Yahoo! Inc. All rights reserved.
Copyrights licensed under the New BSD License.
See the accompanying LICENSE file for terms.

Authors: Nera Liu <neraliu@yahoo-inc.com>
         Adonis Fung <adon@yahoo-inc.com>
         Albert Yu <albertyu@yahoo-inc.com>
*/
(function() {

    var filter = xssFilters._privFilters;

    describe("private-xss-filters: existence tests", function() {
        it('filter y exists', function() {
            expect(filter.y).to.be.ok();
        });
        it('filter ya exists', function() {
            expect(filter.ya).to.be.ok();
        });
        it('filter yd exists', function() {
            expect(filter.yd).to.be.ok();
        });
        it('filter yc exists', function() {
            expect(filter.yc).to.be.ok();
        });
        it('filter yavd exists', function() {
            expect(filter.yavd).to.be.ok();
        });
        it('filter yavs exists', function() {
            expect(filter.yavs).to.be.ok();
        });
        it('filter yavu exists', function() {
            expect(filter.yavu).to.be.ok();
        });
        it('filter yu exists', function() {
            expect(filter.yu).to.be.ok();
        });
        it('filter yuc exists', function() {
            expect(filter.yuc).to.be.ok();
        });
        it('filter yubl exists', function() {
            expect(filter.yubl).to.be.ok();
        });
        it('filter yufull exists', function() {
            expect(filter.yufull).to.be.ok();
        });
        it('filter yublf exists', function() {
            expect(filter.yublf).to.be.ok();
        });
        
    });

    describe("private-xss-filters: alias tests", function() {
        it('filter yu being an alias of encodeURI', function() {
            expect(filter.yu).to.eql(encodeURI);
        });

        it('filter yuc being an alias of encodeURIComponent', function() {
            expect(filter.yuc).to.eql(encodeURIComponent);
        });
    });

    describe("private-xss-filters: encodeURI() and encodeURIComponent() tests", function() {
        it('percentage encoded ASCII chars of decimal 0-32 chars', function() {
            var chars = [
                    '\u0000',
                    '\u0001', '\u0002', '\u0003', '\u0004', 
                    '\u0005', '\u0006', '\u0007', '\u0008', 
                    '\u0009', '\u000A', '\u000B', '\u000C', 
                    '\u000D', '\u000E', '\u000F', '\u0010', 
                    '\u0011', '\u0012', '\u0013', '\u0014', 
                    '\u0015', '\u0016', '\u0017', '\u0018', 
                    '\u0019', '\u001A', '\u001B', '\u001C', 
                    '\u001D', '\u001E', '\u001F', '\u0020'],
                percentEncoded = [
                    "%00",
                    "%01", "%02", "%03", "%04", 
                    "%05", "%06", "%07", "%08", 
                    "%09", "%0A", "%0B", "%0C", 
                    "%0D", "%0E", "%0F", "%10", 
                    "%11", "%12", "%13", "%14", 
                    "%15", "%16", "%17", "%18", 
                    "%19", "%1A", "%1B", "%1C", 
                    "%1D", "%1E", "%1F", "%20"];

            expect(chars.map(encodeURI)).to.eql(percentEncoded);
            expect(chars.map(encodeURIComponent)).to.eql(percentEncoded);
        });
    });

    describe("private-xss-filters: error and data type tests", function() {

        // an feature indicator of which encodeURI() and encodeURIComponent is used
        it('filter yuc and yu throw URI malformed', function() {
            expect(function() { filter.yu('foo\uD800'); }).to.throwError(/(?:malformed|invalid character|illegal UTF-16 sequence)/);
            expect(function() { filter.yuc('foo\uD800'); }).to.throwError(/(?:malformed|invalid character|illegal UTF-16 sequence)/);
        });

        it('filters handling of undefined input', function() {
            expect(filter.y()).to.eql('undefined');
            expect(filter.ya()).to.eql('undefined');
            expect(filter.yd()).to.eql('undefined');
            expect(filter.yc()).to.eql('undefined');

            expect(filter.yavd()).to.eql('undefined');
            expect(filter.yavs()).to.eql('undefined');
            expect(filter.yavu()).to.eql('undefined');

            expect(filter.yu()).to.eql('undefined');
            expect(filter.yuc()).to.eql('undefined');
            // yubl will not be independently used
            // expect(filter.yubl()).to.eql('undefined');

            expect(filter.yublf()).to.eql('undefined');
        });

        it('filters handling of null input', function() {
            expect(filter.y(null)).to.eql('null');
            expect(filter.ya(null)).to.eql('null');
            expect(filter.yd(null)).to.eql('null');
            expect(filter.yc(null)).to.eql('null');

            expect(filter.yavd(null)).to.eql('null');
            expect(filter.yavs(null)).to.eql('null');
            expect(filter.yavu(null)).to.eql('null');

            expect(filter.yu(null)).to.eql('null');
            expect(filter.yuc(null)).to.eql('null');
            // yubl will not be independently used
            // expect(filter.yubl()).to.eql('undefined');

            expect(filter.yublf(null)).to.eql('null');
        });


        it('filters handling of array input', function() {
            var array = ['a', 'b'], result = 'a,b';

            expect(filter.y(array)).to.eql(result);
            expect(filter.ya(array)).to.eql(result);
            expect(filter.yd(array)).to.eql(result);
            expect(filter.yc(array)).to.eql(result);

            expect(filter.yavd(array)).to.eql(result);
            expect(filter.yavs(array)).to.eql(result);
            expect(filter.yavu(array)).to.eql(result);

            expect(filter.yu(array)).to.eql(result);
            expect(filter.yuc(array)).to.eql('a%2Cb');
            // yubl will not be independently used
            // expect(filter.yubl()).to.eql('undefined');

            expect(filter.yublf(array)).to.eql(result);
        });


        it('filters handling of object input', function() {
            var object = {'a':1, 'b':0}, result = '[object Object]';

            expect(filter.y(object)).to.eql(result);
            expect(filter.ya(object)).to.eql(result);
            expect(filter.yd(object)).to.eql(result);
            expect(filter.yc(object)).to.eql(result + ' ');

            expect(filter.yavd(object)).to.eql(result);
            expect(filter.yavs(object)).to.eql(result);
            expect(filter.yavu(object)).to.eql('[object&#32;Object]');

            expect(filter.yu(object)).to.eql('%5Bobject%20Object%5D');
            expect(filter.yuc(object)).to.eql('%5Bobject%20Object%5D');
            // yubl will not be independently used
            // expect(filter.yubl()).to.eql('undefined');

            expect(filter.yublf(object)).to.eql('%5Bobject%20Object%5D');
        });

        it('filters handling of empty string', function() {
            var str = '', result = '';

            expect(filter.y(str)).to.eql(result);
            expect(filter.ya(str)).to.eql(result);
            expect(filter.yd(str)).to.eql(result);
            expect(filter.yc(str)).to.eql(result);

            expect(filter.yavd(str)).to.eql(result);
            expect(filter.yavs(str)).to.eql(result);
            expect(filter.yavu(str)).to.eql('\uFFFD');

            expect(filter.yu(str)).to.eql(result);
            expect(filter.yuc(str)).to.eql(result);
            expect(filter.yubl(str)).to.eql(result);
            expect(filter.yublf(str)).to.eql(result);
        });
    });

    describe("private-xss-filters: unchained state transition tests", function() {
        
        it('filter y state transition test', function() {
            var s = "foo&<>\"'` bar&<>\"' &lt;";
            var o = filter.y(s);
            expect(o).to.eql('foo&amp;&lt;&gt;&quot;&#39;&#96; bar&amp;&lt;&gt;&quot;&#39; &amp;lt;');
        });

        it('filter ya state transition test', function() {
            var s = "foo&<>\"'` bar&<>\"' &lt; &quot; &#39;";
            var o = filter.ya(s);
            expect(o).to.eql('foo&amp;<>"\'` bar&amp;<>"\' &amp;lt; &amp;quot; &amp;#39;');
        });

        it('filter yd state transition test', function() {
            testutils.test_yd(filter.yd, ['foo&&lt;>\'"']);
        });

        it('filter yc state transition test', function() {
            testutils.test_yc(filter.yc, [
                '-- > --! > <!--[if IE] ><script>alert("yahoo\'s filters")</script>', 
                'foo-- ', 
                'foo--! ', 
                '[if IE] ', 
                'foo- ', 
                'foo- ',
                ' ><script>alert(1)</script>',
                '---------- ><script>alert(1)</script>',
                '--\uFFFD>']);
        });

        it('filter yav-single-quoted state transition test', function() {
            testutils.test_yav(filter.yavs, [
                'foo&<>&#39;"` \t\n\x0B\f\r', '\f', '',
                '&#39;&#39;', ' &#39;&#39;', '\t&#39;&#39;', '\n&#39;&#39;', '\f&#39;&#39;',
                '""',         ' ""',         '\t""',         '\n""',         '\f""',
                '``',         ' ``',         '\t``',         '\n``',         '\f``']);
        });

        it('filter yav-double-quoted state transition test', function() {
            testutils.test_yav(filter.yavd, [
                'foo&<>\'&quot;` \t\n\x0B\f\r', '\f', '',
                "''",           " ''",           "\t''",           "\n''",           "\f''", 
                '&quot;&quot;', ' &quot;&quot;', '\t&quot;&quot;', '\n&quot;&quot;', '\f&quot;&quot;',
                '``',           ' ``',           '\t``',           '\n``',           '\f``']);
        });
        
        it('filter yav-unquoted state transition test', function() {
            testutils.test_yav(filter.yavu, [
                'foo&&lt;&gt;&#39;&quot;&#96;&#32;&#9;&#10;&#11;&#12;&#13;', '&#12;', '\uFFFD',
                "&#39;&#39;",  "&#32;&#39;&#39;", "&#9;&#39;&#39;", "&#10;&#39;&#39;", "&#12;&#39;&#39;",
                '&quot;&quot;', '&#32;&quot;&quot;', '&#9;&quot;&quot;', '&#10;&quot;&quot;', '&#12;&quot;&quot;',
                '&#96;&#96;',  '&#32;&#96;&#96;', '&#9;&#96;&#96;', '&#10;&#96;&#96;', '&#12;&#96;&#96;']);

            var s = "\x00=<>''onerror=alert(1)";
            var o = filter.yavu(s);
            expect(o).to.eql("\uFFFD&#61;&lt;&gt;&#39;&#39;onerror&#61;alert(1)");
        });

        it('filter yu state transition test', function() {
            testutils.test_yu(filter.yu);
        });

        it('filter yuc state transition test', function() {
            testutils.test_yuc(filter.yuc);
        });

        it('filter yubl state transition test', function() {
            // it is known that yubl, when used independently is vulnerable to attack
            testutils.test_yubl(filter.yubl, [
                'x-\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\u0009\
\u000A\u000B\u000C\u000D\u000E\u000F\u0010\u0011\u0012\
\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001A\u001B\
\u001C\u001D\u001E\u001F\u0020j\nav&#x61;\rscript\t&col\u0000on;'
            ]);

        });

        it('filter yufull state transition test', function() {
            testutils.test_yufull(filter.yufull, [
                null, // default
                null, // default
                null, // default
                null, // default
                null, // default
                'http://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]',
                null, // default
                null  // default
            ]);
        });

        it('filter yublf state transition test', function() {
            testutils.test_yufull(filter.yublf, [
                null, // default
                null, // default
                null, // default
                null, // default
                null, // default
                'http://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]',
                null, // default
                null  // default
            ]);
            testutils.test_yubl(filter.yublf, [
                '%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F%20j%0Aav&#x61;%0Dscript%09&col%00on;'
            ]);
        });
        
    });

    describe("private-xss-filters: css expression tests", function() {

        var testPatterns = [ undefined, null,
            '&',
            '1.1', '10%', '+10px', '-10px', '#fff', 
            '\uD7FF', '\uD800', '\uDFFF', '\u1234567',
            '\u0000', ' ', '\r\n\t\f\x0B', '\\', '\\n\\r\\f\\0\\9\\a\\f',
            '-ide_nt', '"string"', "'string'",
            '- \ _ : ; ( ) " \' / , % # ! * @ . { } []', 
            'http://username:password@www.evil.com:8080/?k1=v1&k2=v2#hash',
            'url(https://www.evil.com)', 
            'u\x00\x00rl(https://www.evil.com)', 
            '\\u\\r\x00\x00\\l\x00\\((evil.com))',
            'expression(body.scrollTop + 50 + px)', 
            '(((()))) \\28 \\29'
        ];

        it('filter yceu[uds] test', function() {
            var expectedResults = [ 'undefined', 'null',
                ';-x:\'&\';-v:',
                '1.1', '10%', '+10px', '-10px', '#fff', 
                ';-x:\'\uD7FF\';-v:', ';-x:\'\uD800\';-v:', ';-x:\'\uDFFF\';-v:', ';-x:\'\u1234567\';-v:',
                ';-x:\'\uFFFD\';-v:', ';-x:\' \';-v:', ';-x:\'\\d \\a \\9 \\c \\b \';-v:', ';-x:\'\\5c \';-v:', ';-x:\'\\5c n\\5c r\\5c f\\5c 0\\5c 9\\5c a\\5c f\';-v:',
                '-ide_nt', ';-x:\'"string"\';-v:', ';-x:\'\\27 string\\27 \';-v:',
                ';-x:\'-  _ : ; ( ) " \\27  / , % # ! * @ . \\7b  \\7d  \\5b \\5d \';-v:', 
                ';-x:\'http://username:password@www.evil.com:8080/?k1=v1&k2=v2#hash\';-v:',
                ';-x:\'-x-url(https://www.evil.com)\';-v:',
                ';-x:\'u\ufffd\ufffdrl(https://www.evil.com)\';-v:',
                ';-x:\'\\5c u\\5c r\ufffd\ufffd\\5c l\ufffd\\5c ((evil.com))\';-v:',
                ';-x:\'expression(body.scrollTop + 50 + px)\';-v:',
                ';-x:\'(((()))) \\5c 28 \\5c 29\';-v:'
            ];
            testutils.test_yce(filter.yceu, testPatterns, expectedResults);
        });
        it('filter yced[uds] test', function() {
            var expectedResults = [ 'undefined', 'null',
                '&', 
                '1.1', '10%', '+10px', '-10px', '#fff', 
                '\uD7FF', '\uD800', '\uDFFF', '\u1234567',
                '\uFFFD', ' ', '\\d \\a \\9 \\c \\b ', '\\5c ', '\\5c n\\5c r\\5c f\\5c 0\\5c 9\\5c a\\5c f',
                '-ide_nt', '\\22 string\\22 ', "'string'",
                '-  _ : ; ( ) \\22  \' / , % # ! * @ . \\7b  \\7d  \\5b \\5d ',
                'http://username:password@www.evil.com:8080/?k1=v1&k2=v2#hash',
                '-x-url(https://www.evil.com)',
                'u\ufffd\ufffdrl(https://www.evil.com)',
                '\\5c u\\5c r\ufffd\ufffd\\5c l\ufffd\\5c ((evil.com))',
                'expression(body.scrollTop + 50 + px)',
                '(((()))) \\5c 28 \\5c 29'
            ];
            testutils.test_yce(filter.yced, testPatterns, expectedResults);
        });
        it('filter yces[uds] test', function() {
            var expectedResults = [ 'undefined', 'null',
                '&', 
                '1.1', '10%', '+10px', '-10px', '#fff', 
                '\uD7FF', '\uD800', '\uDFFF', '\u1234567',
                '\uFFFD', ' ', '\\d \\a \\9 \\c \\b ', '\\5c ', '\\5c n\\5c r\\5c f\\5c 0\\5c 9\\5c a\\5c f',
                '-ide_nt', '"string"', "\\27 string\\27 ",
                '-  _ : ; ( ) " \\27  / , % # ! * @ . \\7b  \\7d  \\5b \\5d ',
                'http://username:password@www.evil.com:8080/?k1=v1&k2=v2#hash',
                '-x-url(https://www.evil.com)',
                'u\ufffd\ufffdrl(https://www.evil.com)',
                '\\5c u\\5c r\ufffd\ufffd\\5c l\ufffd\\5c ((evil.com))',
                'expression(body.scrollTop + 50 + px)',
                '(((()))) \\5c 28 \\5c 29'
            ];
            testutils.test_yce(filter.yces, testPatterns, expectedResults);
        });
    });

    describe("private-xss-filters: css url tests", function() {
        var testPatterns = [ undefined, null,
            '&',
            '1.1', '10%', '+10px', '-10px', '#fff', 
            '\\a', '\uD7FF', '\u1234567',
            '\u0000', ' ', '\r\n\t\f\x0B', '\\', '\\n\\r\\f\\0\\9\\a\\f',
            '-ide_nt', '"string"', "'string'",
            '- \ _ : ; ( ) " \' / , % # ! * @ . { } [ ]', 
            'http://username:password@www.evil.com:8080/?k1=v1&k2=v2#hash',
            '\u0000\u0008\u000b\u007f\u000e-\u001f',
            '&rpar;&#x00029;&#41;&lpar;&#x00028;&#40;&apos;&#x00027;&#39;&quot;&QUOT;&#x00022;&#34',
            'javascript:alert(1)',
            '(((()))) \\28 \\29'
        ];

        it('filter yceuu[uds] attribute test', function() {
            var expectedResults = [ 'undefined', 'null',
                '&', 
                '1.1', '10%25', '+10px', '-10px', '#fff', 
                '%5Ca', '%ED%9F%BF', '%E1%88%B4567',
                '%EF%BF%BD', '%20', '%0D%0A%09%0C%0B', '%5C', '%5Cn%5Cr%5Cf%5C0%5C9%5Ca%5Cf',
                '-ide_nt', '%22string%22', "\\27 string\\27 ",
                '-%20%20_%20:%20;%20%28%20%29%20%22%20\\27 %20/%20,%20%25%20#%20!%20*%20@%20.%20%7B%20%7D%20%5B%20%5D',
                'http://username:password@www.evil.com:8080/?k1=v1&k2=v2#hash',
                '%EF%BF%BD%08%0B%7F%0E-%1F',
                '%29%29%29%28%28%28\\27 \\27 \\27 %22%22%22%22',
                '##javascript:alert%281%29',
                '%28%28%28%28%29%29%29%29%20%5C28%20%5C29'
            ];
            testutils.test_yce(filter.yceuu, testPatterns, expectedResults);
        });
        it('filter yceud[uds] test', function() {
            var expectedResults = [ 'undefined', 'null',
                '&', 
                '1.1', '10%25', '+10px', '-10px', '#fff', 
                '%5Ca', '%ED%9F%BF', '%E1%88%B4567',
                '%EF%BF%BD', '%20', '%0D%0A%09%0C%0B', '%5C', '%5Cn%5Cr%5Cf%5C0%5C9%5Ca%5Cf',
                '-ide_nt', '%22string%22', "'string'",
                '-%20%20_%20:%20;%20(%20)%20%22%20\'%20/%20,%20%25%20#%20!%20*%20@%20.%20%7B%20%7D%20%5B%20%5D',
                'http://username:password@www.evil.com:8080/?k1=v1&k2=v2#hash',
                '%EF%BF%BD%08%0B%7F%0E-%1F',
                ')))(((\'\'\'%22%22%22%22',
                '##javascript:alert(1)',
                '(((())))%20%5C28%20%5C29'
            ];
            testutils.test_yce(filter.yceud, testPatterns, expectedResults);
        });
        it('filter yceus[uds] test', function() {
            var expectedResults = [ 'undefined', 'null',
                '&', 
                '1.1', '10%25', '+10px', '-10px', '#fff', 
                '%5Ca', '%ED%9F%BF', '%E1%88%B4567',
                '%EF%BF%BD', '%20', '%0D%0A%09%0C%0B', '%5C', '%5Cn%5Cr%5Cf%5C0%5C9%5Ca%5Cf',
                '-ide_nt', '%22string%22', "\\27 string\\27 ",
                '-%20%20_%20:%20;%20(%20)%20%22%20\\27 %20/%20,%20%25%20#%20!%20*%20@%20.%20%7B%20%7D%20%5B%20%5D',
                'http://username:password@www.evil.com:8080/?k1=v1&k2=v2#hash',
                '%EF%BF%BD%08%0B%7F%0E-%1F',
                ')))(((\\27 \\27 \\27 %22%22%22%22',
                '##javascript:alert(1)',
                '(((())))%20%5C28%20%5C29'
            ];
            testutils.test_yce(filter.yceus, testPatterns, expectedResults);
        });
    });

    describe("private-xss-filters: utility tests", function() {
        it('htmlDecode d exists', function() {
            expect(filter.d).to.be.ok();
        });
        it('htmlDecode d test', function() {
            expect(filter.d(null)).to.equal('null');
            expect(filter.d()).to.equal('undefined');
            expect(filter.d('&Aacute;&#0;&#x0D;&#x80;&#x82;&#x94;&#x9F;&#xD800;&#xFDD0;')).to.equal('&Aacute;\uFFFD\uFFFD\u20AC\u201A\u201D\u0178\uFFFD\uFFFD');
        });

        it('frCoPt exists', function() {
            expect(filter.frCoPt).to.be.ok();
        });
        it('frCoPt test', function() {
            expect(filter.frCoPt(null)).to.equal('');
            expect(filter.frCoPt()).to.equal('');
            expect(filter.frCoPt(0)).to.equal('\uFFFD');
            expect(filter.frCoPt(10)).to.equal('\n');
            expect(filter.frCoPt(0x0B)).to.equal('\uFFFD');
            expect(filter.frCoPt(0x10FFFF)).to.equal('\uFFFD');
        });
    });

}());
