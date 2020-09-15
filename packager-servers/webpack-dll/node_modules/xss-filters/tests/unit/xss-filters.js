/*
Copyright (c) 2015, Yahoo! Inc. All rights reserved.
Copyrights licensed under the New BSD License.
See the accompanying LICENSE file for terms.

Authors: Nera Liu <neraliu@yahoo-inc.com>
         Adonis Fung <adon@yahoo-inc.com>
         Albert Yu <albertyu@yahoo-inc.com>
*/
(function() {

    var filter = xssFilters;

    delete filter._privFilters;
    delete filter._getPrivFilters;

    describe("xss-filters: existence tests", function() {

        it('filter inHTMLData exists', function() {
            expect(filter.inHTMLData).to.be.ok();
        });
        it('filter inHTMLComment exists', function() {
            expect(filter.inHTMLComment).to.be.ok();
        });
        it('filter inSingleQuotedAttr exists', function() {
            expect(filter.inSingleQuotedAttr).to.be.ok();
        });
        it('filter inDoubleQuotedAttr exists', function() {
            expect(filter.inDoubleQuotedAttr).to.be.ok();
        });
        it('filter inUnQuotedAttr exists', function() {
            expect(filter.inUnQuotedAttr).to.be.ok();
        });

        it('filter uriInSingleQuotedAttr exists', function() {
            expect(filter.uriInSingleQuotedAttr).to.be.ok();
        });
        it('filter uriInDoubleQuotedAttr exists', function() {
            expect(filter.uriInDoubleQuotedAttr).to.be.ok();
        });
        it('filter uriInUnQuotedAttr exists', function() {
            expect(filter.uriInUnQuotedAttr).to.be.ok();
        });
        it('filter uriInHTMLData exists', function() {
            expect(filter.uriInHTMLData).to.be.ok();
        });
        it('filter uriInHTMLComment exists', function() {
            expect(filter.uriInHTMLComment).to.be.ok();
        });


        it('filter uriPathInSingleQuotedAttr exists', function() {
            expect(filter.uriPathInSingleQuotedAttr).to.be.ok();
        });
        it('filter uriPathInDoubleQuotedAttr exists', function() {
            expect(filter.uriPathInDoubleQuotedAttr).to.be.ok();
        });
        it('filter uriPathInUnQuotedAttr exists', function() {
            expect(filter.uriPathInUnQuotedAttr).to.be.ok();
        });
        it('filter uriPathInHTMLData exists', function() {
            expect(filter.uriPathInHTMLData).to.be.ok();
        });
        it('filter uriPathInHTMLComment exists', function() {
            expect(filter.uriPathInHTMLComment).to.be.ok();
        });


        it('filter uriQueryInSingleQuotedAttr exists', function() {
            expect(filter.uriQueryInSingleQuotedAttr).to.be.ok();
        });
        it('filter uriQueryInDoubleQuotedAttr exists', function() {
            expect(filter.uriQueryInDoubleQuotedAttr).to.be.ok();
        });
        it('filter uriQueryInUnQuotedAttr exists', function() {
            expect(filter.uriQueryInUnQuotedAttr).to.be.ok();
        });
        it('filter uriQueryInHTMLData exists', function() {
            expect(filter.uriQueryInHTMLData).to.be.ok();
        });
        it('filter uriQueryInHTMLComment exists', function() {
            expect(filter.uriQueryInHTMLComment).to.be.ok();
        });


        it('filter uriComponentInSingleQuotedAttr exists', function() {
            expect(filter.uriComponentInSingleQuotedAttr).to.be.ok();
        });
        it('filter uriComponentInDoubleQuotedAttr exists', function() {
            expect(filter.uriComponentInDoubleQuotedAttr).to.be.ok();
        });
        it('filter uriComponentInUnQuotedAttr exists', function() {
            expect(filter.uriComponentInUnQuotedAttr).to.be.ok();
        });
        it('filter uriComponentInHTMLData exists', function() {
            expect(filter.uriComponentInHTMLData).to.be.ok();
        });
        it('filter uriComponentInHTMLComment exists', function() {
            expect(filter.uriComponentInHTMLComment).to.be.ok();
        });


        it('filter uriFragmentInSingleQuotedAttr exists', function() {
            expect(filter.uriFragmentInSingleQuotedAttr).to.be.ok();
        });
        it('filter uriFragmentInDoubleQuotedAttr exists', function() {
            expect(filter.uriFragmentInDoubleQuotedAttr).to.be.ok();
        });
        it('filter uriFragmentInUnQuotedAttr exists', function() {
            expect(filter.uriFragmentInUnQuotedAttr).to.be.ok();
        });
        it('filter uriFragmentInHTMLData exists', function() {
            expect(filter.uriFragmentInHTMLData).to.be.ok();
        });
        it('filter uriFragmentInHTMLComment exists', function() {
            expect(filter.uriFragmentInHTMLComment).to.be.ok();
        });

    });

    describe("xss-filters: alias tests", function() {
        it('filter uriQueryInSingleQuotedAttr being an alias of uriPathInSingleQuotedAttr', function() {
            expect(filter.uriQueryInSingleQuotedAttr).to.eql(filter.uriPathInSingleQuotedAttr);
        });
        it('filter uriQueryInDoubleQuotedAttr being an alias of uriPathInDoubleQuotedAttr', function() {
            expect(filter.uriQueryInDoubleQuotedAttr).to.eql(filter.uriPathInDoubleQuotedAttr);
        });
        it('filter uriQueryInUnQuotedAttr being an alias of uriPathInUnQuotedAttr', function() {
            expect(filter.uriQueryInUnQuotedAttr).to.eql(filter.uriPathInUnQuotedAttr);
        });
        it('filter uriQueryInHTMLData being an alias of uriPathInHTMLData', function() {
            expect(filter.uriQueryInHTMLData).to.eql(filter.uriPathInHTMLData);
        });
        it('filter uriQueryInHTMLComment being an alias of uriPathInHTMLComment', function() {
            expect(filter.uriQueryInHTMLComment).to.eql(filter.uriPathInHTMLComment);
        });


        it('filter uriFragmentInHTMLData being an alias of uriComponentInHTMLData', function() {
            expect(filter.uriFragmentInHTMLData).to.eql(filter.uriComponentInHTMLData);
        });
        it('filter uriFragmentInHTMLComment being an alias of uriComponentInHTMLComment', function() {
            expect(filter.uriFragmentInHTMLComment).to.eql(filter.uriComponentInHTMLComment);
        });
    });

    describe("xss-filters: error tests", function() {

        it('filters handling of undefined input', function() {
            for (var f in filter)
                expect(filter[f]()).to.eql('undefined');
        });
    });

    describe("xss-filters: state transition tests", function() {
        
        /*
         * reference:
         * https://html.spec.whatwg.org/multipage/syntax.html#data-state
         */
        it('filter inHTMLData state transition test', function() {
            testutils.test_yd(filter.inHTMLData, ['foo&&lt;>\'"']);
        });

        /*
         * reference
         * https://html.spec.whatwg.org/multipage/syntax.html#comment-state
         */
        it('filter inHTMLComment state transition test', function() {
            testutils.test_yc(filter.inHTMLComment, [
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


        /*
         * reference
         * https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(single-quoted)-state
         */
        it('filter inSingleQuotedAttr state transition test', function() {
            testutils.test_yav(filter.inSingleQuotedAttr, [
                'foo&<>&#39;"` \t\n\x0B\f\r', '\f', '',
                '&#39;&#39;', ' &#39;&#39;', '\t&#39;&#39;', '\n&#39;&#39;', '\f&#39;&#39;',
                '""',         ' ""',         '\t""',         '\n""',         '\f""',
                '``',         ' ``',         '\t``',         '\n``',         '\f``']);
        });

        /*
         * reference
         * https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(double-quoted)-state
         */
        it('filter inDoubleQuotedAttr state transition test', function() {
            testutils.test_yav(filter.inDoubleQuotedAttr, [
                'foo&<>\'&quot;` \t\n\x0B\f\r', '\f', '',
                "''",           " ''",           "\t''",           "\n''",           "\f''", 
                '&quot;&quot;', ' &quot;&quot;', '\t&quot;&quot;', '\n&quot;&quot;', '\f&quot;&quot;',
                '``',           ' ``',           '\t``',           '\n``',           '\f``']);
        });
        
        /*
         * reference
         * https://html.spec.whatwg.org/multipage/syntax.html#attribute-value-(unquoted)-state
         */
        it('filter inUnQuotedAttr state transition test', function() {
            testutils.test_yav(filter.inUnQuotedAttr, [
                'foo&&lt;&gt;&#39;&quot;&#96;&#32;&#9;&#10;&#11;&#12;&#13;', '&#12;', '\uFFFD',
                "&#39;&#39;",  "&#32;&#39;&#39;", "&#9;&#39;&#39;", "&#10;&#39;&#39;", "&#12;&#39;&#39;",
                '&quot;&quot;', '&#32;&quot;&quot;', '&#9;&quot;&quot;', '&#10;&quot;&quot;', '&#12;&quot;&quot;',
                '&#96;&#96;',  '&#32;&#96;&#96;', '&#9;&#96;&#96;', '&#10;&#96;&#96;', '&#12;&#96;&#96;']);
        });
        


        
        it('filter uriInSingleQuotedAttr state transition test', function() {
            // encodeURI('foo&<>\'"` \t\n\x0B\f\r') = foo&%3C%3E'%22%60%20%09%0A%0B%0C%0D
            testutils.test_yav(filter.uriInSingleQuotedAttr, [
                'foo&%3C%3E&#39;%22%60%20%09%0A%0B%0C%0D', '%0C', '',
                '&#39;&#39;', '%20&#39;&#39;', '%09&#39;&#39;', '%0A&#39;&#39;', '%0C&#39;&#39;',
                '%22%22',     '%20%22%22',     '%09%22%22',     '%0A%22%22',     '%0C%22%22',
                '%60%60',     '%20%60%60',     '%09%60%60',     '%0A%60%60',     '%0C%60%60']);
            testutils.test_yufull(filter.uriInSingleQuotedAttr, [
                null, // default
                null, // default
                null, // default
                null, // default
                null, // default
                'http://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]',
                null, // default
                null  // default
            ]);
            testutils.test_yubl(filter.uriInSingleQuotedAttr);
        });
        it('filter uriInDoubleQuotedAttr state transition test', function() {
            // encodeURI('foo&<>\'"` \t\n\x0B\f\r') = foo&%3C%3E'%22%60%20%09%0A%0B%0C%0D
            testutils.test_yav(filter.uriInDoubleQuotedAttr, [
                'foo&%3C%3E\'%22%60%20%09%0A%0B%0C%0D', '%0C', '',
                '\'\'',   '%20\'\'',   '%09\'\'',   '%0A\'\'',   '%0C\'\'',
                '%22%22', '%20%22%22', '%09%22%22', '%0A%22%22', '%0C%22%22',
                '%60%60', '%20%60%60', '%09%60%60', '%0A%60%60', '%0C%60%60']);
            testutils.test_yufull(filter.uriInDoubleQuotedAttr, [
                null, // default
                null, // default
                null, // default
                null, // default
                null, // default
                'http://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]',
                null, // default
                null  // default
            ]);
            testutils.test_yubl(filter.uriInDoubleQuotedAttr);
        });
        it('filter uriInUnQuotedAttr state transition test', function() {
            // encodeURI('foo&<>\'"` \t\n\x0B\f\r') = foo&%3C%3E'%22%60%20%09%0A%0B%0C%0D
            testutils.test_yav(filter.uriInUnQuotedAttr, [
                'foo&%3C%3E&#39;%22%60%20%09%0A%0B%0C%0D', '%0C', '\uFFFD',
                '&#39;&#39;', '%20&#39;&#39;',   '%09&#39;&#39;',     '%0A&#39;&#39;',   '%0C&#39;&#39;',
                '%22%22',  '%20%22%22', '%09%22%22', '%0A%22%22', '%0C%22%22',
                '%60%60',  '%20%60%60', '%09%60%60', '%0A%60%60', '%0C%60%60']);
            testutils.test_yufull(filter.uriInUnQuotedAttr, [
                'http://6.6.6.6/?q&#61;%5Bsomewhere%5D',
                null, // default
                null, // default
                null, // default
                null, // default
                'http://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]',
                null, // default
                '%5B%5D?&&#61;#/:;'
            ]);
            testutils.test_yubl(filter.uriInUnQuotedAttr);
        });
        it('filter uriInHTMLData state transition test', function() {
            testutils.test_yd(filter.uriInHTMLData, ['foo&%3C%3E\'%22']);
            testutils.test_yufull(filter.uriInHTMLData, [
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
        it('filter uriInHTMLComment state transition test', function() {
            testutils.test_yc(filter.uriInHTMLComment, [
                '--%3E%20--!%3E%20%3C!--%5Bif%20IE%5D%3E%3Cscript%3Ealert(%22yahoo\'s%20filters%22)%3C/script%3E', 
                'foo-- ', 
                'foo--! ', 
                '%5Bif%20IE%5D', 
                'foo- ', 
                'foo- ',
                '%3E%3Cscript%3Ealert(1)%3C/script%3E',
                '----------%3E%3Cscript%3Ealert(1)%3C/script%3E',
                '--%00%3E']);
            testutils.test_yufull(filter.uriInHTMLComment, [
                null, // default
                null, // default
                null, // default
                null, // default
                null, // default
                'http://[2001:0db8:85a3:0000:0000:8a2e:0370:7334] ',
                null, // default
                null  // default
            ]);
        });



        it('filter uriPathInSingleQuotedAttr state transition test', function() {
            // encodeURI('foo&<>\'"` \t\n\x0B\f\r') = foo&%3C%3E'%22%60%20%09%0A%0B%0C%0D
            testutils.test_yav(filter.uriPathInSingleQuotedAttr, [
                'foo&%3C%3E&#39;%22%60%20%09%0A%0B%0C%0D', '%0C', '',
                '&#39;&#39;', '%20&#39;&#39;', '%09&#39;&#39;', '%0A&#39;&#39;', '%0C&#39;&#39;',
                '%22%22',     '%20%22%22',     '%09%22%22',     '%0A%22%22',     '%0C%22%22',
                '%60%60',     '%20%60%60',     '%09%60%60',     '%0A%60%60',     '%0C%60%60']);
            testutils.test_yu(filter.uriPathInSingleQuotedAttr);
            testutils.test_yubl(filter.uriPathInSingleQuotedAttr);
        });
        it('filter uriPathInDoubleQuotedAttr state transition test', function() {
            // encodeURI('foo&<>\'"` \t\n\x0B\f\r') = foo&%3C%3E'%22%60%20%09%0A%0B%0C%0D
            testutils.test_yav(filter.uriPathInDoubleQuotedAttr, [
                'foo&%3C%3E\'%22%60%20%09%0A%0B%0C%0D', '%0C', '',
                '\'\'',   '%20\'\'',   '%09\'\'',     '%0A\'\'',   '%0C\'\'',
                '%22%22', '%20%22%22', '%09%22%22', '%0A%22%22', '%0C%22%22',
                '%60%60', '%20%60%60', '%09%60%60', '%0A%60%60', '%0C%60%60']);
            testutils.test_yu(filter.uriPathInDoubleQuotedAttr);
            testutils.test_yubl(filter.uriPathInDoubleQuotedAttr);
        });
        it('filter uriPathInUnQuotedAttr state transition test', function() {
            // encodeURI('foo&<>\'"` \t\n\x0B\f\r') = foo&%3C%3E'%22%60%20%09%0A%0B%0C%0D
            testutils.test_yav(filter.uriPathInUnQuotedAttr, [
                'foo&%3C%3E&#39;%22%60%20%09%0A%0B%0C%0D', '%0C', '\uFFFD',
                '&#39;&#39;', '%20&#39;&#39;',   '%09&#39;&#39;',     '%0A&#39;&#39;',   '%0C&#39;&#39;',
                '%22%22',  '%20%22%22', '%09%22%22', '%0A%22%22', '%0C%22%22',
                '%60%60',  '%20%60%60', '%09%60%60', '%0A%60%60', '%0C%60%60']);
            testutils.test_yu(filter.uriPathInUnQuotedAttr, [
                'http://6.6.6.6/?q&#61;%5Bsomewhere%5D',
                null, // default
                null, // default
                null, // default
                null, // default
                null, // default
                null, // default
                '%5B%5D?&&#61;#/:;'
            ]);
            testutils.test_yubl(filter.uriPathInUnQuotedAttr);
        });
        it('filter uriPathInHTMLData state transition test', function() {
            testutils.test_yd(filter.uriPathInHTMLData, ['foo&%3C%3E\'%22']);
            testutils.test_yu(filter.uriPathInHTMLData);
        });
        it('filter uriPathInHTMLComment state transition test', function() {
            testutils.test_yc(filter.uriPathInHTMLComment, [
                '--%3E%20--!%3E%20%3C!--%5Bif%20IE%5D%3E%3Cscript%3Ealert(%22yahoo\'s%20filters%22)%3C/script%3E', 
                'foo-- ', 
                'foo--! ', 
                '%5Bif%20IE%5D', 
                'foo- ', 
                'foo- ',
                '%3E%3Cscript%3Ealert(1)%3C/script%3E',
                '----------%3E%3Cscript%3Ealert(1)%3C/script%3E',
                '--%00%3E']);
            testutils.test_yu(filter.uriPathInHTMLComment);
        });



        it('filter uriComponentInSingleQuotedAttr state transition test', function() {
            // encodeURIComponent('foo&<>\'"` \t\n\x0B\f\r') = foo%26%3C%3E'%22%60%20%09%0A%0B%0C%0D
            testutils.test_yav(filter.uriComponentInSingleQuotedAttr, [
                'foo%26%3C%3E&#39;%22%60%20%09%0A%0B%0C%0D', '%0C', '',
                '&#39;&#39;', '%20&#39;&#39;', '%09&#39;&#39;', '%0A&#39;&#39;', '%0C&#39;&#39;',
                '%22%22',     '%20%22%22',     '%09%22%22',     '%0A%22%22',     '%0C%22%22',
                '%60%60',     '%20%60%60',     '%09%60%60',     '%0A%60%60',     '%0C%60%60']);
            testutils.test_yuc(filter.uriComponentInSingleQuotedAttr);
        });
        it('filter uriComponentInDoubleQuotedAttr state transition test', function() {
            // encodeURIComponent('foo&<>\'"` \t\n\x0B\f\r') = foo%26%3C%3E'%22%60%20%09%0A%0B%0C%0D
            testutils.test_yav(filter.uriComponentInDoubleQuotedAttr, [
                'foo%26%3C%3E\'%22%60%20%09%0A%0B%0C%0D', '%0C', '',
                '\'\'',   '%20\'\'',   '%09\'\'',     '%0A\'\'',   '%0C\'\'',
                '%22%22', '%20%22%22', '%09%22%22', '%0A%22%22', '%0C%22%22',
                '%60%60', '%20%60%60', '%09%60%60', '%0A%60%60', '%0C%60%60']);
            testutils.test_yuc(filter.uriComponentInDoubleQuotedAttr);
        });
        it('filter uriComponentInUnQuotedAttr state transition test', function() {
            // encodeURIComponent('foo&<>\'"` \t\n\x0B\f\r') = foo%26%3C%3E'%22%60%20%09%0A%0B%0C%0D
            testutils.test_yav(filter.uriComponentInUnQuotedAttr, [
                'foo%26%3C%3E&#39;%22%60%20%09%0A%0B%0C%0D', '%0C', '\uFFFD',
                '&#39;&#39;', '%20&#39;&#39;',   '%09&#39;&#39;',     '%0A&#39;&#39;',   '%0C&#39;&#39;',
                '%22%22',  '%20%22%22', '%09%22%22', '%0A%22%22', '%0C%22%22',
                '%60%60',  '%20%60%60', '%09%60%60', '%0A%60%60', '%0C%60%60']);
            testutils.test_yuc(filter.uriComponentInUnQuotedAttr);
        });
        it('filter uriComponentInHTMLData state transition test', function() {
            testutils.test_yd(filter.uriComponentInHTMLData, ['foo%26%3C%3E\'%22']);
            testutils.test_yuc(filter.uriComponentInHTMLData);
        });
        it('filter uriComponentInHTMLComment state transition test', function() {
            testutils.test_yc(filter.uriComponentInHTMLComment, [
                '--%3E%20--!%3E%20%3C!--%5Bif%20IE%5D%3E%3Cscript%3Ealert(%22yahoo\'s%20filters%22)%3C%2Fscript%3E', 
                'foo-- ', 
                'foo--! ', 
                '%5Bif%20IE%5D', 
                'foo- ', 
                'foo- ',
                '%3E%3Cscript%3Ealert(1)%3C%2Fscript%3E',
                '----------%3E%3Cscript%3Ealert(1)%3C%2Fscript%3E',
                '--%00%3E']);
            testutils.test_yuc(filter.uriComponentInHTMLComment);
        });



        it('filter uriFragmentInSingleQuotedAttr state transition test', function() {
            // encodeuriFragment('foo&<>\'"` \t\n\x0B\f\r') = foo%26%3C%3E'%22%60%20%09%0A%0B%0C%0D
            testutils.test_yav(filter.uriFragmentInSingleQuotedAttr, [
                'foo%26%3C%3E&#39;%22%60%20%09%0A%0B%0C%0D', '%0C', '',
                '&#39;&#39;', '%20&#39;&#39;', '%09&#39;&#39;', '%0A&#39;&#39;', '%0C&#39;&#39;',
                '%22%22',     '%20%22%22',     '%09%22%22',     '%0A%22%22',     '%0C%22%22',
                '%60%60',     '%20%60%60',     '%09%60%60',     '%0A%60%60',     '%0C%60%60']);
            testutils.test_yuc(filter.uriFragmentInSingleQuotedAttr);
        });
        it('filter uriFragmentInDoubleQuotedAttr state transition test', function() {
            // encodeuriFragment('foo&<>\'"` \t\n\x0B\f\r') = foo%26%3C%3E'%22%60%20%09%0A%0B%0C%0D
            testutils.test_yav(filter.uriFragmentInDoubleQuotedAttr, [
                'foo%26%3C%3E\'%22%60%20%09%0A%0B%0C%0D', '%0C', '',
                '\'\'',   '%20\'\'',   '%09\'\'',   '%0A\'\'',   '%0C\'\'',
                '%22%22', '%20%22%22', '%09%22%22', '%0A%22%22', '%0C%22%22',
                '%60%60', '%20%60%60', '%09%60%60', '%0A%60%60', '%0C%60%60']);
            testutils.test_yuc(filter.uriFragmentInDoubleQuotedAttr);
        });
        it('filter uriFragmentInUnQuotedAttr state transition test', function() {
            // encodeuriFragment('foo&<>\'"` \t\n\x0B\f\r') = foo%26%3C%3E'%22%60%20%09%0A%0B%0C%0D
            testutils.test_yav(filter.uriFragmentInUnQuotedAttr, [
                'foo%26%3C%3E&#39;%22%60%20%09%0A%0B%0C%0D', '%0C', '\uFFFD',
                '&#39;&#39;', '%20&#39;&#39;',   '%09&#39;&#39;',   '%0A&#39;&#39;',   '%0C&#39;&#39;',
                '%22%22',  '%20%22%22', '%09%22%22', '%0A%22%22', '%0C%22%22',
                '%60%60',  '%20%60%60', '%09%60%60', '%0A%60%60', '%0C%60%60']);
            testutils.test_yuc(filter.uriFragmentInUnQuotedAttr);
        });
        

    });
}());
