describe('extract-github', function () {
  'use strict';

  var extract = require('../')
    , path = require('path')
    , chai = require('chai')
    , expect = chai.expect
    , fs = require('fs');

  //
  // Expose the fixtures in a readable format.
  //
  var fixture = fs.readdirSync(
    path.join(__dirname, 'fixture')
  ).reduce(function reduced(memo, file) {
    var name = file.slice(0, -3);

    memo[name.replace('-', '_')] = memo[name] = fs.readFileSync(
      path.join(__dirname, 'fixture', file),
      'utf-8'
    );

    return memo;
  }, {});

  it('exports as a function', function () {
    expect(extract).to.be.a('function');
  });

  it('exports .url as a function', function () {
    expect(extract.url).to.be.a('function');
  });

  it('exports .type as a function', function () {
    expect(extract.type).to.be.a('function');
  });

  describe('.type', function () {
    it('detects arrays', function () {
      expect(extract.type([])).to.equal('array');
    });

    it('detects regexp', function () {
      expect(extract.type(/\./)).to.equal('regexp');
    });

    it('detects function', function fn() {
      expect(extract.type(fn)).to.equal('function');
    });

    it('detects string', function () {
      expect(extract.type('foo')).to.equal('string');
    });

    it('detects object', function () {
      expect(extract.type({})).to.equal('object');
    });
  });

  describe('.url', function () {
    it('returns the the string if given a matching string', function () {
      expect(extract.url('github.com', 'github')).to.equal('github.com');
    });

    it('retreives url properties', function () {
      expect(extract.url({ url: 'github.com'}, 'github')).to.equal('github.com');
    });

    it('retreives web properties', function () {
      expect(extract.url({ web: 'github.com'}, 'github')).to.equal('github.com');
    });

    it('returns the matching url only', function () {
      expect(extract.url({ url: 'google.com', web: 'github.com'}, 'github')).to.equal('github.com');
    });

    it('returns undefined on no match', function () {
      expect(extract.url('foo', 'bar')).to.equal(undefined);
    });

    it('has optional string matching', function () {
      expect(extract.url({ url: 'google.com', web: 'github.com'})).to.equal('google.com');
      expect(extract.url('google.com')).to.equal('google.com');
      expect(extract.url({ foo: 'foo' })).to.equal(undefined);
      expect(extract.url({ url: {} })).to.equal(undefined);
    });
  });

  describe('parse', function () {
    it('extracts the url from the homepage', function () {
      var github = extract({ homepage: 'http://github.com/3rd-Eden/extract-github/issues' });

      expect(github.user).to.equal('3rd-Eden');
      expect(github.repo).to.equal('extract-github');
    });

    it('extracts the url from the repository', function () {
      var github = extract({ repository: { web: 'git://github.com:3rd-Eden/extract-github' }});

      expect(github.user).to.equal('3rd-Eden');
      expect(github.repo).to.equal('extract-github');
    });

    it('extracts the url from the bugs', function () {
      var github = extract({ bugs: { url: 'http://github.com/3rd-Eden/extract-github/issues' }});

      expect(github.user).to.equal('3rd-Eden');
      expect(github.repo).to.equal('extract-github');
    });

    it('can default to a username', function () {
      var github = extract('3rd-Eden');
      expect(github.user).to.equal('3rd-Eden');
    });

    describe('README', function () {
      it('extracts the url from the README', function () {
        var github = extract(fs.readFileSync(path.join(__dirname, '../README.md'), 'utf-8'));

        expect(github.user).to.equal('3rd-Eden');
        expect(github.repo).to.equal('extract-github');
      });

      it('correctly parses node-jsconverage\'s README', function () {
        var github = extract(fixture.node_jscoverage);

        expect(github.user).to.equal('visionmedia');
        expect(github.repo).to.equal('node-jscoverage');
      });

      it('correctly parses assign\'s README', function () {
        expect(extract(fixture.assign)).to.equal(undefined);
        expect(extract({ readme: fixture.assign })).to.equal(undefined);
      });
    });

    it('extracts the url from Travis-CI', function () {
      var github = extract('https://travis-ci.org/3rd-Eden/extract-github.png');

      expect(github.user).to.equal('3rd-Eden');
      expect(github.repo).to.equal('extract-github');
    });

    it('extracts github pages', function () {
      var github = extract('http://3rd-eden.github.io/extract-github');

      expect(github.user).to.equal('3rd-eden');
      expect(github.repo).to.equal('extract-github');
    });
  });
});
