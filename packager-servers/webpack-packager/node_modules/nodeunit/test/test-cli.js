var exec = require('child_process').exec,
    path = require('path');

var bin = (process.platform === 'win32' ? 'node ' : "") +
    path.resolve(__dirname, '../bin/nodeunit');
var testfile_fullpath = path.resolve(__dirname, './fixtures/example_test.js');
var fixtures_path = path.resolve(__dirname, './fixtures');

exports['run test suite using absolute path'] = function (test) {
    exec(bin + ' ' + testfile_fullpath, function (err, stdout, stderr) {
        if (err) {
            return test.done(err);
        }
        test.ok(/example test/.test(stdout));
        test.ok(/1 assertion/.test(stdout));
        test.done();
    });
};

exports['runs only top-level suites without recursive flag'] = function (test) {
    exec(bin + ' ' + fixtures_path, function (err, stdout, stderr) {
        if (err) {
            return test.done(err);
        }
        test.ok(/example test/.test(stdout));
        test.ok(!/example test sub/.test(stdout));
        test.done();
    });
};

exports['runs top + nested suites with recursive flag'] = function (test) {
    exec(bin + ' ' + fixtures_path + ' -r', function (err, stdout, stderr) {
        if (err) {
            return test.done(err);
        }
        test.ok(/example test/.test(stdout));
        test.ok(/example test sub/.test(stdout));
        test.done();
    });
};
