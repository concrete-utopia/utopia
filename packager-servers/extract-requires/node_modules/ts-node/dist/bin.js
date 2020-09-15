#!/usr/bin/env node
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.main = void 0;
const path_1 = require("path");
const repl_1 = require("repl");
const util_1 = require("util");
const Module = require("module");
const arg = require("arg");
const diff_1 = require("diff");
const vm_1 = require("vm");
const fs_1 = require("fs");
const os_1 = require("os");
const index_1 = require("./index");
/**
 * Eval filename for REPL/debug.
 */
const EVAL_FILENAME = `[eval].ts`;
/**
 * Eval state management.
 */
class EvalState {
    constructor(path) {
        this.path = path;
        this.input = '';
        this.output = '';
        this.version = 0;
        this.lines = 0;
    }
}
/**
 * Main `bin` functionality.
 */
function main(argv) {
    const args = arg({
        // Node.js-like options.
        '--eval': String,
        '--interactive': Boolean,
        '--print': Boolean,
        '--require': [String],
        // CLI options.
        '--help': Boolean,
        '--script-mode': Boolean,
        '--version': arg.COUNT,
        // Project options.
        '--dir': String,
        '--files': Boolean,
        '--compiler': String,
        '--compiler-options': index_1.parse,
        '--project': String,
        '--ignore-diagnostics': [String],
        '--ignore': [String],
        '--transpile-only': Boolean,
        '--type-check': Boolean,
        '--compiler-host': Boolean,
        '--pretty': Boolean,
        '--skip-project': Boolean,
        '--skip-ignore': Boolean,
        '--prefer-ts-exts': Boolean,
        '--log-error': Boolean,
        '--emit': Boolean,
        // Aliases.
        '-e': '--eval',
        '-i': '--interactive',
        '-p': '--print',
        '-r': '--require',
        '-h': '--help',
        '-s': '--script-mode',
        '-v': '--version',
        '-T': '--transpile-only',
        '-H': '--compiler-host',
        '-I': '--ignore',
        '-P': '--project',
        '-C': '--compiler',
        '-D': '--ignore-diagnostics',
        '-O': '--compiler-options'
    }, {
        argv,
        stopAtPositional: true
    });
    // Only setting defaults for CLI-specific flags
    // Anything passed to `register()` can be `undefined`; `create()` will apply
    // defaults.
    const { '--dir': dir, '--help': help = false, '--script-mode': scriptMode = false, '--version': version = 0, '--require': argsRequire = [], '--eval': code = undefined, '--print': print = false, '--interactive': interactive = false, '--files': files, '--compiler': compiler, '--compiler-options': compilerOptions, '--project': project, '--ignore-diagnostics': ignoreDiagnostics, '--ignore': ignore, '--transpile-only': transpileOnly, '--type-check': typeCheck, '--compiler-host': compilerHost, '--pretty': pretty, '--skip-project': skipProject, '--skip-ignore': skipIgnore, '--prefer-ts-exts': preferTsExts, '--log-error': logError, '--emit': emit } = args;
    if (help) {
        console.log(`
  Usage: ts-node [options] [ -e script | script.ts ] [arguments]

  Options:

    -e, --eval [code]              Evaluate code
    -p, --print                    Print result of \`--eval\`
    -r, --require [path]           Require a node module before execution
    -i, --interactive              Opens the REPL even if stdin does not appear to be a terminal

    -h, --help                     Print CLI usage
    -v, --version                  Print module version information
    -s, --script-mode              Use cwd from <script.ts> instead of current directory

    -T, --transpile-only           Use TypeScript's faster \`transpileModule\`
    -H, --compiler-host            Use TypeScript's compiler host API
    -I, --ignore [pattern]         Override the path patterns to skip compilation
    -P, --project [path]           Path to TypeScript JSON project file
    -C, --compiler [name]          Specify a custom TypeScript compiler
    -D, --ignore-diagnostics [code] Ignore TypeScript warnings by diagnostic code
    -O, --compiler-options [opts]   JSON object to merge with compiler options

    --dir                          Specify working directory for config resolution
    --scope                        Scope compiler to files within \`cwd\` only
    --files                        Load \`files\`, \`include\` and \`exclude\` from \`tsconfig.json\` on startup
    --pretty                       Use pretty diagnostic formatter (usually enabled by default)
    --skip-project                 Skip reading \`tsconfig.json\`
    --skip-ignore                  Skip \`--ignore\` checks
    --prefer-ts-exts               Prefer importing TypeScript files over JavaScript files
    --log-error                    Logs TypeScript errors to stderr instead of throwing exceptions
  `);
        process.exit(0);
    }
    // Output project information.
    if (version === 1) {
        console.log(`v${index_1.VERSION}`);
        process.exit(0);
    }
    const cwd = dir || process.cwd();
    /** Unresolved.  May point to a symlink, not realpath.  May be missing file extension */
    const scriptPath = args._.length ? path_1.resolve(cwd, args._[0]) : undefined;
    const state = new EvalState(scriptPath || path_1.join(cwd, EVAL_FILENAME));
    // Register the TypeScript compiler instance.
    const service = index_1.register({
        dir: getCwd(dir, scriptMode, scriptPath),
        emit,
        files,
        pretty,
        transpileOnly,
        typeCheck,
        compilerHost,
        ignore,
        preferTsExts,
        logError,
        project,
        skipProject,
        skipIgnore,
        compiler,
        ignoreDiagnostics,
        compilerOptions,
        require: argsRequire,
        readFile: code !== undefined
            ? (path) => {
                if (path === state.path)
                    return state.input;
                try {
                    return fs_1.readFileSync(path, 'utf8');
                }
                catch (err) { /* Ignore. */ }
            }
            : undefined,
        fileExists: code !== undefined
            ? (path) => {
                if (path === state.path)
                    return true;
                try {
                    const stats = fs_1.statSync(path);
                    return stats.isFile() || stats.isFIFO();
                }
                catch (err) {
                    return false;
                }
            }
            : undefined
    });
    // Output project information.
    if (version >= 2) {
        console.log(`ts-node v${index_1.VERSION}`);
        console.log(`node ${process.version}`);
        console.log(`compiler v${service.ts.version}`);
        process.exit(0);
    }
    // Create a local module instance based on `cwd`.
    const module = new Module(state.path);
    module.filename = state.path;
    module.paths = Module._nodeModulePaths(cwd);
    // Prepend `ts-node` arguments to CLI for child processes.
    process.execArgv.unshift(__filename, ...process.argv.slice(2, process.argv.length - args._.length));
    process.argv = [process.argv[1]].concat(scriptPath || []).concat(args._.slice(1));
    // Execute the main contents (either eval, script or piped).
    if (code !== undefined && !interactive) {
        evalAndExit(service, state, module, code, print);
    }
    else {
        if (args._.length) {
            Module.runMain();
        }
        else {
            // Piping of execution _only_ occurs when no other script is specified.
            // --interactive flag forces REPL
            if (interactive || process.stdin.isTTY) {
                startRepl(service, state, code);
            }
            else {
                let buffer = code || '';
                process.stdin.on('data', (chunk) => buffer += chunk);
                process.stdin.on('end', () => evalAndExit(service, state, module, buffer, print));
            }
        }
    }
}
exports.main = main;
/**
 * Get project path from args.
 */
function getCwd(dir, scriptMode, scriptPath) {
    // Validate `--script-mode` usage is correct.
    if (scriptMode) {
        if (!scriptPath) {
            throw new TypeError('Script mode must be used with a script name, e.g. `ts-node -s <script.ts>`');
        }
        if (dir) {
            throw new TypeError('Script mode cannot be combined with `--dir`');
        }
        // Use node's own resolution behavior to ensure we follow symlinks.
        // scriptPath may omit file extension or point to a directory with or without package.json.
        // This happens before we are registered, so we tell node's resolver to consider ts, tsx, and jsx files.
        // In extremely rare cases, is is technically possible to resolve the wrong directory,
        // because we do not yet know preferTsExts, jsx, nor allowJs.
        // See also, justification why this will not happen in real-world situations:
        // https://github.com/TypeStrong/ts-node/pull/1009#issuecomment-613017081
        const exts = ['.js', '.jsx', '.ts', '.tsx'];
        const extsTemporarilyInstalled = [];
        for (const ext of exts) {
            if (!hasOwnProperty(require.extensions, ext)) { // tslint:disable-line
                extsTemporarilyInstalled.push(ext);
                require.extensions[ext] = function () { }; // tslint:disable-line
            }
        }
        try {
            return path_1.dirname(require.resolve(scriptPath));
        }
        finally {
            for (const ext of extsTemporarilyInstalled) {
                delete require.extensions[ext]; // tslint:disable-line
            }
        }
    }
    return dir;
}
/**
 * Evaluate a script.
 */
function evalAndExit(service, state, module, code, isPrinted) {
    let result;
    global.__filename = module.filename;
    global.__dirname = path_1.dirname(module.filename);
    global.exports = module.exports;
    global.module = module;
    global.require = module.require.bind(module);
    try {
        result = _eval(service, state, code);
    }
    catch (error) {
        if (error instanceof index_1.TSError) {
            console.error(error);
            process.exit(1);
        }
        throw error;
    }
    if (isPrinted) {
        console.log(typeof result === 'string' ? result : util_1.inspect(result));
    }
}
/**
 * Evaluate the code snippet.
 */
function _eval(service, state, input) {
    const lines = state.lines;
    const isCompletion = !/\n$/.test(input);
    const undo = appendEval(state, input);
    let output;
    try {
        output = service.compile(state.input, state.path, -lines);
    }
    catch (err) {
        undo();
        throw err;
    }
    // Use `diff` to check for new JavaScript to execute.
    const changes = diff_1.diffLines(state.output, output);
    if (isCompletion) {
        undo();
    }
    else {
        state.output = output;
    }
    return changes.reduce((result, change) => {
        return change.added ? exec(change.value, state.path) : result;
    }, undefined);
}
/**
 * Execute some code.
 */
function exec(code, filename) {
    const script = new vm_1.Script(code, { filename: filename });
    return script.runInThisContext();
}
/**
 * Start a CLI REPL.
 */
function startRepl(service, state, code) {
    // Eval incoming code before the REPL starts.
    if (code) {
        try {
            _eval(service, state, `${code}\n`);
        }
        catch (err) {
            console.error(err);
            process.exit(1);
        }
    }
    const repl = repl_1.start({
        prompt: '> ',
        input: process.stdin,
        output: process.stdout,
        // Mimicking node's REPL implementation: https://github.com/nodejs/node/blob/168b22ba073ee1cbf8d0bcb4ded7ff3099335d04/lib/internal/repl.js#L28-L30
        terminal: process.stdout.isTTY && !parseInt(process.env.NODE_NO_READLINE, 10),
        eval: replEval,
        useGlobal: true
    });
    /**
     * Eval code from the REPL.
     */
    function replEval(code, _context, _filename, callback) {
        let err = null;
        let result;
        // TODO: Figure out how to handle completion here.
        if (code === '.scope') {
            callback(err);
            return;
        }
        try {
            result = _eval(service, state, code);
        }
        catch (error) {
            if (error instanceof index_1.TSError) {
                // Support recoverable compilations using >= node 6.
                if (repl_1.Recoverable && isRecoverable(error)) {
                    err = new repl_1.Recoverable(error);
                }
                else {
                    console.error(error);
                }
            }
            else {
                err = error;
            }
        }
        return callback(err, result);
    }
    // Bookmark the point where we should reset the REPL state.
    const resetEval = appendEval(state, '');
    function reset() {
        resetEval();
        // Hard fix for TypeScript forcing `Object.defineProperty(exports, ...)`.
        exec('exports = module.exports', state.path);
    }
    reset();
    repl.on('reset', reset);
    repl.defineCommand('type', {
        help: 'Check the type of a TypeScript identifier',
        action: function (identifier) {
            if (!identifier) {
                repl.displayPrompt();
                return;
            }
            const undo = appendEval(state, identifier);
            const { name, comment } = service.getTypeInfo(state.input, state.path, state.input.length);
            undo();
            if (name)
                repl.outputStream.write(`${name}\n`);
            if (comment)
                repl.outputStream.write(`${comment}\n`);
            repl.displayPrompt();
        }
    });
    // Set up REPL history when available natively via node.js >= 11.
    if (repl.setupHistory) {
        const historyPath = process.env.TS_NODE_HISTORY || path_1.join(os_1.homedir(), '.ts_node_repl_history');
        repl.setupHistory(historyPath, err => {
            if (!err)
                return;
            console.error(err);
            process.exit(1);
        });
    }
}
/**
 * Append to the eval instance and return an undo function.
 */
function appendEval(state, input) {
    const undoInput = state.input;
    const undoVersion = state.version;
    const undoOutput = state.output;
    const undoLines = state.lines;
    // Handle ASI issues with TypeScript re-evaluation.
    if (undoInput.charAt(undoInput.length - 1) === '\n' && /^\s*[\/\[(`-]/.test(input) && !/;\s*$/.test(undoInput)) {
        state.input = `${state.input.slice(0, -1)};\n`;
    }
    state.input += input;
    state.lines += lineCount(input);
    state.version++;
    return function () {
        state.input = undoInput;
        state.output = undoOutput;
        state.version = undoVersion;
        state.lines = undoLines;
    };
}
/**
 * Count the number of lines.
 */
function lineCount(value) {
    let count = 0;
    for (const char of value) {
        if (char === '\n') {
            count++;
        }
    }
    return count;
}
const RECOVERY_CODES = new Set([
    1003,
    1005,
    1109,
    1126,
    1160,
    1161,
    2355 // "A function whose declared type is neither 'void' nor 'any' must return a value."
]);
/**
 * Check if a function can recover gracefully.
 */
function isRecoverable(error) {
    return error.diagnosticCodes.every(code => RECOVERY_CODES.has(code));
}
/** Safe `hasOwnProperty` */
function hasOwnProperty(object, property) {
    return Object.prototype.hasOwnProperty.call(object, property);
}
if (require.main === module) {
    main(process.argv.slice(2));
}
//# sourceMappingURL=bin.js.map