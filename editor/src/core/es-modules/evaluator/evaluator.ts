import { SafeFunction } from '../../shared/code-exec-utils'
import * as Babel from '@babel/standalone'
import * as BabelTransformCommonJS from '@babel/plugin-transform-modules-commonjs'

function getFileExtension(filepath: string) {
  const lastDot = filepath.lastIndexOf('.')
  return filepath.slice(lastDot + 1)
}

function isEsModuleError(error: Error) {
  return (
    error.name === 'SyntaxError' &&
    // this might accidentally trip on SyntaxErrors that have export and import in their text but are not related
    // I cannot test for a more explicit string because Node.js (Jest) and Chrome has slightly different error messages,
    // and it made me feel like I shouldn't codify the string matching to today's version of Chrome, so
    // I went with this more generic solution.
    (error.message.indexOf('export') > -1 || error.message.indexOf('import') > -1)
  )
}

function transformToCommonJS(filePath: string, moduleCode: string): string {
  const plugins = [BabelTransformCommonJS]
  const result = Babel.transform(moduleCode, {
    presets: ['es2015', 'react'],
    plugins: plugins,
    sourceType: 'module',
    sourceFileName: filePath,
  }).code
  return result
}

function evaluateJs(
  filePath: string,
  moduleCode: string,
  partialModule: { exports: unknown },
  requireFn: (toImport: string) => unknown,
) {
  let module = partialModule

  function firstErrorHandler(error: Error): void {
    if (isEsModuleError(error)) {
      const transpiledCode = transformToCommonJS(filePath, moduleCode)
      evaluateWithHandler(transpiledCode, secondErrorHandler)
    } else {
      throw error
    }
  }

  function secondErrorHandler(error: Error): void {
    throw error
  }

  function evaluateWithHandler(code: string, errorHandler: (error: Error) => void): unknown {
    // https://nodejs.org/api/modules.html#modules_module_exports
    let exports = module.exports

    // https://nodejs.org/api/process.html#process_process_env
    // This is a hacky solution, ideally we'd want a transpiler / loader that replaces process.env.NODE_ENV with a user-defined value
    let process = {
      env: {
        NODE_ENV: 'production',
      },
    }

    // evaluating the module code https://nodejs.org/api/modules.html#modules_the_module_wrapper
    SafeFunction(
      false,
      { require: requireFn, exports: exports, module: module, process: process },
      code,
      [],
      errorHandler,
    )(null)
    return module
  }

  evaluateWithHandler(moduleCode, firstErrorHandler)

  return module
}

export function evaluator(
  filepath: string,
  moduleCode: string,
  partialModule: { exports: unknown },
  requireFn: (toImport: string) => unknown,
) {
  const fileExtension = getFileExtension(filepath)
  switch (fileExtension) {
    case 'js':
      return evaluateJs(filepath, moduleCode, partialModule, requireFn)
    default:
      throw new Error(`error evaluating file ${filepath} – unsupported file type ${fileExtension}`)
  }
}
