import { SafeFunction } from '../../shared/code-exec-utils'
import { transformCssNodeModule } from '../../shared/css-style-loader'
import { createEsModuleError } from '../package-manager/package-manager'

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

function evaluateJs(
  filePath: string,
  moduleCode: string,
  partialModule: { exports: {} },
  requireFn: (toImport: string) => unknown,
) {
  // https://nodejs.org/api/modules.html#modules_module_exports
  let module = partialModule
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
    moduleCode,
    [],
    (error) => {
      // we throw the error here, the require fn will catch it
      if (isEsModuleError(error)) {
        error.message = createEsModuleError(filePath, error.message)
      }
      throw error
    },
  )()

  return module
}

function evaluateCss(
  filepath: string,
  moduleCode: string,
  partialModule: { exports: {} },
  requireFn: (toImport: string) => unknown,
) {
  const transpiledCode = transformCssNodeModule(filepath, moduleCode)
  return evaluateJs(filepath, transpiledCode, partialModule, requireFn)
}

export function evaluator(
  filepath: string,
  moduleCode: string,
  partialModule: { exports: {} },
  requireFn: (toImport: string) => unknown,
) {
  const fileExtension = getFileExtension(filepath)
  switch (fileExtension) {
    case 'js':
      return evaluateJs(filepath, moduleCode, partialModule, requireFn)
    case 'css':
      return evaluateCss(filepath, moduleCode, partialModule, requireFn)
    default:
      throw new Error(`error evaluating file ${filepath} – unsupported file type ${fileExtension}`)
  }
}
