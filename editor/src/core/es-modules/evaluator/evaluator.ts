import { SafeFunction } from '../../shared/code-exec-utils'
import { transformCssNodeModule } from '../../shared/css-style-loader'

function getFileExtension(filepath: string) {
  const lastDot = filepath.lastIndexOf('.')
  return filepath.slice(lastDot + 1)
}

function evaluateJs(
  moduleCode: string,
  partialExports: {},
  requireFn: (toImport: string) => unknown,
) {
  let exports = partialExports
  // https://nodejs.org/api/modules.html#modules_module_exports
  let module = {
    exports: exports,
  }

  // https://nodejs.org/api/process.html#process_process_env
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
  )()

  return module.exports
}

function evaluateCss(
  filepath: string,
  moduleCode: string,
  partialExports: {},
  requireFn: (toImport: string) => unknown,
) {
  const transpiledCode = transformCssNodeModule(filepath, moduleCode)
  return evaluateJs(transpiledCode, partialExports, requireFn)
}

export function evaluator(
  filepath: string,
  moduleCode: string,
  partialExports: {},
  requireFn: (toImport: string) => unknown,
) {
  const fileExtension = getFileExtension(filepath)
  switch (fileExtension) {
    case 'js':
      return evaluateJs(moduleCode, partialExports, requireFn)
    case 'css':
      return evaluateCss(filepath, moduleCode, partialExports, requireFn)
    default:
      throw new Error(`error evaluating file ${filepath} – unsupported file type ${fileExtension}`)
  }
}
