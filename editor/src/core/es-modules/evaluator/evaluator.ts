import { SafeFunction } from '../../shared/code-exec-utils'
import { transformCssNodeModule } from '../../shared/css-style-loader'

function getFileExtension(filepath: string) {
  const lastDot = filepath.lastIndexOf('.')
  return filepath.slice(lastDot + 1)
}

function evaluateJs(
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
  return evaluateJs(transpiledCode, partialModule, requireFn)
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
      return evaluateJs(moduleCode, partialModule, requireFn)
    case 'css':
      return evaluateCss(filepath, moduleCode, partialModule, requireFn)
    default:
      throw new Error(`error evaluating file ${filepath} – unsupported file type ${fileExtension}`)
  }
}
