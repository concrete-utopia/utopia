import { safeFunction } from '../../shared/code-exec-utils'
import * as Babel from '@babel/standalone'
import * as BabelParser from '@babel/parser'
import traverse from '@babel/traverse'
import * as BabelTransformCommonJS from '@babel/plugin-transform-modules-commonjs'
import * as BabelExportNamespaceFrom from '@babel/plugin-proposal-export-namespace-from'
import * as BabelClassProperties from '@babel/plugin-proposal-class-properties'

import type { FileEvaluationCache } from '../package-manager/package-manager'
import type { RawSourceMap } from '../../workers/ts/ts-typings/RawSourceMap'

function getFileExtension(filepath: string) {
  const lastDot = filepath.lastIndexOf('.')
  return filepath.slice(lastDot + 1)
}

function isJSXLikeFilePath(filepath: string): boolean {
  const fileExtension = getFileExtension(filepath)
  return fileExtension === 'jsx' || fileExtension === 'tsx'
}

function isEsModuleError(error: Error) {
  return (
    error.name === 'SyntaxError' &&
    // this might accidentally trip on SyntaxErrors that have export and import in their text but are not related
    // I cannot test for a more explicit string because Node.js (Jest) and Chrome has slightly different error messages,
    // and it made me feel like I shouldn't codify the string matching to today's version of Chrome, so
    // I went with this more generic solution.
    (error.message.indexOf('export') > -1 ||
      error.message.indexOf('import') > -1 ||
      error.message.indexOf('*') > -1)
  )
}

function transformToCommonJS(
  filePath: string,
  moduleCode: string,
): { transpiledCode: string; sourceMap: RawSourceMap } {
  const plugins = [BabelTransformCommonJS, BabelExportNamespaceFrom, BabelClassProperties]
  const result = Babel.transform(moduleCode, {
    presets: ['es2016', 'react'],
    plugins: plugins,
    sourceType: 'module',
    sourceFileName: filePath,
    sourceMaps: true,
  })
  const sourceMap: RawSourceMap = {
    ...result.map,
    file: filePath,
  }
  return {
    transpiledCode: result.code,
    sourceMap: sourceMap,
  }
}

function includesReactImport(moduleCode: string): boolean {
  const ast = BabelParser.parse(moduleCode, { sourceType: 'module', plugins: ['jsx'] })
  let reactImportFound: boolean = false
  traverse(ast, {
    ImportDeclaration: (path) => {
      if (path.node.specifiers.some((specifier) => specifier.local.name === 'React')) {
        reactImportFound = true
        path.stop()
      }
    },
  })
  return reactImportFound
}

const reactImportText = `import * as React from 'react'`

function addReactImport(filePath: string, moduleCode: string): string {
  if (isJSXLikeFilePath(filePath) && !includesReactImport(moduleCode)) {
    return `${reactImportText}\n${moduleCode}`
  } else {
    return moduleCode
  }
}

function evaluateJs(
  filePath: string,
  moduleCodeBefore: string,
  fileEvaluationCache: FileEvaluationCache,
  requireFn: (toImport: string) => unknown,
): any {
  let module = fileEvaluationCache
  // With a lot of configurations of how to handle JSX, a React import is implicitly added to the code.
  // As this is a fallback case for code in the project and the code hasn't been transpiled yet, it may not have a React import.
  // This streamlines in a React import if it's missing, so that the transpiled code can refer to `React.createElement`.
  const moduleCode = addReactImport(filePath, moduleCodeBefore)

  function firstErrorHandler(error: Error): void {
    if (isEsModuleError(error)) {
      const { transpiledCode, sourceMap } = transformToCommonJS(filePath, moduleCode)
      evaluateWithHandler(transpiledCode, sourceMap, secondErrorHandler)
    } else {
      throw error
    }
  }

  function secondErrorHandler(error: Error): void {
    throw error
  }

  function evaluateWithHandler(
    code: string,
    sourceMap: RawSourceMap | null,
    errorHandler: (error: Error) => void,
  ): unknown {
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
    safeFunction(
      false,
      { require: requireFn, exports: exports, module: module, process: process },
      filePath,
      code,
      sourceMap,
      [],
      errorHandler,
    )(null)
    return module
  }

  evaluateWithHandler(moduleCode, null, firstErrorHandler)

  return module
}

export function evaluator(
  filepath: string,
  moduleCode: string,
  fileEvaluationCache: FileEvaluationCache,
  requireFn: (toImport: string) => unknown,
): any {
  const fileExtension = getFileExtension(filepath)
  switch (fileExtension) {
    case 'js':
    case 'jsx':
    case 'cjs':
    case 'mjs':
      return evaluateJs(filepath, moduleCode, fileEvaluationCache, requireFn)
    default:
      throw new Error(`error evaluating file ${filepath} – unsupported file type ${fileExtension}`)
  }
}
