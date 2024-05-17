import type { TextFile } from '../shared/project-file-types'
import {
  isParseSuccess,
  RevisionsState,
  textFile,
  textFileContents,
} from '../shared/project-file-types'
import { emptySet } from '../shared/set-utils'
import { lintAndParse } from '../workers/parser-printer/parser-printer'
import { appJSFile, getDefaultUIJsFile } from './new-project-files'

export function appJSFilePreParsed(): TextFile {
  const appFile = appJSFile()
  const result = lintAndParse(
    '/src/app.js',
    appFile.fileContents.code,
    null,
    'trim-bounds',
    'do-not-apply-steganography',
  )
  return textFile(
    textFileContents(appFile.fileContents.code, result, RevisionsState.BothMatch),
    null,
    isParseSuccess(result) ? result : null,
    0,
  )
}

export function getDefaultUIJsFilePreParsed(): TextFile {
  const uijsFile = getDefaultUIJsFile()
  const result = lintAndParse(
    'code.tsx',
    uijsFile.fileContents.code,
    null,
    'trim-bounds',
    'do-not-apply-steganography',
  )
  return textFile(
    textFileContents(uijsFile.fileContents.code, result, RevisionsState.BothMatch),
    null,
    isParseSuccess(result) ? result : null,
    0,
  )
}
