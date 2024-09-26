import type { NodeModules, ProjectContents, TextFile } from '../../core/shared/project-file-types'
import {
  esCodeFile,
  isParseSuccess,
  RevisionsState,
  textFile,
  textFileContents,
} from '../../core/shared/project-file-types'
import {
  lintAndParse,
  printCode,
  printCodeOptions,
} from '../../core/workers/parser-printer/parser-printer'
import type { ProjectContentTreeRoot } from '../assets'
import { contentsToTree, getTextFileByPath, treeToContents } from '../assets'
import type { EditorState } from '../editor/store/editor-state'
import { StoryboardFilePath } from '../editor/store/editor-state'
import { createComplexDefaultProjectContents } from '../../sample-projects/sample-project-utils'
import { replaceAll } from '../../core/shared/string-utils'
import { emptySet } from '../../core/shared/set-utils'
import { parseProjectContents } from '../../sample-projects/sample-project-utils.test-utils'

export const SampleNodeModules: NodeModules = {
  '/node_modules/non-existant-dummy-library/index.js': esCodeFile(
    `import * as React from 'react'
export const Spring = (props) => {
  return <div {...props} />
}`,
    'NODE_MODULES',
    '/node_modules/non-existant-dummy-library/index.js',
  ),
  '/node_modules/non-existant-dummy-library/package.json': esCodeFile(
    JSON.stringify({ main: './index.js' }),
    'NODE_MODULES',
    '/node_modules/non-existant-dummy-library/package.json',
  ),
  '/node_modules/@emotion/react/index.js': esCodeFile(
    `export {}`,
    'NODE_MODULES',
    '/node_modules/@emotion/react/index.js',
  ),
  '/node_modules/@emotion/react/package.json': esCodeFile(
    JSON.stringify({ main: './index.js' }),
    'NODE_MODULES',
    '/node_modules/@emotion/react/package.json',
  ),
}

export function createCodeFile(path: string, contents: string): TextFile {
  const result = lintAndParse(path, [], contents, 'trim-bounds', 'do-not-apply-steganography')
  return textFile(textFileContents(contents, result, RevisionsState.BothMatch), null, null, 0)
}

export function defaultProjectContentsForNormalising(): ProjectContentTreeRoot {
  const defaultProjectContents = treeToContents(
    parseProjectContents(contentsToTree(createComplexDefaultProjectContents())),
  )

  const unparsedCode = replaceAll(
    (defaultProjectContents[StoryboardFilePath] as TextFile).fileContents.code,
    `data-uid='`,
    `data-uid='unparse-`,
  )

  const projectContents: ProjectContents = {
    ...defaultProjectContents,
    '/utopia/unparsedstoryboard.js': createCodeFile('/utopia/unparsedstoryboard.js', unparsedCode),
  }

  return contentsToTree(projectContents)
}

export function printParsedCodeForFile(
  actualResult: EditorState,
  filename: string,
  stripUIDs: boolean = true,
): string {
  const codeFile = getTextFileByPath(actualResult.projectContents, filename)
  const parsed = codeFile.fileContents.parsed
  if (isParseSuccess(parsed)) {
    return printCode(
      filename,
      printCodeOptions(false, true, false, stripUIDs),
      parsed.imports,
      parsed.topLevelElements,
      parsed.jsxFactoryFunction,
      parsed.exportsDetail,
    )
  } else {
    throw new Error('No parsed version of the file.')
  }
}
