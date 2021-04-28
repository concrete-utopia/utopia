import { directory } from '../../core/model/project-file-utils'
import { objectMap } from '../../core/shared/object-utils'
import {
  esCodeFile,
  InstancePath,
  isTextFile,
  NodeModules,
  ProjectContents,
  ProjectFile,
  RevisionsState,
  TextFile,
  textFile,
  textFileContents,
  unparsed,
} from '../../core/shared/project-file-types'
import { lintAndParse, parseCode } from '../../core/workers/parser-printer/parser-printer'
import { ProjectContentTreeRoot, contentsToTree, getContentsTreeFileFromString } from '../assets'
import { DefaultPackageJson, StoryboardFilePath } from '../editor/store/editor-state'
import * as TP from '../../core/shared/template-path'
import { createComplexDefaultProjectContents } from '../../sample-projects/sample-project-utils'
import { replaceAll } from '../../core/shared/string-utils'

export const SampleNodeModules: NodeModules = {
  '/node_modules/utopia-api/index.js': esCodeFile(
    `export {}`,
    'NODE_MODULES',
    '/node_modules/utopia-api/index.js',
  ),
  '/node_modules/utopia-api/package.json': esCodeFile(
    JSON.stringify({ main: './index.js' }),
    'NODE_MODULES',
    '/node_modules/utopia-api/package.json',
  ),
  '/node_modules/uuiui/index.js': esCodeFile(
    `export {}`,
    'NODE_MODULES',
    '/node_modules/uuiui/index.js',
  ),
  '/node_modules/uuiui/package.json': esCodeFile(
    JSON.stringify({ main: './index.js' }),
    'NODE_MODULES',
    '/node_modules/uuiui/package.json',
  ),
  '/node_modules/react/index.js': esCodeFile(
    `export {}`,
    'NODE_MODULES',
    '/node_modules/react/index.js',
  ),
  '/node_modules/react/package.json': esCodeFile(
    JSON.stringify({ main: './index.js' }),
    'NODE_MODULES',
    '/node_modules/react/package.json',
  ),
  '/node_modules/react-dom/index.js': esCodeFile(
    `export {}`,
    'NODE_MODULES',
    '/node_modules/react-dom/index.js',
  ),
  '/node_modules/react-dom/package.json': esCodeFile(
    JSON.stringify({ main: './index.js' }),
    'NODE_MODULES',
    '/node_modules/react-dom/package.json',
  ),
}

export function createCodeFile(path: string, contents: string): TextFile {
  const result = lintAndParse(path, contents, null)
  return textFile(textFileContents(contents, result, RevisionsState.CodeAhead), null, Date.now())
}

export function defaultProjectContentsForNormalising(): ProjectContentTreeRoot {
  const defaultProjectContents = createComplexDefaultProjectContents()

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

export function getTextFileByPath(projectContents: ProjectContentTreeRoot, path: string): TextFile {
  const possibleResult = getContentsTreeFileFromString(projectContents, path)
  if (isTextFile(possibleResult)) {
    return possibleResult
  } else {
    throw new Error(`Unable to find a text file at path ${path}.`)
  }
}

export function instancePathFromString(path: string): InstancePath {
  const fromStringResult = TP.fromString(path)
  if (TP.isScenePath(fromStringResult)) {
    throw new Error(`${path} represents a scene path.`)
  } else {
    return TP.dynamicPathToStaticPath(fromStringResult)
  }
}
