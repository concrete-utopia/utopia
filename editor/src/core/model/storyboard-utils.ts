import {
  addFileToProjectContents,
  getContentsTreeFileFromString,
  walkContentsTree,
} from '../../components/assets'
import { EditorModel } from '../../components/editor/action-types'
import { EditorState } from '../../components/editor/store/editor-state'
import {
  isUtopiaJSXComponent,
  JSXElement,
  UtopiaJSXComponent,
  utopiaJSXComponent,
} from '../shared/element-template'
import { forEachValue } from '../shared/object-utils'
import {
  addModifierExportToDetail,
  EmptyExportsDetail,
  ExportDetail,
  forEachParseSuccess,
  importAlias,
  isParsedTextFile,
  isTextFile,
  RevisionsState,
  textFile,
  textFileContents,
} from '../shared/project-file-types'
import { addImport, parseSuccess } from '../workers/common/project-file-utils'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
  createSceneFromComponent,
  createStoryboardElement,
} from './scene-utils'

export const StoryboardFilePath: string = '/src/storyboard.js'

const PossiblyMainComponentNames: Array<string> = ['App', 'Application', 'Main']

interface DefaultComponentToImport {
  type: 'DEFAULT_COMPONENT_TO_IMPORT'
  path: string
}

interface NamedComponentToImport {
  type: 'NAMED_COMPONENT_TO_IMPORT'
  path: string
  possibleMainComponentName: boolean
  toImport: string
}

function defaultComponentToImport(path: string): DefaultComponentToImport {
  return {
    type: 'DEFAULT_COMPONENT_TO_IMPORT',
    path: path,
  }
}

function namedComponentToImport(
  path: string,
  possibleMainComponentName: boolean,
  toImport: string,
): NamedComponentToImport {
  return {
    type: 'NAMED_COMPONENT_TO_IMPORT',
    path: path,
    possibleMainComponentName: possibleMainComponentName,
    toImport: toImport,
  }
}

type ComponentToImport = DefaultComponentToImport | NamedComponentToImport

function betterImportCandidate(
  path: string,
  isDefaultImport: boolean,
  name: string | null,
  current: ComponentToImport | null,
): boolean {
  if (current == null) {
    return true
  } else {
    if (path.split('/').length < current.path.split('/').length) {
      return true
    } else {
      if (name != null && current.type === 'NAMED_COMPONENT_TO_IMPORT') {
        const candidatePossibleMainComponentName = PossiblyMainComponentNames.includes(name)
        return candidatePossibleMainComponentName > current.possibleMainComponentName
      }
      if (isDefaultImport && current.type === 'NAMED_COMPONENT_TO_IMPORT') {
        return true
      }
    }
  }

  return false
}

export function addStoryboardFileToProject(editorModel: EditorModel): EditorModel | null {
  const storyboardFile = getContentsTreeFileFromString(
    editorModel.projectContents,
    StoryboardFilePath,
  )
  if (storyboardFile == null) {
    let currentImportCandidate: ComponentToImport | null = null
    walkContentsTree(editorModel.projectContents, (fullPath, file) => {
      if (isParsedTextFile(file)) {
        // For those successfully parsed files, we want to search all of the components.
        forEachParseSuccess((success) => {
          if (success.exportsDetail.defaultExport != null) {
            if (betterImportCandidate(fullPath, true, null, currentImportCandidate)) {
              currentImportCandidate = defaultComponentToImport(fullPath)
            }
          }

          forEachValue((exportDetail, exportName) => {
            if (betterImportCandidate(fullPath, false, exportName, currentImportCandidate)) {
              const possibleMainComponentName = PossiblyMainComponentNames.includes(exportName)
              currentImportCandidate = namedComponentToImport(
                fullPath,
                possibleMainComponentName,
                exportName,
              )
            }
          }, success.exportsDetail.namedExports)
        }, file.fileContents.parsed)
      }
    })

    if (currentImportCandidate == null) {
      return null
    } else {
      return addStoryboardFileForComponent(currentImportCandidate, editorModel)
    }
  } else {
    return null
  }
}

function addStoryboardFileForComponent(
  createFileWithComponent: ComponentToImport,
  editorModel: EditorState,
) {
  // Add import of storyboard and scene components.
  let imports = addImport(
    'utopia-api',
    null,
    [importAlias('Storyboard'), importAlias('Scene'), importAlias('jsx')],
    null,
    {},
  )
  // Create the storyboard variable.
  let sceneElement: JSXElement
  switch (createFileWithComponent.type) {
    case 'NAMED_COMPONENT_TO_IMPORT':
      sceneElement = createSceneFromComponent(createFileWithComponent.toImport, 'scene-1')
      imports = addImport(
        createFileWithComponent.path,
        null,
        [importAlias(createFileWithComponent.toImport)],
        null,
        imports,
      )
      break
    case 'DEFAULT_COMPONENT_TO_IMPORT':
      sceneElement = createSceneFromComponent('StoryboardComponent', 'scene-1')
      imports = addImport(createFileWithComponent.path, 'StoryboardComponent', [], null, imports)
      break
    default:
      const _exhaustiveCheck: never = createFileWithComponent
      throw new Error(`Unhandled type ${JSON.stringify(createFileWithComponent)}`)
  }
  const storyboardElement = createStoryboardElement([sceneElement], BakedInStoryboardUID)
  const storyboardComponent = utopiaJSXComponent(
    BakedInStoryboardVariableName,
    false,
    null,
    [],
    storyboardElement,
    null,
    [],
  )
  // Add the component import.
  // Create the file.
  const success = parseSuccess(
    imports,
    [storyboardComponent],
    {},
    'jsx',
    null,
    addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
  )
  const storyboardFileContents = textFile(
    textFileContents('', success, RevisionsState.ParsedAhead),
    null,
    0,
  )

  // Update the model.
  const updatedProjectContents = addFileToProjectContents(
    editorModel.projectContents,
    StoryboardFilePath,
    storyboardFileContents,
  )
  return {
    ...editorModel,
    projectContents: updatedProjectContents,
  }
}
