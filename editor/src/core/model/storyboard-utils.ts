import {
  addFileToProjectContents,
  getContentsTreeFileFromString,
  ProjectContentTreeRoot,
  walkContentsTree,
} from '../../components/assets'
import { EditorModel } from '../../components/editor/action-types'
import { updateFile } from '../../components/editor/actions/action-creators'
import { EditorState } from '../../components/editor/store/editor-state'
import {
  Compare,
  compareCompose,
  compareField,
  compareIfIs,
  compareOn,
  comparePrimitive,
} from '../../utils/compare'
import {
  isUtopiaJSXComponent,
  JSXElement,
  UtopiaJSXComponent,
  utopiaJSXComponent,
} from '../shared/element-template'
import { forEachValue } from '../shared/object-utils'
import { forceNotNull } from '../shared/optional-utils'
import {
  addModifierExportToDetail,
  addNamedExportToDetail,
  EmptyExportsDetail,
  ExportDetail,
  forEachParseSuccess,
  importAlias,
  isParsedTextFile,
  isParseSuccess,
  isTextFile,
  ParseSuccess,
  RevisionsState,
  textFile,
  textFileContents,
} from '../shared/project-file-types'
import { fastForEach } from '../shared/utils'
import { addImport, parseSuccess } from '../workers/common/project-file-utils'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'
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

interface UnexportedRenderedComponent {
  type: 'UNEXPORTED_RENDERED_COMPONENT'
  path: string
  elementName: string
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

function unexportedRenderedComponent(
  path: string,
  elementName: string,
): UnexportedRenderedComponent {
  return {
    type: 'UNEXPORTED_RENDERED_COMPONENT',
    path: path,
    elementName: elementName,
  }
}

type ComponentToImport =
  | DefaultComponentToImport
  | NamedComponentToImport
  | UnexportedRenderedComponent

function isDefaultComponentToImport(
  toImport: ComponentToImport,
): toImport is DefaultComponentToImport {
  return toImport.type === 'DEFAULT_COMPONENT_TO_IMPORT'
}

function isNamedComponentToImport(toImport: ComponentToImport): toImport is NamedComponentToImport {
  return toImport.type === 'NAMED_COMPONENT_TO_IMPORT'
}

function isUnexportedRenderedComponent(
  toImport: ComponentToImport,
): toImport is UnexportedRenderedComponent {
  return toImport.type === 'UNEXPORTED_RENDERED_COMPONENT'
}

const compareDefaultComponentToImport: Compare<DefaultComponentToImport> = compareCompose(
  compareOn((toImport) => toImport.path.split('/').length, comparePrimitive),
)

const compareNamedComponentToImport: Compare<NamedComponentToImport> = compareCompose(
  compareOn((toImport) => toImport.path.split('/').length, comparePrimitive),
  compareField('possibleMainComponentName', comparePrimitive),
  compareField('toImport', comparePrimitive),
)

const compareUnexportedRenderedComponent: Compare<UnexportedRenderedComponent> = compareCompose(
  compareOn((toImport) => toImport.path.split('/').length, comparePrimitive),
  compareOn(
    (toImport) => PossiblyMainComponentNames.includes(toImport.elementName),
    comparePrimitive,
  ),
)

// Highest priority last.
const componentToImportTypesInPriorityOrder: Array<ComponentToImport['type']> = [
  'NAMED_COMPONENT_TO_IMPORT',
  'DEFAULT_COMPONENT_TO_IMPORT',
  'UNEXPORTED_RENDERED_COMPONENT',
]

const compareComponentToImport: Compare<ComponentToImport> = compareCompose(
  compareOn((toImport) => toImport.path.split('/').length, comparePrimitive),
  compareIfIs(isDefaultComponentToImport, compareDefaultComponentToImport),
  compareIfIs(isNamedComponentToImport, compareNamedComponentToImport),
  compareIfIs(isUnexportedRenderedComponent, compareUnexportedRenderedComponent),
  compareOn(
    (toImport) => componentToImportTypesInPriorityOrder.indexOf(toImport.type),
    comparePrimitive,
  ),
)

export function addStoryboardFileToProject(editorModel: EditorModel): EditorModel | null {
  const storyboardFile = getContentsTreeFileFromString(
    editorModel.projectContents,
    StoryboardFilePath,
  )
  if (storyboardFile == null) {
    let currentImportCandidate: ComponentToImport | null = null
    function updateCandidate(importCandidate: ComponentToImport): void {
      if (currentImportCandidate == null) {
        currentImportCandidate = importCandidate
      } else {
        if (compareComponentToImport(importCandidate, currentImportCandidate) > 0) {
          currentImportCandidate = importCandidate
        }
      }
    }
    walkContentsTree(editorModel.projectContents, (fullPath, file) => {
      if (isParsedTextFile(file)) {
        // For those successfully parsed files, we want to search all of the components.
        forEachParseSuccess((success) => {
          if (success.exportsDetail.defaultExport != null) {
            updateCandidate(defaultComponentToImport(fullPath))
          }

          forEachValue((exportDetail, exportName) => {
            const possibleMainComponentName = PossiblyMainComponentNames.includes(exportName)
            updateCandidate(namedComponentToImport(fullPath, possibleMainComponentName, exportName))
          }, success.exportsDetail.namedExports)

          const namedExportKeys = Object.keys(success.exportsDetail.namedExports)
          for (const topLevelElement of success.topLevelElements) {
            if (isUtopiaJSXComponent(topLevelElement) && topLevelElement.usedInReactDOMRender) {
              // Exported as the default, so exclude it.
              if (
                success.exportsDetail.defaultExport != null &&
                success.exportsDetail.defaultExport.name === topLevelElement.name
              ) {
                continue
              }

              // Exported by name, so exclude it.
              if (namedExportKeys.includes(topLevelElement.name)) {
                continue
              }

              updateCandidate(unexportedRenderedComponent(fullPath, topLevelElement.name))
            }
          }
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
): EditorState {
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
  let updatedProjectContents: ProjectContentTreeRoot = editorModel.projectContents
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
    case 'UNEXPORTED_RENDERED_COMPONENT':
      sceneElement = createSceneFromComponent(createFileWithComponent.elementName, 'scene-1')
      imports = addImport(
        createFileWithComponent.path,
        null,
        [importAlias(createFileWithComponent.elementName)],
        null,
        imports,
      )
      // Modify the targeted file to export the component we're interested in.
      const fileToModify = forceNotNull(
        `Unable to find file at ${createFileWithComponent.path}`,
        getContentsTreeFileFromString(updatedProjectContents, createFileWithComponent.path),
      )
      if (isTextFile(fileToModify)) {
        if (isParseSuccess(fileToModify.fileContents.parsed)) {
          const currentSuccess: ParseSuccess = fileToModify.fileContents.parsed
          const updatedExports = addModifierExportToDetail(
            currentSuccess.exportsDetail,
            createFileWithComponent.elementName,
          )
          const updatedParseSuccess = parseSuccess(
            currentSuccess.imports,
            currentSuccess.topLevelElements,
            currentSuccess.highlightBounds,
            currentSuccess.jsxFactoryFunction,
            currentSuccess.combinedTopLevelArbitraryBlock,
            updatedExports,
          )
          const updatedContents = textFileContents(
            fileToModify.fileContents.code,
            updatedParseSuccess,
            RevisionsState.ParsedAhead,
          )
          const updatedTextFile = textFile(
            updatedContents,
            fileToModify.lastSavedContents,
            fileToModify.lastRevisedTime,
          )
          updatedProjectContents = addFileToProjectContents(
            updatedProjectContents,
            createFileWithComponent.path,
            updatedTextFile,
          )
        } else {
          throw new Error(`Unexpectedly ${createFileWithComponent.path} is not a parse success.`)
        }
      } else {
        throw new Error(`${createFileWithComponent.path} was not a text file as expected.`)
      }
      break
    default:
      const _exhaustiveCheck: never = createFileWithComponent
      throw new Error(`Unhandled type ${JSON.stringify(createFileWithComponent)}`)
  }
  const storyboardElement = createStoryboardElement([sceneElement], BakedInStoryboardUID)
  const storyboardComponent = utopiaJSXComponent(
    BakedInStoryboardVariableName,
    false,
    'var',
    'block',
    null,
    [],
    storyboardElement,
    null,
    false,
    emptyComments,
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
  updatedProjectContents = addFileToProjectContents(
    updatedProjectContents,
    StoryboardFilePath,
    storyboardFileContents,
  )
  return {
    ...editorModel,
    projectContents: updatedProjectContents,
  }
}
