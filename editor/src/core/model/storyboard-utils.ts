import type { ProjectContentTreeRoot } from '../../components/assets'
import {
  addFileToProjectContents,
  getProjectFileByFilePath,
  walkContentsTree,
} from '../../components/assets'
import type { EditorModel } from '../../components/editor/action-types'
import type { EditorState } from '../../components/editor/store/editor-state'
import { StoryboardFilePath } from '../../components/editor/store/editor-state'
import type { Compare } from '../../utils/compare'
import {
  compareCompose,
  compareField,
  compareIfIs,
  compareOn,
  comparePrimitive,
} from '../../utils/compare'
import type { JSXElement } from '../shared/element-template'
import {
  emptyComments,
  isUtopiaJSXComponent,
  unparsedCode,
  UtopiaJSXComponent,
  utopiaJSXComponent,
} from '../shared/element-template'
import { forEachValue } from '../shared/object-utils'
import { forceNotNull } from '../shared/optional-utils'
import type { ParseSuccess } from '../shared/project-file-types'
import {
  EmptyExportsDetail,
  ExportDetail,
  exportVariable,
  exportVariables,
  forEachParseSuccess,
  importAlias,
  isParsedTextFile,
  isParseSuccess,
  isTextFile,
  mergeExportsDetail,
  parseSuccess,
  RevisionsState,
  textFile,
  textFileContents,
} from '../shared/project-file-types'
import { addImport } from '../workers/common/project-file-utils'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
  createSceneFromComponent,
  createStoryboardElement,
} from './scene-utils'

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

export function addStoryboardFileToProject(
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot | null {
  const storyboardFile = getProjectFileByFilePath(projectContents, StoryboardFilePath)
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
    walkContentsTree(projectContents, (fullPath, file) => {
      if (isParsedTextFile(file)) {
        // For those successfully parsed files, we want to search all of the components.
        forEachParseSuccess((success) => {
          let namedExportKeys: Array<string> = []
          for (const exportDetail of success.exportsDetail) {
            switch (exportDetail.type) {
              case 'EXPORT_DEFAULT_FUNCTION_OR_CLASS':
                updateCandidate(defaultComponentToImport(fullPath))
                break
              case 'EXPORT_CLASS':
                {
                  const possibleMainComponentName = PossiblyMainComponentNames.includes(
                    exportDetail.className,
                  )
                  updateCandidate(
                    namedComponentToImport(
                      fullPath,
                      possibleMainComponentName,
                      exportDetail.className,
                    ),
                  )
                }
                break
              case 'EXPORT_FUNCTION':
                {
                  const possibleMainComponentName = PossiblyMainComponentNames.includes(
                    exportDetail.functionName,
                  )
                  updateCandidate(
                    namedComponentToImport(
                      fullPath,
                      possibleMainComponentName,
                      exportDetail.functionName,
                    ),
                  )
                }
                break
              case 'EXPORT_VARIABLES':
              case 'EXPORT_DESTRUCTURED_ASSIGNMENT':
              case 'REEXPORT_VARIABLES':
                {
                  for (const exportVar of exportDetail.variables) {
                    const exportName = exportVar.variableAlias ?? exportVar.variableName
                    if (exportName !== 'default') {
                      namedExportKeys.push(exportName)
                      const possibleMainComponentName =
                        PossiblyMainComponentNames.includes(exportName)
                      updateCandidate(
                        namedComponentToImport(fullPath, possibleMainComponentName, exportName),
                      )
                    }
                  }
                }
                break
              case 'EXPORT_DEFAULT_IDENTIFIER':
                {
                  const possibleMainComponentName = PossiblyMainComponentNames.includes(
                    exportDetail.name,
                  )
                  updateCandidate(
                    namedComponentToImport(fullPath, possibleMainComponentName, exportDetail.name),
                  )
                }
                break
              case 'REEXPORT_WILDCARD':
                break
              case 'EXPORT_VARIABLES_WITH_MODIFIER':
                {
                  for (const exportName of exportDetail.variables) {
                    if (exportName !== 'default') {
                      namedExportKeys.push(exportName)
                      const possibleMainComponentName =
                        PossiblyMainComponentNames.includes(exportName)
                      updateCandidate(
                        namedComponentToImport(fullPath, possibleMainComponentName, exportName),
                      )
                    }
                  }
                }
                break
              default:
                const _exhaustiveCheck: never = exportDetail
                throw new Error(`Unhandled type ${JSON.stringify(exportDetail)}`)
            }
          }
        }, file.fileContents.parsed)
      }
    })

    if (currentImportCandidate == null) {
      return null
    } else {
      return addStoryboardFileForComponent(currentImportCandidate, projectContents)
    }
  } else {
    return null
  }
}

function addStoryboardFileForComponent(
  createFileWithComponent: ComponentToImport,
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  // Add import of storyboard and scene components.
  let importsResolution = addImport(StoryboardFilePath, [], 'react', null, [], 'React', {})
  importsResolution = addImport(
    StoryboardFilePath,
    [],
    'utopia-api',
    null,
    [importAlias('Storyboard'), importAlias('Scene')],
    null,
    importsResolution.imports,
  )
  // Create the storyboard variable.
  let sceneElement: JSXElement
  let updatedProjectContents: ProjectContentTreeRoot = projectContents
  switch (createFileWithComponent.type) {
    case 'NAMED_COMPONENT_TO_IMPORT':
      sceneElement = createSceneFromComponent(
        StoryboardFilePath,
        createFileWithComponent.toImport,
        'scene-1',
      )
      importsResolution = addImport(
        StoryboardFilePath,
        [],
        createFileWithComponent.path,
        null,
        [importAlias(createFileWithComponent.toImport)],
        null,
        importsResolution.imports,
      )
      break
    case 'DEFAULT_COMPONENT_TO_IMPORT':
      sceneElement = createSceneFromComponent(StoryboardFilePath, 'StoryboardComponent', 'scene-1')
      importsResolution = addImport(
        StoryboardFilePath,
        [],
        createFileWithComponent.path,
        'StoryboardComponent',
        [],
        null,
        importsResolution.imports,
      )
      break
    case 'UNEXPORTED_RENDERED_COMPONENT':
      sceneElement = createSceneFromComponent(
        StoryboardFilePath,
        createFileWithComponent.elementName,
        'scene-1',
      )
      importsResolution = addImport(
        StoryboardFilePath,
        [],
        createFileWithComponent.path,
        null,
        [importAlias(createFileWithComponent.elementName)],
        null,
        importsResolution.imports,
      )
      // Modify the targeted file to export the component we're interested in.
      const fileToModify = forceNotNull(
        `Unable to find file at ${createFileWithComponent.path}`,
        getProjectFileByFilePath(updatedProjectContents, createFileWithComponent.path),
      )
      if (isTextFile(fileToModify)) {
        if (isParseSuccess(fileToModify.fileContents.parsed)) {
          const currentSuccess: ParseSuccess = fileToModify.fileContents.parsed
          const updatedExports = mergeExportsDetail(currentSuccess.exportsDetail, [
            exportVariables([exportVariable(createFileWithComponent.elementName, null)]),
          ])
          const updatedParseSuccess = parseSuccess(
            currentSuccess.imports,
            currentSuccess.topLevelElements,
            currentSuccess.highlightBounds,
            currentSuccess.jsxFactoryFunction,
            currentSuccess.combinedTopLevelArbitraryBlock,
            updatedExports,
            currentSuccess.fullHighlightBounds,
          )
          const updatedContents = textFileContents(
            fileToModify.fileContents.code,
            updatedParseSuccess,
            RevisionsState.ParsedAhead,
          )
          const updatedTextFile = textFile(
            updatedContents,
            fileToModify.lastSavedContents,
            updatedParseSuccess,
            fileToModify.versionNumber + 1,
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
    [],
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
    importsResolution.imports,
    [unparsedCode('\n\n'), storyboardComponent],
    {},
    null,
    null,
    [exportVariables([exportVariable(BakedInStoryboardVariableName, null)])],
    {},
  )
  const storyboardFileContents = textFile(
    textFileContents('', success, RevisionsState.ParsedAhead),
    null,
    success,
    0,
  )

  // Update the model.
  updatedProjectContents = addFileToProjectContents(
    updatedProjectContents,
    StoryboardFilePath,
    storyboardFileContents,
  )

  return updatedProjectContents
}
