import {
  addFileToProjectContents,
  getContentsTreeFileFromString,
  walkContentsTree,
} from '../../components/assets'
import { EditorModel } from '../../components/editor/action-types'
import { EditorState } from '../../components/editor/store/editor-state'
import { forEachRight, left, right } from '../shared/either'
import {
  isUtopiaJSXComponent,
  UtopiaJSXComponent,
  utopiaJSXComponent,
} from '../shared/element-template'
import { importAlias, isUIJSFile, RevisionsState } from '../shared/project-file-types'
import { generateUID } from '../shared/uid-utils'
import { addImport, parseSuccess } from '../workers/common/project-file-utils'
import { uiJsFile } from './project-file-utils'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
  createSceneFromComponent,
  createStoryboardElement,
} from './scene-utils'

export const StoryboardFilePath: string = '/src/storyboard.js'

const PossiblyMainComponentNames: Array<string> = ['App', 'Application', 'Main']

interface ComponentToImport {
  path: string
  component: UtopiaJSXComponent
}

function componentToImport(path: string, component: UtopiaJSXComponent): ComponentToImport {
  return {
    path: path,
    component: component,
  }
}

export function addStoryboardFileToProject(editorModel: EditorModel): EditorModel {
  const storyboardFile = getContentsTreeFileFromString(
    editorModel.projectContents,
    StoryboardFilePath,
  )
  if (storyboardFile == null) {
    let namedComponentToImport: ComponentToImport | null = null as any
    let firstComponentToImport: ComponentToImport | null = null as any
    walkContentsTree(editorModel.projectContents, (fullPath, file) => {
      // Just in case we ever find a main looking component, skip everything else.
      if (namedComponentToImport == null) {
        if (isUIJSFile(file)) {
          // For those successfully parsed files, we want to search all of the components.
          forEachRight(file.fileContents, (success) => {
            for (const topLevelElement of success.topLevelElements) {
              if (isUtopiaJSXComponent(topLevelElement)) {
                // Check for components that have a name which _looks_ like it is the main one.
                if (PossiblyMainComponentNames.includes(topLevelElement.name)) {
                  namedComponentToImport = componentToImport(fullPath, topLevelElement)
                }
                // Capture one at the start.
                if (firstComponentToImport == null) {
                  firstComponentToImport = componentToImport(fullPath, topLevelElement)
                }
              }
            }
          })
        }
      }
    })

    const createFileWithComponent: ComponentToImport | null =
      firstComponentToImport ?? namedComponentToImport
    if (createFileWithComponent == null) {
      return editorModel
    } else {
      return addStoryboardFileForComponent(createFileWithComponent, editorModel)
    }
  } else {
    return editorModel
  }
}

function addStoryboardFileForComponent(
  createFileWithComponent: ComponentToImport,
  editorModel: EditorState,
) {
  // Add import of storyboard and scene components.
  const baseImports = addImport(
    'utopia-api',
    null,
    [importAlias('Storyboard'), importAlias('Scene'), importAlias('jsx')],
    null,
    {},
  )
  // Create the storyboard variable.
  const sceneElement = createSceneFromComponent(createFileWithComponent.component, 'scene-1')
  const storyboardElement = createStoryboardElement([sceneElement], BakedInStoryboardUID)
  const storyboardComponent = utopiaJSXComponent(
    BakedInStoryboardVariableName,
    false,
    null,
    [],
    storyboardElement,
    null,
  )
  // Add the component import.
  const imports = addImport(
    createFileWithComponent.path,
    null,
    [importAlias(createFileWithComponent.component.name)],
    null,
    baseImports,
  )
  // Create the file.
  const success = parseSuccess(imports, [storyboardComponent], left({}), false, '', {}, 'jsx', null)
  const storyboardFileContents = uiJsFile(right(success), null, RevisionsState.ParsedAhead, 0)

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
