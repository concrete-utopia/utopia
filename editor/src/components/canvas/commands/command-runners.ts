import { foldEither } from '../../../core/shared/either'
import {
  emptyComments,
  isJSXElement,
  jsxAttributeValue,
  JSXElement,
} from '../../../core/shared/element-template'
import {
  getNumberPropertyFromProps,
  jsxSimpleAttributeToValue,
  setJSXValuesAtPaths,
  ValueAtPath,
} from '../../../core/shared/jsx-attributes'
import { ElementPath, RevisionsState } from '../../../core/shared/project-file-types'
import {
  EditorStatePatch,
  EditorState,
  modifyUnderlyingForOpenFile,
  forUnderlyingTargetFromEditorState,
  withUnderlyingTargetFromEditorState,
  removeElementAtPath,
  insertElementAtPath,
} from '../../editor/store/editor-state'
import { Spec } from 'immutability-helper'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import {
  getProjectContentKeyPathElements,
  ProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../assets'
import { drop } from '../../../core/shared/array-utils'
import { StrategyState } from '../canvas-strategies/interaction-state'
import {
  applyUtopiaJSXComponentsChanges,
  getUtopiaJSXComponentsFromSuccess,
} from '../../../core/model/project-file-utils'
import {
  AdjustNumberProperty,
  CommandFunction,
  PathMappings,
  ReparentElement,
  SetProperty,
  UpdateSelectedViews,
} from './commands'

export const runAdjustNumberProperty: CommandFunction<AdjustNumberProperty> = (
  editorState: EditorState,
  pathMappings: PathMappings,
  command: AdjustNumberProperty,
) => {
  // Handle updating the existing value, treating a value that can't be parsed
  // as zero.
  let newValue: number = 0

  // Identify the current value, whatever that may be.
  let targetPropertyNonExistant: boolean = false
  let inequalityValue: number | null = null
  const currentValue = withUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    null,
    (success, element, underlyingTarget, underlyingFilePath) => {
      if (isJSXElement(element)) {
        // Check for the inequality adjustment target while we're here.
        if (typeof command.value !== 'number') {
          inequalityValue = getNumberPropertyFromProps(element.props, command.value.property)
        }

        // Try the property we're updating.
        const fromProperty = getNumberPropertyFromProps(element.props, command.property)
        if (fromProperty == null) {
          targetPropertyNonExistant = true
        } else {
          return fromProperty
        }
      }
      return null
    },
  )

  if (targetPropertyNonExistant && !command.createIfNonExistant) {
    return {
      editorStatePatch: {},
      pathMappings: pathMappings,
      commandDescription: `Adjust Number Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} not applied as the property does not exist.`,
    }
  } else {
    if (typeof command.value === 'number') {
      if (currentValue != null) {
        newValue += currentValue
      }

      // Change the value.
      newValue += command.value
    } else {
      if (currentValue != null && inequalityValue != null) {
        switch (command.value.condition) {
          case 'less-than':
            if (inequalityValue <= currentValue) {
              return {
                editorStatePatch: {},
                pathMappings: pathMappings,
                commandDescription: `Adjust Number Prop: ${EP.toUid(command.target)}/${PP.toString(
                  command.property,
                )} not applied as value is large enough already.`,
              }
            } else {
              newValue = inequalityValue
            }
            break
          case 'greater-than':
            if (inequalityValue >= currentValue) {
              return {
                editorStatePatch: {},
                pathMappings: pathMappings,
                commandDescription: `Adjust Number Prop: ${EP.toUid(command.target)}/${PP.toString(
                  command.property,
                )} not applied as value is small enough already.`,
              }
            } else {
              newValue = inequalityValue
            }
            break
          default:
            const _exhaustiveCheck: never = command.value.condition
            throw new Error(`Unhandled command condition of ${JSON.stringify(command.value)}`)
        }
      }
    }

    const propsToUpdate: Array<ValueAtPath> = [
      {
        path: command.property,
        value: jsxAttributeValue(newValue, emptyComments),
      },
    ]

    // Apply the update to the properties.
    const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
      editorState,
      command.target,
      propsToUpdate,
    )

    return {
      editorStatePatch: propertyUpdatePatch,
      pathMappings: pathMappings,
      commandDescription: `Adjust Number Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} by ${command.value}`,
    }
  }
}

export function applyValuesAtPath(
  editorState: EditorState,
  target: ElementPath,
  jsxValuesAndPathsToSet: ValueAtPath[],
): { editorStateWithChanges: EditorState; editorStatePatch: EditorStatePatch } {
  let editorStatePatch: EditorStatePatch = {}

  const workingEditorState = modifyUnderlyingForOpenFile(
    target,
    editorState,
    (element: JSXElement) => {
      return foldEither(
        () => {
          return element
        },
        (updatedProps) => {
          return {
            ...element,
            props: updatedProps,
          }
        },
        setJSXValuesAtPaths(element.props, jsxValuesAndPathsToSet),
      )
    },
  )

  forUnderlyingTargetFromEditorState(
    target,
    workingEditorState,
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      const projectContentFilePatch: Spec<ProjectContentFile> = {
        content: {
          fileContents: {
            revisionsState: {
              $set: RevisionsState.ParsedAhead,
            },
            parsed: {
              topLevelElements: {
                $set: success.topLevelElements,
              },
              imports: {
                $set: success.imports,
              },
            },
          },
        },
      }
      // ProjectContentTreeRoot is a bit awkward to patch.
      const pathElements = getProjectContentKeyPathElements(underlyingFilePath)
      if (pathElements.length === 0) {
        throw new Error('Invalid path length.')
      }
      const remainderPath = drop(1, pathElements)
      const projectContentsTreePatch: Spec<ProjectContentsTree> = remainderPath.reduceRight(
        (working: Spec<ProjectContentsTree>, pathPart: string) => {
          return {
            children: {
              [pathPart]: working,
            },
          }
        },
        projectContentFilePatch,
      )

      // Finally patch the last part of the path in.
      const projectContentTreeRootPatch: Spec<ProjectContentTreeRoot> = {
        [pathElements[0]]: projectContentsTreePatch,
      }

      editorStatePatch = {
        projectContents: projectContentTreeRootPatch,
      }
    },
  )
  return {
    editorStateWithChanges: workingEditorState,
    editorStatePatch: editorStatePatch,
  }
}

export const runReparentElement: CommandFunction<ReparentElement> = (
  editorState: EditorState,
  pathMappings: PathMappings,
  command: ReparentElement,
) => {
  let editorStatePatch: EditorStatePatch = {}
  forUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)
      const withElementRemoved = removeElementAtPath(command.target, components)
      const withElementInserted = insertElementAtPath(
        editorState.projectContents,
        editorState.canvas.openFile?.filename ?? null,
        command.newParent,
        underlyingElement,
        withElementRemoved,
        null,
      )

      const updatedTopLevelElements = applyUtopiaJSXComponentsChanges(
        success.topLevelElements,
        withElementInserted,
      )
      const projectContentFilePatch: Spec<ProjectContentFile> = {
        content: {
          fileContents: {
            revisionsState: {
              $set: RevisionsState.ParsedAhead,
            },
            parsed: {
              topLevelElements: {
                $set: updatedTopLevelElements,
              },
              imports: {
                $set: success.imports,
              },
            },
          },
        },
      }
      // ProjectContentTreeRoot is a bit awkward to patch.
      const pathElements = getProjectContentKeyPathElements(underlyingFilePath)
      if (pathElements.length === 0) {
        throw new Error('Invalid path length.')
      }
      const remainderPath = drop(1, pathElements)
      const projectContentsTreePatch: Spec<ProjectContentsTree> = remainderPath.reduceRight(
        (working: Spec<ProjectContentsTree>, pathPart: string) => {
          return {
            children: {
              [pathPart]: working,
            },
          }
        },
        projectContentFilePatch,
      )

      // Finally patch the last part of the path in.
      const projectContentTreeRootPatch: Spec<ProjectContentTreeRoot> = {
        [pathElements[0]]: projectContentsTreePatch,
      }

      editorStatePatch = {
        projectContents: projectContentTreeRootPatch,
      }
    },
  )

  return {
    editorStatePatch: editorStatePatch,
    pathMappings: pathMappings,
    commandDescription: `Reparent Element ${EP.toUid(command.target)} to new parent ${EP.toUid(
      command.newParent,
    )}`,
  }
}

export const runUpdateSelectedViews: CommandFunction<UpdateSelectedViews> = (
  _: EditorState,
  pathMappings: PathMappings,
  command: UpdateSelectedViews,
) => {
  const editorStatePatch = {
    selectedViews: {
      $set: command.value,
    },
  }
  return {
    editorStatePatch: editorStatePatch,
    pathMappings: pathMappings,
    commandDescription: `Update Selected Views: ${command.value.map(EP.toString).join(', ')}`,
  }
}

export const runSetProperty: CommandFunction<SetProperty> = (
  editorState: EditorState,
  pathMappings: PathMappings,
  command: SetProperty,
) => {
  const propsToUpdate: Array<ValueAtPath> = [
    {
      path: command.property,
      value: command.value,
    },
  ]

  // Apply the update to the properties.
  const { editorStatePatch: propertyUpdatePatch } = applyValuesAtPath(
    editorState,
    command.target,
    propsToUpdate,
  )

  return {
    editorStatePatch: propertyUpdatePatch,
    pathMappings: pathMappings,
    commandDescription: `Set Property ${EP.toUid(command.target)}/${PP.toString(
      command.property,
    )} to ${jsxSimpleAttributeToValue(command.value).value}`,
  }
}
