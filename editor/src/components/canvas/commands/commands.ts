import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { LayoutPinnedProp, LayoutPinnedProps } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { foldEither, forEachRight, isLeft, isRight, right } from '../../../core/shared/either'
import {
  emptyComments,
  isJSXElement,
  JSXAttribute,
  JSXAttributes,
  jsxAttributeValue,
  JSXElement,
} from '../../../core/shared/element-template'
import {
  getJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  setJSXValuesAtPaths,
  unsetJSXValuesAtPaths,
  ValueAtPath,
} from '../../../core/shared/jsx-attributes'
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import { ElementPath, PropertyPath, RevisionsState } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import {
  EditorStatePatch,
  EditorState,
  modifyUnderlyingForOpenFile,
  forUnderlyingTargetFromEditorState,
  withUnderlyingTargetFromEditorState,
  removeElementAtPath,
  insertElementAtPath,
} from '../../editor/store/editor-state'
import { SelectModeCanvasSessionState } from '../canvas-strategies/canvas-strategy-types'
import { cssNumberAsNumberIfPossible, getPropsToSetToMoveElement } from '../canvas-utils'
import update, { Spec } from 'immutability-helper'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { canvasPoint } from '../../../core/shared/math-utils'
import {
  getProjectContentKeyPathElements,
  ProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../assets'
import { drop } from '../../../core/shared/array-utils'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { CommandDescription, StrategyState } from '../../../interactions_proposal'
import {
  applyUtopiaJSXComponentsChanges,
  getUtopiaJSXComponentsFromSuccess,
} from '../../../core/model/project-file-utils'

export interface PathMapping {
  from: ElementPath
  to: ElementPath
}

export type PathMappings = Array<PathMapping>

export interface CommandFunctionResult {
  editorStatePatch: EditorStatePatch
  pathMappings: PathMappings
  strategyState: StrategyState
  commandDescription: string
}

export type CommandFunction<T> = (
  editorState: EditorState,
  strategyState: StrategyState,
  pathMappings: PathMappings,
  command: T,
) => CommandFunctionResult

export type TransientOrNot = 'transient' | 'permanent'

export interface BaseCommand {
  transient: TransientOrNot
}

export interface MoveElement extends BaseCommand {
  type: 'MOVE_ELEMENT'
  target: ElementPath
  x: number
  y: number
}

export function moveElement(
  transient: TransientOrNot,
  target: ElementPath,
  x: number,
  y: number,
): MoveElement {
  return {
    type: 'MOVE_ELEMENT',
    transient: transient,
    target: target,
    x: x,
    y: y,
  }
}
export interface ReparentElement extends BaseCommand {
  type: 'REPARENT_ELEMENT'
  target: ElementPath
  newParent: ElementPath
}

export function reparentElement(
  transient: TransientOrNot,
  target: ElementPath,
  newParent: ElementPath,
): ReparentElement {
  return {
    type: 'REPARENT_ELEMENT',
    transient: transient,
    target: target,
    newParent: newParent,
  }
}

export interface UpdateElementIndex extends BaseCommand {
  type: 'UPDATE_ELEMENT_INDEX'
  target: ElementPath
  newIndex: number
}

export function updateElementIndex(
  transient: TransientOrNot,
  target: ElementPath,
  newIndex: number,
): UpdateElementIndex {
  return {
    type: 'UPDATE_ELEMENT_INDEX',
    transient: transient,
    target: target,
    newIndex: newIndex,
  }
}

export interface WildcardPatch extends BaseCommand {
  type: 'WILDCARD_PATCH'
  patch: EditorStatePatch
}

export function wildcardPatch(transient: TransientOrNot, patch: EditorStatePatch): WildcardPatch {
  return {
    type: 'WILDCARD_PATCH',
    transient: transient,
    patch: patch,
  }
}

export type AdjustNumberCondition = 'less-than' | 'greater-than'

export interface AdjustNumberInequalityCondition {
  property: PropertyPath
  condition: AdjustNumberCondition
}

export function adjustNumberInequalityCondition(
  property: PropertyPath,
  condition: AdjustNumberCondition,
): AdjustNumberInequalityCondition {
  return {
    property: property,
    condition: condition,
  }
}

export interface AdjustNumberProperty extends BaseCommand {
  type: 'ADJUST_NUMBER_PROPERTY'
  target: ElementPath
  property: PropertyPath
  value: number | AdjustNumberInequalityCondition
  createIfNonExistant: boolean
}

export function adjustNumberProperty(
  transient: TransientOrNot,
  target: ElementPath,
  property: PropertyPath,
  value: number | AdjustNumberInequalityCondition,
  createIfNonExistant: boolean,
): AdjustNumberProperty {
  return {
    type: 'ADJUST_NUMBER_PROPERTY',
    transient: transient,
    target: target,
    property: property,
    value: value,
    createIfNonExistant: createIfNonExistant,
  }
}

export interface SetProperty extends BaseCommand {
  type: 'SET_PROPERTY'
  target: ElementPath
  property: PropertyPath
  value: JSXAttribute
}

export function setProperty(
  transient: TransientOrNot,
  target: ElementPath,
  property: PropertyPath,
  value: JSXAttribute,
): SetProperty {
  return {
    type: 'SET_PROPERTY',
    transient: transient,
    target: target,
    property: property,
    value: value,
  }
}

export interface UpdateSelectedViews extends BaseCommand {
  type: 'UPDATE_SELECTED_VIEWS'
  value: Array<ElementPath>
}

export function updateSelectedViews(
  transient: TransientOrNot,
  value: Array<ElementPath>,
): UpdateSelectedViews {
  return {
    type: 'UPDATE_SELECTED_VIEWS',
    transient: transient,
    value: value,
  }
}

export interface DeleteProperty extends BaseCommand {
  type: 'DELETE_PROPERTY'
  target: ElementPath
  property: PropertyPath
}

export function deleteProperty(
  transient: TransientOrNot,
  target: ElementPath,
  property: PropertyPath,
): DeleteProperty {
  return {
    type: 'DELETE_PROPERTY',
    transient: transient,
    target: target,
    property: property,
  }
}

export interface StrategySwitched extends BaseCommand {
  type: 'STRATEGY_SWITCHED'
  reason: 'automatic' | 'user-input'
  newStrategy: string
  accumulatedCommands: boolean
  dataReset: boolean
  previousFitness: number
  newFitness: number
}

export function strategySwitched(
  reason: 'automatic' | 'user-input',
  newStrategy: string,
  accumulatedCommands: boolean,
  dataReset: boolean,
  previousFitness: number,
  newFitness: number,
): StrategySwitched {
  return {
    type: 'STRATEGY_SWITCHED',
    transient: 'transient',
    reason,
    newStrategy,
    accumulatedCommands,
    dataReset,
    previousFitness,
    newFitness,
  }
}

export type CanvasCommand =
  | MoveElement
  | WildcardPatch
  | AdjustNumberProperty
  | SetProperty
  | ReparentElement
  | UpdateSelectedViews
  | DeleteProperty
  | UpdateElementIndex
  | StrategySwitched

export const runMoveElementCommand: CommandFunction<MoveElement> = (
  editorState: EditorState,
  strategyState: StrategyState,
  pathMappings: PathMappings,
  command: MoveElement,
) => {
  // Currently this is moving something by an amount,
  // probably should set position directly.
  const possibleElementMetadata = MetadataUtils.findElementByElementPath(
    editorState.jsxMetadata,
    command.target,
  )
  const elementMetadata = forceNotNull(
    `Could not find element at ${JSON.stringify(command.target)}`,
    possibleElementMetadata,
  )
  const parentTarget = MetadataUtils.findParent(editorState.jsxMetadata, command.target)
  const parentFrame =
    parentTarget == null
      ? null
      : MetadataUtils.getFrameInCanvasCoords(parentTarget, editorState.jsxMetadata)

  let editorStatePatch: EditorStatePatch = {}

  switch (elementMetadata.specialSizeMeasurements.position) {
    // For a relative pinned element.
    // For an absolute pinned element.
    case 'relative':
    case 'absolute':
      forEachRight(elementMetadata.element, (elementChild) => {
        if (isJSXElement(elementChild)) {
          let frameProps: { [k: string]: string | number | undefined } = {}
          fastForEach(LayoutPinnedProps, (p) => {
            if (p !== 'width' && p !== 'height') {
              const value = getLayoutProperty(p, right(elementChild.props), ['style'])
              if (isLeft(value) || value.value != null) {
                frameProps[p] = cssNumberAsNumberIfPossible(value.value)
              }
            }
          })

          let framePointsToUse: Array<LayoutPinnedProp> = [
            ...(Object.keys(frameProps) as Array<LayoutPinnedProp>),
          ]
          const horizontalExistingFramePoints = framePointsToUse.filter(
            (p) => p === 'left' || p === 'right',
          )
          if (horizontalExistingFramePoints.length === 0) {
            framePointsToUse.push('left')
          }
          const verticalExistingFramePoints = framePointsToUse.filter(
            (p) => p === 'top' || p === 'bottom',
          )
          if (verticalExistingFramePoints.length === 0) {
            framePointsToUse.push('top')
          }
          const whatToMove = getPropsToSetToMoveElement(
            canvasPoint({ x: command.x, y: command.y }),
            framePointsToUse,
            frameProps,
            parentFrame,
          )
          editorStatePatch = applyValuesAtPath(editorState, command.target, whatToMove)
            .editorStatePatch
        }
      })
      break
    // For everything else...
    default:
  }

  return {
    editorStatePatch: editorStatePatch,
    strategyState: strategyState,
    pathMappings: pathMappings,
    commandDescription: `Move Element ${EP.toUid(command.target)} by x: ${command.x}, y: ${
      command.y
    }`,
  }
}

export const runWildcardPatch: CommandFunction<WildcardPatch> = (
  editorState: EditorState,
  strategyState: StrategyState,
  pathMappings: PathMappings,
  command: WildcardPatch,
) => {
  return {
    editorStatePatch: command.patch,
    strategyState: strategyState,
    pathMappings: pathMappings,
    commandDescription: `Wildcard Patch: ${JSON.stringify(command.patch, null, 2)}`,
  }
}

function getNumberPropertyFromProps(props: JSXAttributes, property: PropertyPath): number | null {
  const possibleProperty = getJSXAttributeAtPath(props, property)
  const currentValue = optionalMap(jsxSimpleAttributeToValue, possibleProperty?.attribute)
  if (currentValue !== null && isRight(currentValue) && typeof currentValue.value === 'number') {
    return currentValue.value
  } else {
    return null
  }
}

export const runAdjustNumberProperty: CommandFunction<AdjustNumberProperty> = (
  editorState: EditorState,
  strategyState: StrategyState,
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
      strategyState: strategyState,
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
                strategyState: strategyState,
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
                strategyState: strategyState,
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
      strategyState: strategyState,
      pathMappings: pathMappings,
      commandDescription: `Adjust Number Prop: ${EP.toUid(command.target)}/${PP.toString(
        command.property,
      )} by ${command.value}`,
    }
  }
}

export const runSetProperty: CommandFunction<SetProperty> = (
  editorState: EditorState,
  strategyState: StrategyState,
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
    strategyState: strategyState,
    pathMappings: pathMappings,
    commandDescription: `Set Property ${EP.toUid(command.target)}/${PP.toString(
      command.property,
    )} to ${jsxSimpleAttributeToValue(command.value).value}`,
  }
}

export const runReparentElement: CommandFunction<ReparentElement> = (
  editorState: EditorState,
  strategyState: StrategyState,
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
    strategyState: strategyState,
    pathMappings: pathMappings,
    commandDescription: `Reparent Element ${EP.toUid(command.target)} to new parent ${EP.toUid(
      command.newParent,
    )}`,
  }
}

export const runUpdateSelectedViews: CommandFunction<UpdateSelectedViews> = (
  _: EditorState,
  strategyState: StrategyState,
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
    strategyState: strategyState,
    pathMappings: pathMappings,
    commandDescription: `Update Selected Views: ${command.value.map(EP.toString).join(', ')}`,
  }
}

const runUpdateElementIndex: CommandFunction<UpdateElementIndex> = (
  editorState: EditorState,
  strategyState: StrategyState,
  pathMappings: PathMappings,
  command: UpdateElementIndex,
) => {
  let editorStatePatch: EditorStatePatch = {}
  forUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)
      const parentPath = EP.parentPath(command.target)
      const withElementRemoved = removeElementAtPath(command.target, components)
      const withElementInserted = insertElementAtPath(
        editorState.projectContents,
        editorState.canvas.openFile?.filename ?? null,
        parentPath,
        underlyingElement,
        withElementRemoved,
        {
          type: 'absolute',
          index: command.newIndex,
        },
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
    strategyState: strategyState,
    pathMappings: pathMappings,
    commandDescription: `Shifted element to index ${command.newIndex}`,
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

function runDeleteProperty(
  editorState: EditorState,
  strategyState: StrategyState,
  pathMappings: PathMappings,
  command: DeleteProperty,
): CommandFunctionResult {
  let editorStatePatch: EditorStatePatch = {}

  const workingEditorState = modifyUnderlyingForOpenFile(
    command.target,
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
        unsetJSXValuesAtPaths(element.props, [command.property]),
      )
    },
  )

  forUnderlyingTargetFromEditorState(
    command.target,
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
    strategyState: strategyState,
    pathMappings: pathMappings,
    commandDescription: `Delete Property ${EP.toUid(command.target)}/${PP.toString(
      command.property,
    )}`,
  }
}

function runStrategySwitchedCommand(
  strategyState: StrategyState,
  pathMappings: PathMappings,
  command: StrategySwitched,
): CommandFunctionResult {
  let commandDescription: string = `Strategy switched to ${command.newStrategy} ${
    command.reason === 'automatic'
      ? `automatically (fitness ${command.previousFitness} -> ${command.newFitness})`
      : 'by user input'
  }. ${command.dataReset ? 'Interaction data reset.' : ''}`

  return {
    editorStatePatch: {},
    strategyState: strategyState,
    pathMappings: pathMappings,
    commandDescription: commandDescription,
  }
}

export const runCanvasCommand: CommandFunction<CanvasCommand> = (
  editorState: EditorState,
  strategyState: StrategyState,
  pathMappings: PathMappings,
  command: CanvasCommand,
) => {
  switch (command.type) {
    case 'MOVE_ELEMENT':
      return runMoveElementCommand(editorState, strategyState, pathMappings, command)
    case 'WILDCARD_PATCH':
      return runWildcardPatch(editorState, strategyState, pathMappings, command)
    case 'ADJUST_NUMBER_PROPERTY':
      return runAdjustNumberProperty(editorState, strategyState, pathMappings, command)
    case 'SET_PROPERTY':
      return runSetProperty(editorState, strategyState, pathMappings, command)
    case 'REPARENT_ELEMENT':
      return runReparentElement(editorState, strategyState, pathMappings, command)
    case 'UPDATE_SELECTED_VIEWS':
      return runUpdateSelectedViews(editorState, strategyState, pathMappings, command)
    case 'DELETE_PROPERTY':
      return runDeleteProperty(editorState, strategyState, pathMappings, command)
    case 'UPDATE_ELEMENT_INDEX':
      return runUpdateElementIndex(editorState, strategyState, pathMappings, command)
    case 'STRATEGY_SWITCHED':
      return runStrategySwitchedCommand(strategyState, pathMappings, command)
    default:
      const _exhaustiveCheck: never = command
      throw new Error(`Unhandled canvas command ${JSON.stringify(command)}`)
  }
}

export function foldAndApplyCommands(
  editorState: EditorState,
  priorPatchedState: EditorState,
  strategyState: StrategyState,
  commands: Array<CanvasCommand>,
  transient: TransientOrNot,
): {
  editorState: EditorState
  editorStatePatches: Array<EditorStatePatch>
  newStrategyState: StrategyState
  commandDescriptions: Array<CommandDescription>
} {
  const commandResult = foldCommands(editorState, strategyState, commands, transient)
  const updatedEditorState = applyStatePatches(
    editorState,
    priorPatchedState,
    commandResult.editorStatePatches,
  )
  return {
    editorState: updatedEditorState,
    editorStatePatches: commandResult.editorStatePatches,
    newStrategyState: commandResult.newStrategyState,
    commandDescriptions: commandResult.commandDescriptions,
  }
}

function foldCommands(
  editorState: EditorState,
  strategyState: StrategyState,
  commands: Array<CanvasCommand>,
  transient: TransientOrNot,
): {
  editorStatePatches: Array<EditorStatePatch>
  newStrategyState: StrategyState
  commandDescriptions: Array<CommandDescription>
} {
  let statePatches: Array<EditorStatePatch> = []
  let workingEditorState: EditorState = editorState
  let workingStrategyState: StrategyState = strategyState
  let workingPathMappings: PathMappings = []
  let workingCommandDescriptions: Array<CommandDescription> = []
  for (const command of commands) {
    // Allow every command if this is a transient fold, otherwise only allow commands that are not transient.
    if (transient === 'transient' || command.transient === 'permanent') {
      // Run the command with our current states.
      const commandResult = runCanvasCommand(
        workingEditorState,
        workingStrategyState,
        workingPathMappings,
        command,
      )
      // Capture values from the result.
      const statePatch = commandResult.editorStatePatch
      workingPathMappings = commandResult.pathMappings
      // Apply the update to the editor state.
      workingEditorState = update(workingEditorState, statePatch)
      // Collate the patches.
      statePatches.push(statePatch)
      workingCommandDescriptions.push({
        description: commandResult.commandDescription,
        transient: command.transient === 'transient',
      })
    }
  }

  return {
    editorStatePatches: statePatches,
    newStrategyState: workingStrategyState,
    commandDescriptions: workingCommandDescriptions,
  }
}

export function applyStatePatches(
  editorState: EditorState,
  priorPatchedState: EditorState,
  patches: Array<EditorStatePatch>,
): EditorState {
  if (patches.length === 0) {
    return editorState
  } else {
    return keepDeepReferenceEqualityIfPossible(
      priorPatchedState,
      patches.reduce((workingState, patch) => {
        return update(workingState, patch)
      }, editorState),
    )
  }
}
