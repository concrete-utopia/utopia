import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { LayoutPinnedProp, LayoutPinnedProps } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { foldEither, forEachRight, isLeft, right } from '../../../core/shared/either'
import { isJSXElement, JSXElement } from '../../../core/shared/element-template'
import { setJSXValuesAtPaths, ValueAtPath } from '../../../core/shared/jsx-attributes'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { ElementPath, RevisionsState } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import {
  EditorStatePatch,
  EditorState,
  modifyUnderlyingForOpenFile,
  forUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import { SelectModeCanvasSessionState } from '../canvas-strategies/canvas-strategy-types'
import { cssNumberAsNumberIfPossible, getPropsToSetToMoveElement } from '../canvas-utils'
import update, { Spec } from 'immutability-helper'
import * as EP from '../../../core/shared/element-path'
import { canvasPoint } from '../../../core/shared/math-utils'
import {
  getProjectContentKeyPathElements,
  ProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../assets'
import { drop } from '../../../core/shared/array-utils'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'

export interface PathMapping {
  from: ElementPath
  to: ElementPath
}

export type PathMappings = Array<PathMapping>

export interface CommandFunctionResult {
  sessionState: SelectModeCanvasSessionState
  editorStatePatch: EditorStatePatch
  pathMappings: PathMappings
}

export type CommandFunction<T> = (
  editorState: EditorState,
  sessionState: SelectModeCanvasSessionState,
  pathMappings: PathMappings,
  command: T,
) => CommandFunctionResult

export interface SetDragMinimumExceededCommand {
  type: 'SET_DRAG_MININUM_EXCEEDED'
}

export const setDragMinimumExceededCommand: SetDragMinimumExceededCommand = {
  type: 'SET_DRAG_MININUM_EXCEEDED',
}

export interface MoveElement {
  type: 'MOVE_ELEMENT'
  target: ElementPath
  x: number
  y: number
}

export function moveElement(target: ElementPath, x: number, y: number): MoveElement {
  return {
    type: 'MOVE_ELEMENT',
    target: target,
    x: x,
    y: y,
  }
}

export interface WildcardPatch {
  type: 'WILDCARD_PATCH'
  patch: EditorStatePatch
}

export function wildcardPatch(patch: EditorStatePatch): WildcardPatch {
  return {
    type: 'WILDCARD_PATCH',
    patch: patch,
  }
}

export type CanvasCommand = SetDragMinimumExceededCommand | MoveElement | WildcardPatch

export const runSetDragMinimumExceededCommand: CommandFunction<SetDragMinimumExceededCommand> = (
  editorState: EditorState,
  sessionState: SelectModeCanvasSessionState,
  pathMappings: PathMappings,
  command: SetDragMinimumExceededCommand,
) => {
  return {
    sessionState: {
      ...sessionState,
      dragDeltaMinimumPassed: true,
    },
    editorStatePatch: {},
    pathMappings: pathMappings,
  }
}

export const runMoveElementCommand: CommandFunction<MoveElement> = (
  editorState: EditorState,
  sessionState: SelectModeCanvasSessionState,
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
        }
      })
      break
    // For everything else...
    default:
  }

  return {
    sessionState: sessionState,
    editorStatePatch: editorStatePatch,
    pathMappings: pathMappings,
  }
}

export const runWildcardPatch: CommandFunction<WildcardPatch> = (
  editorState: EditorState,
  sessionState: SelectModeCanvasSessionState,
  pathMappings: PathMappings,
  command: WildcardPatch,
) => {
  return {
    sessionState: sessionState,
    editorStatePatch: command.patch,
    pathMappings: pathMappings,
  }
}

export function applyValuesAtPath(
  editorState: EditorState,
  target: ElementPath,
  jsxValuesAndPathsToSet: ValueAtPath[],
): EditorStatePatch {
  let result: EditorStatePatch = {}

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

      result = {
        projectContents: projectContentTreeRootPatch,
      }
    },
  )
  return result
}

export const runCanvasCommand: CommandFunction<CanvasCommand> = (
  editorState: EditorState,
  sessionState: SelectModeCanvasSessionState,
  pathMappings: PathMappings,
  command: CanvasCommand,
) => {
  switch (command.type) {
    case 'MOVE_ELEMENT':
      return runMoveElementCommand(editorState, sessionState, pathMappings, command)
    case 'SET_DRAG_MININUM_EXCEEDED':
      return runSetDragMinimumExceededCommand(editorState, sessionState, pathMappings, command)
    case 'WILDCARD_PATCH':
      return runWildcardPatch(editorState, sessionState, pathMappings, command)
    default:
      const _exhaustiveCheck: never = command
      throw new Error(`Unhandled canvas command ${JSON.stringify(command)}`)
  }
}

export function foldCommands(
  editorState: EditorState,
  sessionState: SelectModeCanvasSessionState,
  commands: Array<CanvasCommand>,
): { statePatches: Array<EditorStatePatch>; sessionState: SelectModeCanvasSessionState } {
  let statePatches: Array<EditorStatePatch> = []
  let workingEditorState: EditorState = editorState
  let workingSessionState: SelectModeCanvasSessionState = sessionState
  let workingPathMappings: PathMappings = []
  for (const command of commands) {
    // Run the command with our current states.
    const commandResult = runCanvasCommand(
      workingEditorState,
      workingSessionState,
      workingPathMappings,
      command,
    )
    // Capture values from the result.
    workingSessionState = commandResult.sessionState
    const statePatch = commandResult.editorStatePatch
    workingPathMappings = commandResult.pathMappings
    // Apply the update to the editor state.
    workingEditorState = update(workingEditorState, statePatch)
    // Collate the patches.
    statePatches.push(statePatch)
  }

  return {
    statePatches: statePatches,
    sessionState: workingSessionState,
  }
}

export function applyStatePatches(
  editorState: EditorState,
  priorPatchedState: EditorState,
  patches: Array<EditorStatePatch>,
): EditorState {
  return keepDeepReferenceEqualityIfPossible(
    priorPatchedState,
    patches.reduce((workingState, patch) => {
      return update(workingState, patch)
    }, editorState),
  )
}
