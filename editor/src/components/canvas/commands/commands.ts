import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { LayoutPinnedProp, LayoutPinnedProps } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  applyUtopiaJSXComponentsChanges,
  getUtopiaJSXComponentsFromSuccess,
} from '../../../core/model/project-file-utils'
import { foldEither, forEachRight, isLeft, right } from '../../../core/shared/either'
import { isJSXElement, JSXElement } from '../../../core/shared/element-template'
import { setJSXValuesAtPaths, ValueAtPath } from '../../../core/shared/jsx-attributes'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import {
  TransientFilesState,
  EditorStatePatch,
  EditorState,
  modifyUnderlyingForOpenFile,
  forUnderlyingTargetFromEditorState,
  transformElementAtPath,
  withUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import {
  SelectModeCanvasSessionState,
  SelectModeCanvasSessionProps,
} from '../canvas-strategies/canvas-strategy-types'
import { cssNumberAsNumberIfPossible, getPropsToSetToMoveElement } from '../canvas-utils'
import update from 'immutability-helper'
import * as EP from '../../../core/shared/element-path'
import { canvasPoint, point } from '../../../core/shared/math-utils'

export interface CommandFunctionResult {
  sessionState: SelectModeCanvasSessionState
  transientFilesState: TransientFilesState
  editorStatePatch: EditorStatePatch
}

export type CommandFunction<T> = (
  editorState: EditorState,
  sessionState: SelectModeCanvasSessionState,
  command: T,
) => CommandFunctionResult

export interface SetDragMinimumExceededCommand {
  type: 'SET_DRAG_MININUM_EXCEEDED'
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

export type CanvasCommand = SetDragMinimumExceededCommand | MoveElement

export const runSetDragMinimumExceededCommand: CommandFunction<SetDragMinimumExceededCommand> = (
  editorState: EditorState,
  sessionState: SelectModeCanvasSessionState,
  command: SetDragMinimumExceededCommand,
) => {
  return {
    sessionState: {
      ...sessionState,
      dragDeltaMinimumPassed: true,
    },
    transientFilesState: {},
    editorStatePatch: {},
  }
}

export const runMoveElementCommand: CommandFunction<MoveElement> = (
  editorState: EditorState,
  sessionState: SelectModeCanvasSessionState,
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

  let workingEditorState: EditorState = editorState
  let workingFileState: TransientFilesState = {}

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
          const result = applyValuesAtPath(
            workingEditorState,
            workingFileState,
            command.target,
            whatToMove,
          )
          workingEditorState = result.editorState
          workingFileState = result.transientFilesState
        }
      })
      break
    // For everything else...
    default:
  }

  return {
    sessionState: sessionState,
    transientFilesState: workingFileState,
    editorStatePatch: {},
  }
}

function applyValuesAtPath(
  editorState: EditorState,
  filesState: TransientFilesState,
  target: ElementPath,
  jsxValuesAndPathsToSet: ValueAtPath[],
): { editorState: EditorState; transientFilesState: TransientFilesState } {
  let workingEditorState = { ...editorState }
  let transientFilesState = { ...filesState }

  workingEditorState = modifyUnderlyingForOpenFile(target, editorState, (element: JSXElement) => {
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
  })

  forUnderlyingTargetFromEditorState(
    target,
    workingEditorState,
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      transientFilesState[underlyingFilePath] = {
        topLevelElementsIncludingScenes: success.topLevelElements,
        imports: success.imports,
      }
      return success
    },
  )
  return { editorState: workingEditorState, transientFilesState: transientFilesState }
}

export const runCanvasCommand: CommandFunction<CanvasCommand> = (
  editorState: EditorState,
  sessionState: SelectModeCanvasSessionState,
  command: CanvasCommand,
) => {
  switch (command.type) {
    case 'MOVE_ELEMENT':
      return runMoveElementCommand(editorState, sessionState, command)
    case 'SET_DRAG_MININUM_EXCEEDED':
      return runSetDragMinimumExceededCommand(editorState, sessionState, command)
    default:
      const _exhaustiveCheck: never = command
      throw new Error(`Unhandled canvas command ${JSON.stringify(command)}`)
  }
}

export function foldCommands(
  editorState: EditorState,
  sessionState: SelectModeCanvasSessionState,
  commands: Array<CanvasCommand>,
): void {
  let workingEditorState: EditorState = editorState
  let workingSessionState: SelectModeCanvasSessionState = sessionState
  for (const command of commands) {
    const commandResult = runCanvasCommand(workingEditorState, workingSessionState, command)
    workingSessionState = commandResult.sessionState
    workingEditorState = update(workingEditorState, commandResult.editorStatePatch)
    commandResult.editorStatePatch
    // Possibly need some way to combine editor state patches if we want to return an accumulated one of those.
  }

  // Depends on what we want to return out of here.
}
