import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { foldEither, isRight } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  emptyComments,
  isJSXElement,
  jsxAttributeValue,
  JSXElement,
} from '../../../core/shared/element-template'
import {
  getJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  setJSXValuesAtPaths,
  ValueAtPath,
} from '../../../core/shared/jsx-attributes'
import {
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  magnitude,
  rectContainsPoint,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import {
  EditorState,
  forUnderlyingTargetFromEditorState,
  modifyUnderlyingForOpenFile,
  TransientFilesState,
  withUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import {
  CanvasStrategy,
  CanvasStrategyUpdateFnResult,
  FlexAlignControlRectProps,
  SelectModeCanvasSession,
  SelectModeCanvasSessionProps,
  SelectModeCanvasSessionState,
} from './canvas-strategy-types'
import { aperture, mapDropNulls } from '../../../core/shared/array-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { optionalMap } from '../../../core/shared/optional-utils'

export const flexGapStrategy: CanvasStrategy = {
  name: 'Change Flex Gap',
  fitnessFn: (editor, currentSession) => {
    if (editor.selectedViews.length === 1) {
      const selectedView = editor.selectedViews[0]

      const isFlexLayouted = MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
        MetadataUtils.findElementByElementPath(editor.jsxMetadata, selectedView),
      )
      if (isFlexLayouted) {
        return 10 // fit!
      }
    }
    return 0 // not fit
  },
  updateFn: (
    lifecycle: 'transient' | 'final',
    editorState: EditorState,
    sessionProps: SelectModeCanvasSessionProps,
    sessionState: SelectModeCanvasSessionState,
  ): CanvasStrategyUpdateFnResult => {
    if (sessionProps.activeControl.type === 'FLEX_GAP_HANDLE') {
      // Only looks at the first selected element.
      const targetedElement = editorState.selectedViews[0]
      const targetParent = MetadataUtils.getParent(editorState.jsxMetadata, targetedElement)
      const isFlexLayouted = MetadataUtils.isFlexLayoutedContainer(targetParent)
      const gapPropPath = stylePropPathMappingFn('gap', ['style'])

      if (targetParent !== null && isFlexLayouted && sessionProps.drag !== null) {
        // Identify the current flex gap value, whatever that may be.
        const currentGap = withUnderlyingTargetFromEditorState(
          targetParent.elementPath,
          editorState,
          null,
          (success, element, underlyingTarget, underlyingFilePath) => {
            if (isJSXElement(element)) {
              return getJSXAttributeAtPath(element.props, gapPropPath)
            } else {
              return null
            }
          },
        )

        // Handle updating the existing gap value, treating a value that can't be parsed
        // as zero.
        let newGap: number = 0
        const flexDirection = MetadataUtils.getFlexDirection(targetParent)
        if (flexDirection.startsWith('row')) {
          newGap += sessionProps.mousePosition.x - sessionProps.start.x
        } else {
          newGap += sessionProps.mousePosition.y - sessionProps.start.y
        }
        const currentGapValue = optionalMap(jsxSimpleAttributeToValue, currentGap?.attribute)
        if (currentGapValue !== null && isRight(currentGapValue)) {
          newGap += currentGapValue.value
        }
        const propsToUpdate: Array<ValueAtPath> = [
          {
            path: gapPropPath,
            value: jsxAttributeValue(newGap, emptyComments),
          },
        ]

        // Apply the update into the transient state.
        const { transientFilesState: transientFilesStateAfterUpdate } = applyValuesAtPath(
          editorState,
          {},
          targetParent.elementPath,
          propsToUpdate,
        )
        // Identify the siblings so that the metadata gets updated for those as well,
        // which should result in the gap controls also being updated.
        const siblingsOfTarget = MetadataUtils.getSiblings(
          editorState.jsxMetadata,
          targetedElement,
        ).map((metadata) => metadata.elementPath)
        return {
          newSessionState: sessionState,
          transientFilesState: transientFilesStateAfterUpdate,
          editorStatePatch: {
            canvas: {
              domWalkerAdditionalElementsToUpdate: {
                $set: siblingsOfTarget,
              },
            },
          },
        }
      }
    }

    // Fallback for when the checks above are not satisfied.
    return {
      newSessionState: sessionState,
      transientFilesState: {},
      editorStatePatch: {},
    }
  },
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
