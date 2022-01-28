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
  withUnderlyingTarget,
  withUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import {
  CanvasStrategyUpdateFnResult,
  FlexAlignControlRectProps,
  SelectModeCanvasSession,
  SelectModeCanvasSessionProps,
  SelectModeCanvasSessionState,
} from './canvas-strategy-types'
import { aperture, mapDropNulls, safeIndex } from '../../../core/shared/array-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { forceNotNull, optionalMap } from '../../../core/shared/optional-utils'
import { adjustNumberProperty, applyValuesAtPath, wildcardPatch } from '../commands/commands'
import { CanvasStrategy } from '../../../interactions_proposal'

export const flexGapStrategy: CanvasStrategy = {
  name: 'Change Flex Gap',
  isApplicable: (canvasState, interactionState) => {
    if (
      canvasState.selectedElements.length === 1 &&
      interactionState != null &&
      interactionState.activeControl.type === 'FLEX_GAP_HANDLE'
    ) {
      const selectedView = canvasState.selectedElements[0]
      return selectedView.specialSizeMeasurements.parentLayoutSystem === 'flex'
    }
    return false
  },
  controlsToRender: (canvasState, interactionState) => {
    return []
  },
  fitness: (canvasState, interactionState) => {
    return flexGapStrategy.isApplicable(canvasState, interactionState) &&
      interactionState.interactionData.type === 'DRAG'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'FLEX_GAP_HANDLE'
    ) {
      // Only looks at the first selected element.
      const targetedElement = forceNotNull(
        'Could not get first element.',
        safeIndex(canvasState.selectedElements, 0),
      )
      const targetParent = MetadataUtils.getParent(
        canvasState.metadata,
        targetedElement.elementPath,
      )
      const gapPropPath = stylePropPathMappingFn('gap', ['style'])

      if (targetParent !== null) {
        const flexDirection = MetadataUtils.getFlexDirection(targetParent)
        let gapChange: number = 0
        if (flexDirection.startsWith('row')) {
          gapChange = interactionState.interactionData.drag?.x ?? 0
        } else {
          gapChange = interactionState.interactionData.drag?.y ?? 0
        }
        const adjustProperty = adjustNumberProperty(
          'permanent',
          targetedElement.elementPath,
          gapPropPath,
          gapChange,
        )

        // Identify the siblings so that the metadata gets updated for those as well,
        // which should result in the gap controls also being updated.
        const siblingsOfTarget = MetadataUtils.getSiblings(
          canvasState.metadata,
          targetedElement.elementPath,
        ).map((metadata) => metadata.elementPath)
        return [
          adjustProperty,
          wildcardPatch('transient', {
            highlightedViews: {
              $set: [],
            },
            canvas: {
              domWalkerAdditionalElementsToUpdate: {
                $set: siblingsOfTarget,
              },
            },
          }),
        ]
      }
    }

    // Fallback for when the checks above are not satisfied.
    return []
  },
}
