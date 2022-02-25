import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { safeIndex } from '../../../core/shared/array-utils'
import { foldEither } from '../../../core/shared/either'
import {
  emptyComments,
  isJSXElement,
  jsxAttributeValue,
} from '../../../core/shared/element-template'
import { getNumberPropertyFromProps } from '../../../core/shared/jsx-attributes'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { ProjectContentTreeRoot } from '../../assets'
import { withUnderlyingTarget } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { setProperty, wildcardPatch } from '../commands/commands'
import { ParentPaddingControl } from '../controls/parent-padding-controls'
import * as EP from '../../../core/shared/element-path'
import { CanvasStrategy } from './canvas-strategy-types'

function getElementPadding(
  target: ElementPath,
  projectContents: ProjectContentTreeRoot,
  openFile: string | null,
): number | null {
  return withUnderlyingTarget(target, projectContents, {}, openFile, null, (_success, element) => {
    return getNumberPropertyFromProps(element.props, stylePropPathMappingFn('padding', ['style']))
  })
}

export const parentPaddingAdjustStrategy: CanvasStrategy = {
  name: 'Change Parent Padding',
  strategyGroups: new Set(),
  isApplicable: (canvasState, interactionState, metadata) => {
    if (canvasState.selectedElements.length === 1) {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        metadata,
        canvasState.selectedElements[0],
      )

      if (
        elementMetadata == null ||
        interactionState == null ||
        interactionState.interactionData.type !== 'DRAG'
      ) {
        return false
      } else {
        const parentPath = EP.parentPath(elementMetadata.elementPath)
        const parentPadding = getElementPadding(
          parentPath,
          canvasState.projectContents,
          canvasState.openFile ?? null,
        )
        const paddingChange = interactionState.interactionData.drag?.x ?? 0
        const newPadding = Math.max((parentPadding ?? 0) + paddingChange, 0)
        const hasPositivePadding = parentPadding != null && newPadding > 0
        return hasPositivePadding && elementMetadata?.specialSizeMeasurements.position === 'static'
      }
    }
    return false
  },
  controlsToRender: [
    {
      control: ParentPaddingControl,
      key: 'parent-padding-control',
      show: 'visible-only-while-active',
    },
  ], // parent padding control
  fitness: (canvasState, interactionState, sessionState) => {
    return parentPaddingAdjustStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    )
      ? 2
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      // Only looks at the first selected element.
      const targetedElement = forceNotNull(
        'Could not get first element.',
        safeIndex(canvasState.selectedElements, 0),
      )
      const parentPath = EP.parentPath(targetedElement)
      const parentPadding = getElementPadding(
        parentPath,
        canvasState.projectContents,
        canvasState.openFile ?? null,
      )
      const paddingChange = interactionState.interactionData.drag.x
      if (parentPadding !== null) {
        const newPadding = Math.max(parentPadding + paddingChange, 0)
        const adjustProperty = setProperty(
          'permanent',
          parentPath,
          stylePropPathMappingFn('padding', ['style']),
          jsxAttributeValue(newPadding, emptyComments),
        )

        return [
          adjustProperty,
          wildcardPatch('transient', {
            highlightedViews: {
              $set: [],
            },
          }),
        ]
      }
    }

    // Fallback for when the checks above are not satisfied.
    return []
  },
}
