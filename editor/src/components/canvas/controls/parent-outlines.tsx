import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { stripNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import type { CanvasRectangle } from '../../../core/shared/math-utils'
import { isInfinityRectangle } from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'
import type { ThemeObject } from '../../../uuiui/styles/theme/theme-helpers'
import { isInsertMode } from '../../editor/editor-modes'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import {
  findMaybeConditionalExpression,
  findFirstNonConditionalAncestor,
} from '../../../core/model/conditionals'
import { treatElementAsGroupLike } from '../canvas-strategies/strategies/group-helpers'

export const ParentOutlinesTestIdSuffix = `-parent-outlines-control`

export const ImmediateParentOutlinesTestId = (targetPaths: Array<ElementPath>): string =>
  `${targetPaths.map(EP.toString).sort()}-immediate${ParentOutlinesTestIdSuffix}`

export const ParentOutlinesTestId = (targetPaths: Array<ElementPath>): string =>
  `${targetPaths.map(EP.toString).sort()}${ParentOutlinesTestIdSuffix}`

interface ImmediateParentOutlinesProps {
  targets: Array<ElementPath>
}

export const ImmediateParentOutlines = controlForStrategyMemoized(
  ({ targets }: ImmediateParentOutlinesProps) => {
    const colorTheme = useColorTheme()
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'ImmediateParentOutlines canvas scale',
    )
    const parentFrame = useEditorState(
      Substores.fullStore,
      (store) => {
        // Prevent this from showing when manipulating the child of a group.
        function anyGroups(paths: Array<ElementPath>): boolean {
          return paths.some((path) => {
            return treatElementAsGroupLike(store.editor.jsxMetadata, path)
          })
        }

        const parentHighlightPaths = store.editor.canvas.controls.parentHighlightPaths
        if (
          parentHighlightPaths != null &&
          parentHighlightPaths.length === 1 &&
          !anyGroups(parentHighlightPaths)
        ) {
          return MetadataUtils.getFrameInCanvasCoords(
            parentHighlightPaths[0],
            store.editor.jsxMetadata,
          )
        }

        if (!isInsertMode(store.editor.mode)) {
          const targetParents = uniqBy(
            stripNulls(targets.map((view) => EP.parentPath(view))),
            EP.pathsEqual,
          )
          if (
            targetParents.length === 1 &&
            !EP.isStoryboardPath(targetParents[0]) &&
            !anyGroups(targetParents)
          ) {
            return MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, targets[0])
              ?.specialSizeMeasurements.immediateParentBounds
          }
        }
        return null
      },
      'ImmediateParentOutlines frame',
    )

    return parentFrame == null || isInfinityRectangle(parentFrame)
      ? null
      : drawOutlines(parentFrame, scale, colorTheme, ImmediateParentOutlinesTestId(targets), false)
  },
)

interface ParentOutlinesProps {
  targetParent: ElementPath
}

export const ParentOutlines = controlForStrategyMemoized(
  ({ targetParent }: ParentOutlinesProps) => {
    const colorTheme = useColorTheme()
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'ParentOutlines canvas scale',
    )

    const { parentFrame, isSlot } = useEditorState(
      Substores.metadata,
      (store) => {
        if (EP.isStoryboardPath(targetParent)) {
          return { parentFrame: null, isSlot: false }
        }

        // Prevent this from showing when manipulating the child of a group.
        if (treatElementAsGroupLike(store.editor.jsxMetadata, targetParent)) {
          return { parentFrame: null, isSlot: false }
        }

        const isSlotTarget =
          findMaybeConditionalExpression(targetParent, store.editor.jsxMetadata) != null
        const target = isSlotTarget
          ? findFirstNonConditionalAncestor(targetParent, store.editor.jsxMetadata)
          : targetParent
        return {
          parentFrame: MetadataUtils.getFrameInCanvasCoords(target, store.editor.jsxMetadata),
          isSlot: isSlotTarget,
        }
      },
      'ImmediateParentOutlines frame',
    )

    return parentFrame == null || isInfinityRectangle(parentFrame)
      ? null
      : drawOutlines(parentFrame, scale, colorTheme, ParentOutlinesTestId([targetParent]), isSlot)
  },
)

function drawOutlines(
  parentFrame: CanvasRectangle,
  scale: number,
  colorTheme: ThemeObject,
  testId: string,
  isSlot: boolean,
) {
  return (
    <CanvasOffsetWrapper key={testId}>
      <div
        style={{
          position: 'absolute',
          left: parentFrame.x,
          top: parentFrame.y,
          width: parentFrame.width,
          height: parentFrame.height,
          outlineStyle: isSlot ? 'solid' : 'dotted',
          outlineColor: isSlot ? colorTheme.brandNeonGreen.value : colorTheme.primary.value,
          outlineWidth: 1 / scale,
          pointerEvents: 'none',
        }}
        data-testid={testId}
      />
    </CanvasOffsetWrapper>
  )
}
