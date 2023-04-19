import React, { useMemo } from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, stripNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { CanvasRectangle, isInfinityRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'
import { isInsertMode } from '../../editor/editor-modes'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

export const ImmediateParentBoundsTestId = (targets: Array<ElementPath>): string =>
  `${targets.map(EP.toString).sort()}-immediate-parent-bounds-control`

export const ParentBoundsTestId = (targets: Array<ElementPath>): string =>
  `${targets.map(EP.toString).sort()}-parent-bounds-control`

interface ImmediateParentBoundsProps {
  targets: Array<ElementPath>
}
export const ImmediateParentBounds = controlForStrategyMemoized(
  ({ targets }: ImmediateParentBoundsProps) => {
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'ParentBounds canvas scale',
    )
    const parentFrame = useEditorState(
      Substores.fullStore,
      (store) => {
        const parentHighlightPaths = store.editor.canvas.controls.parentHighlightPaths
        if (parentHighlightPaths != null && parentHighlightPaths.length === 1) {
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
          if (targetParents.length === 1 && !EP.isStoryboardPath(targetParents[0])) {
            return MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, targets[0])
              ?.specialSizeMeasurements.immediateParentBounds
          }
        }
        return null
      },
      'ImmediateParentBounds frame',
    )

    return parentFrame == null || isInfinityRectangle(parentFrame)
      ? null
      : drawBounds(parentFrame, scale, ImmediateParentBoundsTestId(targets))
  },
)

interface ParentBoundsProps {
  targetParent: ElementPath
}
export const ParentBounds = controlForStrategyMemoized(({ targetParent }: ParentBoundsProps) => {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'ParentBounds canvas scale',
  )

  const parentFrame = useEditorState(
    Substores.canvasAndMetadata,
    (store) => {
      if (store.editor.canvas.controls.parentOutlineHighlight != null) {
        return null
      }
      if (!EP.isStoryboardPath(targetParent)) {
        return MetadataUtils.getFrameInCanvasCoords(targetParent, store.editor.jsxMetadata)
      } else {
        return null
      }
    },
    'ParentBounds frame',
  )

  return parentFrame == null || isInfinityRectangle(parentFrame)
    ? null
    : drawBounds(parentFrame, scale, ParentBoundsTestId([targetParent]))
})

function drawBounds(parentFrame: CanvasRectangle, scale: number, testId: string) {
  return (
    <CanvasOffsetWrapper key={testId}>
      <div style={{ pointerEvents: 'none' }} data-testid={testId}>
        <CenteredCrossSVG
          id='parent-cross-top-left'
          centerX={parentFrame.x}
          centerY={parentFrame.y}
          scale={scale}
        />
        <CenteredCrossSVG
          id='parent-cross-top-right'
          centerX={parentFrame.x + parentFrame.width}
          centerY={parentFrame.y}
          scale={scale}
        />
        <CenteredCrossSVG
          id='parent-cross-bottom-right'
          centerX={parentFrame.x + parentFrame.width}
          centerY={parentFrame.y + parentFrame.height}
          scale={scale}
        />
        <CenteredCrossSVG
          id='parent-cross-bottom-left'
          centerX={parentFrame.x}
          centerY={parentFrame.y + parentFrame.height}
          scale={scale}
        />
      </div>
    </CanvasOffsetWrapper>
  )
}

interface CenteredCrossSVGProps {
  id: string
  scale: number
  centerX: number
  centerY: number
}

const CenteredCrossSVG = React.memo(({ id, centerX, centerY, scale }: CenteredCrossSVGProps) => {
  const colorTheme = useColorTheme()
  return (
    <svg
      id={id}
      style={{
        left: centerX,
        top: centerY,
        position: 'absolute',
        width: 6,
        height: 6,
        transformOrigin: 'center center',
        transform: `translateX(-50%) translateY(-50%) scale(${1 / scale})`,
      }}
      width='4px'
      height='4px'
      viewBox='0 0 4 4'
      version='1.1'
    >
      <g
        stroke='none'
        strokeWidth='1'
        fill='none'
        fillRule='evenodd'
        strokeLinecap='round'
        strokeLinejoin='round'
      >
        <g id='cross_svg' stroke={colorTheme.primary.value}>
          <line x1='0.5' y1='0.5' x2='3.5' y2='3.5'></line>
          <line x1='0.5' y1='3.5' x2='3.5' y2='0.5'></line>
        </g>
      </g>
    </svg>
  )
})
