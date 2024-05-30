/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import React from 'react'
import { jsx } from '@emotion/react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { useColorTheme } from '../../../uuiui'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { clearHighlightedViews, setHighlightedView } from '../../editor/actions/action-creators'
import { selectComponents } from '../../editor/actions/meta-actions'
import type { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import {
  isInfinityRectangle,
  isFiniteRectangle,
  windowPoint,
  point,
  offsetRect,
} from '../../../core/shared/math-utils'
import type { EditorDispatch } from '../../editor/action-types'
import { isZeroSizedElement, ZeroControlSize } from './outline-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { useMaybeHighlightElement } from './select-mode/select-mode-hooks'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { EditorModes } from '../../editor/editor-modes'
import * as EditorActions from '../../editor/actions/action-creators'
import CanvasActions from '../canvas-actions'
import { Modifier } from '../../../utils/modifiers'
import { useWindowToCanvasCoordinates } from '../dom-lookup-hooks'
import { boundingArea, createInteractionViaMouse } from '../canvas-strategies/interaction-state'
import * as PP from '../../../core/shared/property-path'
import type { InspectorStrategy } from '../../inspector/inspector-strategies/inspector-strategy'
import { executeFirstApplicableStrategy } from '../../inspector/inspector-strategies/inspector-strategy'
import {
  isIntrinsicallyInlineElement,
  sizeToDimensionsFromFrame,
} from '../../inspector/inspector-common'
import type { CanvasCommand } from '../commands/commands'
import { setProperty } from '../commands/set-property-command'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'

export const ZeroSizedControlTestID = 'zero-sized-control'
export const ZeroSizedEventsControlTestID = `${ZeroSizedControlTestID}-events`
interface ZeroSizedElementControlProps {
  showAllPossibleElements: boolean
}

export const ZeroSizedElementControls = controlForStrategyMemoized(
  ({ showAllPossibleElements }: ZeroSizedElementControlProps) => {
    const highlightedViews = useEditorState(
      Substores.highlightedHoveredViews,
      (store) => store.editor.highlightedViews,
      'ZeroSizedElementControls highlightedViews',
    )
    const selectedElements = useEditorState(
      Substores.selectedViews,
      (store) => store.editor.selectedViews,
      'ZeroSizedElementControls selectedElements',
    )
    const canvasOffset = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.realCanvasOffset,
      'ZeroSizedElementControls canvasOffset',
    )
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'ZeroSizedElementControls scale',
    )
    const dispatch = useDispatch()

    const projectContents = useEditorState(
      Substores.projectContents,
      (store) => store.editor.projectContents,
      'ZeroSizedElementControls projectContents',
    )

    const zeroSizeElements = useEditorState(
      Substores.metadataAndPropertyControlsInfo,
      (store) => {
        if (showAllPossibleElements) {
          return Object.values(store.editor.jsxMetadata).filter((element) => {
            return (
              element.globalFrame != null &&
              isFiniteRectangle(element.globalFrame) &&
              isZeroSizedElement(element.globalFrame) &&
              MetadataUtils.targetElementSupportsChildren(
                projectContents,
                element.elementPath,
                store.editor.jsxMetadata,
                store.editor.elementPathTree,
                store.editor.propertyControlsInfo,
              )
            )
          })
        } else {
          return selectedElements.flatMap((view) => {
            const children = MetadataUtils.getChildrenOrdered(
              store.editor.jsxMetadata,
              store.editor.elementPathTree,
              view,
            )
            return children.filter((child) => {
              if (child.globalFrame == null) {
                return false
              } else {
                return (
                  isFiniteRectangle(child.globalFrame) &&
                  isZeroSizedElement(child.globalFrame) &&
                  MetadataUtils.targetElementSupportsChildren(
                    projectContents,
                    child.elementPath,
                    store.editor.jsxMetadata,
                    store.editor.elementPathTree,
                    store.editor.propertyControlsInfo,
                  )
                )
              }
            })
          })
        }
      },
      'ZeroSizedElementControls zeroSizeElements',
    )

    return (
      <React.Fragment>
        {zeroSizeElements.map((element) => {
          let isHighlighted =
            highlightedViews.find((view) => EP.pathsEqual(element.elementPath, view)) != null
          return (
            <ZeroSizeSelectControl
              key={`zero-size-element-${EP.toString(element.elementPath)}`}
              element={element}
              dispatch={dispatch}
              canvasOffset={canvasOffset}
              scale={scale}
              isHighlighted={isHighlighted}
            />
          )
        })}
      </React.Fragment>
    )
  },
)

const convertFlexChildToSizedStrategy = (
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  element: ElementInstanceMetadata,
): InspectorStrategy => ({
  name: 'convert flex child to non-zero frame',
  strategy: () => {
    if (
      element.specialSizeMeasurements.parentLayoutSystem !== 'flex' ||
      element.specialSizeMeasurements.parentFlexDirection == null
    ) {
      return null
    }
    return sizeToDimensionsFromFrame(metadata, pathTrees, element.elementPath, {
      width: 100,
      height: 100,
    })
  },
})

function maybeAddDisplayInlineBlockCommands(
  element: ElementInstanceMetadata,
): Array<CanvasCommand> {
  return isIntrinsicallyInlineElement(element)
    ? [setProperty('always', element.elementPath, PP.create('style', 'display'), 'inline-block')]
    : []
}

const convertToSizedStrategy = (
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  element: ElementInstanceMetadata,
): InspectorStrategy => ({
  name: 'convert element to non-zero frame',
  strategy: () => {
    return [
      ...sizeToDimensionsFromFrame(metadata, pathTrees, element.elementPath, {
        width: 100,
        height: 100,
      }),
      ...maybeAddDisplayInlineBlockCommands(element),
    ]
  },
})

const ZeroSizeSelectControl = React.memo((props: ZeroSizeSelectControlProps) => {
  const colorTheme = useColorTheme()
  const { dispatch, element, canvasOffset, scale } = props

  const onControlMouseDown = useZeroSizeStartDrag(element.elementPath)

  const onControlMouseOver = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      if (!props.isHighlighted) {
        dispatch([setHighlightedView(element.elementPath)], 'everyone')
      }
    },
    [dispatch, element.elementPath, props.isHighlighted],
  )

  const onControlMouseOut = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      if (props.isHighlighted) {
        dispatch([clearHighlightedViews()], 'everyone')
      }
    },
    [dispatch, props.isHighlighted],
  )

  if (element.globalFrame == null || isInfinityRectangle(element.globalFrame)) {
    return null
  } else {
    const frame = element.globalFrame
    return (
      <>
        <div
          style={{
            position: 'absolute',
            ...zeroSizedControlDimensions(offsetRect(frame, canvasOffset), scale, true),
          }}
          css={{
            boxShadow: zeroSizedControlBoxShadow(scale, colorTheme.primary.value, 'thin'),
            '&:hover': {
              boxShadow: zeroSizedControlBoxShadow(scale, colorTheme.primary.value, 'thick'),
            },
          }}
        />
        <div
          onMouseDown={onControlMouseDown}
          onMouseOver={onControlMouseOver}
          onMouseOut={onControlMouseOut}
          style={{
            position: 'absolute',
            ...zeroSizedEventControlDimensions(offsetRect(frame, canvasOffset), scale),
          }}
        />
      </>
    )
  }
})

interface ZeroSizeSelectControlProps {
  element: ElementInstanceMetadata
  dispatch: EditorDispatch
  canvasOffset: CanvasPoint
  scale: number
  isHighlighted: boolean
}

export interface ZeroSizeControlProps {
  frame: CanvasRectangle
  canvasOffset: CanvasPoint
  scale: number
  color: string | null | undefined
}

export const ZeroSizeHighlightControl = React.memo((props: ZeroSizeControlProps) => {
  return (
    <div
      className='role-component-highlight-outline-no-size'
      style={{
        position: 'absolute',
        ...zeroSizedControlDimensions(
          offsetRect(props.frame, props.canvasOffset),
          props.scale,
          true,
        ),
        boxShadow: zeroSizedControlBoxShadow(props.scale, props.color, 'thin'),
      }}
    />
  )
})

export const ZeroSizeOutlineControl = React.memo(
  (props: Omit<ZeroSizeControlProps, 'canvasOffset'>) => {
    const colorTheme = useColorTheme()

    return (
      <CanvasOffsetWrapper>
        <div
          className='role-outline-no-size'
          style={{
            position: 'absolute',
            ...zeroSizedControlDimensions(props.frame, props.scale, true),
            boxShadow: zeroSizedControlBoxShadow(
              props.scale,
              colorTheme.primary.value,
              'thin',
              true,
            ),
          }}
        />
      </CanvasOffsetWrapper>
    )
  },
)

interface ZeroSizeResizeControlWrapperProps {
  targets: Array<ElementPath>
}

export const ZeroSizeResizeControlWrapper = controlForStrategyMemoized(
  ({ targets }: ZeroSizeResizeControlWrapperProps) => {
    const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()
    const zeroSizeElements = useEditorState(
      Substores.metadata,
      (store) => {
        return mapDropNulls((path) => {
          const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
          const frame = MetadataUtils.getFrameInCanvasCoords(path, store.editor.jsxMetadata)
          if (frame != null && isFiniteRectangle(frame) && isZeroSizedElement(frame)) {
            return element
          } else {
            return null
          }
        }, targets)
      },
      'ZeroSizeResizeControlWrapper zeroSizeElements',
    )

    const dispatch = useDispatch()
    const scale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'ZeroSizeResizeControlWrapper scale',
    )

    return (
      <React.Fragment>
        {zeroSizeElements.map((element) => {
          if (element.globalFrame != null && isFiniteRectangle(element.globalFrame)) {
            return (
              <React.Fragment key={EP.toString(element.elementPath)}>
                <ZeroSizeOutlineControl frame={element.globalFrame} scale={scale} color={null} />
                <ZeroSizeResizeControl
                  element={element}
                  frame={element.globalFrame}
                  dispatch={dispatch}
                  scale={scale}
                  color={null}
                  maybeClearHighlightsOnHoverEnd={maybeClearHighlightsOnHoverEnd}
                />
              </React.Fragment>
            )
          } else {
            return null
          }
        })}
      </React.Fragment>
    )
  },
)

interface ZeroSizeResizeControlProps {
  frame: CanvasRectangle
  scale: number
  color: string | null | undefined
  element: ElementInstanceMetadata
  dispatch: EditorDispatch
  maybeClearHighlightsOnHoverEnd: () => void
}

export const ZeroSizeResizeControl = React.memo((props: ZeroSizeResizeControlProps) => {
  const { dispatch, element, maybeClearHighlightsOnHoverEnd } = props
  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const elementPathTreesRef = useRefEditorState((store) => store.editor.elementPathTree)

  const onControlMouseDown = useZeroSizeStartDrag(element.elementPath)

  const onControlMouseMove = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      event.stopPropagation()
      maybeClearHighlightsOnHoverEnd()
    },
    [maybeClearHighlightsOnHoverEnd],
  )

  const onControlMouseUp = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      event.stopPropagation()
      dispatch([CanvasActions.clearInteractionSession(true)], 'everyone')
    },
    [dispatch],
  )

  const onControlDoubleClick = React.useCallback(() => {
    const isTextElement = MetadataUtils.isSpan(element)
    if (isTextElement) {
      dispatch(
        [
          EditorActions.switchEditorMode(
            EditorModes.textEditMode(element.elementPath, null, 'existing', 'no-text-selection'),
          ),
          CanvasActions.clearInteractionSession(false),
        ],
        'everyone',
      )
      return
    }

    executeFirstApplicableStrategy(dispatch, [
      convertFlexChildToSizedStrategy(metadataRef.current, elementPathTreesRef.current, element),
      convertToSizedStrategy(metadataRef.current, elementPathTreesRef.current, element),
    ])
  }, [dispatch, element, elementPathTreesRef, metadataRef])

  return (
    <CanvasOffsetWrapper>
      <div
        className='role-resize-no-size'
        data-testid={ZeroSizedControlTestID}
        style={{
          position: 'absolute',
          ...zeroSizedControlDimensions(props.frame, props.scale),
        }}
      />
      <div
        onMouseMove={onControlMouseMove}
        onMouseDown={onControlMouseDown}
        onMouseUp={onControlMouseUp}
        onDoubleClick={onControlDoubleClick}
        data-testid={ZeroSizedEventsControlTestID}
        style={{
          position: 'absolute',
          ...zeroSizedEventControlDimensions(props.frame, props.scale),
        }}
      />
    </CanvasOffsetWrapper>
  )
})

function useZeroSizeStartDrag(
  target: ElementPath,
): (event: React.MouseEvent<HTMLDivElement>) => void {
  const dispatch = useDispatch()
  const windowToCanvasCoordinates = useWindowToCanvasCoordinates()
  const selectedElements = useRefEditorState((store) => store.editor.selectedViews)

  return React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      event.stopPropagation()

      const start = windowToCanvasCoordinates(
        windowPoint(point(event.clientX, event.clientY)),
      ).canvasPositionRounded

      const isSelected = selectedElements.current.some((selectedElement) =>
        EP.pathsEqual(selectedElement, target),
      )
      const optionalSelectActions = isSelected ? [] : selectComponents([target], false)

      dispatch(
        [
          ...optionalSelectActions,
          CanvasActions.createInteractionSession(
            createInteractionViaMouse(
              start,
              Modifier.modifiersForEvent(event),
              boundingArea(),
              'zero-drag-not-permitted',
            ),
          ),
        ],
        'everyone',
      )
    },
    [dispatch, target, windowToCanvasCoordinates, selectedElements],
  )
}

function getScaleRatio(scale: number): number {
  return 1 / scale
}

function zeroSizedControlDimensions(
  rect: CanvasRectangle,
  scale: number,
  borderRadius: boolean = false,
): {
  left: number
  top: number
  width: number
  height: number
  borderRadius?: number
} {
  const ratio = getScaleRatio(scale)
  return {
    left: rect.x - (ZeroControlSize / 2) * ratio,
    top: rect.y - (ZeroControlSize / 2) * ratio,
    width: rect.width + ZeroControlSize * ratio,
    height: rect.height + ZeroControlSize * ratio,
    borderRadius: borderRadius ? (ZeroControlSize / 2) * ratio : undefined,
  }
}

// So that we can capture events on what looks like the border
// we need some bounds that overlap around the dimensions of the
// control.
function zeroSizedEventControlDimensions(
  rect: CanvasRectangle,
  scale: number,
): {
  left: number
  top: number
  width: number
  height: number
} {
  const ratio = getScaleRatio(scale)
  const borderAdjustment = ZeroControlSize / 2
  const result = {
    left: rect.x - (ZeroControlSize / 2) * ratio - borderAdjustment * ratio,
    top: rect.y - (ZeroControlSize / 2) * ratio - borderAdjustment * ratio,
    width: rect.width + (ZeroControlSize + borderAdjustment * 2) * ratio,
    height: rect.height + (ZeroControlSize + borderAdjustment * 2) * ratio,
  }
  return result
}

function zeroSizedControlBoxShadow(
  scale: number,
  color: string | null | undefined,
  size: 'thick' | 'thin',
  inset: boolean = false,
): string {
  const ratio = getScaleRatio(scale)
  const multiplier = size === 'thick' ? 2 : 1
  const boxShadow = `0px 0px 0px ${ratio * multiplier}px ${color}`
  return inset ? `${boxShadow}, inset ${boxShadow}` : boxShadow
}
