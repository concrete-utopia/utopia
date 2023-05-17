/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { useColorTheme } from '../../../uuiui'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  emptyComments,
  isNullJSXAttributeValue,
  jsExpressionValue,
} from '../../../core/shared/element-template'
import {
  clearHighlightedViews,
  setHighlightedView,
  setProp_UNSAFE,
} from '../../editor/actions/action-creators'
import { selectComponents } from '../../editor/actions/meta-actions'
import {
  CanvasPoint,
  CanvasRectangle,
  isInfinityRectangle,
  isFiniteRectangle,
  windowPoint,
  point,
} from '../../../core/shared/math-utils'
import { EditorDispatch } from '../../editor/action-types'
import { isZeroSizedElement, ZeroControlSize } from './outline-utils'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { useMaybeHighlightElement } from './select-mode/select-mode-hooks'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { styleStringInArray } from '../../../utils/common-constants'
import { EditorModes } from '../../editor/editor-modes'
import * as EditorActions from '../../editor/actions/action-creators'
import CanvasActions from '../canvas-actions'
import { Modifier } from '../../../utils/modifiers'
import { useWindowToCanvasCoordinates } from '../dom-lookup-hooks'
import { boundingArea, createInteractionViaMouse } from '../canvas-strategies/interaction-state'
import {
  findMaybeConditionalExpression,
  getConditionalActiveCase,
} from '../../../core/model/conditionals'
export const ZeroSizedControlTestID = 'zero-sized-control'
interface ZeroSizedElementControlProps {
  showAllPossibleElements: boolean
}

function isConditionalSlot(
  path: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
  spyMetadata: ElementInstanceMetadataMap,
): boolean {
  const conditional = findMaybeConditionalExpression(path, jsxMetadata)
  if (conditional == null) {
    return false
  }
  const clause = getConditionalActiveCase(path, conditional, spyMetadata)
  if (clause == null) {
    return false
  }
  const branch = clause == 'true-case' ? conditional.whenTrue : conditional.whenFalse
  return isNullJSXAttributeValue(branch)
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
      Substores.metadata,
      (store) => {
        if (showAllPossibleElements) {
          return Object.values(store.editor.jsxMetadata).filter((element) => {
            if (
              isConditionalSlot(
                element.elementPath,
                store.editor.jsxMetadata,
                store.editor.spyMetadata,
              )
            ) {
              return true
            }
            return (
              element.globalFrame != null &&
              isFiniteRectangle(element.globalFrame) &&
              isZeroSizedElement(element.globalFrame) &&
              MetadataUtils.targetElementSupportsChildren(projectContents, element)
            )
          })
        } else {
          return selectedElements.flatMap((view) => {
            const children = MetadataUtils.getChildrenUnordered(store.editor.jsxMetadata, view)
            return children.filter((child) => {
              if (child.globalFrame == null) {
                return false
              } else {
                return (
                  isFiniteRectangle(child.globalFrame) &&
                  isZeroSizedElement(child.globalFrame) &&
                  MetadataUtils.targetElementSupportsChildren(projectContents, child)
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

interface ZeroSizeSelectControlProps {
  element: ElementInstanceMetadata
  dispatch: EditorDispatch
  canvasOffset: CanvasPoint
  scale: number
  isHighlighted: boolean
}

const ZeroSizeSelectControl = React.memo((props: ZeroSizeSelectControlProps) => {
  const colorTheme = useColorTheme()
  const { dispatch, element, canvasOffset, scale } = props
  const controlSize = 1 / scale

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

  const jsxMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'ZeroSizeSelectControl jsxMetadata',
  )

  const spyMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.spyMetadata,
    'ZeroSizeSelectControl spyMetadata',
  )

  if (element.globalFrame == null || isInfinityRectangle(element.globalFrame)) {
    return null
  } else {
    const frame = element.globalFrame

    const hintColor = isConditionalSlot(element.elementPath, jsxMetadata, spyMetadata)
      ? colorTheme.brandNeonGreen.value
      : colorTheme.primary.value

    return (
      <div
        onMouseDown={onControlMouseDown}
        onMouseOver={onControlMouseOver}
        onMouseOut={onControlMouseOut}
        style={{
          position: 'absolute',
          left: frame.x + canvasOffset.x - ZeroControlSize / 2,
          top: frame.y + canvasOffset.y - ZeroControlSize / 2,
          width: frame.width + ZeroControlSize,
          height: frame.height + ZeroControlSize,
          borderRadius: ZeroControlSize / 2,
        }}
        css={{
          boxShadow: `0px 0px 0px ${controlSize}px ${hintColor}`,
          '&:hover': {
            boxShadow: `0px 0px 0px ${controlSize * 2}px ${hintColor}`,
          },
        }}
      />
    )
  }
})

export interface ZeroSizeControlProps {
  frame: CanvasRectangle
  canvasOffset: CanvasPoint
  scale: number
  color: string | null | undefined
}

export const ZeroSizeHighlightControl = React.memo((props: ZeroSizeControlProps) => {
  const controlSize = (1 / props.scale) * 2
  return (
    <div
      className='role-component-highlight-outline-no-size'
      style={{
        position: 'absolute',
        left: props.frame.x + props.canvasOffset.x - ZeroControlSize / 2,
        top: props.frame.y + props.canvasOffset.y - ZeroControlSize / 2,
        width: props.frame.width + ZeroControlSize,
        height: props.frame.height + ZeroControlSize,
        borderRadius: ZeroControlSize / 2,
        boxShadow: `0px 0px 0px ${controlSize}px ${props.color}`,
      }}
    />
  )
})

export const ZeroSizeOutlineControl = React.memo(
  (props: Omit<ZeroSizeControlProps, 'canvasOffset'>) => {
    const colorTheme = useColorTheme()
    const controlSize = 1 / props.scale

    return (
      <CanvasOffsetWrapper>
        <div
          className='role-outline-no-size'
          style={{
            position: 'absolute',
            left: props.frame.x - ZeroControlSize / 2,
            top: props.frame.y - ZeroControlSize / 2,
            width: props.frame.width + ZeroControlSize,
            height: props.frame.height + ZeroControlSize,
            borderRadius: ZeroControlSize / 2,
            boxShadow: `0px 0px 0px ${controlSize}px ${colorTheme.primary.value}, inset 0px 0px 0px ${controlSize}px ${colorTheme.primary.value}`,
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
    const { maybeHighlightOnHover, maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()
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
              <React.Fragment>
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
    } else {
      let propsToSet: Array<{ path: PropertyPath; value: any }> = []

      const isFlexParent = element.specialSizeMeasurements.parentLayoutSystem === 'flex'
      if (props.frame.width === 0 || element.specialSizeMeasurements.display === 'inline') {
        if (
          isFlexParent &&
          (element.specialSizeMeasurements.parentFlexDirection === 'row' ||
            element.specialSizeMeasurements.parentFlexDirection === 'row-reverse')
        ) {
          propsToSet.push({
            path: stylePropPathMappingFn('flexBasis', styleStringInArray),
            value: 100,
          })
        } else {
          propsToSet.push({
            path: stylePropPathMappingFn('width', styleStringInArray),
            value: 100,
          })
        }
      }
      if (props.frame.height === 0 || element.specialSizeMeasurements.display === 'inline') {
        if (
          isFlexParent &&
          (element.specialSizeMeasurements.parentFlexDirection === 'column' ||
            element.specialSizeMeasurements.parentFlexDirection === 'column-reverse')
        ) {
          propsToSet.push({
            path: stylePropPathMappingFn('flexBasis', styleStringInArray),
            value: 100,
          })
        } else {
          propsToSet.push({
            path: stylePropPathMappingFn('height', styleStringInArray),
            value: 100,
          })
        }
      }
      if (!isFlexParent && element.specialSizeMeasurements.display === 'inline') {
        propsToSet.push({
          path: stylePropPathMappingFn('position', styleStringInArray),
          value: 'absolute',
        })
      }
      const setPropActions = propsToSet.map((prop) => {
        return setProp_UNSAFE(
          element.elementPath,
          prop.path,
          jsExpressionValue(prop.value, emptyComments),
        )
      })
      dispatch([...setPropActions, CanvasActions.clearInteractionSession(false)], 'everyone')
    }
  }, [dispatch, element, props.frame])

  return (
    <CanvasOffsetWrapper>
      <div
        onMouseMove={onControlMouseMove}
        onMouseDown={onControlMouseDown}
        onMouseUp={onControlMouseUp}
        onDoubleClick={onControlDoubleClick}
        className='role-resize-no-size'
        data-testid={ZeroSizedControlTestID}
        style={{
          position: 'absolute',
          left: props.frame.x - ZeroControlSize / 2,
          top: props.frame.y - ZeroControlSize / 2,
          width: props.frame.width + ZeroControlSize,
          height: props.frame.height + ZeroControlSize,
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
