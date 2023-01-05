/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx, css } from '@emotion/react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { useColorTheme } from '../../../uuiui'
import {
  ElementInstanceMetadata,
  emptyComments,
  jsxAttributeValue,
} from '../../../core/shared/element-template'
import {
  clearHighlightedViews,
  setHighlightedView,
  setProp_UNSAFE,
} from '../../editor/actions/action-creators'
import { selectComponents } from '../../editor/actions/meta-actions'
import { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import { EditorDispatch } from '../../editor/action-types'
import { isZeroSizedElement, ZeroControlSize } from './outline-utils'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { useEditorState } from '../../editor/store/store-hook'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { useMaybeHighlightElement } from './select-mode/select-mode-hooks'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'

interface ZeroSizedElementControlProps {
  showAllPossibleElements: boolean
}

export const ZeroSizedElementControls = controlForStrategyMemoized(
  ({ showAllPossibleElements }: ZeroSizedElementControlProps) => {
    const highlightedViews = useEditorState(
      (store) => store.editor.highlightedViews,
      'ZeroSizedElementControls highlightedViews',
    )
    const selectedElements = useEditorState(
      (store) => store.editor.selectedViews,
      'ZeroSizedElementControls selectedElements',
    )
    const canvasOffset = useEditorState(
      (store) => store.editor.canvas.realCanvasOffset,
      'ZeroSizedElementControls canvasOffset',
    )
    const scale = useEditorState(
      (store) => store.editor.canvas.scale,
      'ZeroSizedElementControls scale',
    )
    const dispatch = useEditorState((store) => store.dispatch, 'ZeroSizedElementControls dispatch')

    const zeroSizeElements = useEditorState((store) => {
      if (showAllPossibleElements) {
        return Object.values(store.editor.jsxMetadata).filter((element) => {
          return (
            element.globalFrame != null &&
            isZeroSizedElement(element.globalFrame) &&
            MetadataUtils.targetElementSupportsChildren(store.editor.projectContents, element)
          )
        })
      } else {
        return selectedElements.flatMap((view) => {
          const children = MetadataUtils.getChildren(store.editor.jsxMetadata, view)
          return children.filter((child) => {
            if (child.globalFrame == null) {
              return false
            } else {
              return isZeroSizedElement(child.globalFrame)
            }
          })
        })
      }
    }, 'ZeroSizedElementControls zeroSizeElements')

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
  const onControlMouseDown = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      event.stopPropagation()
      dispatch(selectComponents([element.elementPath], false), 'everyone')
    },
    [dispatch, element.elementPath],
  )

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

  if (element.globalFrame == null) {
    return null
  } else {
    const frame = element.globalFrame
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
    const zeroSizeElements = useEditorState((store) => {
      return mapDropNulls((path) => {
        const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
        const frame = MetadataUtils.getFrameInCanvasCoords(path, store.editor.jsxMetadata)
        if (frame != null && isZeroSizedElement(frame)) {
          return element
        } else {
          return null
        }
      }, targets)
    }, 'ZeroSizeResizeControlWrapper zeroSizeElements')

    const dispatch = useEditorState(
      (store) => store.dispatch,
      'ZeroSizeResizeControlWrapper dispatch',
    )
    const scale = useEditorState(
      (store) => store.editor.canvas.scale,
      'ZeroSizeResizeControlWrapper scale',
    )

    return (
      <React.Fragment>
        {zeroSizeElements.map((element) => {
          if (element.globalFrame != null) {
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
  element: ElementInstanceMetadata | null
  dispatch: EditorDispatch
  maybeClearHighlightsOnHoverEnd: () => void
}

export const ZeroSizeResizeControl = React.memo((props: ZeroSizeResizeControlProps) => {
  const { dispatch, element, maybeClearHighlightsOnHoverEnd } = props

  const onControlStopPropagation = React.useCallback((event: React.MouseEvent<HTMLDivElement>) => {
    event.stopPropagation()
  }, [])

  const onControlMouseMove = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      event.stopPropagation()
      maybeClearHighlightsOnHoverEnd()
    },
    [maybeClearHighlightsOnHoverEnd],
  )

  const onControlDoubleClick = React.useCallback(() => {
    let propsToSet: Array<{ path: PropertyPath; value: any }> = []
    if (element != null) {
      const isFlexParent = element.specialSizeMeasurements.parentLayoutSystem === 'flex'
      if (props.frame.width === 0 || element.specialSizeMeasurements.display === 'inline') {
        if (
          isFlexParent &&
          (element.specialSizeMeasurements.parentFlexDirection === 'row' ||
            element.specialSizeMeasurements.parentFlexDirection === 'row-reverse')
        ) {
          propsToSet.push({
            path: stylePropPathMappingFn('flexBasis', ['style']),
            value: 100,
          })
        } else {
          propsToSet.push({
            path: stylePropPathMappingFn('width', ['style']),
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
            path: stylePropPathMappingFn('flexBasis', ['style']),
            value: 100,
          })
        } else {
          propsToSet.push({
            path: stylePropPathMappingFn('height', ['style']),
            value: 100,
          })
        }
      }
      if (!isFlexParent && element.specialSizeMeasurements.display === 'inline') {
        propsToSet.push({
          path: stylePropPathMappingFn('position', ['style']),
          value: 'absolute',
        })
      }
      const setPropActions = propsToSet.map((prop) => {
        return setProp_UNSAFE(
          element.elementPath,
          prop.path,
          jsxAttributeValue(prop.value, emptyComments),
        )
      })
      dispatch(setPropActions, 'everyone')
    }
  }, [dispatch, element, props.frame])

  return (
    <CanvasOffsetWrapper>
      <div
        onMouseMove={onControlMouseMove}
        onMouseDown={onControlStopPropagation}
        onMouseUp={onControlStopPropagation}
        onDoubleClick={onControlDoubleClick}
        className='role-resize-no-size'
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
