/** @jsx jsx */
import React from 'react'
import { jsx, css } from '@emotion/react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { useColorTheme } from '../../../uuiui'
import { ControlProps } from './new-canvas-controls'
import { ElementInstanceMetadata, jsxAttributeValue } from '../../../core/shared/element-template'
import { betterReactMemo } from '../../../uuiui-deps'
import {
  clearHighlightedViews,
  selectComponents,
  setHighlightedView,
  setProp_UNSAFE,
} from '../../editor/actions/action-creators'
import { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import { EditorAction, EditorDispatch } from '../../editor/action-types'
import { isZeroSizedElement, ZeroControlSize } from './outline-utils'
import { createLayoutPropertyPath } from '../../../core/layout/layout-helpers-new'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import { ElementPath } from '../../../core/shared/project-file-types'

const EmptyChildren: ElementInstanceMetadata[] = []
export const ZeroSizedElementControls = betterReactMemo(
  'ZeroSizedElementControls',
  (props: ControlProps) => {
    let zeroSizeChildren = EmptyChildren
    if (props.cmdKeyPressed) {
      zeroSizeChildren = props.selectedViews.flatMap((view) => {
        const children = MetadataUtils.getChildren(props.componentMetadata, view)
        return children.filter((child) => {
          return isZeroSizedElement(child?.globalFrame)
        })
      })
    }

    return (
      <React.Fragment>
        {zeroSizeChildren.map((element) => {
          let isHighlighted =
            props.highlightedViews.find((view) => EP.pathsEqual(element.elementPath, view)) != null
          return (
            <ZeroSizeSelectControl
              key={`zero-size-element-${EP.toString(element.elementPath)}`}
              element={element}
              dispatch={props.dispatch}
              canvasOffset={props.canvasOffset}
              scale={props.scale}
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

const ZeroSizeSelectControl = betterReactMemo(
  'ZeroSizeSelectControl',
  (props: ZeroSizeSelectControlProps) => {
    const colorTheme = useColorTheme()
    const { dispatch, element, canvasOffset, scale } = props
    const controlSize = 1 / scale
    const onControlMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        event.stopPropagation()
        dispatch([selectComponents([element.elementPath], false)], 'everyone')
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
          css={{
            boxShadow: `0px 0px 0px ${controlSize}px ${colorTheme.primary.value}`,
            '&:hover': {
              boxShadow: `0px 0px 0px ${controlSize * 2}px ${colorTheme.primary.value}`,
            },
          }}
        />
      )
    }
  },
)

export interface ZeroSizeControlProps {
  frame: CanvasRectangle
  canvasOffset: CanvasPoint
  scale: number
  color: string | null | undefined
}

export const ZeroSizeHighlightControl = betterReactMemo(
  'ZeroSizeHighlightControl',
  (props: ZeroSizeControlProps) => {
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
  },
)

export const ZeroSizeOutlineControl = betterReactMemo(
  'ZeroSizeOutlineControl',
  (props: ZeroSizeControlProps) => {
    const colorTheme = useColorTheme()
    const controlSize = 1 / props.scale

    return (
      <div
        className='role-outline-no-size'
        style={{
          position: 'absolute',
          left: props.frame.x + props.canvasOffset.x - ZeroControlSize / 2,
          top: props.frame.y + props.canvasOffset.y - ZeroControlSize / 2,
          width: props.frame.width + ZeroControlSize,
          height: props.frame.height + ZeroControlSize,
          borderRadius: ZeroControlSize / 2,
          boxShadow: `0px 0px 0px ${controlSize}px ${colorTheme.primary.value}, inset 0px 0px 0px ${controlSize}px ${colorTheme.primary.value}`,
        }}
      />
    )
  },
)

interface ZeroSizeResizeControlProps extends ZeroSizeControlProps {
  element: ElementInstanceMetadata | null
  dispatch: EditorDispatch
}

export const ZeroSizeResizeControl = betterReactMemo(
  'ZeroSizeResizeControl',
  (props: ZeroSizeResizeControlProps) => {
    const { dispatch, element } = props
    const onControlStopPropagation = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        event.stopPropagation()
      },
      [],
    )
    const onControlDoubleClick = React.useCallback(() => {
      let setPropActions: EditorAction[] = []
      if (element != null) {
        if (props.frame.width === 0) {
          setPropActions.push(
            setProp_UNSAFE(
              element.elementPath,
              createLayoutPropertyPath('Width'),
              jsxAttributeValue(100, emptyComments),
            ),
          )
        }
        if (props.frame.height === 0) {
          setPropActions.push(
            setProp_UNSAFE(
              element.elementPath,
              createLayoutPropertyPath('Height'),
              jsxAttributeValue(100, emptyComments),
            ),
          )
        }
        dispatch(setPropActions, 'everyone')
      }
    }, [dispatch, element, props.frame])

    return (
      <div
        onMouseDown={onControlStopPropagation}
        onMouseOver={onControlStopPropagation}
        onDoubleClick={onControlDoubleClick}
        className='role-resize-no-size'
        style={{
          position: 'absolute',
          left: props.frame.x + props.canvasOffset.x - ZeroControlSize / 2,
          top: props.frame.y + props.canvasOffset.y - ZeroControlSize / 2,
          width: props.frame.width + ZeroControlSize,
          height: props.frame.height + ZeroControlSize,
        }}
      />
    )
  },
)
