import React from 'react'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import type { EdgePosition } from '../../canvas-types'
import { CSSCursor } from '../../canvas-types'
import { SmallElementSize, useBoundingBox } from '../bounding-box-hooks'
import { ResizeEdge } from './resize-edge'

const RESIZE_MOUSE_AREA_SIZE = 10

export function useResizeEdges(
  targets: Array<ElementPath>,
  params: {
    onEdgeMouseDown: (e: React.MouseEvent<HTMLDivElement>, position: EdgePosition) => void
    onEdgeMouseMove: (e: React.MouseEvent<HTMLDivElement>) => void
    onEdgeDoubleClick: (
      e: React.MouseEvent<HTMLDivElement>,
      direction: 'horizontal' | 'vertical',
    ) => void
    cursors?: {
      top?: CSSCursor
      left?: CSSCursor
      bottom?: CSSCursor
      right?: CSSCursor
    }
  },
) {
  const scale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'useResizeEdges scale',
  )

  const topRef = useBoundingBox(targets, (ref, boundingBox) => {
    const isSmallElement = shouldUseSmallElementResizeControl(boundingBox.height, scale)
    const lineSize = RESIZE_MOUSE_AREA_SIZE / scale
    const height = isSmallElement ? lineSize / 2 : lineSize
    const offsetLeft = `0px`
    const offsetTop = `${-lineSize / 2}px`

    ref.current.style.width = boundingBox.width + 'px'
    ref.current.style.height = height + 'px'
    ref.current.style.transform = `translate(${offsetLeft}, ${offsetTop})`
  })

  const leftRef = useBoundingBox(targets, (ref, boundingBox) => {
    const isSmallElement = shouldUseSmallElementResizeControl(boundingBox.width, scale)
    const lineSize = RESIZE_MOUSE_AREA_SIZE / scale
    const width = isSmallElement ? lineSize / 2 : lineSize
    const offsetLeft = `${-lineSize / 2}px`
    const offsetTop = `0px`

    ref.current.style.width = `${width}px`
    ref.current.style.transform = `translate(${offsetLeft}, ${offsetTop})`
    ref.current.style.height = boundingBox.height + 'px'
  })

  const bottomRef = useBoundingBox(targets, (ref, boundingBox) => {
    const isSmallElement = shouldUseSmallElementResizeControl(boundingBox.height, scale)
    const lineSize = RESIZE_MOUSE_AREA_SIZE / scale
    const height = isSmallElement ? lineSize / 2 : lineSize
    const offsetLeft = `0px`
    const offsetTop = isSmallElement ? `0px` : `${-lineSize / 2}px`

    ref.current.style.transform = `translate(${offsetLeft}, ${offsetTop})`
    ref.current.style.top = boundingBox.height + 'px'
    ref.current.style.width = boundingBox.width + 'px'
    ref.current.style.height = height + 'px'
  })

  const rightRef = useBoundingBox(targets, (ref, boundingBox) => {
    const isSmallElement = shouldUseSmallElementResizeControl(boundingBox.width, scale)
    const lineSize = RESIZE_MOUSE_AREA_SIZE / scale
    const width = isSmallElement ? lineSize / 2 : lineSize
    const offsetLeft = isSmallElement ? `0px` : `${-lineSize / 2}px`
    const offsetTop = `0px`

    ref.current.style.transform = `translate(${offsetLeft}, ${offsetTop})`
    ref.current.style.left = boundingBox.width + 'px'
    ref.current.style.width = width + 'px'
    ref.current.style.height = boundingBox.height + 'px'
  })

  const topOnMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      params.onEdgeMouseDown(e, { x: 0.5, y: 0 })
    },
    [params],
  )

  const leftOnMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      params.onEdgeMouseDown(e, { x: 0, y: 0.5 })
    },
    [params],
  )

  const bottomOnMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      params.onEdgeMouseDown(e, { x: 0.5, y: 1 })
    },
    [params],
  )

  const rightOnMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      params.onEdgeMouseDown(e, { x: 1, y: 0.5 })
    },
    [params],
  )

  const topOnDoubleClick = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      params.onEdgeDoubleClick(e, 'horizontal')
    },
    [params],
  )

  const leftOnDoubleClick = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      params.onEdgeDoubleClick(e, 'vertical')
    },
    [params],
  )

  const bottomOnDoubleClick = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      params.onEdgeDoubleClick(e, 'horizontal')
    },
    [params],
  )

  const rightOnDoubleClick = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      params.onEdgeDoubleClick(e, 'vertical')
    },
    [params],
  )

  return {
    top: (
      <ResizeEdge
        ref={topRef}
        position={{ x: 0.5, y: 0 }}
        cursor={params.cursors?.top ?? CSSCursor.ResizeNS}
        direction='horizontal'
        onMouseDown={topOnMouseDown}
        onMouseMove={params.onEdgeMouseMove}
        onDoubleClick={topOnDoubleClick}
      />
    ),
    left: (
      <ResizeEdge
        ref={leftRef}
        position={{ x: 0, y: 0.5 }}
        cursor={params.cursors?.left ?? CSSCursor.ResizeEW}
        direction='vertical'
        onMouseDown={leftOnMouseDown}
        onMouseMove={params.onEdgeMouseMove}
        onDoubleClick={leftOnDoubleClick}
      />
    ),
    bottom: (
      <ResizeEdge
        ref={bottomRef}
        position={{ x: 0.5, y: 1 }}
        cursor={params.cursors?.bottom ?? CSSCursor.ResizeNS}
        direction='horizontal'
        onMouseDown={bottomOnMouseDown}
        onMouseMove={params.onEdgeMouseMove}
        onDoubleClick={bottomOnDoubleClick}
      />
    ),
    right: (
      <ResizeEdge
        ref={rightRef}
        position={{ x: 1, y: 0.5 }}
        cursor={params.cursors?.right ?? CSSCursor.ResizeEW}
        direction='vertical'
        onMouseDown={rightOnMouseDown}
        onMouseMove={params.onEdgeMouseMove}
        onDoubleClick={rightOnDoubleClick}
      />
    ),
  }
}

function shouldUseSmallElementResizeControl(size: number, scale: number): boolean {
  return size <= SmallElementSize / scale
}
