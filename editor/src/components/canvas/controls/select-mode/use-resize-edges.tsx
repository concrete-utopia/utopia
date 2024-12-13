import React from 'react'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import type { EdgePosition } from '../../canvas-types'
import { CSSCursor } from '../../canvas-types'
import { SmallElementSize, useBoundingBox } from '../bounding-box-hooks'
import { ResizeEdge } from './resize-edge'

const RESIZE_MOUSE_AREA_SIZE = 10

export function useResizeEdges(
  targets: ElementPath[],
  params: {
    onEdgeMouseDown: (position: EdgePosition) => (e: React.MouseEvent<HTMLDivElement>) => void
    onEdgeMouseMove: (e: React.MouseEvent<HTMLDivElement>) => void
    onEdgeDoubleClick: (
      direction: 'horizontal' | 'vertical',
    ) => (e: React.MouseEvent<HTMLDivElement>) => void
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

  return {
    top: (
      <ResizeEdge
        ref={topRef}
        position={{ x: 0.5, y: 0 }}
        cursor={params.cursors?.top ?? CSSCursor.ResizeNS}
        direction='horizontal'
        onMouseDown={params.onEdgeMouseDown({ x: 0.5, y: 0 })}
        onMouseMove={params.onEdgeMouseMove}
        onDoubleClick={params.onEdgeDoubleClick('horizontal')}
      />
    ),
    left: (
      <ResizeEdge
        ref={leftRef}
        position={{ x: 0, y: 0.5 }}
        cursor={params.cursors?.left ?? CSSCursor.ResizeEW}
        direction='vertical'
        onMouseDown={params.onEdgeMouseDown({ x: 0, y: 0.5 })}
        onMouseMove={params.onEdgeMouseMove}
        onDoubleClick={params.onEdgeDoubleClick('vertical')}
      />
    ),
    bottom: (
      <ResizeEdge
        ref={bottomRef}
        position={{ x: 0.5, y: 1 }}
        cursor={params.cursors?.bottom ?? CSSCursor.ResizeNS}
        direction='horizontal'
        onMouseDown={params.onEdgeMouseDown({ x: 0.5, y: 1 })}
        onMouseMove={params.onEdgeMouseMove}
        onDoubleClick={params.onEdgeDoubleClick('horizontal')}
      />
    ),
    right: (
      <ResizeEdge
        ref={rightRef}
        position={{ x: 1, y: 0.5 }}
        cursor={params.cursors?.right ?? CSSCursor.ResizeEW}
        direction='vertical'
        onMouseDown={params.onEdgeMouseDown({ x: 1, y: 0.5 })}
        onMouseMove={params.onEdgeMouseMove}
        onDoubleClick={params.onEdgeDoubleClick('vertical')}
      />
    ),
  }
}

function shouldUseSmallElementResizeControl(size: number, scale: number): boolean {
  return size <= SmallElementSize / scale
}
