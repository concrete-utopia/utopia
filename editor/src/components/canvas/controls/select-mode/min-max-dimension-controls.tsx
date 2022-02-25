import * as React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { safeIndex } from '../../../../core/shared/array-utils'
import { useEditorState } from '../../../editor/store/store-hook'
import Utils from '../../../../utils/utils'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import { LayoutTargetableProp } from '../../../../core/layout/layout-helpers-new'
import { CanvasRectangle, CanvasVector } from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'

type PositionCalculation = (
  canvasOffset: CanvasVector,
  elementFrame: CanvasRectangle,
  dimensionValue: number,
) => number

function getDimensionPosition(
  canvasOffset: CanvasVector,
  elementMetadata: ElementInstanceMetadata | null,
  styleProp: LayoutTargetableProp,
  positionCalculation: PositionCalculation,
): number | null {
  if (elementMetadata == null || elementMetadata.globalFrame == null) {
    return null
  } else {
    const styleValue = Utils.pathOr(null, ['style', styleProp], elementMetadata.props)
    if (typeof styleValue === 'number') {
      return positionCalculation(canvasOffset, elementMetadata.globalFrame, styleValue)
    } else {
      return null
    }
  }
}

interface DimensionMarkerProps {
  markerKey: string
  left: number | null
  top: number | null
}

const DimensionMarker_ = ({ left, top, markerKey }: DimensionMarkerProps) => {
  if (left == null || top == null) {
    return null
  } else {
    return (
      <div
        key={markerKey}
        style={{
          position: 'absolute',
          left: left,
          top: top,
          height: left != null ? '100%' : 1,
          width: top != null ? '100%' : 1,
          border: 'none',
          borderLeft: left != null ? '1px dashed #f00' : undefined,
          borderTop: top != null ? '1px dashed #f00' : undefined,
          color: '#fff',
          backgroundColor: 'transparent',
        }}
      />
    )
  }
}

DimensionMarker_.displayName = 'DimensionMarker'
const DimensionMarker = React.memo(DimensionMarker_)

const MinMaxDimensionControls_ = () => {
  const canvasOffset = useEditorState(
    (store) => store.editor.canvas.roundedCanvasOffset,
    'MinMaxDimensionControls canvasOffset',
  )

  const targetedSingleElement = useEditorState((store) => {
    if (store.editor.selectedViews.length === 1) {
      return safeIndex(store.editor.selectedViews, 0) ?? null
    } else {
      return null
    }
  }, 'MinMaxDimensionControls targetedSingleElement')

  const jsxMetadata = useEditorState(
    (store) => store.editor.jsxMetadata,
    'MinMaxDimensionControls jsxMetadata',
  )

  const elementMetadata = React.useMemo(() => {
    return MetadataUtils.findElementByElementPath(jsxMetadata, targetedSingleElement)
  }, [jsxMetadata, targetedSingleElement])

  const minWidthPosition = React.useMemo(() => {
    return getDimensionPosition(
      canvasOffset,
      elementMetadata,
      'minWidth',
      (offsetOfCanvas, elementFrame, minWidth) => {
        return offsetOfCanvas.x + elementFrame.x + minWidth
      },
    )
  }, [canvasOffset, elementMetadata])

  const maxWidthPosition = React.useMemo(() => {
    return getDimensionPosition(
      canvasOffset,
      elementMetadata,
      'maxWidth',
      (offsetOfCanvas, elementFrame, maxWidth) => {
        return offsetOfCanvas.x + elementFrame.x + maxWidth
      },
    )
  }, [canvasOffset, elementMetadata])

  const minHeightPosition = React.useMemo(() => {
    return getDimensionPosition(
      canvasOffset,
      elementMetadata,
      'minHeight',
      (offsetOfCanvas, elementFrame, minHeight) => {
        return offsetOfCanvas.y + elementFrame.y + minHeight
      },
    )
  }, [canvasOffset, elementMetadata])

  const maxHeightPosition = React.useMemo(() => {
    return getDimensionPosition(
      canvasOffset,
      elementMetadata,
      'maxHeight',
      (offsetOfCanvas, elementFrame, maxHeight) => {
        return offsetOfCanvas.y + elementFrame.y + maxHeight
      },
    )
  }, [canvasOffset, elementMetadata])

  return (
    <>
      <DimensionMarker top={0} left={minWidthPosition} markerKey={'min-width-marker'} />
      <DimensionMarker top={0} left={maxWidthPosition} markerKey={'max-width-marker'} />
      <DimensionMarker top={minHeightPosition} left={0} markerKey={'min-height-marker'} />
      <DimensionMarker top={maxHeightPosition} left={0} markerKey={'max-height-marker'} />
    </>
  )
}

MinMaxDimensionControls_.displayName = 'MinMaxDimensionControls'
export const MinMaxDimensionControls = React.memo(MinMaxDimensionControls_)
