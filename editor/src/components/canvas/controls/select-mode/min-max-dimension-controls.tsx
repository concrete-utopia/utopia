import * as React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { safeIndex } from '../../../../core/shared/array-utils'
import { useEditorState } from '../../../editor/store/store-hook'
import Utils from '../../../../utils/utils'

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

  const minWidth: number | null = React.useMemo(() => {
    if (elementMetadata == null) {
      return null
    } else {
      const styleValue = Utils.pathOr(null, ['style', 'minWidth'], elementMetadata.props)
      if (typeof styleValue === 'number') {
        return styleValue
      } else {
        return null
      }
    }
  }, [elementMetadata])

  const minWidthPosition = React.useMemo(() => {
    if (minWidth == null || elementMetadata == null || elementMetadata.globalFrame == null) {
      return null
    } else {
      return canvasOffset.x + elementMetadata.globalFrame.x + minWidth
    }
  }, [canvasOffset, elementMetadata, minWidth])

  // TODO: Add in other bounds.

  return (
    <>
      ( minWidthPosition == null ? null :
      <div
        key={`min-width-marker`}
        style={{
          position: 'absolute',
          left: minWidthPosition ?? undefined,
          top: 0,
          width: 1,
          height: '100%',
          border: 'none',
          borderLeft: '1px dashed #f00',
          color: '#fff',
          backgroundColor: 'transparent',
        }}
      />
      )
    </>
  )
}

MinMaxDimensionControls_.displayName = 'MinMaxDimensionControls'
export const MinMaxDimensionControls = React.memo(MinMaxDimensionControls_)
