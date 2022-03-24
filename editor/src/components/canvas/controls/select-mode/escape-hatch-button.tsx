import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { Button, Icn, Tooltip, useColorTheme } from '../../../../uuiui'
import { convertSelectionToAbsolute } from '../../../editor/actions/action-creators'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { getMultiselectBounds } from '../../canvas-strategies/shared-absolute-move-strategy-helpers'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

export const EscapeHatchButtonControl = React.memo(() => {
  const notAbsoluteSelectedElements = useEditorState((store) => {
    return (
      store.editor.selectedViews.length > 0 &&
      store.editor.selectedViews.every((path) => {
        return (
          MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
            ?.specialSizeMeasurements.position !== 'absolute'
        )
      })
    )
  }, 'EscapeHatchButtonControl notAbsoluteSelectedElements')

  if (notAbsoluteSelectedElements) {
    return (
      <CanvasOffsetWrapper>
        <ConversionButton />
      </CanvasOffsetWrapper>
    )
  } else {
    return null
  }
})

const ConversionButton = React.memo(() => {
  const boundingBox = useEditorState(
    (store) => getMultiselectBounds(store.editor.jsxMetadata, store.editor.selectedViews),
    'ConversionButton frames',
  )
  const dispatch = useRefEditorState((store) => store.dispatch)
  const colorTheme = useColorTheme()
  const buttonMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      e.stopPropagation()
      dispatch.current([convertSelectionToAbsolute()], 'everyone')
    },
    [dispatch],
  )
  if (boundingBox != null) {
    return (
      <Tooltip title={'Convert to Absolute Position'}>
        <Button
          style={{
            position: 'absolute',
            left: boundingBox.x - 30,
            top: boundingBox.y - 5,
            borderRadius: 2,
            width: 20,
            height: 20,
            boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor.o(20).value} 0px 0px 1px, ${
              colorTheme.canvasControlsSizeBoxShadowColor.o(21).value
            } 0px 1px 2px 1px`,
            backgroundColor: colorTheme.inspectorBackground.value,
          }}
          onMouseDown={buttonMouseDown}
        >
          <Icn color='main' category='layout/systems' type='pins' />
        </Button>
      </Tooltip>
    )
  } else {
    return null
  }
})
