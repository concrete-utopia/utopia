import React from 'react'
import { FlexDirection, FlexWrap } from 'utopia-api'
import { createLayoutPropertyPath } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { jsxAttributeValue } from '../../../core/shared/element-template'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import { when } from '../../../utils/react-conditionals'
import { Icn, IcnProps, useColorTheme } from '../../../uuiui'
import { InlineLink } from '../../../uuiui/inline-button'
import { setProp_UNSAFE } from '../../editor/actions/action-creators'
import { useEditorState } from '../../editor/store/store-hook'
import { flexDirectionOptions } from '../../inspector/sections/layout-section/flex-container-subsection/flex-container-controls'

const getFlexDirectionIcon = (
  flexWrap: FlexWrap | null,
  flexDirection: FlexDirection | null,
): IcnProps | null => {
  if (flexWrap == null) {
    return null
  }
  return (
    flexDirectionOptions(flexWrap).find((option) => option.value === flexDirection)?.icon ?? null
  )
}

export const LayoutParentControl = (): JSX.Element | null => {
  const colorTheme = useColorTheme()

  const dispatch = useEditorState((store) => store.dispatch, 'LayoutParentControl dispatch')

  const { canvasOffset, scale } = useEditorState((store) => {
    return {
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
      scale: store.editor.canvas.scale,
    }
  }, 'LayoutParentControl canvas')
  const { parentTarget, parentLayout, parentFrame, flexWrap, flexDirection } = useEditorState(
    (store) => {
      if (store.editor.selectedViews.length !== 1) {
        return {
          parentTarget: null,
          parentLayout: null,
          parentFrame: null,
          flexWrap: null,
          flexDirection: null,
        }
      }
      const element = MetadataUtils.getParent(
        store.editor.jsxMetadata,
        store.editor.selectedViews[0],
      )
      return {
        parentTarget: element?.elementPath,
        parentLayout: element?.specialSizeMeasurements.layoutSystemForChildren,
        parentFrame: element?.globalFrame,
        flexWrap: element?.props?.style?.flexWrap ?? 'nowrap',
        flexDirection: element?.props?.style?.flexDirection ?? 'row',
      }
    },
    'LayoutParentControl',
  )

  const flexDirectionIcon = getFlexDirectionIcon(flexWrap, flexDirection)
  const flexDirectionClicked = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      if (parentTarget != null) {
        const newValue = flexDirection === 'row' ? 'column' : 'row'
        dispatch(
          [
            setProp_UNSAFE(
              parentTarget,
              createLayoutPropertyPath('flexDirection'),
              jsxAttributeValue(newValue, emptyComments),
            ),
          ],
          'canvas',
        )
      }
    },
    [parentTarget, flexDirection, dispatch],
  )

  const onControlMouseDown = React.useCallback((event: React.MouseEvent<HTMLDivElement>) => {
    event.nativeEvent.stopPropagation()
    event.nativeEvent.stopImmediatePropagation()
    event.stopPropagation()
  }, [])

  if (parentFrame == null) {
    return null
  }

  return (
    <div
      style={{
        borderRadius: 5,
        boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor.o(50).value} 0px 0px ${
          1 / scale
        }px, ${colorTheme.canvasControlsSizeBoxShadowColor.o(21).value} 0px ${1 / scale}px ${
          2 / scale
        }px ${1 / scale}px`,
        height: 25,
        backgroundColor: '#f5f5f5',
        position: 'absolute',
        left: parentFrame.x + canvasOffset.x,
        top: parentFrame.y + canvasOffset.y - 25 - 8,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'flex-start',
        textTransform: 'capitalize',
      }}
      onMouseDown={onControlMouseDown}
    >
      <InlineLink style={{ padding: '0 10px' }}>{parentLayout}</InlineLink>
      {when(
        flexDirectionIcon != null && parentLayout === 'flex',
        <div
          style={{
            padding: '0 5px',
          }}
          onClick={flexDirectionClicked}
        >
          <Icn {...(flexDirectionIcon as IcnProps)} />
        </div>,
      )}
    </div>
  )
}
