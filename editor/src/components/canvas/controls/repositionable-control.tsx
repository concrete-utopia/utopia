import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import Utils from '../../../utils/utils'
import * as EP from '../../../core/shared/element-path'
import { ControlProps } from './new-canvas-controls'
import { getSelectionColor } from './outline-control'
import { betterReactMemo } from '../../../uuiui-deps'
import { useColorTheme } from '../../../uuiui'

const Size = 6

export const RepositionableControl = betterReactMemo(
  'RepositionableControl',
  (props: ControlProps) => {
    const colorTheme = useColorTheme()

    const outlineOffset = 0.5 / props.scale

    let indicators: JSX.Element[] = []
    Utils.fastForEach(props.selectedViews, (selectedView) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(selectedView, props.componentMetadata)
      if (frame == null) {
        return
      }

      const selectionColor = getSelectionColor(
        selectedView,
        props.componentMetadata,
        props.focusedElementPath,
        colorTheme,
      )

      indicators.push(
        <div
          key={EP.toComponentId(selectedView)}
          className='role-outline'
          style={{
            position: 'absolute',
            left: props.canvasOffset.x + frame.x - Size / 2 + outlineOffset,
            top: props.canvasOffset.y + frame.y - Size / 2 + outlineOffset,
            borderRadius: '50%',
            width: Size,
            height: Size,
            backgroundColor: selectionColor,
            pointerEvents: 'none',
          }}
        />,
      )
    })

    return <>{indicators}</>
  },
)
