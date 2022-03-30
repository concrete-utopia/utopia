/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import Tippy from '@tippyjs/react'
import React from 'react'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { getMultiselectBounds } from '../canvas-strategies/shared-absolute-move-strategy-helpers'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

export const FlowMoveControls = React.memo(() => {
  const colorTheme = useColorTheme()
  const frame = useEditorState((store) => {
    return getMultiselectBounds(store.editor.jsxMetadata, store.editor.selectedViews)
  }, 'flowmovecontrols frame')
  if (frame == null) {
    return null
  } else {
    return (
      <CanvasOffsetWrapper>
        <Tippy
          css={{
            fontWeight: 400,
            fontSize: 11,
            fontFamily:
              "utopian-inter, -apple-system, BlinkMacSystemFont, Helvetica, 'Segoe UI', Roboto, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol'",
            backgroundColor: `${colorTheme.neutralInvertedBackground.value} !important`,
            color: `${colorTheme.neutralInvertedForeground.value} !important`,
            '& .tippy-content': {
              padding: '4px 8px !important',
            },
            '&[data-placement^=top] .tippy-arrow::before': {
              borderTopColor: `${colorTheme.neutralInvertedBackground.value} !important`,
            },
            '&[data-placement^=right] .tippy-arrow::before': {
              borderRightColor: `${colorTheme.neutralInvertedBackground.value} !important`,
            },
            '&[data-placement^=bottom] .tippy-arrow::before': {
              borderBottomColor: `${colorTheme.neutralInvertedBackground.value} !important`,
            },
            '&[data-placement^=left] .tippy-arrow::before': {
              borderLeftColor: `${colorTheme.neutralInvertedBackground.value} !important`,
            },
          }}
          visible
          arrow
          content={'Hold Mouse to Convert to Absolute'}
          placement={'top'}
          delay={[100, 100]}
          animation='fade'
          theme='material'
        >
          <div
            style={{
              position: 'absolute',
              top: frame.y,
              left: frame.x,
              width: frame.width,
              height: frame.height,
            }}
          ></div>
        </Tippy>
      </CanvasOffsetWrapper>
    )
  }
})
