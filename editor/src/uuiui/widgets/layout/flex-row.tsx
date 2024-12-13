import styled from '@emotion/styled'
import { commonSenseUtopiaLayoutShorthands } from './common-layout-shorthands'
import { flexRowStyle } from '../../styles/layout-styles'
import React from 'react'

/**
 * **Flexbox Row, with vertically centered content**
 * - Uses alignItems: 'center' to center things vertically, override if req'd
 * - justifyContent (main axis, across): flex-start, center, flex-end, space-between, space-around, space-evenly
 * - align-items: (cross axis, down) flex-start | flex-end | center | baseline | stretch
 */

export const FlexRow = styled.div([commonSenseUtopiaLayoutShorthands, { ...flexRowStyle }])
FlexRow.displayName = 'FlexRow'

export const SimpleFlexRow = React.forwardRef<HTMLDivElement, JSX.IntrinsicElements['div']>(
  (props, ref) => {
    const mergedProps = React.useMemo(() => {
      return {
        ...props,
        style: {
          ...flexRowStyle,
          ...props.style,
        },
      }
    }, [props])
    return <div ref={ref} {...mergedProps} />
  },
)
SimpleFlexRow.displayName = 'SimpleFlexRow'
