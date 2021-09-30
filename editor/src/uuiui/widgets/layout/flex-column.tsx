import styled from '@emotion/styled'
import { commonSenseUtopiaLayoutShorthands } from './common-layout-shorthands'
import { flexColumnStyle } from '../../styles/layout-styles'
import React from 'react'

/**
 * **Flexbox Column, with horizontally centered content**
 * - Uses alignItems: 'center' to center things vertically, override if req'd
 * - justifyContent (main axis, across): flex-start, center, flex-end, space-between, space-around, space-evenly
 * - align-items: (cross axis, down) flex-start | flex-end | center | baseline | stretch
 */

export const FlexColumn = styled.div([commonSenseUtopiaLayoutShorthands, { ...flexColumnStyle }])
FlexColumn.displayName = 'FlexColumn'

export const SimpleFlexColumn = (props: JSX.IntrinsicElements['div']) => {
  const mergedProps = React.useMemo(() => {
    return {
      ...props,
      style: {
        ...flexColumnStyle,
        ...props.style,
      },
    }
  }, [props])
  return <div {...mergedProps} />
}
SimpleFlexColumn.displayName = 'SimpleFlexColumn'
