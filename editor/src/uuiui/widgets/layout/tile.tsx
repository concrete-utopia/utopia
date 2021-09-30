import styled from '@emotion/styled'
import { commonSenseUtopiaLayoutShorthands } from './common-layout-shorthands'
import { tileStyle } from '../../styles/layout-styles'
import React from 'react'

/**
 * **Tile, Flex container centering content horizontally and vertically**
 */

export const Tile = styled.div([commonSenseUtopiaLayoutShorthands, { ...tileStyle }])
Tile.displayName = 'Tile'

export const SimpleTile = (props: JSX.IntrinsicElements['div']) => {
  const mergedProps = React.useMemo(() => {
    return {
      ...props,
      style: {
        ...tileStyle,
        ...props.style,
      },
    }
  }, [props])
  return <div {...mergedProps} />
}
SimpleTile.displayName = 'SimpleTile'
