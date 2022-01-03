/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import { FlexRow } from '../layout/flex-row'
import React from 'react'

export const ActionSheet = (props: any) => {
  const stopProp = React.useCallback((e: React.SyntheticEvent) => {
    e.stopPropagation()
  }, [])

  return (
    <FlexRow
      style={{
        label: 'actionsheet',
        position: 'absolute',
        right: '0px',
        top: '0px',
        bottom: '0px',
      }}
      css={{
        '& > div': {
          marginRight: '4px',
        },
      }}
      className='actionsheet'
      {...props}
      onMouseDown={stopProp}
      onMouseUp={stopProp}
      onClick={stopProp}
    />
  )
}
