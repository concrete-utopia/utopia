/** @jsxRuntime classic */
/** @jsx jsx */

import { jsx } from '@emotion/react'
import React from 'react'
import { cartesianProducts } from '../../core/shared/array-utils'
import { useColorTheme } from '../../uuiui'

type FlexJustifyContent =
  | 'flex-start'
  | 'center'
  | 'flex-end'
  | 'space-around'
  | 'space-between'
  | 'space-evenly'

type FlexAlignment = 'auto' | 'flex-start' | 'center' | 'flex-end' | 'stretch'

const NineBlockSectors = cartesianProducts<FlexAlignment, FlexJustifyContent>(
  ['flex-start', 'center', 'flex-end'],
  ['flex-start', 'center', 'flex-end'],
)

interface SlabsProps {
  flexDirection: 'row' | 'column'
  justifyContent: FlexJustifyContent
  alignItems: FlexAlignment
}

const Slabs = React.memo<SlabsProps>(({ flexDirection, alignItems, justifyContent }) => {
  return (
    <div
      css={{
        display: 'flex',
        flexDirection,
        alignItems,
        justifyContent,
        gap: 1,
        padding: 1,
        width: '100%',
        height: '100%',
        opacity: 0,
        transition: 'opacity 16ms ease-in',
        '&:hover': {
          opacity: 0.5,
        },
      }}
    >
      <div style={{ width: '100%', height: '50%', borderRadius: 2, backgroundColor: 'white' }} />
      <div style={{ width: '100%', height: '65%', borderRadius: 2, backgroundColor: 'white' }} />
      <div style={{ width: '100%', height: '35%', borderRadius: 2, backgroundColor: 'white' }} />
    </div>
  )
})

const DotSize = 3

interface NineBlockControlProps {}

export const NineBlockControl = React.memo<NineBlockControlProps>(() => {
  const colorTheme = useColorTheme()

  return (
    <div
      style={{
        display: 'grid',
        height: 100,
        aspectRatio: '1',
        gridTemplateRows: '1fr 1fr 1fr',
        gridTemplateColumns: '1fr 1fr 1fr',
        boxShadow: '0px 0px 2px 1px rgba(0, 0, 0, 0.5)',
        boxSizing: 'border-box',
      }}
    >
      {NineBlockSectors.map(([alignItems, justifyContent], index) => (
        <div
          key={index}
          style={{
            gridRow: `${Math.floor(index / 3) + 1} / ${Math.floor(index / 3) + 2}`,
            gridColumn: `${(index % 3) + 1} / ${(index % 3) + 2}`,
            backgroundColor: colorTheme.darkPrimary.value,
            boxSizing: 'border-box',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
          }}
        >
          <Slabs flexDirection='row' alignItems={alignItems} justifyContent={justifyContent} />
        </div>
      ))}
    </div>
  )
})
