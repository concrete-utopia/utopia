import React from 'react'
import { Dot } from './inspector-common-components'
import { styled } from '@stitches/react'
import { createSelector } from 'reselect'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import { detectFlexAlignJustifyContent } from './inspector-common'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { useColorTheme } from '../../uuiui'

const ThreeBarContainer = styled('div', {
  display: 'flex',
  aspectRatio: '1',
  width: 100,
  border: '1px solid black',
  padding: 2,
})

const DotContainer = styled('div', {
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'center',
  width: '100%',
  flexGrow: 1,
})

const Slab = styled('div', {
  height: 8,
  backgroundColor: 'black',
  borderRadius: 2,
})

const DotLayer = styled('div', {
  position: 'absolute',
  height: '100%',
  width: '100%',
  display: 'flex',
  flexDirection: 'column',
  opacity: 1,
  backgroundColor: 'white',
  '&:hover': {
    opacity: 0,
  },
})

const SlabLayer = styled('div', {
  position: 'absolute',
  height: '100%',
  width: '100%',
  display: 'flex',
  flexDirection: 'column',
})

const DotSize = 4

const justifyAlignSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectFlexAlignJustifyContent,
)

export const ThreeBarControl = React.memo(() => {
  const justifyContentAlignItems = useEditorState(
    Substores.metadata,
    justifyAlignSelector,
    'ThreeBarControl justifyContentAlignItems',
  )

  const colorTheme = useColorTheme()

  const dotColor = colorTheme.fg0.value
  const slabColor = colorTheme.fg0.value

  return (
    <ThreeBarContainer>
      <div
        style={{
          flexGrow: 1,
          height: '100%',
          contain: 'layout',
          cursor: 'pointer',
          display: 'flex',
        }}
      >
        <SlabLayer
          style={{
            justifyContent: 'space-around',
            alignItems: 'flex-start',
            opacity: justifyContentAlignItems?.alignItems === 'flex-start' ? 1 : 0.5,
          }}
        >
          <Slab style={{ width: 20, backgroundColor: slabColor }} />
          <Slab style={{ width: 30, backgroundColor: slabColor }} />
          <Slab style={{ width: 20, backgroundColor: slabColor }} />
        </SlabLayer>
        <DotLayer
          style={{
            opacity: justifyContentAlignItems?.alignItems === 'flex-start' ? 0 : undefined,
          }}
        >
          <DotContainer>
            <Dot size={DotSize} bgColor={dotColor} />
          </DotContainer>
          <DotContainer>
            <Dot size={DotSize} bgColor={dotColor} />
          </DotContainer>
          <DotContainer>
            <Dot size={DotSize} bgColor={dotColor} />
          </DotContainer>
        </DotLayer>
      </div>
      {/* ----------------------------------------- */}
      <div
        style={{
          flexGrow: 1,
          height: '100%',
          contain: 'layout',
          cursor: 'pointer',
        }}
      >
        <SlabLayer
          style={{
            justifyContent: 'space-around',
            alignItems: 'center',
            opacity: justifyContentAlignItems?.alignItems === 'center' ? 1 : 0.5,
          }}
        >
          <Slab style={{ width: 20, backgroundColor: slabColor }} />
          <Slab style={{ width: 30, backgroundColor: slabColor }} />
          <Slab style={{ width: 20, backgroundColor: slabColor }} />
        </SlabLayer>
        <DotLayer
          style={{
            opacity: justifyContentAlignItems?.alignItems === 'center' ? 0 : undefined,
          }}
        >
          <DotContainer>
            <Dot size={DotSize} bgColor={dotColor} />
          </DotContainer>
          <DotContainer>
            <Dot size={DotSize} bgColor={dotColor} />
          </DotContainer>
          <DotContainer>
            <Dot size={DotSize} bgColor={dotColor} />
          </DotContainer>
        </DotLayer>
      </div>
      {/* ----------------------------------------- */}
      <div
        style={{
          flexGrow: 1,
          height: '100%',
          contain: 'layout',
          cursor: 'pointer',
        }}
      >
        <SlabLayer
          style={{
            justifyContent: 'space-around',
            alignItems: 'flex-end',
            opacity: justifyContentAlignItems?.alignItems === 'flex-end' ? 1 : 0.5,
          }}
        >
          <Slab style={{ width: 20, backgroundColor: slabColor }} />
          <Slab style={{ width: 30, backgroundColor: slabColor }} />
          <Slab style={{ width: 20, backgroundColor: slabColor }} />
        </SlabLayer>
        <DotLayer
          style={{
            opacity: justifyContentAlignItems?.alignItems === 'flex-end' ? 0 : undefined,
          }}
        >
          <DotContainer>
            <Dot size={DotSize} bgColor={dotColor} />
          </DotContainer>
          <DotContainer>
            <Dot size={DotSize} bgColor={dotColor} />
          </DotContainer>
          <DotContainer>
            <Dot size={DotSize} bgColor={dotColor} />
          </DotContainer>
        </DotLayer>
      </div>
    </ThreeBarContainer>
  )
})
