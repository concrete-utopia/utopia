import React from 'react'
import { Dot } from './inspector-common-components'
import { styled } from '@stitches/react'
import { createSelector } from 'reselect'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import { detectFlexAlignJustifyContent } from './inspector-common'
import { Substores, useEditorState } from '../editor/store/store-hook'

const ThreeBarContainer = styled('div', {
  display: 'flex',
  aspectRatio: '1',
  width: 100,
  border: '1px solid black',
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

const Layer = styled('div', {
  position: 'absolute',
  height: '100%',
  width: '100%',
  display: 'flex',
  flexDirection: 'column',
})

const SlabLayer = styled('div', {
  position: 'absolute',
  height: '100%',
  width: '100%',
  display: 'flex',
  flexDirection: 'column',
  opacity: 0,
  '&:hover': {
    opacity: 0.5,
  },
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
        <Layer>
          <DotContainer>
            <Dot size={DotSize} bgColor='black' />
          </DotContainer>
          <DotContainer>
            <Dot size={DotSize} bgColor='black' />
          </DotContainer>
          <DotContainer>
            <Dot size={DotSize} bgColor='black' />
          </DotContainer>
        </Layer>
        <SlabLayer
          style={{
            justifyContent: 'space-around',
            alignItems: 'flex-start',
            opacity: justifyContentAlignItems?.alignItems === 'flex-start' ? 1 : undefined,
          }}
        >
          <Slab style={{ width: 20 }} />
          <Slab style={{ width: 30 }} />
          <Slab style={{ width: 20 }} />
        </SlabLayer>
      </div>
      <div
        style={{
          flexGrow: 1,
          height: '100%',
          contain: 'layout',
          cursor: 'pointer',
        }}
      >
        <Layer>
          <DotContainer>
            <Dot size={DotSize} bgColor='black' />
          </DotContainer>
          <DotContainer>
            <Dot size={DotSize} bgColor='black' />
          </DotContainer>
          <DotContainer>
            <Dot size={DotSize} bgColor='black' />
          </DotContainer>
        </Layer>
        <SlabLayer
          style={{
            justifyContent: 'space-around',
            alignItems: 'center',
            opacity: justifyContentAlignItems?.alignItems === 'center' ? 1 : undefined,
          }}
        >
          <Slab style={{ width: 20 }} />
          <Slab style={{ width: 30 }} />
          <Slab style={{ width: 20 }} />
        </SlabLayer>
      </div>
      <div
        style={{
          flexGrow: 1,
          height: '100%',
          contain: 'layout',
          cursor: 'pointer',
        }}
      >
        <Layer>
          <DotContainer>
            <Dot size={DotSize} bgColor='black' />
          </DotContainer>
          <DotContainer>
            <Dot size={DotSize} bgColor='black' />
          </DotContainer>
          <DotContainer>
            <Dot size={DotSize} bgColor='black' />
          </DotContainer>
        </Layer>
        <SlabLayer
          style={{
            justifyContent: 'space-around',
            alignItems: 'flex-end',
            opacity: justifyContentAlignItems?.alignItems === 'flex-end' ? 1 : undefined,
          }}
        >
          <Slab style={{ width: 20 }} />
          <Slab style={{ width: 30 }} />
          <Slab style={{ width: 20 }} />
        </SlabLayer>
      </div>
    </ThreeBarContainer>
  )
})
