import React from 'react'
import { Dot } from './inspector-common-components'
import { styled } from '@stitches/react'
import { justifyAlignSelector, metadataSelector, selectedViewsSelector } from './inpector-selectors'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { useColorTheme } from '../../uuiui'
import { useDispatch } from '../editor/store/dispatch-context'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import { setFlexAlignStrategies } from './inspector-strategies/inspector-strategies'

const ThreeBarContainerWidth = 70
const SlabHeight = 7
const LongSlabWidth = 20
const ShortSlabWidth = 12
const SlabHoverOpacity = 0.3

const ThreeBarContainer = styled('div', {
  display: 'flex',
  aspectRatio: '1',
  width: ThreeBarContainerWidth,
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
  height: SlabHeight,
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

const DotSize = 2

export const ThreeBarControl = React.memo(() => {
  const justifyContentAlignItems = useEditorState(
    Substores.metadata,
    justifyAlignSelector,
    'ThreeBarControl justifyContentAlignItems',
  )

  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const dotColor = colorTheme.fg0.value
  const slabColor = colorTheme.fg0.value

  const setAlignItemsStart = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        metadataRef.current,
        selectedViewsRef.current,
        setFlexAlignStrategies('flex-start'),
      ),
    [dispatch, metadataRef, selectedViewsRef],
  )

  const setAlignItemsCenter = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        metadataRef.current,
        selectedViewsRef.current,
        setFlexAlignStrategies('center'),
      ),
    [dispatch, metadataRef, selectedViewsRef],
  )

  const setAlignItemsEnd = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        metadataRef.current,
        selectedViewsRef.current,
        setFlexAlignStrategies('flex-end'),
      ),
    [dispatch, metadataRef, selectedViewsRef],
  )
  return (
    <ThreeBarContainer style={{ flexDirection: 'column' }}>
      <div
        onClick={setAlignItemsStart}
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
            opacity: justifyContentAlignItems?.alignItems === 'flex-start' ? 1 : SlabHoverOpacity,
            flexDirection: 'row',
          }}
        >
          <Slab style={{ width: ShortSlabWidth, backgroundColor: slabColor }} />
          <Slab style={{ width: LongSlabWidth, backgroundColor: slabColor }} />
          <Slab style={{ width: ShortSlabWidth, backgroundColor: slabColor }} />
        </SlabLayer>
        <DotLayer
          style={{
            opacity: justifyContentAlignItems?.alignItems === 'flex-start' ? 0 : undefined,
            flexDirection: 'row',
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
        onClick={setAlignItemsCenter}
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
            opacity: justifyContentAlignItems?.alignItems === 'center' ? 1 : SlabHoverOpacity,
            flexDirection: 'row',
          }}
        >
          <Slab style={{ width: ShortSlabWidth, backgroundColor: slabColor }} />
          <Slab style={{ width: LongSlabWidth, backgroundColor: slabColor }} />
          <Slab style={{ width: ShortSlabWidth, backgroundColor: slabColor }} />
        </SlabLayer>
        <DotLayer
          style={{
            opacity: justifyContentAlignItems?.alignItems === 'center' ? 0 : undefined,
            flexDirection: 'row',
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
        onClick={setAlignItemsEnd}
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
            opacity: justifyContentAlignItems?.alignItems === 'flex-end' ? 1 : SlabHoverOpacity,
            flexDirection: 'row',
          }}
        >
          <Slab style={{ width: 8, height: ShortSlabWidth, backgroundColor: slabColor }} />
          <Slab style={{ width: 8, height: LongSlabWidth, backgroundColor: slabColor }} />
          <Slab style={{ width: 8, height: ShortSlabWidth, backgroundColor: slabColor }} />
        </SlabLayer>
        <DotLayer
          style={{
            opacity: justifyContentAlignItems?.alignItems === 'flex-end' ? 0 : undefined,
            flexDirection: 'row',
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
