import React from 'react'
import { Dot } from './inspector-common-components'
import { styled } from '@stitches/react'
import {
  flexDirectionSelector,
  justifyAlignSelector,
  metadataSelector,
  selectedViewsSelector,
} from './inpector-selectors'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { useColorTheme } from '../../uuiui'
import { useDispatch } from '../editor/store/dispatch-context'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import { setFlexAlignStrategies } from './inspector-strategies/inspector-strategies'
import { FlexDirection } from './common/css-utils'
import { FlexAlignment } from './inspector-common'

export const ThreeBarControlTestId = (alignItems: FlexAlignment): string =>
  `ThreeBarControlStartTestId-${alignItems}`

const ThreeBarContainerWidth = 100
const SlabHoverOpacity = 0.3

function SlabSide(side: 'width' | 'height', variety: 'short' | 'long') {
  const SlabHeightN = 10
  const LongSlabWidthN = 30
  const ShortSlabWidthN = LongSlabWidthN / 1.618
  switch (side) {
    case 'width':
      return variety === 'long' ? LongSlabWidthN : ShortSlabWidthN
    case 'height':
      return SlabHeightN
  }
}

function SlabWidth(flexDirection: FlexDirection, variety: 'short' | 'long'): number {
  return SlabSide(flexDirection.startsWith('col') ? 'width' : 'height', variety)
}

function SlabHeight(flexDirection: FlexDirection, variety: 'short' | 'long'): number {
  return SlabSide(flexDirection.startsWith('row') ? 'width' : 'height', variety)
}

const ContainerFlexDirection = (flexDirection: FlexDirection): FlexDirection => {
  if (flexDirection.startsWith('row')) {
    return 'column'
  }
  return 'row'
}

const LayerFlexDirection = (flexDirection: FlexDirection): FlexDirection => {
  if (flexDirection.startsWith('row')) {
    return 'row'
  }
  return 'column'
}

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
  height: '100%',
  flexGrow: 1,
})

const Slab = styled('div', {
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

const Section = styled('div', {
  flexGrow: 1,
  contain: 'layout',
  cursor: 'pointer',
})

const DotSize = 2

export const ThreeBarControl = React.memo(() => {
  const justifyContentAlignItems = useEditorState(
    Substores.metadata,
    justifyAlignSelector,
    'ThreeBarControl justifyContentAlignItems',
  )

  const flexDirection = useEditorState(
    Substores.metadata,
    flexDirectionSelector,
    'ThreeBarControl flexDirection',
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

  const shortSlabHeight = SlabHeight(flexDirection, 'short')
  const shortSlabWidth = SlabWidth(flexDirection, 'short')
  const longSlabHeight = SlabHeight(flexDirection, 'long')
  const longSlabWidth = SlabWidth(flexDirection, 'long')

  return (
    <ThreeBarContainer style={{ flexDirection: ContainerFlexDirection(flexDirection) }}>
      <Section data-testid={ThreeBarControlTestId('flex-start')} onClick={setAlignItemsStart}>
        <SlabLayer
          style={{
            justifyContent: 'space-around',
            alignItems: 'flex-start',
            opacity: justifyContentAlignItems?.alignItems === 'flex-start' ? 1 : SlabHoverOpacity,
            flexDirection: LayerFlexDirection(flexDirection),
          }}
        >
          <Slab
            style={{ width: shortSlabWidth, height: shortSlabHeight, backgroundColor: slabColor }}
          />
          <Slab
            style={{ width: longSlabWidth, height: longSlabHeight, backgroundColor: slabColor }}
          />
          <Slab
            style={{ width: shortSlabWidth, height: shortSlabHeight, backgroundColor: slabColor }}
          />
        </SlabLayer>
        <DotLayer
          style={{
            opacity: justifyContentAlignItems?.alignItems === 'flex-start' ? 0 : undefined,
            flexDirection: LayerFlexDirection(flexDirection),
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
      </Section>
      {/* ----------------------------------------- */}
      <Section data-testid={ThreeBarControlTestId('center')} onClick={setAlignItemsCenter}>
        <SlabLayer
          style={{
            justifyContent: 'space-around',
            alignItems: 'center',
            opacity: justifyContentAlignItems?.alignItems === 'center' ? 1 : SlabHoverOpacity,
            flexDirection: LayerFlexDirection(flexDirection),
          }}
        >
          <Slab
            style={{ width: shortSlabWidth, height: shortSlabHeight, backgroundColor: slabColor }}
          />
          <Slab
            style={{ width: longSlabWidth, height: longSlabHeight, backgroundColor: slabColor }}
          />
          <Slab
            style={{ width: shortSlabWidth, height: shortSlabHeight, backgroundColor: slabColor }}
          />
        </SlabLayer>
        <DotLayer
          style={{
            opacity: justifyContentAlignItems?.alignItems === 'center' ? 0 : undefined,
            flexDirection: LayerFlexDirection(flexDirection),
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
      </Section>
      {/* ----------------------------------------- */}
      <Section data-testid={ThreeBarControlTestId('flex-end')} onClick={setAlignItemsEnd}>
        <SlabLayer
          style={{
            justifyContent: 'space-around',
            alignItems: 'flex-end',
            opacity: justifyContentAlignItems?.alignItems === 'flex-end' ? 1 : SlabHoverOpacity,
            flexDirection: LayerFlexDirection(flexDirection),
          }}
        >
          <Slab
            style={{ width: shortSlabWidth, height: shortSlabHeight, backgroundColor: slabColor }}
          />
          <Slab
            style={{ width: longSlabWidth, height: longSlabHeight, backgroundColor: slabColor }}
          />
          <Slab
            style={{ width: shortSlabWidth, height: shortSlabHeight, backgroundColor: slabColor }}
          />
        </SlabLayer>
        <DotLayer
          style={{
            opacity: justifyContentAlignItems?.alignItems === 'flex-end' ? 0 : undefined,
            flexDirection: LayerFlexDirection(flexDirection),
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
      </Section>
    </ThreeBarContainer>
  )
})
