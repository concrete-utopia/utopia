import React from 'react'
import { Dot } from './inspector-common-components'
import { styled } from '@stitches/react'
import {
  flexDirectionSelector,
  metadataSelector,
  numberOfFlexContainersSelector,
  packedFlexSettingSelector,
  selectedViewsSelector,
} from './inpector-selectors'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { UtopiaTheme, colorTheme } from '../../uuiui'
import { useDispatch } from '../editor/store/dispatch-context'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import { setFlexAlignStrategies } from './inspector-strategies/inspector-strategies'
import type { FlexDirection } from './common/css-utils'
import type { FlexAlignment } from './inspector-common'
import { detectFlexAlignJustifyContent, detectFlexDirection } from './inspector-common'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import type { ElementPath } from '../../core/shared/project-file-types'
import { createSelector } from 'reselect'
import type { MetadataSubstate } from '../editor/store/store-hook-substore-types'
import createCachedSelector from 're-reselect'

export const ThreeBarControlTestId = (alignItems: FlexAlignment): string =>
  `ThreeBarControlStartTestId-${alignItems}`

const ThreeBarContainerWidth = 100
const SlabHoverOpacity = 0.3

function SlabSide(side: 'width' | 'height', variety: 'short' | 'long') {
  const SlabHeightN = 10
  const LongSlabWidthN = 20
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
  margin: 2,
  display: 'flex',
  aspectRatio: '1',
  width: ThreeBarContainerWidth,
  boxSizing: 'border-box',
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
  margin: 3,
})

const DotLayer = styled('div', {
  position: 'absolute',
  height: '100%',
  width: '100%',
  display: 'flex',
  flexDirection: 'column',
  opacity: 1,
  backgroundColor: colorTheme.bg1.value,
  '&:hover': {
    opacity: 0,
  },
})

const SlabLayer = styled('div', {
  position: 'absolute',
  height: '100%',
  width: '100%',
  display: 'flex',
  padding: 1,
  flexDirection: 'column',
})

const Section = styled('div', {
  flexGrow: 1,
  contain: 'layout',
  cursor: 'pointer',
})

const DotSize = 2

const DefaultFlexAlignment: FlexAlignment = 'flex-start'

function SlabOpacity(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  alignItems: FlexAlignment,
): number {
  return (detectFlexAlignJustifyContent(metadata, selectedViews)?.alignItems ??
    DefaultFlexAlignment) === alignItems
    ? 1
    : SlabHoverOpacity
}

function DotOpacity(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  alignItems: FlexAlignment,
): number | undefined {
  return (detectFlexAlignJustifyContent(metadata, selectedViews)?.alignItems ??
    DefaultFlexAlignment) === alignItems
    ? 0
    : undefined
}

function layerFlexDirection(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
): FlexDirection {
  return LayerFlexDirection(detectFlexDirection(metadata, selectedViews))
}

const slabOpacitySelector = createCachedSelector(
  metadataSelector,
  selectedViewsSelector,
  (_: MetadataSubstate, x: FlexAlignment) => x,
  SlabOpacity,
)((_, x) => x)

const dotOpacitySelector = createCachedSelector(
  metadataSelector,
  selectedViewsSelector,
  (_: MetadataSubstate, x: FlexAlignment) => x,
  DotOpacity,
)((_, x) => x)

const layerFlexDirectionSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  layerFlexDirection,
)

interface ThreeBarSectionProps {
  alignItems: FlexAlignment
  onClick: () => void
}

const ThreeBarSection = React.memo<ThreeBarSectionProps>(({ alignItems, onClick }) => {
  const slabOpacity = useEditorState(
    Substores.metadata,
    (store) => slabOpacitySelector(store, alignItems),
    'ThreeBarControl justifyContentAlignItems',
  )

  const dotOpacity = useEditorState(
    Substores.metadata,
    (store) => dotOpacitySelector(store, alignItems),
    '',
  )

  const layerFlexDirectionValue = useEditorState(
    Substores.metadata,
    layerFlexDirectionSelector,
    'ThreeBarControl flexDirection',
  )

  const dotColor = colorTheme.fg7.value
  const slabColor = colorTheme.fg0.value

  const shortSlabHeight = SlabHeight(layerFlexDirectionValue, 'short')
  const shortSlabWidth = SlabWidth(layerFlexDirectionValue, 'short')
  const longSlabHeight = SlabHeight(layerFlexDirectionValue, 'long')
  const longSlabWidth = SlabWidth(layerFlexDirectionValue, 'long')

  return (
    <Section data-testid={ThreeBarControlTestId(alignItems)} onClick={onClick}>
      <SlabLayer
        style={{
          justifyContent: 'space-around',
          alignItems: alignItems,
          opacity: slabOpacity,
          flexDirection: layerFlexDirectionValue,
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
          opacity: dotOpacity,
          flexDirection: layerFlexDirectionValue,
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
  )
})

export const ThreeBarControl = React.memo(() => {
  const flexDirection = useEditorState(
    Substores.metadata,
    flexDirectionSelector,
    'ThreeBarControl flexDirection',
  )

  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const dispatch = useDispatch()

  const nFlexContainers = useEditorState(
    Substores.metadata,
    numberOfFlexContainersSelector,
    'FlexDirectionToggle, nFlexContainers',
  )

  const packedSpacedSetting =
    useEditorState(
      Substores.metadata,
      packedFlexSettingSelector,
      'FlexSection packedFlexSetting',
    ) ?? 'packed'

  const setAlignItemsStart = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        setFlexAlignStrategies(metadataRef.current, selectedViewsRef.current, 'flex-start'),
      ),
    [dispatch, metadataRef, selectedViewsRef],
  )

  const setAlignItemsCenter = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        setFlexAlignStrategies(metadataRef.current, selectedViewsRef.current, 'center'),
      ),
    [dispatch, metadataRef, selectedViewsRef],
  )

  const setAlignItemsEnd = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        setFlexAlignStrategies(metadataRef.current, selectedViewsRef.current, 'flex-end'),
      ),
    [dispatch, metadataRef, selectedViewsRef],
  )

  const shouldShow = nFlexContainers > 0 && packedSpacedSetting === 'spaced'

  return (
    <ThreeBarContainer
      style={{
        display: shouldShow ? 'flex' : 'none',
        flexDirection: ContainerFlexDirection(flexDirection),
        border: `1px solid ${colorTheme.fg8.value}`,
        borderRadius: UtopiaTheme.inputBorderRadius,
        overflow: 'hidden',
      }}
    >
      <ThreeBarSection alignItems={'flex-start'} onClick={setAlignItemsStart} />
      <ThreeBarSection alignItems={'center'} onClick={setAlignItemsCenter} />
      <ThreeBarSection alignItems={'flex-end'} onClick={setAlignItemsEnd} />
    </ThreeBarContainer>
  )
})
