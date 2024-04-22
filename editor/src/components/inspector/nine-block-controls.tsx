import React from 'react'
import { createSelector } from 'reselect'
import { cartesianProduct } from '../../core/shared/array-utils'
import type { Size } from '../../core/shared/math-utils'
import { size } from '../../core/shared/math-utils'
import { UtopiaTheme, colorTheme } from '../../uuiui'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import type { FlexDirection } from './common/css-utils'
import {
  justifyAlignSelector,
  metadataSelector,
  numberOfFlexContainersSelector,
  packedFlexSettingSelector,
  selectedViewsSelector,
} from './inpector-selectors'
import type {
  FlexAlignment,
  FlexJustifyContent,
  JustifyContentFlexAlignemt,
  StartCenterEnd,
} from './inspector-common'
import {
  DefaultFlexDirection,
  detectFlexDirection,
  isFlexColumn,
  justifyContentAlignItemsEquals,
} from './inspector-common'
import { setFlexAlignJustifyContentStrategies } from './inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import type { MetadataSubstate } from '../editor/store/store-hook-substore-types'
import { Dot } from './inspector-common-components'
import { styled } from '@stitches/react'
import { useSetHoveredControlsHandlers } from '../canvas/controls/select-mode/select-mode-hooks'
import type { SubduedPaddingControlProps } from '../canvas/controls/select-mode/subdued-padding-control'
import { SubduedPaddingControl } from '../canvas/controls/select-mode/subdued-padding-control'
import { EdgePieces } from '../canvas/padding-utils'
import type { CanvasControlWithProps } from './common/inspector-atoms'
import createCachedSelector from 're-reselect'

export const NineBlockControlTestId = 'NineBlockControlTestId'

export const NineBlockTestId = (
  alignItems: FlexAlignment,
  justifyContent: FlexJustifyContent,
): string => `NineBlockTestId-${alignItems}-${justifyContent}`

type NineKey = `${StartCenterEnd}-${StartCenterEnd}`

export const NineBlockSectors = cartesianProduct<StartCenterEnd, StartCenterEnd>(
  ['flex-start', 'center', 'flex-end'],
  ['flex-start', 'center', 'flex-end'],
)

const slabSize = (desiredSize: Size, flexDirection: FlexDirection): Size => {
  if (isFlexColumn(flexDirection)) {
    return size(desiredSize.height, desiredSize.width)
  }
  return desiredSize
}

const sizeCSSPercent = ({ width, height }: Size): React.CSSProperties => ({
  width: `${width}%`,
  height: `${height}%`,
})

interface SlabsProps {
  flexDirection: FlexDirection
  justifyContent: StartCenterEnd
  alignItems: StartCenterEnd
  bgColor: string
}

const Slabs = React.memo<SlabsProps>(({ flexDirection, alignItems, justifyContent, bgColor }) => {
  return (
    <div
      style={{
        display: 'flex',
        gap: 1,
        alignItems: alignItems,
        flexDirection: flexDirection,
        justifyContent: justifyContent,
        padding: 1,
        width: '100%',
        height: '100%',
      }}
    >
      <div
        style={{
          ...sizeCSSPercent(slabSize(size(100, 50), flexDirection)),
          borderRadius: 2,
          backgroundColor: bgColor,
        }}
      />
      <div
        style={{
          ...sizeCSSPercent(slabSize(size(100, 65), flexDirection)),
          borderRadius: 2,
          backgroundColor: bgColor,
        }}
      />
      <div
        style={{
          ...sizeCSSPercent(slabSize(size(100, 35), flexDirection)),
          borderRadius: 2,
          backgroundColor: bgColor,
        }}
      />
    </div>
  )
})

const DotSize = 2

const flexDirectionSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectFlexDirection,
)

const DefaultJustifyContentFlexAlignment: JustifyContentFlexAlignemt = {
  justifyContent: 'flex-start',
  alignItems: 'flex-start',
}

const isSelectedSelector = createCachedSelector(
  justifyAlignSelector,
  flexDirectionSelector,
  (_: MetadataSubstate, x: JustifyContentFlexAlignemt) => x,
  (detectedJustifyContentFlexAlignment, flexDirection, fixedJustifyContentFlexAlignment) => {
    return justifyContentAlignItemsEquals(
      flexDirection,
      fixedJustifyContentFlexAlignment,
      detectedJustifyContentFlexAlignment ?? DefaultJustifyContentFlexAlignment,
    )
  },
)((_, x) => `${x.alignItems}_${x.justifyContent}`)

const opacity = (isSelected: boolean, flexDirectionMatches: boolean): number => {
  if (!flexDirectionMatches) {
    return 0
  }
  if (!isSelected) {
    return 0.5
  }
  return 1
}

const DotContainer = styled('div', {
  position: 'absolute',
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'center',
  width: '100%',
  height: '100%',
  opacity: 1,
  '&:hover': {
    opacity: 0,
  },
})

interface NineBlockControlCellProps {
  bgColor: string
  fgColor: string
  alignItems: StartCenterEnd
  justifyContent: StartCenterEnd
  onSelect: () => void
}

const NineBlockControlCell = React.memo<NineBlockControlCellProps>((props) => {
  const { bgColor, fgColor, alignItems, justifyContent, onSelect } = props

  const flexDirection = useEditorState(
    Substores.metadata,
    flexDirectionSelector,
    'FlexDirectionToggle flexDirection',
  )

  const alignItemsJustifyContent = React.useMemo(
    () => ({ alignItems, justifyContent }),
    [alignItems, justifyContent],
  )

  const isSelected = useEditorState(
    Substores.metadata,
    (store) => isSelectedSelector(store, alignItemsJustifyContent),
    'NineBlockControlCell isSelected',
  )

  return (
    <div
      onMouseDown={onSelect}
      data-testid={NineBlockTestId(alignItems, justifyContent)}
      style={{
        display: 'flex',
        margin: 3,
        alignItems: 'center',
        position: 'relative',
        boxSizing: 'border-box',
        justifyContent: 'center',
        cursor: 'pointer',
      }}
    >
      <div
        style={{
          position: 'absolute',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          width: '100%',
          height: '100%',
          opacity: opacity(isSelected, flexDirection === 'row'),
        }}
      >
        <Slabs
          justifyContent={justifyContent}
          alignItems={alignItems}
          flexDirection={'row'}
          bgColor={fgColor}
        />
      </div>
      <div
        style={{
          position: 'absolute',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          width: '100%',
          height: '100%',
          opacity: opacity(isSelected, flexDirection === 'column'),
        }}
      >
        <Slabs
          justifyContent={alignItems}
          alignItems={justifyContent}
          flexDirection={'column'}
          bgColor={fgColor}
        />
      </div>
      <DotContainer
        style={{
          backgroundColor: bgColor,
          opacity: isSelected ? 0 : undefined,
        }}
      >
        <Dot bgColor={colorTheme.fg7.value} size={DotSize} />
      </DotContainer>
    </div>
  )
})

export const NineBlockControl = React.memo(() => {
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

  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const flexDirectionRef = useRefEditorState(flexDirectionSelector)

  const setAlignItemsJustifyContent = React.useCallback(
    (intendedFlexAlignment: StartCenterEnd, intendedJustifyContent: StartCenterEnd) => {
      const strategies = isFlexColumn(flexDirectionRef.current ?? DefaultFlexDirection)
        ? setFlexAlignJustifyContentStrategies(
            metadataRef.current,
            selectedViewsRef.current,
            intendedJustifyContent,
            intendedFlexAlignment,
          )
        : setFlexAlignJustifyContentStrategies(
            metadataRef.current,
            selectedViewsRef.current,
            intendedFlexAlignment,
            intendedJustifyContent,
          )
      executeFirstApplicableStrategy(dispatch, strategies)
    },
    [dispatch, flexDirectionRef, metadataRef, selectedViewsRef],
  )

  const paddingControlsForHover: Array<CanvasControlWithProps<SubduedPaddingControlProps>> =
    React.useMemo(
      () =>
        EdgePieces.map((side) => ({
          control: SubduedPaddingControl,
          props: {
            side: side,
            hoveredOrFocused: 'hovered',
          },
          key: `subdued-padding-control-hovered-${side}`,
        })),
      [],
    )

  const { onMouseEnter, onMouseLeave } = useSetHoveredControlsHandlers<SubduedPaddingControlProps>()
  const onMouseEnterWithPaddingControls = React.useCallback(
    () => onMouseEnter(paddingControlsForHover),
    [onMouseEnter, paddingControlsForHover],
  )

  const callbacks: {
    [key in NineKey]: () => void
  } = React.useMemo(
    () => ({
      'flex-start-flex-start': () => setAlignItemsJustifyContent('flex-start', 'flex-start'),
      'flex-start-center': () => setAlignItemsJustifyContent('flex-start', 'center'),
      'flex-start-flex-end': () => setAlignItemsJustifyContent('flex-start', 'flex-end'),
      'center-flex-start': () => setAlignItemsJustifyContent('center', 'flex-start'),
      'center-center': () => setAlignItemsJustifyContent('center', 'center'),
      'center-flex-end': () => setAlignItemsJustifyContent('center', 'flex-end'),
      'flex-end-flex-start': () => setAlignItemsJustifyContent('flex-end', 'flex-start'),
      'flex-end-center': () => setAlignItemsJustifyContent('flex-end', 'center'),
      'flex-end-flex-end': () => setAlignItemsJustifyContent('flex-end', 'flex-end'),
    }),
    [setAlignItemsJustifyContent],
  )

  const shouldShow = nFlexContainers > 0 && packedSpacedSetting === 'packed'

  return (
    <div
      data-testid={NineBlockControlTestId}
      onMouseEnter={onMouseEnterWithPaddingControls}
      onMouseLeave={onMouseLeave}
      style={{
        height: 100,
        display: shouldShow ? 'grid' : 'none',
        aspectRatio: '1',
        boxSizing: 'border-box',
        gridTemplateRows: '1fr 1fr 1fr',
        gridTemplateColumns: '1fr 1fr 1fr',
        backgroundColor: colorTheme.bg1.value,
        border: `1px solid ${colorTheme.fg8.value}`,
        borderRadius: UtopiaTheme.inputBorderRadius,
        overflow: 'hidden',
      }}
    >
      <NineBlockControlCell
        onSelect={callbacks['flex-start-flex-start']}
        bgColor={colorTheme.bg1.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'flex-start'}
        justifyContent={'flex-start'}
      />
      <NineBlockControlCell
        onSelect={callbacks['flex-start-center']}
        bgColor={colorTheme.bg1.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'flex-start'}
        justifyContent={'center'}
      />
      <NineBlockControlCell
        onSelect={callbacks['flex-start-flex-end']}
        bgColor={colorTheme.bg1.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'flex-start'}
        justifyContent={'flex-end'}
      />
      <NineBlockControlCell
        onSelect={callbacks['center-flex-start']}
        bgColor={colorTheme.bg1.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'center'}
        justifyContent={'flex-start'}
      />
      <NineBlockControlCell
        onSelect={callbacks['center-center']}
        bgColor={colorTheme.bg1.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'center'}
        justifyContent={'center'}
      />
      <NineBlockControlCell
        onSelect={callbacks['center-flex-end']}
        bgColor={colorTheme.bg1.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'center'}
        justifyContent={'flex-end'}
      />
      <NineBlockControlCell
        onSelect={callbacks['flex-end-flex-start']}
        bgColor={colorTheme.bg1.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'flex-end'}
        justifyContent={'flex-start'}
      />
      <NineBlockControlCell
        onSelect={callbacks['flex-end-center']}
        bgColor={colorTheme.bg1.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'flex-end'}
        justifyContent={'center'}
      />
      <NineBlockControlCell
        onSelect={callbacks['flex-end-flex-end']}
        bgColor={colorTheme.bg1.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'flex-end'}
        justifyContent={'flex-end'}
      />
    </div>
  )
})
