import React from 'react'
import { styled } from '@stitches/react'
import { createSelector } from 'reselect'
import { cartesianProduct } from '../../core/shared/array-utils'
import { size, Size } from '../../core/shared/math-utils'
import { useColorTheme } from '../../uuiui'
import { useDispatch } from '../editor/store/dispatch-context'
import { EditorStorePatched } from '../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { FlexDirection } from './common/css-utils'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import {
  DefaultFlexDirection,
  detectFlexAlignJustifyContent,
  detectFlexDirection,
  FlexAlignment,
  FlexJustifyContent,
  isFlexColumn,
  justifyContentAlignItemsEquals,
  JustifyContentFlexAlignemt,
  numberOfFlexContainers,
  StartCenterEnd,
} from './inspector-common'
import { setFlexAlignJustifyContentStrategies } from './inspector-strategies/inspector-strategies'
import { runFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'

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

const justifyAlignSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectFlexAlignJustifyContent,
)

const flexDirectionSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectFlexDirection,
)

const isSelectedSelector = createSelector(
  justifyAlignSelector,
  flexDirectionSelector,
  (_: EditorStorePatched, x: JustifyContentFlexAlignemt) => x,
  (detectedJustifyContentFlexAlignment, flexDirection, fixedJustifyContentFlexAlignment) =>
    detectedJustifyContentFlexAlignment != null &&
    justifyContentAlignItemsEquals(
      flexDirection,
      fixedJustifyContentFlexAlignment,
      detectedJustifyContentFlexAlignment,
    ),
)

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
  '&:hover': {
    opacity: 0,
  },
})

interface NineBlockControlCellProps {
  bgColor: string
  fgColor: string
  alignItems: StartCenterEnd
  justifyContent: StartCenterEnd
  onClick: () => void
}

const NineBlockControlCell = React.memo<NineBlockControlCellProps>((props) => {
  const { bgColor, fgColor, alignItems, justifyContent, onClick } = props

  const flexDirection = useEditorState(flexDirectionSelector, 'FlexDirectionToggle flexDirection')

  const alignItemsJustifyContent = React.useMemo(
    () => ({ alignItems, justifyContent }),
    [alignItems, justifyContent],
  )

  const isSelected = useEditorState(
    (store) => isSelectedSelector(store, alignItemsJustifyContent),
    'NineBlockControlCell isSelected',
  )

  return (
    <div
      onClick={onClick}
      data-testid={NineBlockTestId(alignItems, justifyContent)}
      style={{
        display: 'flex',
        padding: 1,
        alignItems: 'center',
        position: 'relative',
        boxSizing: 'border-box',
        justifyContent: 'center',
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
          opacity: isSelected ? 0 : 1,
        }}
      >
        <div
          style={{
            backgroundColor: fgColor,
            width: DotSize,
            height: DotSize,
            borderRadius: DotSize / 2,
          }}
        />
      </DotContainer>
    </div>
  )
})

const numberOfFlexContainersSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  numberOfFlexContainers,
)

export const NineBlockControl = React.memo(() => {
  const colorTheme = useColorTheme()

  const dispatch = useDispatch()

  const nFlexContainers = useEditorState(
    numberOfFlexContainersSelector,
    'FlexDirectionToggle, nFlexContainers',
  )

  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const flexDirectionRef = useRefEditorState(flexDirectionSelector)

  const setAlignItemsJustifyContent = React.useCallback(
    (intendedFlexAlignment: StartCenterEnd, intendedJustifyContent: StartCenterEnd) => {
      const strategies = isFlexColumn(flexDirectionRef.current ?? DefaultFlexDirection)
        ? setFlexAlignJustifyContentStrategies(intendedJustifyContent, intendedFlexAlignment)
        : setFlexAlignJustifyContentStrategies(intendedFlexAlignment, intendedJustifyContent)
      runFirstApplicableStrategy(
        dispatch,
        metadataRef.current,
        selectedViewsRef.current,
        strategies,
      )
    },
    [dispatch, flexDirectionRef, metadataRef, selectedViewsRef],
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

  if (nFlexContainers === 0) {
    return null
  }

  return (
    <div
      style={{
        margin: 2,
        height: 100,
        display: 'grid',
        aspectRatio: '1',
        boxSizing: 'border-box',
        gridTemplateRows: '1fr 1fr 1fr',
        gridTemplateColumns: '1fr 1fr 1fr',
        backgroundColor: colorTheme.bg0.value,
        border: `1px solid ${colorTheme.fg5.value}`,
      }}
    >
      <NineBlockControlCell
        onClick={callbacks['flex-start-flex-start']}
        bgColor={colorTheme.bg0.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'flex-start'}
        justifyContent={'flex-start'}
      />
      <NineBlockControlCell
        onClick={callbacks['flex-start-center']}
        bgColor={colorTheme.bg0.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'flex-start'}
        justifyContent={'center'}
      />
      <NineBlockControlCell
        onClick={callbacks['flex-start-flex-end']}
        bgColor={colorTheme.bg0.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'flex-start'}
        justifyContent={'flex-end'}
      />
      <NineBlockControlCell
        onClick={callbacks['center-flex-start']}
        bgColor={colorTheme.bg0.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'center'}
        justifyContent={'flex-start'}
      />
      <NineBlockControlCell
        onClick={callbacks['center-center']}
        bgColor={colorTheme.bg0.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'center'}
        justifyContent={'center'}
      />
      <NineBlockControlCell
        onClick={callbacks['center-flex-end']}
        bgColor={colorTheme.bg0.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'center'}
        justifyContent={'flex-end'}
      />
      <NineBlockControlCell
        onClick={callbacks['flex-end-flex-start']}
        bgColor={colorTheme.bg0.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'flex-end'}
        justifyContent={'flex-start'}
      />
      <NineBlockControlCell
        onClick={callbacks['flex-end-center']}
        bgColor={colorTheme.bg0.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'flex-end'}
        justifyContent={'center'}
      />
      <NineBlockControlCell
        onClick={callbacks['flex-end-flex-end']}
        bgColor={colorTheme.bg0.value}
        fgColor={colorTheme.fg0.value}
        alignItems={'flex-end'}
        justifyContent={'flex-end'}
      />
    </div>
  )
})
