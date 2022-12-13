/** @jsxRuntime classic */
/** @jsx jsx */

import { jsx } from '@emotion/react'
import React from 'react'
import { cartesianProduct } from '../../core/shared/array-utils'
import { size, Size } from '../../core/shared/math-utils'
import { useColorTheme } from '../../uuiui'
import { useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { FlexDirection } from './common/css-utils'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import {
  DefaultFlexDirection,
  detectFlexAlignJustifyContent,
  filterKeepFlexContainers,
  isFlexColumn,
  justifyContentAlignItemsEquals,
  StartCenterEnd,
} from './inspector-common'
import { runStrategies, setFlexAlignJustifyContentStrategies } from './inspector-strategies'

const NineBlockSectors = cartesianProduct<StartCenterEnd, StartCenterEnd>(
  ['flex-start', 'center', 'flex-end'],
  ['flex-start', 'center', 'flex-end'],
)

const slabSize = (desiredSize: Size, flexDirection: FlexDirection): Size => {
  if (isFlexColumn(flexDirection)) {
    return size(desiredSize.height, desiredSize.height)
  }
  return desiredSize
}

const slabAlignment = (
  justifyContent: StartCenterEnd,
  alignItems: StartCenterEnd,
  flexDirection: FlexDirection,
): { justifyContent: StartCenterEnd; alignItems: StartCenterEnd } => {
  if (isFlexColumn(flexDirection)) {
    return { justifyContent: alignItems, alignItems: justifyContent }
  }
  return { justifyContent, alignItems }
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
      css={{
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

interface NineBlockControlProps {
  flexDirection: FlexDirection | null
}

export const NineBlockControl = React.memo<NineBlockControlProps>(({ flexDirection }) => {
  const colorTheme = useColorTheme()

  const dispatch = useEditorState((store) => store.dispatch, 'NineBlockControl dispatch')
  const detectedJustifyContentFlexAlignment = useEditorState(
    (store) => detectFlexAlignJustifyContent(metadataSelector(store), selectedViewsSelector(store)),
    'NineBlockControl [flexJustifyContent, flexAlignment]',
  )

  const nFlexContainers = useEditorState(
    (store) =>
      filterKeepFlexContainers(metadataSelector(store), selectedViewsSelector(store)).length,
    'FlexDirectionToggle, nFlexContainers',
  )

  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const flexDirectionWithDefault: FlexDirection = flexDirection ?? DefaultFlexDirection

  const setAlignItemsJustifyContent = React.useCallback(
    (intendedFlexAlignment: StartCenterEnd, intendedJustifyContent: StartCenterEnd) => {
      const strategies = isFlexColumn(flexDirectionWithDefault)
        ? setFlexAlignJustifyContentStrategies(intendedJustifyContent, intendedFlexAlignment)
        : setFlexAlignJustifyContentStrategies(intendedFlexAlignment, intendedJustifyContent)
      runStrategies(dispatch, metadataRef.current, selectedViewsRef.current, strategies)
    },
    [dispatch, flexDirectionWithDefault, metadataRef, selectedViewsRef],
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
      {NineBlockSectors.map(([alignItems, justifyContent], index) => {
        const isSelected =
          detectedJustifyContentFlexAlignment != null &&
          justifyContentAlignItemsEquals(
            flexDirectionWithDefault,
            { alignItems, justifyContent },
            detectedJustifyContentFlexAlignment,
          )
        return (
          <div
            onClick={() => setAlignItemsJustifyContent(alignItems, justifyContent)}
            key={`${alignItems}-${justifyContent}`}
            style={{
              display: 'flex',
              padding: 1,
              alignItems: 'center',
              position: 'relative',
              boxSizing: 'border-box',
              justifyContent: 'center',
              gridColumn: `${(index % 3) + 1} / ${(index % 3) + 2}`,
              gridRow: `${Math.floor(index / 3) + 1} / ${Math.floor(index / 3) + 2}`,
            }}
          >
            <div
              css={{
                position: 'absolute',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                width: '100%',
                height: '100%',
                backgroundColor: colorTheme.bg0.value,
                opacity: isSelected ? 1 : 0.5,
              }}
            >
              <Slabs
                {...slabAlignment(justifyContent, alignItems, flexDirectionWithDefault)}
                flexDirection={flexDirectionWithDefault}
                bgColor={colorTheme.fg5.value}
              />
            </div>
            <div
              css={{
                position: 'absolute',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                backgroundColor: colorTheme.bg0.value,
                width: '100%',
                height: '100%',
                opacity: isSelected ? 0 : 1,
                '&:hover': {
                  opacity: 0,
                },
              }}
            >
              <div
                css={{
                  backgroundColor: colorTheme.fg5.value,
                  width: DotSize,
                  height: DotSize,
                  borderRadius: DotSize / 2,
                }}
              />
            </div>
          </div>
        )
      })}
    </div>
  )
})
