/** @jsxRuntime classic */
/** @jsx jsx */

import { jsx } from '@emotion/react'
import React from 'react'
import { cartesianProduct } from '../../core/shared/array-utils'
import { useColorTheme } from '../../uuiui'
import { useEditorState } from '../editor/store/store-hook'
import {
  detectFlexAlignJustifyContent,
  filterKeepFlexContainers,
  FlexAlignment,
  FlexJustifyContent,
} from './inspector-common'
import { runStrategies, setFlexAlignJustifyContentStrategies } from './inspector-strategies'

const NineBlockSectors = cartesianProduct<FlexAlignment, FlexJustifyContent>(
  ['flex-start', 'center', 'flex-end'],
  ['flex-start', 'center', 'flex-end'],
)

interface SlabsProps {
  flexDirection: 'row' | 'column'
  justifyContent: FlexJustifyContent
  alignItems: FlexAlignment
  bgColor: string
}

const Slabs = React.memo<SlabsProps>(({ flexDirection, alignItems, justifyContent, bgColor }) => {
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
      }}
    >
      <div style={{ width: '100%', height: '50%', borderRadius: 2, backgroundColor: bgColor }} />
      <div style={{ width: '100%', height: '65%', borderRadius: 2, backgroundColor: bgColor }} />
      <div style={{ width: '100%', height: '35%', borderRadius: 2, backgroundColor: bgColor }} />
    </div>
  )
})

const DotSize = 2

interface NineBlockControlProps {}

export const NineBlockControl = React.memo<NineBlockControlProps>(() => {
  const colorTheme = useColorTheme()

  const dispatch = useEditorState((store) => store.dispatch, 'NineBlockControl dispatch')
  const metadata = useEditorState((store) => store.editor.jsxMetadata, 'NineBlockControl metadata')
  const selectedViews = useEditorState(
    (store) => store.editor.selectedViews,
    'NineBlockControl selectedViews',
  )

  const [hovered, setHovered] = React.useState<number>(-1)

  // TODO: detect if it's set via css only or code or jsx prop
  const [flexJustifyContent, flexAlignment] = detectFlexAlignJustifyContent(
    metadata,
    selectedViews[0],
  )

  const setAlignItemsJustifyContent = React.useCallback(
    (intendedFlexAlignment: FlexAlignment, intendedJustifyContent: FlexJustifyContent) => {
      const strategies = setFlexAlignJustifyContentStrategies(
        intendedFlexAlignment,
        intendedJustifyContent,
      )
      runStrategies(dispatch, metadata, selectedViews, strategies)
    },
    [dispatch, metadata, selectedViews],
  )

  if (filterKeepFlexContainers(metadata, selectedViews).length === 0) {
    return null
  }

  return (
    <div
      style={{
        gap: 3,
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
        const isSelected = alignItems === flexAlignment && justifyContent === flexJustifyContent
        return (
          <div
            onMouseEnter={() => setHovered(index)}
            onMouseLeave={() => setHovered(-1)}
            onClick={() => setAlignItemsJustifyContent(alignItems, justifyContent)}
            key={`${alignItems}-${justifyContent}`}
            style={{
              display: 'flex',
              alignItems: 'center',
              position: 'relative',
              boxSizing: 'border-box',
              justifyContent: 'center',
              gridColumn: `${(index % 3) + 1} / ${(index % 3) + 2}`,
              gridRow: `${Math.floor(index / 3) + 1} / ${Math.floor(index / 3) + 2}`,
            }}
          >
            {hovered === index || isSelected ? (
              <div
                css={{
                  position: 'absolute',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                  width: '100%',
                  height: '100%',
                  opacity: isSelected ? 1 : 0.5,
                }}
              >
                <Slabs
                  flexDirection='row'
                  alignItems={alignItems}
                  justifyContent={justifyContent}
                  bgColor={colorTheme.fg5.value}
                />
              </div>
            ) : (
              <div
                css={{
                  position: 'absolute',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                  width: '100%',
                  height: '100%',
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
            )}
          </div>
        )
      })}
    </div>
  )
})
