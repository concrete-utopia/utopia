import React from 'react'
import { useDragLayer } from 'react-dnd'
import type { RegularNavigatorEntry } from '../editor/store/editor-state'
import { navigatorEntryToKey, regularNavigatorEntry } from '../editor/store/editor-state'
import type { NavigatorItemDragAndDropWrapperProps } from './navigator-item/navigator-item-dnd-container'
import type { WindowPoint } from '../../core/shared/math-utils'
import { windowPoint, zeroPoint } from '../../core/shared/math-utils'
import { ItemLabel } from './navigator-item/item-label'
import { NO_OP } from '../../core/shared/utils'
import { colorTheme, FlexRow, Icn } from '../../uuiui'
import { useLayoutOrElementIcon } from './layout-element-icons'
import { emptyElementPath } from '../../core/shared/element-path'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { navigatorDepth } from './navigator-utils'
import { getElementPadding } from './navigator-item/navigator-item'
import { metadataSelector } from '../inspector/inpector-selectors'
import createCachedSelector from 're-reselect'
import type { MetadataSubstate } from '../editor/store/store-hook-substore-types'

const depthSelector = createCachedSelector(
  metadataSelector,
  (_: MetadataSubstate, navigatorEntry: RegularNavigatorEntry) => navigatorEntry,
  (metadata, navigatorEntry) => navigatorDepth(navigatorEntry, metadata) + 1,
)((_, navigatorEntry) => navigatorEntryToKey(navigatorEntry))

export const NavigatorDragLayer = React.memo(() => {
  const { item, initialOffset, difference } = useDragLayer((monitor) => ({
    item: monitor.getItem() as NavigatorItemDragAndDropWrapperProps | null,
    initialOffset: (monitor.getInitialSourceClientOffset() as WindowPoint) ?? zeroPoint,
    difference: (monitor.getDifferenceFromInitialOffset() as WindowPoint) ?? zeroPoint,
  }))

  const navigatorEntry = React.useMemo(
    () => regularNavigatorEntry(item?.elementPath ?? emptyElementPath),
    [item?.elementPath],
  )

  const hidden = item == null

  const entryNavigatorDepth = useEditorState(
    Substores.metadata,
    (store) => depthSelector(store, navigatorEntry),
    'NavigatorDragLayer metadata',
  )

  const offset = windowPoint({
    x: initialOffset.x + difference.x,
    y: initialOffset.y + difference.y,
  })

  const icon = useLayoutOrElementIcon(navigatorEntry)?.iconProps ?? {}
  const label = item?.label ?? ''
  const selected = item?.selected ?? false

  return hidden ? null : (
    <div
      style={{
        position: 'fixed',
        pointerEvents: 'none',
        left: 0,
        top: 0,
        width: '100%',
        height: '100%',
        zIndex: 100,
      }}
    >
      <FlexRow
        style={{
          paddingLeft: getElementPadding(entryNavigatorDepth),
          width: '300px',
          transform: `translate(${offset.x}px, ${offset.y}px)`,
          fontWeight: 600,
        }}
      >
        <FlexRow
          style={{
            width: 'min-content',
            background: colorTheme.bg1transparentgradient.value,
            padding: 3,
            borderRadius: 2,
          }}
        >
          <Icn {...icon} color={'main'} />
          <ItemLabel
            key={`label-${label}`}
            testId={`navigator-item-label-${label}`}
            name={label}
            isDynamic={false}
            target={navigatorEntry}
            selected={selected}
            dispatch={NO_OP}
            inputVisible={false}
          />
        </FlexRow>
      </FlexRow>
    </div>
  )
})
NavigatorDragLayer.displayName = 'NavigatorDragLayer'
