import React from 'react'
import { useDragLayer } from 'react-dnd'
import {
  RegularNavigatorEntry,
  navigatorEntryToKey,
  regularNavigatorEntry,
} from '../editor/store/editor-state'
import { NavigatorItemDragAndDropWrapperProps } from './navigator-item/navigator-item-dnd-container'
import { WindowPoint, windowPoint, zeroPoint } from '../../core/shared/math-utils'
import { ItemLabel } from './navigator-item/item-label'
import { NO_OP } from '../../core/shared/utils'
import { FlexRow, Icn } from '../../uuiui'
import { useLayoutOrElementIcon } from './layout-element-icons'
import { emptyElementPath } from '../../core/shared/element-path'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { navigatorDepth } from './navigator-utils'
import { getElementPadding } from './navigator-item/navigator-item'
import { metadataSelector } from '../inspector/inpector-selectors'
import createCachedSelector from 're-reselect'
import { MetadataSubstate } from '../editor/store/store-hook-substore-types'

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

  return (
    <div
      style={{
        position: 'fixed',
        pointerEvents: 'none',
        left: 0,
        top: 0,
        width: '100%',
        height: '100%',
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
    </div>
  )
})
