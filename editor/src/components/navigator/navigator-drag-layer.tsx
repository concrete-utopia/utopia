import React from 'react'
import { useDragLayer } from 'react-dnd'
import { NavigatorItem } from './navigator-item/navigator-item'
import { regularNavigatorEntry } from '../editor/store/editor-state'
import { NavigatorItemDragAndDropWrapperProps } from './navigator-item/navigator-item-dnd-container'
import { WindowPoint, windowPoint, zeroPoint } from '../../core/shared/math-utils'
import { LayoutIcon } from './navigator-item/layout-icon'
import { ItemLabel } from './navigator-item/item-label'
import { NO_OP } from '../../core/shared/utils'
import { FlexRow, Icn, useColorTheme } from '../../uuiui'
import { useLayoutOrElementIcon } from './layout-element-icons'
import { emptyElementPath } from '../../core/shared/element-path'

function getOffset(
  initialOffset: WindowPoint | null,
  currentOffset: WindowPoint | null,
): WindowPoint | null {
  if (initialOffset == null || currentOffset == null) {
    return null
  }

  return currentOffset
}

export const NavigatorDragLayer = React.memo(() => {
  const { item, initialOffset, currentOffset } = useDragLayer((monitor) => ({
    item: monitor.getItem() as NavigatorItemDragAndDropWrapperProps | null,
    initialOffset: monitor.getInitialClientOffset() as WindowPoint | null,
    currentOffset: monitor.getClientOffset() as WindowPoint | null,
  }))

  const offset = getOffset(initialOffset, currentOffset) ?? zeroPoint

  const icon =
    useLayoutOrElementIcon(regularNavigatorEntry(item?.elementPath ?? emptyElementPath))
      ?.iconProps ?? {}

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
      {item == null ? null : (
        <FlexRow
          style={{
            width: '300px',
            transform: `translate(${offset.x - 50}px, ${offset.y}px)`,
            fontWeight: 600,
          }}
        >
          <Icn {...icon} color={'main'} />
          <ItemLabel
            key={`label-${item.label}`}
            testId={`navigator-item-label-${item.label}`}
            name={item.label}
            isDynamic={false}
            target={regularNavigatorEntry(item.elementPath)}
            selected={item.selected}
            dispatch={NO_OP}
            inputVisible={false}
          />
        </FlexRow>
      )}
    </div>
  )
})
