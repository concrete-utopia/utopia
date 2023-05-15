import React from 'react'
import { useDragLayer } from 'react-dnd'
import { NavigatorItem } from './navigator-item/navigator-item'
import { regularNavigatorEntry } from '../editor/store/editor-state'
import { NavigatorItemDragAndDropWrapperProps } from './navigator-item/navigator-item-dnd-container'
import { WindowPoint, windowPoint, zeroPoint } from '../../core/shared/math-utils'
import { LayoutIcon } from './navigator-item/layout-icon'
import { ItemLabel } from './navigator-item/item-label'
import { NO_OP } from '../../core/shared/utils'
import { FlexRow, useColorTheme } from '../../uuiui'

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

  const colorTheme = useColorTheme()

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
            transform: `translate(${offset.x}px, ${offset.y}px)`,
          }}
        >
          <LayoutIcon
            key={`layout-type-${item.label}`}
            navigatorEntry={regularNavigatorEntry(item.elementPath)}
            color={'main'}
            warningText={null}
          />
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
