import React from 'react'
import { useDragLayer } from 'react-dnd'
import { NavigatorItem } from './navigator-item/navigator-item'
import { regularNavigatorEntry } from '../editor/store/editor-state'
import { NavigatorItemDragAndDropWrapperProps } from './navigator-item/navigator-item-dnd-container'
import { WindowPoint, windowPoint, zeroPoint } from '../../core/shared/math-utils'

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
        <div
          style={{
            width: '300px',
            opacity: 0.5,
            transform: `translate(${offset.x}px, ${offset.y}px)`,
          }}
        >
          <NavigatorItem
            navigatorEntry={regularNavigatorEntry(item.elementPath)}
            index={item.index}
            getSelectedViewsInRange={item.getSelectedViewsInRange}
            noOfChildren={item.noOfChildren}
            label={item.label}
            dispatch={item.editorDispatch}
            isHighlighted={item.highlighted}
            isElementVisible={item.isElementVisible}
            renamingTarget={item.renamingTarget}
            collapsed={item.collapsed}
            selected={item.selected}
            parentOutline={'none'}
            visibleNavigatorTargets={item.visibleNavigatorTargets}
          />
        </div>
      )}
    </div>
  )
})
