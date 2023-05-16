import React from 'react'
import { useDragLayer } from 'react-dnd'
import { regularNavigatorEntry } from '../editor/store/editor-state'
import { NavigatorItemDragAndDropWrapperProps } from './navigator-item/navigator-item-dnd-container'
import { WindowPoint, windowPoint, zeroPoint } from '../../core/shared/math-utils'
import { ItemLabel } from './navigator-item/item-label'
import { NO_OP } from '../../core/shared/utils'
import { FlexRow, Icn } from '../../uuiui'
import { useLayoutOrElementIcon } from './layout-element-icons'
import { emptyElementPath } from '../../core/shared/element-path'

export const NavigatorDragLayer = React.memo(() => {
  const { item, initialOffset, difference } = useDragLayer((monitor) => ({
    item: monitor.getItem() as NavigatorItemDragAndDropWrapperProps | null,
    initialOffset: (monitor.getInitialSourceClientOffset() as WindowPoint) ?? zeroPoint,
    difference: (monitor.getDifferenceFromInitialOffset() as WindowPoint) ?? zeroPoint,
  }))

  const offset = windowPoint({
    x: initialOffset.x + difference.x,
    y: initialOffset.y + difference.y,
  })
  const navigatorEntry = regularNavigatorEntry(item?.elementPath ?? emptyElementPath)
  const icon = useLayoutOrElementIcon(navigatorEntry)?.iconProps ?? {}

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
            fontWeight: 600,
          }}
        >
          <Icn {...icon} color={'main'} />
          <ItemLabel
            key={`label-${item.label}`}
            testId={`navigator-item-label-${item.label}`}
            name={item.label}
            isDynamic={false}
            target={navigatorEntry}
            selected={item.selected}
            dispatch={NO_OP}
            inputVisible={false}
          />
        </FlexRow>
      )}
    </div>
  )
})
