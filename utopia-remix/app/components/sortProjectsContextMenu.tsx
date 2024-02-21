import {
  Portal as DropdownMenuPortal,
  Content as DropdownMenuContent,
  Label as DropdownMenuLabel,
  Separator as DropdownMenuSeparator,
  CheckboxItem as DropdownMenuCheckboxItem,
  ItemIndicator as DropdownMenuItemIndicator,
} from '@radix-ui/react-dropdown-menu'
import React from 'react'
import { useProjectsStore } from '../store'
import { contextMenuDropdown, contextMenuItem } from '../styles/contextMenu.css'
import { colors } from '../styles/sprinkles.css'
import { CheckIcon } from '@radix-ui/react-icons'

export const SortingContextMenu = React.memo(() => {
  const sortCriteria = useProjectsStore((store) => store.sortCriteria)
  const setSortCriteria = useProjectsStore((store) => store.setSortCriteria)
  const sortAscending = useProjectsStore((store) => store.sortAscending)
  const setSortAscending = useProjectsStore((store) => store.setSortAscending)

  return (
    <DropdownMenuPortal>
      <DropdownMenuContent
        className={contextMenuDropdown()}
        style={{
          right: 30,
        }}
        sideOffset={5}
      >
        <DropdownMenuLabel style={{ color: 'grey', padding: 5 }}>Sort by</DropdownMenuLabel>
        <DropdownMenuCheckboxItem
          className={contextMenuItem()}
          checked={sortCriteria === 'title'}
          onCheckedChange={() => setSortCriteria('title')}
        >
          <div style={{ width: 20 }}>
            <DropdownMenuItemIndicator className='DropdownMenuItemIndicator'>
              <CheckIcon />
            </DropdownMenuItemIndicator>
          </div>
          Alphabetical
        </DropdownMenuCheckboxItem>

        <DropdownMenuCheckboxItem
          className={contextMenuItem()}
          checked={sortCriteria === 'dateCreated'}
          onCheckedChange={() => setSortCriteria('dateCreated')}
        >
          <div style={{ width: 20 }}>
            <DropdownMenuItemIndicator className='DropdownMenuItemIndicator'>
              <CheckIcon />
            </DropdownMenuItemIndicator>
          </div>
          Date Created
        </DropdownMenuCheckboxItem>
        <DropdownMenuCheckboxItem
          className={contextMenuItem()}
          checked={sortCriteria === 'dateModified'}
          onCheckedChange={() => setSortCriteria('dateModified')}
        >
          <div style={{ width: 20 }}>
            <DropdownMenuItemIndicator className='DropdownMenuItemIndicator'>
              <CheckIcon />
            </DropdownMenuItemIndicator>
          </div>
          Date Modified
        </DropdownMenuCheckboxItem>
        <DropdownMenuSeparator style={{ backgroundColor: colors.separator, height: 1 }} />
        <DropdownMenuLabel style={{ color: 'grey', padding: 5 }}>Order</DropdownMenuLabel>
        <DropdownMenuCheckboxItem
          className={contextMenuItem()}
          checked={sortAscending}
          onCheckedChange={() => setSortAscending(true)}
        >
          <div style={{ width: 20 }}>
            <DropdownMenuItemIndicator className='DropdownMenuItemIndicator'>
              <CheckIcon />
            </DropdownMenuItemIndicator>
          </div>
          Ascending
        </DropdownMenuCheckboxItem>
        <DropdownMenuCheckboxItem
          className={contextMenuItem()}
          checked={!sortAscending}
          onCheckedChange={() => setSortAscending(false)}
        >
          <div style={{ width: 20 }}>
            <DropdownMenuItemIndicator className='DropdownMenuItemIndicator'>
              <CheckIcon />
            </DropdownMenuItemIndicator>
          </div>
          Descending
        </DropdownMenuCheckboxItem>
      </DropdownMenuContent>
    </DropdownMenuPortal>
  )
})
SortingContextMenu.displayName = 'ProjectContextMenu'
