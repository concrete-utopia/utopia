import * as DropdownMenu from '@radix-ui/react-dropdown-menu'
import React from 'react'
import { useProjectsStore } from '../store'
import { contextMenuDropdown, contextMenuItem } from '../styles/contextMenu.css'
import { sprinkles } from '../styles/sprinkles.css'
import { CheckIcon } from '@radix-ui/react-icons'

export const SortingContextMenu = React.memo(() => {
  const sortCriteria = useProjectsStore((store) => store.sortCriteria)
  const setSortCriteria = useProjectsStore((store) => store.setSortCriteria)
  const sortAscending = useProjectsStore((store) => store.sortAscending)
  const setSortAscending = useProjectsStore((store) => store.setSortAscending)

  return (
    <DropdownMenu.Portal>
      <DropdownMenu.Content
        className={contextMenuDropdown()}
        style={{
          right: 60,
        }}
        sideOffset={5}
      >
        <DropdownMenu.Label style={{ color: 'grey', padding: 5 }}>Sort by</DropdownMenu.Label>
        <DropdownMenu.CheckboxItem
          className={contextMenuItem()}
          checked={sortCriteria === 'title'}
          onCheckedChange={() => setSortCriteria('title')}
        >
          <div style={{ width: 20 }}>
            <DropdownMenu.ItemIndicator className='DropdownMenuItemIndicator'>
              <CheckIcon />
            </DropdownMenu.ItemIndicator>
          </div>
          Alphabetical
        </DropdownMenu.CheckboxItem>

        <DropdownMenu.CheckboxItem
          className={contextMenuItem()}
          checked={sortCriteria === 'dateCreated'}
          onCheckedChange={() => setSortCriteria('dateCreated')}
        >
          <div style={{ width: 20 }}>
            <DropdownMenu.ItemIndicator className='DropdownMenuItemIndicator'>
              <CheckIcon />
            </DropdownMenu.ItemIndicator>
          </div>
          Date Created
        </DropdownMenu.CheckboxItem>
        <DropdownMenu.CheckboxItem
          className={contextMenuItem()}
          checked={sortCriteria === 'dateModified'}
          onCheckedChange={() => setSortCriteria('dateModified')}
        >
          <div style={{ width: 20 }}>
            <DropdownMenu.ItemIndicator className='DropdownMenuItemIndicator'>
              <CheckIcon />
            </DropdownMenu.ItemIndicator>
          </div>
          Date Modified
        </DropdownMenu.CheckboxItem>
        <DropdownMenu.Separator
          className={sprinkles({ backgroundColor: 'separator' })}
          style={{ height: 1 }}
        />
        <DropdownMenu.Label style={{ color: 'grey', padding: 5 }}>Order</DropdownMenu.Label>
        <DropdownMenu.CheckboxItem
          className={contextMenuItem()}
          checked={sortAscending}
          onCheckedChange={() => setSortAscending(true)}
        >
          <div style={{ width: 20 }}>
            <DropdownMenu.ItemIndicator className='DropdownMenuItemIndicator'>
              <CheckIcon />
            </DropdownMenu.ItemIndicator>
          </div>
          Ascending
        </DropdownMenu.CheckboxItem>
        <DropdownMenu.CheckboxItem
          className={contextMenuItem()}
          checked={!sortAscending}
          onCheckedChange={() => setSortAscending(false)}
        >
          <div style={{ width: 20 }}>
            <DropdownMenu.ItemIndicator className='DropdownMenuItemIndicator'>
              <CheckIcon />
            </DropdownMenu.ItemIndicator>
          </div>
          Descending
        </DropdownMenu.CheckboxItem>
      </DropdownMenu.Content>
    </DropdownMenu.Portal>
  )
})
SortingContextMenu.displayName = 'ProjectContextMenu'
