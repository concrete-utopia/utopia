import * as DropdownMenu from '@radix-ui/react-dropdown-menu'
import React from 'react'
import { useProjectsStore } from '../store'
import { contextMenuDropdown, contextMenuItem } from '../styles/contextMenu.css'
import { sprinkles } from '../styles/sprinkles.css'
import { CheckIcon } from '@radix-ui/react-icons'

export const SortingContextMenu = React.memo(() => {
  const sortCriteria = useProjectsStore((store) => store.sortCriteria)
  const setSortCriteria = useProjectsStore((store) => store.setSortCriteria)
  const setSortByTitle = React.useCallback(() => setSortCriteria('title'), [setSortCriteria])
  const setSortByDateCreated = React.useCallback(
    () => setSortCriteria('dateCreated'),
    [setSortCriteria],
  )
  const setSortByDateModified = React.useCallback(
    () => setSortCriteria('dateModified'),
    [setSortCriteria],
  )

  const sortAscending = useProjectsStore((store) => store.sortAscending)
  const setSortAscending = useProjectsStore((store) => store.setSortAscending)
  const setSortAscendingTrue = React.useCallback(() => setSortAscending(true), [setSortAscending])
  const setSortAscendingFalse = React.useCallback(() => setSortAscending(false), [setSortAscending])

  return (
    <DropdownMenu.Portal>
      <DropdownMenu.Content className={contextMenuDropdown()} sideOffset={5} align='end'>
        <DropdownMenu.Label style={{ color: 'grey', padding: 5 }}>Sort by</DropdownMenu.Label>
        <DropdownMenu.CheckboxItem
          className={contextMenuItem()}
          checked={sortCriteria === 'title'}
          onCheckedChange={setSortByTitle}
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
          onCheckedChange={setSortByDateCreated}
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
          onCheckedChange={setSortByDateModified}
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
          onCheckedChange={setSortAscendingTrue}
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
          onCheckedChange={setSortAscendingFalse}
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
