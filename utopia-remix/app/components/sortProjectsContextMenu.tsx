import * as DropdownMenu from '@radix-ui/react-dropdown-menu'
import React from 'react'
import { useState } from 'react'
import { useProjectsStore } from '../store'
import { contextMenuItem } from '../styles/contextMenuItem.css'
import { colors } from '../styles/sprinkles.css'
import { CheckIcon } from '@radix-ui/react-icons'

export const SortingContextMenu = React.memo(() => {
  const sortCriteria = useProjectsStore((store) => store.sortCriteria)
  const setSortCriteria = useProjectsStore((store) => store.setSortCriteria)
  const sortAscending = useProjectsStore((store) => store.sortAscending)
  const setSortAscending = useProjectsStore((store) => store.setSortAscending)

  return (
    <DropdownMenu.Portal>
      <DropdownMenu.Content
        style={{
          background: 'white',
          color: 'black',
          padding: 4,
          boxShadow: '2px 3px 4px #00000030',
          border: '1px solid #ccc',
          borderRadius: 4,
          display: 'flex',
          flexDirection: 'column',
          gap: 4,
          minWidth: 170,
          position: 'relative',
          right: 30,
        }}
        sideOffset={5}
      >
        <DropdownMenu.DropdownMenuLabel style={{ color: 'grey', padding: 5 }}>
          Sort by
        </DropdownMenu.DropdownMenuLabel>
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
        <DropdownMenu.Separator style={{ backgroundColor: colors.separator, height: 1 }} />
        <DropdownMenu.DropdownMenuLabel style={{ color: 'grey', padding: 5 }}>
          Order
        </DropdownMenu.DropdownMenuLabel>
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
