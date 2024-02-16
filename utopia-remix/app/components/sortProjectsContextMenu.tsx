import * as DropdownMenu from '@radix-ui/react-dropdown-menu'
import React from 'react'
import { contextMenuItem } from '../styles/contextMenuItem.css'
import { colors } from '../styles/sprinkles.css'

export const SortingContextMenu = React.memo(() => {
  return (
    <DropdownMenu.Portal>
      <DropdownMenu.Content
        style={{
          background: 'white',
          padding: 4,
          boxShadow: '2px 3px 4px #00000030',
          border: '1px solid #ccc',
          borderRadius: 4,
          display: 'flex',
          flexDirection: 'column',
          gap: 4,
          minWidth: 170,
          position: 'relative',
          right: 25,
        }}
        sideOffset={5}
      >
        <DropdownMenu.DropdownMenuLabel style={{ color: 'black' }}>
          Sort
        </DropdownMenu.DropdownMenuLabel>
        <DropdownMenu.Item onClick={() => {}} className={contextMenuItem()}>
          Alphabetical
        </DropdownMenu.Item>
        <DropdownMenu.Item onClick={() => {}} className={contextMenuItem()}>
          Date Created
        </DropdownMenu.Item>
        <DropdownMenu.Item onClick={() => {}} className={contextMenuItem()}>
          Date Modified
        </DropdownMenu.Item>
        <DropdownMenu.Separator style={{ backgroundColor: colors.separator, height: 1 }} />
        <DropdownMenu.DropdownMenuLabel style={{ color: 'black' }}>
          Order
        </DropdownMenu.DropdownMenuLabel>
        <DropdownMenu.Item onClick={() => {}} className={contextMenuItem()}>
          Ascending
        </DropdownMenu.Item>
        <DropdownMenu.Item onClick={() => {}} className={contextMenuItem()}>
          Descending
        </DropdownMenu.Item>
      </DropdownMenu.Content>
    </DropdownMenu.Portal>
  )
})
SortingContextMenu.displayName = 'ProjectContextMenu'
