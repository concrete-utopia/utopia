import React from 'react'
import { useProjectsStore } from '../stores/projectsStore'
import { DropdownMenu, Separator } from '@radix-ui/themes'

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
    <DropdownMenu.Content align='end' sideOffset={10}>
      <DropdownMenu.Label style={{ height: 28, fontSize: 12, paddingLeft: 5 }}>
        Sort by
      </DropdownMenu.Label>
      <DropdownMenu.CheckboxItem
        style={{ height: 28, fontSize: 12, paddingLeft: 30 }}
        checked={sortCriteria === 'title'}
        onCheckedChange={setSortByTitle}
      >
        Alphabetical
      </DropdownMenu.CheckboxItem>
      <DropdownMenu.CheckboxItem
        style={{ height: 28, fontSize: 12, paddingLeft: 30 }}
        checked={sortCriteria === 'dateCreated'}
        onCheckedChange={setSortByDateCreated}
      >
        Date Created
      </DropdownMenu.CheckboxItem>
      <DropdownMenu.CheckboxItem
        style={{ height: 28, fontSize: 12, paddingLeft: 30 }}
        checked={sortCriteria === 'dateModified'}
        onCheckedChange={setSortByDateModified}
      >
        Date Modified
      </DropdownMenu.CheckboxItem>
      <Separator size='4' style={{ marginTop: 5, marginBottom: 5 }} />
      <DropdownMenu.Label style={{ height: 28, fontSize: 12, paddingLeft: 5 }}>
        Order
      </DropdownMenu.Label>
      <DropdownMenu.CheckboxItem
        style={{ height: 28, fontSize: 12, paddingLeft: 30 }}
        checked={sortAscending}
        onCheckedChange={setSortAscendingTrue}
      >
        Ascending
      </DropdownMenu.CheckboxItem>
      <DropdownMenu.CheckboxItem
        style={{ height: 28, fontSize: 12, paddingLeft: 30 }}
        checked={!sortAscending}
        onCheckedChange={setSortAscendingFalse}
      >
        Descending
      </DropdownMenu.CheckboxItem>
    </DropdownMenu.Content>
  )
})
SortingContextMenu.displayName = 'ProjectContextMenu'
