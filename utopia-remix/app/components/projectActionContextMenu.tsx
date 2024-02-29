import * as DropdownMenu from '@radix-ui/react-dropdown-menu'
import React from 'react'
import { useProjectsStore } from '../store'
import { contextMenuDropdown, contextMenuItem } from '../styles/contextMenu.css'
import { sprinkles } from '../styles/sprinkles.css'
import { ProjectWithoutContent, operation } from '../types'
import { assertNever } from '../util/assertNever'
import { projectEditorLink } from '../util/links'
import { useFetcherWithOperation } from '../hooks/useFetcherWithOperation'

type ContextMenuEntry =
  | {
      text: string
      onClick: (project: ProjectWithoutContent) => void
    }
  | 'separator'

export const ProjectContextMenu = React.memo(({ project }: { project: ProjectWithoutContent }) => {
  const deleteFetcher = useFetcherWithOperation(operation(project, 'delete'))
  const destroyFetcher = useFetcherWithOperation(operation(project, 'destroy'))
  const restoreFetcher = useFetcherWithOperation(operation(project, 'restore'))
  const renameFetcher = useFetcherWithOperation(operation(project, 'rename'))

  const selectedCategory = useProjectsStore((store) => store.selectedCategory)

  const deleteProject = React.useCallback(
    (projectId: string) => {
      deleteFetcher.submit({}, { method: 'POST', action: `/internal/projects/${projectId}/delete` })
    },
    [deleteFetcher],
  )

  const destroyProject = React.useCallback(
    (projectId: string) => {
      const ok = window.confirm('Are you sure? The project contents will be deleted permanently.')
      if (ok) {
        destroyFetcher.submit(
          {},
          { method: 'POST', action: `/internal/projects/${projectId}/destroy` },
        )
      }
    },
    [destroyFetcher],
  )

  const restoreProject = React.useCallback(
    (projectId: string) => {
      restoreFetcher.submit(
        {},
        { method: 'POST', action: `/internal/projects/${projectId}/restore` },
      )
    },
    [restoreFetcher],
  )

  const renameProject = React.useCallback(
    (projectId: string, newTitle: string) => {
      renameFetcher.submit(
        { title: newTitle },
        { method: 'POST', action: `/internal/projects/${projectId}/rename` },
      )
    },
    [renameFetcher],
  )

  const menuEntries = React.useMemo((): ContextMenuEntry[] => {
    switch (selectedCategory) {
      case 'allProjects':
        return [
          {
            text: 'Open',
            onClick: (project) => {
              window.open(projectEditorLink(project.proj_id), '_blank')
            },
          },
          'separator',
          {
            text: 'Copy Link',
            onClick: (project) => {
              navigator.clipboard.writeText(projectEditorLink(project.proj_id))
              // TODO notification toast
            },
          },
          {
            text: 'Fork',
            onClick: (project) => {
              window.open(projectEditorLink(project.proj_id) + '/?fork=true', '_blank')
            },
          },
          'separator',
          {
            text: 'Rename',
            onClick: (project) => {
              const newTitle = window.prompt('New title:', project.title)
              if (newTitle != null) {
                renameProject(project.proj_id, newTitle)
              }
            },
          },
          {
            text: 'Delete',
            onClick: (project) => {
              deleteProject(project.proj_id)
            },
          },
        ]
      case 'trash':
        return [
          {
            text: 'Restore',
            onClick: (project) => {
              restoreProject(project.proj_id)
            },
          },
          'separator',
          {
            text: 'Delete permanently',
            onClick: (project) => {
              destroyProject(project.proj_id)
            },
          },
        ]
      default:
        assertNever(selectedCategory)
    }
  }, [selectedCategory])

  return (
    <DropdownMenu.Portal>
      <DropdownMenu.Content
        className={contextMenuDropdown()}
        style={{
          right: 75,
        }}
        sideOffset={5}
      >
        {menuEntries.map((entry, index) => {
          if (entry === 'separator') {
            return (
              <DropdownMenu.Separator
                key={`separator-${index}`}
                className={sprinkles({ backgroundColor: 'separator' })}
                style={{ height: 1 }}
              />
            )
          }
          return (
            <DropdownMenu.Item
              key={`entry-${index}`}
              onClick={() => entry.onClick(project)}
              className={contextMenuItem()}
            >
              {entry.text}
            </DropdownMenu.Item>
          )
        })}
      </DropdownMenu.Content>
    </DropdownMenu.Portal>
  )
})
ProjectContextMenu.displayName = 'ProjectContextMenu'
