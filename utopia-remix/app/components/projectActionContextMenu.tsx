import * as DropdownMenu from '@radix-ui/react-dropdown-menu'
import React, { useMemo } from 'react'
import { useProjectsStore } from '../store'
import { contextMenuDropdown, contextMenuItem } from '../styles/contextMenu.css'
import { sprinkles } from '../styles/sprinkles.css'
import type { ProjectWithoutContent } from '../types'
import { operationDelete, operationDestroy, operationRename, operationRestore } from '../types'
import { assertNever } from '../util/assertNever'
import { AccessLevel } from '../types'
import { useProjectEditorLink } from '../util/links'
import { useFetcherWithOperation } from '../hooks/useFetcherWithOperation'
import slugify from 'slugify'
import { SLUGIFY_OPTIONS } from '../routes/internal.projects.$id.rename'
import { SharePopup } from './SharePopup'
import { ContextMenu, Dialog } from '@radix-ui/themes'

type ContextMenuEntry =
  | {
      text: string
      onClick: (project: ProjectWithoutContent) => void
    }
  | 'separator'
  | 'sharing-dialog'
  | null

export const ProjectContextMenu = React.memo(({ project }: { project: ProjectWithoutContent }) => {
  const deleteFetcher = useFetcherWithOperation(project.proj_id, 'delete')
  const destroyFetcher = useFetcherWithOperation(project.proj_id, 'destroy')
  const restoreFetcher = useFetcherWithOperation(project.proj_id, 'restore')
  const renameFetcher = useFetcherWithOperation(project.proj_id, 'rename')

  const selectedCategory = useProjectsStore((store) => store.selectedCategory)

  const deleteProject = React.useCallback(
    (projectId: string) => {
      deleteFetcher.submit(
        operationDelete(project),
        {},
        { method: 'POST', action: `/internal/projects/${projectId}/delete` },
      )
    },
    [deleteFetcher, project],
  )

  const destroyProject = React.useCallback(
    (projectId: string) => {
      const ok = window.confirm('Are you sure? The project contents will be deleted permanently.')
      if (ok) {
        destroyFetcher.submit(
          operationDestroy(project),
          {},
          { method: 'POST', action: `/internal/projects/${projectId}/destroy` },
        )
      }
    },
    [destroyFetcher, project],
  )

  const restoreProject = React.useCallback(
    (projectId: string) => {
      restoreFetcher.submit(
        operationRestore(project),
        {},
        { method: 'POST', action: `/internal/projects/${projectId}/restore` },
      )
    },
    [restoreFetcher, project],
  )

  const renameProject = React.useCallback(
    (projectId: string, newTitle: string) => {
      renameFetcher.submit(
        operationRename(project, slugify(newTitle, SLUGIFY_OPTIONS)),
        { title: newTitle },
        { method: 'POST', action: `/internal/projects/${projectId}/rename` },
      )
    },
    [renameFetcher, project],
  )

  const projectEditorLink = useProjectEditorLink()

  const menuEntries = React.useMemo((): (ContextMenuEntry | null)[] => {
    switch (selectedCategory) {
      case 'allProjects':
        return [
          {
            text: 'Open',
            onClick: (selectedProject) => {
              window.open(projectEditorLink(selectedProject.proj_id), '_blank')
            },
          },
          'separator',
          {
            text: 'Copy Link',
            onClick: (selectedProject) => {
              window.navigator.clipboard.writeText(projectEditorLink(selectedProject.proj_id))
              // TODO notification toast
            },
          },
          {
            text: 'Fork',
            onClick: (selectedProject) => {
              window.open(projectEditorLink(selectedProject.proj_id) + '/?fork=true', '_blank')
            },
          },
          'separator',
          {
            text: 'Rename',
            onClick: (selectedProject) => {
              const newTitle = window.prompt('New title:', selectedProject.title)
              if (newTitle != null) {
                renameProject(selectedProject.proj_id, newTitle)
              }
            },
          },
          {
            text: 'Delete',
            onClick: (selectedProject) => {
              deleteProject(selectedProject.proj_id)
            },
          },
          'separator',
          'sharing-dialog',
        ]
      case 'trash':
        return [
          {
            text: 'Restore',
            onClick: (selectedProject) => {
              restoreProject(selectedProject.proj_id)
            },
          },
          'separator',
          {
            text: 'Delete permanently',
            onClick: (selectedProject) => {
              destroyProject(selectedProject.proj_id)
            },
          },
        ]
      default:
        assertNever(selectedCategory)
    }
  }, [
    selectedCategory,
    projectEditorLink,
    deleteProject,
    destroyProject,
    renameProject,
    restoreProject,
  ])

  const preventDefault = React.useCallback((event: Event) => {
    event.preventDefault()
  }, [])

  return (
    <DropdownMenu.Portal>
      <DropdownMenu.Content className={contextMenuDropdown()} align='end' sideOffset={5}>
        {menuEntries.map((entry, index) => {
          if (entry == null) {
            return null
          }
          if (entry === 'separator') {
            return (
              <DropdownMenu.Separator
                key={`separator-${index}`}
                className={sprinkles({ backgroundColor: 'separator' })}
                style={{ height: 1 }}
              />
            )
          }
          if (entry === 'sharing-dialog') {
            return (
              <Dialog.Root key={`separator-${index}`}>
                <Dialog.Trigger>
                  <DropdownMenu.Item
                    style={{ height: 28, fontSize: 12 }}
                    onSelect={preventDefault}
                    className={contextMenuItem()}
                  >
                    Share
                  </DropdownMenu.Item>
                </Dialog.Trigger>
                <Dialog.Content>
                  <SharePopup project={project} />
                </Dialog.Content>
              </Dialog.Root>
            )
          }
          return (
            <DropdownMenu.Item
              key={`entry-${index}`}
              /* eslint-disable-next-line react/jsx-no-bind */
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
