import * as DropdownMenu from '@radix-ui/react-dropdown-menu'
import { useFetcher } from '@remix-run/react'
import React from 'react'
import { useProjectsStore } from '../store'
import { contextMenuDropdown, contextMenuItem } from '../styles/contextMenu.css'
import { colors } from '../styles/sprinkles.css'
import { AccessLevel, ProjectWithoutContent } from '../types'
import { assertNever } from '../util/assertNever'
import { projectEditorLink } from '../util/links'

type ContextMenuEntry =
  | {
      text: string
      onClick: (project: ProjectWithoutContent) => void
    }
  | 'separator'

export const ProjectContextMenu = React.memo(({ project }: { project: ProjectWithoutContent }) => {
  const fetcher = useFetcher()
  const selectedCategory = useProjectsStore((store) => store.selectedCategory)
  const accessLevel = AccessLevel.PUBLIC

  const deleteProject = React.useCallback(
    (projectId: string) => {
      fetcher.submit({}, { method: 'POST', action: `/internal/projects/${projectId}/delete` })
    },
    [fetcher],
  )

  const destroyProject = React.useCallback(
    (projectId: string) => {
      const ok = window.confirm('Are you sure? The project contents will be deleted permanently.')
      if (ok) {
        fetcher.submit({}, { method: 'POST', action: `/internal/projects/${projectId}/destroy` })
      }
    },
    [fetcher],
  )

  const restoreProject = React.useCallback(
    (projectId: string) => {
      fetcher.submit({}, { method: 'POST', action: `/internal/projects/${projectId}/restore` })
    },
    [fetcher],
  )

  const renameProject = React.useCallback(
    (projectId: string, newTitle: string) => {
      fetcher.submit(
        { title: newTitle },
        { method: 'POST', action: `/internal/projects/${projectId}/rename` },
      )
    },
    [fetcher],
  )

  const changeAccessLevel = React.useCallback(
    (projectId: string, accessLevel: AccessLevel) => {
      fetcher.submit(
        { accessLevel: accessLevel.toString() },
        { method: 'POST', action: `/internal/projects/${projectId}/access` },
      )
    },
    [fetcher],
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
          {
            text: accessLevel === AccessLevel.PUBLIC ? 'Make Private' : 'Make Public',
            onClick: (project) => {
              changeAccessLevel(
                project.proj_id,
                accessLevel === AccessLevel.PUBLIC ? AccessLevel.PRIVATE : AccessLevel.PUBLIC,
              )
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
                style={{ backgroundColor: colors.separator, height: 1 }}
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
