import { useFetcher } from '@remix-run/react'
import React from 'react'
import { Item, Menu, Separator, useContextMenu } from 'react-contexify'
import { Category } from '../routes/projects'
import { ProjectWithoutContent } from '../types'
import { assertNever } from '../util/assertNever'
import { projectEditorLink } from '../util/links'

const CONTEXT_MENU_ID = 'context-menu'

type ContextMenuAction = 'delete' | 'destroy' | 'restore' | 'open' | 'copy-link' | 'rename' // | 'fork' | 'share'

type ContextMenuEntry =
  | {
      id: ContextMenuAction
      text: string
      onClick: (params: {
        id: ContextMenuAction
        props: { project: ProjectWithoutContent }
      }) => void
    }
  | 'separator'

export const ProjectContextMenu = React.memo(
  ({ selectedCategory }: { selectedCategory: Category }) => {
    const fetcher = useFetcher()

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

    const menuEntries = React.useMemo((): ContextMenuEntry[] => {
      switch (selectedCategory) {
        case 'allProjects':
          return [
            {
              id: 'open',
              text: 'Open',
              onClick: (params) => {
                window.open(projectEditorLink(params.props.project.proj_id), '_blank')
              },
            },
            'separator',
            {
              id: 'copy-link',
              text: 'Copy Link',
              onClick: (params) => {
                navigator.clipboard.writeText(projectEditorLink(params.props.project.proj_id))
                // TODO notification toast
              },
            },
            //   {
            //     id: 'share',
            //     text: 'Share',
            //     onClick: (params) => {},
            //   },
            //   {
            //     id: 'fork',
            //     text: 'Fork',
            //     onClick: (params) => {},
            //   },
            'separator',
            {
              id: 'rename',
              text: 'Rename',
              onClick: (params) => {
                const newTitle = window.prompt('New title:', params.props.project.title)
                if (newTitle != null) {
                  renameProject(params.props.project.proj_id, newTitle)
                }
              },
            },
            {
              id: 'delete',
              text: 'Delete',
              onClick: (params) => {
                deleteProject(params.props.project.proj_id)
              },
            },
          ]
        case 'trash':
          return [
            {
              id: 'restore',
              text: 'Restore',
              onClick: (params) => {
                restoreProject(params.props.project.proj_id)
              },
            },
            'separator',
            {
              id: 'destroy',
              text: 'Delete permanently',
              onClick: (params) => {
                destroyProject(params.props.project.proj_id)
              },
            },
          ]
        default:
          assertNever(selectedCategory)
      }
    }, [selectedCategory])

    return (
      <>
        <Menu id={CONTEXT_MENU_ID}>
          {menuEntries.map((entry, index) => {
            if (entry === 'separator') {
              return <Separator key={`separator-${index}`} />
            }
            return (
              <Item key={entry.id} id={entry.id} onClick={entry.onClick as any}>
                {entry.text}
              </Item>
            )
          })}
        </Menu>
        <fetcher.Form />
      </>
    )
  },
)
ProjectContextMenu.displayName = 'ActionMenu'

export function useProjectContextMenu() {
  const contextMenu = useContextMenu({
    id: CONTEXT_MENU_ID,
  })
  return { showProjectContextMenu: contextMenu.show }
}
