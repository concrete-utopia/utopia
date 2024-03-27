import { DotFilledIcon } from '@radix-ui/react-icons'
import { ContextMenu, Flex, Separator, Text } from '@radix-ui/themes'
import React from 'react'
import slugify from 'slugify'
import { when } from '~/util/react-conditionals'
import { useFetcherWithOperation } from '../hooks/useFetcherWithOperation'
import { useOpenShareDialog } from '../hooks/useOpenShareDialog'
import { SLUGIFY_OPTIONS } from '../routes/internal.projects.$id.rename'
import { useProjectsStore } from '../stores/projectsStore'
import type { ProjectListing } from '../types'
import { operationDelete, operationDestroy, operationRename, operationRestore } from '../types'
import { assertNever } from '../util/assertNever'
import { useCopyProjectLinkToClipboard } from '../util/copyProjectLink'
import { useProjectEditorLink } from '../util/links'

type ContextMenuEntry =
  | {
      text: string
      onClick: (project: ProjectListing) => void
    }
  | 'separator'
  | 'sharing-dialog'

function contextMenuEntry(
  text: string,
  onClick: (project: ProjectListing) => void,
): ContextMenuEntry {
  return { text, onClick }
}

export const ProjectActionsMenu = React.memo(
  ({
    // the project that can receive actions
    project,
  }: {
    project: ProjectListing
  }) => {
    const deleteFetcher = useFetcherWithOperation(project.proj_id, 'delete')
    const destroyFetcher = useFetcherWithOperation(project.proj_id, 'destroy')
    const restoreFetcher = useFetcherWithOperation(project.proj_id, 'restore')
    const renameFetcher = useFetcherWithOperation(project.proj_id, 'rename')
    const selectedCategory = useProjectsStore((store) => store.selectedCategory)

    const deleteProject = React.useCallback(
      (projectId: string) => {
        deleteFetcher.submit(
          operationDelete(project.proj_id),
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
            operationDestroy(project.proj_id),
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
          operationRestore(project.proj_id),
          {},
          { method: 'POST', action: `/internal/projects/${projectId}/restore` },
        )
      },
      [restoreFetcher, project],
    )

    const renameProject = React.useCallback(
      (projectId: string, newTitle: string) => {
        renameFetcher.submit(
          operationRename(project.proj_id, slugify(newTitle, SLUGIFY_OPTIONS)),
          { title: newTitle },
          { method: 'POST', action: `/internal/projects/${projectId}/rename` },
        )
      },
      [renameFetcher, project],
    )

    const projectEditorLink = useProjectEditorLink()
    const copyProjectLink = useCopyProjectLinkToClipboard()

    const actions = React.useMemo(
      () => ({
        open: contextMenuEntry('Open', (selectedProject) => {
          window.open(projectEditorLink(selectedProject.proj_id), '_blank')
        }),
        copyLink: contextMenuEntry('Copy Link', (selectedProject) =>
          copyProjectLink(selectedProject.proj_id),
        ),
        fork: contextMenuEntry('Fork', (selectedProject) => {
          window.open(projectEditorLink(selectedProject.proj_id) + '/?fork=true', '_blank')
        }),
        rename: contextMenuEntry('Rename', (selectedProject) => {
          const newTitle = window.prompt('New title:', selectedProject.title)
          if (newTitle != null) {
            renameProject(selectedProject.proj_id, newTitle)
          }
        }),
        delete: contextMenuEntry('Archive', (selectedProject) => {
          deleteProject(selectedProject.proj_id)
        }),
        restore: contextMenuEntry('Unarchive', (selectedProject) => {
          restoreProject(selectedProject.proj_id)
        }),
        destroy: contextMenuEntry('Delete Permanently', (selectedProject) => {
          destroyProject(selectedProject.proj_id)
        }),
      }),
      [
        projectEditorLink,
        copyProjectLink,
        renameProject,
        deleteProject,
        restoreProject,
        destroyProject,
      ],
    )

    const menuEntries = React.useMemo((): ContextMenuEntry[] => {
      switch (selectedCategory) {
        case 'allProjects':
        case 'public':
        case 'sharing':
        case 'private':
          return [
            actions.open,
            'separator',
            actions.copyLink,
            actions.fork,
            'sharing-dialog',
            'separator',
            actions.rename,
            actions.delete,
          ]
        case 'sharedWithMe':
          return [actions.open, 'separator', actions.copyLink, actions.fork]
        case 'archive':
          return [actions.restore, 'separator', actions.destroy]
        default:
          assertNever(selectedCategory)
      }
    }, [selectedCategory, actions])

    const onOpenShareDialog = useOpenShareDialog(project.proj_id)

    return (
      <ContextMenu.Content style={{ width: 170 }}>
        {menuEntries.map((entry, index) => {
          if (entry === 'separator') {
            return (
              <Separator
                key={`separator-${index}`}
                size='4'
                style={{ marginTop: 5, marginBottom: 5 }}
              />
            )
          }
          if (entry === 'sharing-dialog') {
            return (
              <ContextMenu.Item
                key={`entry-${index}`}
                style={{ height: 28, fontSize: 12 }}
                onSelect={onOpenShareDialog}
              >
                <Flex justify={'between'} align={'center'} width={'100%'}>
                  <Text>Sharing</Text>
                  {when(
                    project.hasPendingRequests === true,
                    <DotFilledIcon color='red' height={22} width={22} />,
                  )}
                </Flex>
              </ContextMenu.Item>
            )
          }
          return (
            <ContextMenu.Item
              key={`entry-${index}`}
              /* eslint-disable-next-line react/jsx-no-bind */
              onSelect={() => entry.onClick(project)}
              style={{ height: 28, fontSize: 12 }}
              color={entry.text === 'Delete Permanently' ? 'red' : undefined}
            >
              {entry.text}
            </ContextMenu.Item>
          )
        })}
      </ContextMenu.Content>
    )
  },
)
ProjectActionsMenu.displayName = 'ProjectActionsMenu'
