import React from 'react'
import { useProjectsStore } from '../store'
import type { ProjectAccessRequestWithUserDetails } from '../types'
import {
  AccessRequestStatus,
  operationDelete,
  operationDestroy,
  operationRename,
  operationRestore,
} from '../types'
import type { ProjectWithoutContent } from '../types'
import { assertNever } from '../util/assertNever'
import { useProjectEditorLink } from '../util/links'
import { useFetcherWithOperation } from '../hooks/useFetcherWithOperation'
import slugify from 'slugify'
import { SLUGIFY_OPTIONS } from '../routes/internal.projects.$id.rename'
import { ContextMenu, Separator, Dialog, Flex, Text } from '@radix-ui/themes'
import { DotFilledIcon } from '@radix-ui/react-icons'
import { SharingDialog } from './sharingDialog'
import { when } from '~/util/react-conditionals'
import { useCopyProjectLinkToClipboard } from '../util/use-copy-project-link'

type ContextMenuEntry =
  | {
      text: string
      onClick: (project: ProjectWithoutContent) => void
    }
  | 'separator'
  | 'sharing-dialog'

export const ProjectActionsMenu = React.memo(
  ({
    project,
    accessRequests,
  }: {
    project: ProjectWithoutContent
    accessRequests: ProjectAccessRequestWithUserDetails[]
  }) => {
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
    const copyProjectLink = useCopyProjectLinkToClipboard()

    const menuEntries = React.useMemo((): ContextMenuEntry[] => {
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
              onClick: (selectedProject) => copyProjectLink(selectedProject.proj_id),
            },
            {
              text: 'Fork',
              onClick: (selectedProject) => {
                window.open(projectEditorLink(selectedProject.proj_id) + '/?fork=true', '_blank')
              },
            },
            'sharing-dialog',
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
              text: 'Delete Permanently',
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
      copyProjectLink,
    ])

    const preventDefault = React.useCallback((event: Event) => {
      event.preventDefault()
    }, [])

    const pendingAccessRequests = React.useMemo(
      () => accessRequests.filter((r) => r.status === AccessRequestStatus.PENDING),
      [accessRequests],
    )

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
              <Dialog.Root key={`separator-${index}`}>
                <Dialog.Trigger>
                  <ContextMenu.Item style={{ height: 28, fontSize: 12 }} onSelect={preventDefault}>
                    <Flex justify={'between'} align={'center'} width={'100%'}>
                      <Text>Share</Text>
                      {when(
                        pendingAccessRequests.length > 0,
                        <DotFilledIcon color='red' height={22} width={22} />,
                      )}
                    </Flex>
                  </ContextMenu.Item>
                </Dialog.Trigger>
                <Dialog.Content>
                  <SharingDialog project={project} accessRequests={accessRequests} />
                </Dialog.Content>
              </Dialog.Root>
            )
          }
          return (
            <ContextMenu.Item
              key={`entry-${index}`}
              /* eslint-disable-next-line react/jsx-no-bind */
              onSelect={() => entry.onClick(project)}
              style={{ height: 28, fontSize: 12 }}
              color={
                entry.text === 'Delete Permanently' || entry.text === 'Delete' ? 'red' : undefined
              }
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
