import * as DropdownMenu from '@radix-ui/react-dropdown-menu'
import React from 'react'
import { useProjectsStore } from '../store'
import { contextMenuDropdown, contextMenuItem } from '../styles/contextMenu.css'
import { sprinkles } from '../styles/sprinkles.css'
import type { ProjectAccessRequestWithUserDetails, ProjectWithoutContent } from '../types'
import {
  AccessRequestStatus,
  operationDelete,
  operationDestroy,
  operationRename,
  operationRestore,
} from '../types'
import { assertNever } from '../util/assertNever'
import { useProjectEditorLink } from '../util/links'
import { useFetcherWithOperation } from '../hooks/useFetcherWithOperation'
import slugify from 'slugify'
import { SLUGIFY_OPTIONS } from '../routes/internal.projects.$id.rename'
import { SharePopup } from './SharePopup'
import { ContextMenu, Dialog, Flex, Text } from '@radix-ui/themes'
import { DotFilledIcon } from '@radix-ui/react-icons'

type ContextMenuEntry =
  | {
      text: string
      onClick: (project: ProjectWithoutContent) => void
    }
  | 'separator'
  | 'sharing-dialog'

export const ProjectContextMenu = React.memo(
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

    const pendingAccessRequests = React.useMemo(
      () => accessRequests.filter((r) => r.status === AccessRequestStatus.PENDING),
      [accessRequests],
    )

    return (
      <DropdownMenu.Portal>
        <DropdownMenu.Content className={contextMenuDropdown()} align='end' sideOffset={5}>
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
            if (entry === 'sharing-dialog') {
              return (
                <Dialog.Root key={`separator-${index}`}>
                  <Dialog.Trigger>
                    <DropdownMenu.Item
                      style={{ height: 28, fontSize: 12 }}
                      onSelect={preventDefault}
                      className={contextMenuItem()}
                    >
                      <Flex justify={'between'} align={'center'} width={'100%'}>
                        <Text>Share</Text>
                        {pendingAccessRequests.length > 0 ? (
                          <DotFilledIcon color={'#DB9A9A'} height={22} width={22} />
                        ) : null}
                      </Flex>
                    </DropdownMenu.Item>
                  </Dialog.Trigger>
                  <Dialog.Content>
                    <SharePopup project={project} accessRequests={accessRequests} />
                  </Dialog.Content>
                </Dialog.Root>
              )
            }
            function onClick() {
              if (entry != null && typeof entry !== 'string') {
                entry.onClick(project)
              }
            }
            return (
              <DropdownMenu.Item
                key={`entry-${index}`}
                onClick={onClick}
                className={contextMenuItem()}
              >
                {entry.text}
              </DropdownMenu.Item>
            )
          })}
        </DropdownMenu.Content>
      </DropdownMenu.Portal>
    )
  },
)
ProjectContextMenu.displayName = 'ProjectContextMenu'
