import React from 'react'
import { useProjectsStore } from '../store'
import {
  operationChangeAccess,
  operationDelete,
  operationDestroy,
  operationRename,
  operationRestore,
} from '../types'
import type { ProjectWithoutContent } from '../types'
import { assertNever } from '../util/assertNever'
import { AccessLevel } from '../types'
import { useProjectEditorLink } from '../util/links'
import { useFetcherWithOperation } from '../hooks/useFetcherWithOperation'
import slugify from 'slugify'
import { SLUGIFY_OPTIONS } from '../routes/internal.projects.$id.rename'
import {
  ContextMenu,
  Separator,
  Dialog,
  Flex,
  IconButton,
  Text,
  Button,
  DropdownMenu,
} from '@radix-ui/themes'
import { CaretDownIcon, Cross2Icon, GlobeIcon, LockClosedIcon } from '@radix-ui/react-icons'

type ContextMenuEntry =
  | {
      text: string
      onClick: (project: ProjectWithoutContent) => void
    }
  | 'separator'
  | 'sharing-dialog'

export const ProjectActionsMenu = React.memo(({ project }: { project: ProjectWithoutContent }) => {
  const deleteFetcher = useFetcherWithOperation(project.proj_id, 'delete')
  const destroyFetcher = useFetcherWithOperation(project.proj_id, 'destroy')
  const restoreFetcher = useFetcherWithOperation(project.proj_id, 'restore')
  const renameFetcher = useFetcherWithOperation(project.proj_id, 'rename')
  const changeAccessFetcher = useFetcherWithOperation(project.proj_id, 'changeAccess')

  const selectedCategory = useProjectsStore((store) => store.selectedCategory)
  let accessLevel = project.ProjectAccess?.access_level ?? AccessLevel.PRIVATE

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

  const changeAccessLevel = React.useCallback(
    (projectId: string, newAccessLevel: AccessLevel) => {
      changeAccessFetcher.submit(
        operationChangeAccess(project, newAccessLevel),
        { accessLevel: newAccessLevel.toString() },
        { method: 'POST', action: `/internal/projects/${projectId}/access` },
      )
    },
    [changeAccessFetcher, project],
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
            text: 'Delete Permanently...',
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
                <ContextMenu.Item
                  style={{ height: 28, fontSize: 12 }}
                  /* eslint-disable-next-line react/jsx-no-bind */
                  onSelect={(event) => event.preventDefault()}
                >
                  Share
                </ContextMenu.Item>
              </Dialog.Trigger>
              <Dialog.Content>
                <Flex direction='column' style={{ gap: 20 }}>
                  <Flex justify='between' align='center'>
                    <Text size='1'>Project Sharing</Text>
                    <Dialog.Close>
                      <IconButton variant='ghost' color='gray'>
                        <Cross2Icon width='18' height='18' />
                      </IconButton>
                    </Dialog.Close>
                  </Flex>
                  <Flex justify='between'>
                    <DropdownMenu.Root>
                      <DropdownMenu.Trigger>
                        <Button
                          color='gray'
                          variant='ghost'
                          highContrast
                          style={{ fontSize: 12, display: 'flex', flexDirection: 'row', gap: 10 }}
                        >
                          {accessLevel === AccessLevel.PUBLIC ? (
                            <GlobeIcon width='16' height='16' />
                          ) : (
                            <LockClosedIcon width='16' height='16' />
                          )}
                          {accessLevel === AccessLevel.PUBLIC ? 'Public' : 'Private'}
                          <CaretDownIcon />
                        </Button>
                      </DropdownMenu.Trigger>
                      <DropdownMenu.Content>
                        <DropdownMenu.CheckboxItem
                          style={{ height: 28, fontSize: 12, paddingLeft: 30 }}
                          checked={accessLevel === AccessLevel.PUBLIC}
                          disabled={accessLevel === AccessLevel.PUBLIC ? true : false}
                          /* eslint-disable-next-line react/jsx-no-bind */
                          onCheckedChange={() => {
                            if (accessLevel === AccessLevel.PUBLIC) {
                              return
                            }
                            changeAccessLevel(project.proj_id, AccessLevel.PUBLIC)
                          }}
                        >
                          {accessLevel === AccessLevel.PUBLIC ? 'Public' : 'Make Public'}
                        </DropdownMenu.CheckboxItem>
                        <DropdownMenu.CheckboxItem
                          style={{ height: 28, fontSize: 12, paddingLeft: 30 }}
                          checked={accessLevel === AccessLevel.PRIVATE}
                          disabled={accessLevel === AccessLevel.PRIVATE ? true : false}
                          /* eslint-disable-next-line react/jsx-no-bind */
                          onCheckedChange={() => {
                            if (accessLevel === AccessLevel.PRIVATE) {
                              return
                            }
                            changeAccessLevel(project.proj_id, AccessLevel.PRIVATE)
                          }}
                        >
                          {accessLevel === AccessLevel.PRIVATE ? 'Private' : 'Make Private'}
                        </DropdownMenu.CheckboxItem>
                      </DropdownMenu.Content>
                    </DropdownMenu.Root>
                  </Flex>
                </Flex>
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
              entry.text === 'Delete Permanently...' || entry.text === 'Delete' ? 'red' : undefined
            }
          >
            {entry.text}
          </ContextMenu.Item>
        )
      })}
    </ContextMenu.Content>
  )
})
ProjectActionsMenu.displayName = 'ProjectActionsMenu'
