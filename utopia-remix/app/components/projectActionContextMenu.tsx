import React from 'react'
import { useProjectsStore } from '../store'
import {
  ProjectWithoutContent,
  operationChangeAccess,
  operationDelete,
  operationDestroy,
  operationRename,
  operationRestore,
} from '../types'
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
    [deleteFetcher],
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
    [destroyFetcher],
  )

  const restoreProject = React.useCallback(
    (projectId: string) => {
      restoreFetcher.submit(
        operationRestore(project),
        {},
        { method: 'POST', action: `/internal/projects/${projectId}/restore` },
      )
    },
    [restoreFetcher],
  )

  const renameProject = React.useCallback(
    (projectId: string, newTitle: string) => {
      renameFetcher.submit(
        operationRename(project, slugify(newTitle, SLUGIFY_OPTIONS)),
        { title: newTitle },
        { method: 'POST', action: `/internal/projects/${projectId}/rename` },
      )
    },
    [renameFetcher],
  )

  const changeAccessLevel = React.useCallback(
    (projectId: string, accessLevel: AccessLevel) => {
      changeAccessFetcher.submit(
        operationChangeAccess(project, accessLevel),
        { accessLevel: accessLevel.toString() },
        { method: 'POST', action: `/internal/projects/${projectId}/access` },
      )
    },
    [changeAccessFetcher],
  )
  const projectEditorLink = useProjectEditorLink()

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
          'sharing-dialog',
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
            text: 'Delete Permanently...',
            onClick: (project) => {
              destroyProject(project.proj_id)
            },
          },
        ]
      default:
        assertNever(selectedCategory)
    }
  }, [selectedCategory, accessLevel, projectEditorLink])

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
            <Dialog.Root>
              <Dialog.Trigger>
                <ContextMenu.Item
                  style={{ height: 28, fontSize: 12 }}
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
