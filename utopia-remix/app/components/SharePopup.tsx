import { Dialog, Flex, IconButton, Text, Button, DropdownMenu } from '@radix-ui/themes'
import { CaretDownIcon, Cross2Icon, GlobeIcon, LockClosedIcon } from '@radix-ui/react-icons'
import {
  operationApproveAccessRequest,
  operationChangeAccess,
  type ProjectWithoutContent,
} from '../types'
import { AccessLevel } from '../types'
import { useFetcherWithOperation } from '../hooks/useFetcherWithOperation'
import React from 'react'

export function SharePopup({ project }: { project: ProjectWithoutContent }) {
  let accessLevel = project.ProjectAccess?.access_level ?? AccessLevel.PRIVATE

  const changeAccessFetcher = useFetcherWithOperation(project.proj_id, 'changeAccess')
  const approveAccessRequestFetcher = useFetcherWithOperation(
    project.proj_id,
    'approveAccessRequest',
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

  const approveAccessRequest = React.useCallback(
    (projectId: string, tokenId: string) => {
      approveAccessRequestFetcher.submit(
        operationApproveAccessRequest(project, tokenId),
        { tokenId: tokenId },
        {
          method: 'POST',
          action: `/internal/projects/${projectId}/access/request/${tokenId}/approve`,
        },
      )
    },
    [approveAccessRequestFetcher, project],
  )

  const projectAccessRequests = project.ProjectAccessRequest ?? []

  return (
    <>
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
    </>
  )
}

// const shareOptions: ContextMenuEntry[] = useMemo(() => {
//   return [
//     accessLevel === AccessLevel.PRIVATE
//       ? {
//           text: 'Make Collaborative',
//           onClick: (selectedProject) => {
//             changeAccessLevel(selectedProject.proj_id, AccessLevel.COLLABORATIVE)
//           },
//         }
//       : null,
//     {
//       text: accessLevel === AccessLevel.PUBLIC ? 'Make Private' : 'Make Public',
//       onClick: (selectedProject) => {
//         changeAccessLevel(
//           selectedProject.proj_id,
//           accessLevel === AccessLevel.PUBLIC ? AccessLevel.PRIVATE : AccessLevel.PUBLIC,
//         )
//       },
//     },
//     ...projectAccessRequests
//       .filter((request) => request.status === AccessRequestStatus.PENDING)
//       .map((request) => ({
//         text: `Approve access to ${request.user_id}`,
//         onClick: (selectedProject: ProjectWithoutContent) => {
//           approveAccessRequest(selectedProject.proj_id, request.token)
//         },
//       })),
//   ]
// }, [changeAccessLevel, approveAccessRequest, project, accessLevel])
