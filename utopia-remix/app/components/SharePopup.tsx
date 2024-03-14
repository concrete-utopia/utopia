import { Dialog, Flex, IconButton, Text, Button, DropdownMenu, Separator } from '@radix-ui/themes'
import {
  CaretDownIcon,
  Cross2Icon,
  GlobeIcon,
  LockClosedIcon,
  Link2Icon,
  PersonIcon,
} from '@radix-ui/react-icons'
import {
  asAccessLevel,
  operationApproveAccessRequest,
  operationChangeAccess,
  type ProjectWithoutContent,
  AccessRequestStatus,
  type ProjectAccessRequestWithUserDetails,
} from '../types'
import { AccessLevel } from '../types'
import { useFetcherWithOperation } from '../hooks/useFetcherWithOperation'
import React from 'react'
import { when } from '../util/react-conditionals'
import moment from 'moment'

export function SharePopup({
  project,
  accessRequests,
}: {
  project: ProjectWithoutContent
  accessRequests: ProjectAccessRequestWithUserDetails[]
}) {
  const accessLevel = asAccessLevel(project.ProjectAccess?.access_level) ?? AccessLevel.PRIVATE

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

  const changeProjectAccessLevel = React.useCallback(
    (newAccessLevel: AccessLevel) => {
      changeAccessLevel(project.proj_id, newAccessLevel)
    },
    [changeAccessLevel, project],
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

  return (
    <>
      <Flex direction='column' style={{ gap: 20 }}>
        <Flex justify='between' align='center'>
          <Text size='3'>Project Sharing</Text>
          <Dialog.Close>
            <IconButton variant='ghost' color='gray'>
              <Cross2Icon width='18' height='18' />
            </IconButton>
          </Dialog.Close>
        </Flex>
        <Flex justify='between'>
          <Text size='1'>Project Visibility</Text>
          <VisibilityDropdown
            accessLevel={accessLevel}
            changeProjectAccessLevel={changeProjectAccessLevel}
          />
        </Flex>
        {when(
          accessRequests.length > 0,
          <>
            <Separator />
            <AccessRequests
              project={project}
              approveAccessRequest={approveAccessRequest}
              accessRequests={accessRequests}
            />
          </>,
        )}
      </Flex>
    </>
  )
}

function AccessRequests({
  project,
  approveAccessRequest,
  accessRequests,
}: {
  project: ProjectWithoutContent
  approveAccessRequest: (projectId: string, tokenId: string) => void
  accessRequests: ProjectAccessRequestWithUserDetails[]
}) {
  return accessRequests
    .sort((a, b) => {
      if (a.status !== b.status) {
        return a.status - b.status
      }
      return moment(a.updated_at).unix() - moment(b.updated_at).unix()
    })
    .map((request) => {
      function onApprove() {
        approveAccessRequest(project.proj_id, request.token)
      }
      const status = request.status
      return (
        <Flex key={request.token} justify='between'>
          <Text size='1'>{request.User?.name ?? request.User?.email ?? request.user_id}</Text>
          {status === AccessRequestStatus.PENDING ? (
            <Button size='1' variant='ghost' onClick={onApprove}>
              Approve
            </Button>
          ) : (
            <Text size='1' color={status === AccessRequestStatus.APPROVED ? 'green' : 'red'}>
              {status === AccessRequestStatus.APPROVED ? 'Approved' : 'Rejected'}
            </Text>
          )}
        </Flex>
      )
    })
}

const VisibilityUIComponents = {
  [AccessLevel.PUBLIC]: { text: 'Public', icon: <GlobeIcon width='16' height='16' /> },
  [AccessLevel.PRIVATE]: { text: 'Private', icon: <LockClosedIcon width='16' height='16' /> },
  [AccessLevel.COLLABORATIVE]: {
    text: 'Collaborative',
    icon: <PersonIcon width='16' height='16' />,
  },
  [AccessLevel.WITH_LINK]: {
    text: 'With Link',
    icon: <Link2Icon width='16' height='16' />,
  },
}

function VisibilityDropdown({
  accessLevel,
  changeProjectAccessLevel,
}: {
  accessLevel: AccessLevel
  changeProjectAccessLevel: (newAccessLevel: AccessLevel) => void
}) {
  return (
    <DropdownMenu.Root>
      <DropdownMenu.Trigger>
        <Button
          color='gray'
          variant='ghost'
          highContrast
          style={{ fontSize: 12, display: 'flex', flexDirection: 'row', gap: 10 }}
        >
          {VisibilityUIComponents[accessLevel].icon}
          {VisibilityUIComponents[accessLevel].text}
          <CaretDownIcon />
        </Button>
      </DropdownMenu.Trigger>
      <DropdownMenu.Content>
        {[AccessLevel.PUBLIC, AccessLevel.PRIVATE, AccessLevel.COLLABORATIVE].map((level) => {
          function onCheckedChange() {
            if (accessLevel === level) {
              return
            }
            changeProjectAccessLevel(level)
          }
          return (
            <DropdownMenu.CheckboxItem
              key={level}
              style={{ height: 28, fontSize: 12, paddingLeft: 30 }}
              checked={accessLevel === level}
              disabled={accessLevel === level}
              onCheckedChange={onCheckedChange}
            >
              {VisibilityUIComponents[level].text}
            </DropdownMenu.CheckboxItem>
          )
        })}
      </DropdownMenu.Content>
    </DropdownMenu.Root>
  )
}
