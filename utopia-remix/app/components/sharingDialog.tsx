import {
  CaretDownIcon,
  CheckIcon,
  ChevronDownIcon,
  Cross2Icon,
  GlobeIcon,
  Link2Icon,
  LockClosedIcon,
  MinusCircledIcon,
  PersonIcon,
} from '@radix-ui/react-icons'
import { Button, Dialog, DropdownMenu, Flex, IconButton, Separator, Text } from '@radix-ui/themes'
import { AnimatePresence, motion } from 'framer-motion'
import moment from 'moment'
import React from 'react'
import { useFetcherDataUnkown } from '../hooks/useFetcherData'
import { useFetcherWithOperation } from '../hooks/useFetcherWithOperation'
import { useProjectAccessMatchesSelectedCategory } from '../hooks/useProjectMatchingCategory'
import type {
  SharingProjectAccessRequests,
  SharingProjectAccessRequestsState,
} from '../stores/projectsStore'
import { useProjectsStore } from '../stores/projectsStore'
import { sprinkles } from '../styles/sprinkles.css'
import type { ProjectSharingDetails, UpdateAccessRequestAction } from '../types'
import {
  AccessLevel,
  AccessRequestStatus,
  asAccessLevel,
  mustAccessRequestStatus,
  operationChangeAccess,
  operationUpdateAccessRequest,
  type ProjectAccessRequestWithUserDetails,
  type ProjectListing,
} from '../types'
import { assertNever } from '../util/assertNever'
import { useCopyProjectLinkToClipboard } from '../util/copyProjectLink'
import { isLikeApiError } from '../util/errors'
import { useProjectEditorLink } from '../util/links'
import { unless, when } from '../util/react-conditionals'
import { Spinner } from './spinner'
import { UserAvatar } from './userAvatar'

export const SharingDialogWrapper = React.memo(
  ({ project }: { project: ProjectListing | null }) => {
    const sharingProjectId = useProjectsStore((store) => store.sharingProjectId)
    const setSharingProjectId = useProjectsStore((store) => store.setSharingProjectId)

    const onOpenChange = React.useCallback(
      (open: boolean) => {
        // Note: this _only_ reacts to Radix internal changes, so concretely meaning only for open===false calls.
        if (!open) {
          setSharingProjectId(null)
        }
      },
      [setSharingProjectId],
    )

    const accessRequests = useProjectsStore((store) => store.sharingProjectAccessRequests)

    return (
      <Dialog.Root open={sharingProjectId === project?.proj_id} onOpenChange={onOpenChange}>
        <Dialog.Content>
          <SharingDialog project={project} accessRequests={accessRequests} />
        </Dialog.Content>
      </Dialog.Root>
    )
  },
)
SharingDialogWrapper.displayName = 'SharingDialogWrapper'

type SharingDialogProps = {
  project: ProjectListing | null
  accessRequests: SharingProjectAccessRequests
}

export const SharingDialog = React.memo((props: SharingDialogProps) => {
  const { project, accessRequests } = props
  const setSharingProjectId = useProjectsStore((store) => store.setSharingProjectId)

  const projectAccessLevel = React.useMemo(() => {
    return asAccessLevel(project?.ProjectAccess?.access_level) ?? AccessLevel.PRIVATE
  }, [project])

  const [accessLevel, setAccessLevel] = React.useState<AccessLevel>(projectAccessLevel)

  const projectAccessMatchesSelectedCategory = useProjectAccessMatchesSelectedCategory(
    asAccessLevel(project?.ProjectAccess?.access_level),
  )

  const changeAccessFetcherCallback = React.useCallback(
    (data: unknown) => {
      if (isLikeApiError(data)) {
        setAccessLevel(projectAccessLevel)
      }
      if (!projectAccessMatchesSelectedCategory) {
        setSharingProjectId(null)
      }
    },
    [setSharingProjectId, projectAccessMatchesSelectedCategory, projectAccessLevel],
  )

  const changeAccessFetcher = useFetcherWithOperation(project?.proj_id ?? null, 'changeAccess')
  useFetcherDataUnkown(changeAccessFetcher, changeAccessFetcherCallback)

  const changeProjectAccessLevel = React.useCallback(
    (newAccessLevel: AccessLevel) => {
      if (project == null) {
        return
      }
      setAccessLevel(newAccessLevel)
      changeAccessFetcher.submit(
        operationChangeAccess(project.proj_id, newAccessLevel),
        { accessLevel: newAccessLevel.toString() },
        { method: 'POST', action: `/internal/projects/${project.proj_id}/access` },
      )
    },
    [changeAccessFetcher, project],
  )

  if (project == null) {
    return null
  }

  return (
    <SharingDialogContent
      project={{ ...project, ProjectAccessRequest: accessRequests.requests }}
      accessRequestsState={accessRequests.state}
      accessLevel={accessLevel}
      changeProjectAccessLevel={changeProjectAccessLevel}
      asDialog={true}
    />
  )
})
SharingDialog.displayName = 'SharingDialog'

export const SharingDialogContent = React.memo(
  ({
    project,
    accessRequestsState,
    accessLevel,
    changeProjectAccessLevel,
    asDialog,
  }: {
    project: ProjectSharingDetails
    accessRequestsState: SharingProjectAccessRequestsState
    accessLevel: AccessLevel
    changeProjectAccessLevel: (newAccessLevel: AccessLevel) => void
    asDialog: boolean
  }) => {
    return (
      <Flex direction='column' style={{ maxHeight: '75vh', padding: asDialog ? 0 : 14 }} gap='4'>
        <Flex direction='column' gap='4'>
          <Flex justify='between' align='center' gap='2'>
            <Flex align={'center'} gap='2'>
              <Text size='3'>Project Sharing</Text>
              <AnimatePresence>
                {when(
                  accessRequestsState === 'loading',
                  <motion.div style={{ opacity: 0.1 }} exit={{ opacity: 0 }}>
                    <Spinner className={sprinkles({ backgroundColor: 'black' })} />
                  </motion.div>,
                )}
              </AnimatePresence>
            </Flex>
            {when(
              asDialog,
              <Dialog.Close>
                <IconButton variant='ghost' color='gray'>
                  <Cross2Icon width='18' height='18' />
                </IconButton>
              </Dialog.Close>,
            )}
          </Flex>
          <Flex justify='between' align='center' style={{ paddingTop: 4 }}>
            <Text size='1'>Project Visibility</Text>
            <VisibilityDropdown
              accessLevel={accessLevel}
              changeProjectAccessLevel={changeProjectAccessLevel}
            />
          </Flex>
          {when(
            accessLevel === AccessLevel.COLLABORATIVE || accessLevel === AccessLevel.PUBLIC,
            <ProjectLink projectId={project.proj_id} />,
          )}
        </Flex>
        <Separator size='4' />
        <AccessRequestsList projectId={project.proj_id} accessLevel={accessLevel} />
      </Flex>
    )
  },
)
SharingDialogContent.displayName = 'SharingDialogContent'

type AccessRequestListProps = {
  projectId: string
  accessLevel: AccessLevel
}

const AccessRequestsList = React.memo(({ projectId, accessLevel }: AccessRequestListProps) => {
  const accessRequests = useProjectsStore((store) => store.sharingProjectAccessRequests)

  const hasGonePrivate = React.useMemo(() => {
    return accessRequests.requests.length > 0 && accessLevel === AccessLevel.PRIVATE
  }, [accessLevel, accessRequests])

  const showAccessRequests = React.useMemo(() => {
    return accessRequests.state === 'ready' && accessRequests.requests.length > 0
  }, [accessRequests])

  return (
    <AnimatePresence>
      <motion.div style={{ flex: '1 1' }}>
        <Flex direction={'column'} gap='4' style={{}}>
          {when(hasGonePrivate, <HasGonePrivate />)}

          <OwnerCollaboratorRow />

          {when(
            showAccessRequests,
            <motion.div
              animate={{ height: 'auto', opacity: 1 }}
              initial={{ height: 0, opacity: 0 }}
              exit={{ height: 0, opacity: 0 }}
            >
              <Flex direction={'column'} gap='4'>
                <AccessRequests
                  projectId={projectId}
                  projectAccessLevel={accessLevel}
                  accessRequests={accessRequests.requests}
                />
              </Flex>
            </motion.div>,
          )}
        </Flex>
      </motion.div>
    </AnimatePresence>
  )
})
AccessRequestsList.displayName = 'AccessRequestsList'

const HasGonePrivate = React.memo(() => {
  return (
    <Text size='1' style={{ opacity: 0.5 }}>
      This project was changed to private, previous collaborators can no longer view it.
    </Text>
  )
})
HasGonePrivate.displayName = 'HasGonePrivate'

const OwnerCollaboratorRow = React.memo(() => {
  const myUser = useProjectsStore((store) => store.myUser)
  if (myUser == null) {
    return null
  }

  return (
    <CollaboratorRow
      picture={myUser.picture ?? null}
      name={`${myUser.name ?? myUser.email ?? myUser.id} (you)`}
      starBadge={true}
    >
      <Text size='1' style={{ cursor: 'default' }}>
        Owner
      </Text>
    </CollaboratorRow>
  )
})
OwnerCollaboratorRow.displayName = 'OwnerCollaboratorRow'

function AccessRequests(props: {
  projectId: string
  projectAccessLevel: AccessLevel
  accessRequests: ProjectAccessRequestWithUserDetails[]
}) {
  const { projectId, projectAccessLevel } = props

  // the access requests, including in-flight optimistic statuses
  const [accessRequests, setAccessRequests] = React.useState(props.accessRequests)
  // the last successfully-obtained access requests that can be used to roll-back in case of issues when updating requests
  const [previousAccessRequests, setPreviousAccessRequests] = React.useState(props.accessRequests)

  React.useEffect(() => {
    setAccessRequests(props.accessRequests)
    setPreviousAccessRequests(props.accessRequests)
  }, [props.accessRequests])

  const updateAccessRequestFetcher = useFetcherWithOperation(projectId, 'updateAccessRequest')
  const onUpdateAccessRequest = React.useCallback(
    (token: string, action: UpdateAccessRequestAction) => () => {
      setPreviousAccessRequests(accessRequests)
      setAccessRequests((reqs) => {
        switch (action) {
          case 'destroy':
            return reqs.filter((r) => r.token !== token)
          case 'approve':
            return reqs.map((r) =>
              r.token === token ? { ...r, status: AccessRequestStatus.APPROVED } : r,
            )
          case 'reject':
            return reqs.map((r) =>
              r.token === token ? { ...r, status: AccessRequestStatus.REJECTED } : r,
            )
          default:
            assertNever(action)
        }
      })
      updateAccessRequestFetcher.submit(
        operationUpdateAccessRequest(projectId, token, action),
        { tokenId: token },
        {
          method: 'POST',
          action: `/internal/projects/${projectId}/access/request/${token}/${action}`,
        },
      )
    },
    [updateAccessRequestFetcher, projectId, accessRequests],
  )
  const resetAccessRequests = React.useCallback(
    (data: unknown) => {
      if (isLikeApiError(data)) {
        setAccessRequests(previousAccessRequests)
      }
    },
    [previousAccessRequests],
  )
  useFetcherDataUnkown(updateAccessRequestFetcher, resetAccessRequests)

  const isCollaborative = React.useMemo(() => {
    return projectAccessLevel === AccessLevel.COLLABORATIVE
  }, [projectAccessLevel])

  return accessRequests
    .sort((a, b) => {
      return moment(a.created_at).unix() - moment(b.created_at).unix()
    })
    .map((request) => {
      const user = request.User
      if (user == null) {
        return null
      }

      const status = mustAccessRequestStatus(request.status)

      return (
        <CollaboratorRow
          key={`collaborator-${request.token}`}
          picture={user.picture}
          name={user.name ?? user.email ?? user.user_id}
          isDisabled={!isCollaborative}
        >
          {when(
            isCollaborative,
            <AccessRequestDropdown
              status={request.status}
              onApprove={onUpdateAccessRequest(request.token, 'approve')}
              onReject={onUpdateAccessRequest(request.token, 'reject')}
              onDestroy={onUpdateAccessRequest(request.token, 'destroy')}
            />,
          )}
          {unless(
            isCollaborative,
            <Text
              size='1'
              style={{
                cursor: 'default',
                fontStyle: !isCollaborative ? 'italic' : 'normal',
              }}
            >
              {when(status === AccessRequestStatus.PENDING, 'Pending')}
              {when(status === AccessRequestStatus.APPROVED, 'Collaborator')}
              {when(status === AccessRequestStatus.REJECTED, 'Blocked')}
            </Text>,
          )}
        </CollaboratorRow>
      )
    })
}

const AccessRequestDropdown = React.memo(
  ({
    status,
    onApprove,
    onReject,
    onDestroy,
  }: {
    status: AccessRequestStatus
    onApprove: () => void
    onReject: () => void
    onDestroy: () => void
  }) => {
    return (
      <DropdownMenu.Root>
        <DropdownMenu.Trigger>
          {/* this needs to be inlined (and as a ternary) because DropdownMenu.Trigger requires a direct single child */}
          {status === AccessRequestStatus.PENDING ? (
            <Button size='1' color='amber' radius='full'>
              Requests Access
              <ChevronDownIcon />
            </Button>
          ) : status === AccessRequestStatus.APPROVED ? (
            <Button size='1' radius='medium' variant='ghost' color='gray' highContrast={true}>
              Collaborator
              <ChevronDownIcon />
            </Button>
          ) : status === AccessRequestStatus.REJECTED ? (
            <Button size='1' radius='medium' variant='ghost' color='red'>
              Denied Access
              <ChevronDownIcon />
            </Button>
          ) : null}
        </DropdownMenu.Trigger>
        <DropdownMenu.Content>
          {when(
            status !== AccessRequestStatus.REJECTED,
            <DropdownMenu.Item style={{ height: 28 }} color={'red'} onClick={onReject}>
              <Flex align='center' gap='2'>
                <Cross2Icon />
                <Text size='1'>Block</Text>
              </Flex>
            </DropdownMenu.Item>,
          )}
          {when(
            status !== AccessRequestStatus.APPROVED,
            <DropdownMenu.Item style={{ height: 28 }} onClick={onApprove}>
              <Flex align='center' gap='2'>
                <CheckIcon />
                <Text size='1'>Allow To Collaborate</Text>
              </Flex>
            </DropdownMenu.Item>,
          )}
          <DropdownMenu.Separator />
          <DropdownMenu.Item style={{ height: 28 }} color='gray' onClick={onDestroy}>
            <Flex align='center' gap='2'>
              <MinusCircledIcon />
              <Text size='1'>
                {status === AccessRequestStatus.PENDING ? 'Delete Request' : 'Remove From Project'}
              </Text>
            </Flex>
          </DropdownMenu.Item>
        </DropdownMenu.Content>
      </DropdownMenu.Root>
    )
  },
)
AccessRequestDropdown.displayName = 'AccessRequestDropdown'

const CollaboratorRow = React.memo(
  ({
    picture,
    name,
    children,
    isDisabled,
    starBadge,
  }: {
    picture: string | null
    name: string
    children: React.ReactNode
    isDisabled?: boolean
    starBadge?: boolean
  }) => {
    return (
      <Flex
        justify='between'
        style={{
          filter: isDisabled === true ? 'grayscale(1)' : undefined,
          opacity: isDisabled === true ? 0.5 : 1,
        }}
      >
        <Flex align='center' gap='2'>
          <UserAvatar
            picture={picture}
            starBadge={starBadge}
            size={22}
            name={name}
            className={sprinkles({ borderRadius: 'full' })}
          />
          <Text size='1'>{name}</Text>
        </Flex>
        {children}
      </Flex>
    )
  },
)
CollaboratorRow.displayName = 'CollaboratorRow'

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
  const onCheckedChange = React.useCallback(
    (newLevel: AccessLevel) => () => {
      if (accessLevel === newLevel) {
        return
      }
      changeProjectAccessLevel(newLevel)
    },
    [changeProjectAccessLevel, accessLevel],
  )
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
          return (
            <DropdownMenu.CheckboxItem
              key={level}
              style={{ height: 28, fontSize: 12, paddingLeft: 30 }}
              checked={accessLevel === level}
              disabled={accessLevel === level}
              onCheckedChange={onCheckedChange(level)}
            >
              {VisibilityUIComponents[level].text}
            </DropdownMenu.CheckboxItem>
          )
        })}
      </DropdownMenu.Content>
    </DropdownMenu.Root>
  )
}

const ProjectLink = React.memo(({ projectId }: { projectId: string }) => {
  const projectLinkRef = React.useRef<HTMLInputElement | null>(null)

  const projectLink = useProjectEditorLink()
  const copyProjectLink = useCopyProjectLinkToClipboard()

  const onClickCopyProjectLink = React.useCallback(() => {
    copyProjectLink(projectId)
    projectLinkRef.current?.select()
  }, [projectId, copyProjectLink])

  return (
    <Flex style={{ gap: 10 }}>
      <Button
        style={{ fontSize: 11, flex: 1, justifyContent: 'flex-start', cursor: 'default' }}
        disabled
      >
        {projectLink(projectId)}
      </Button>
      <Button style={{ fontSize: 11 }} onClick={onClickCopyProjectLink}>
        Copy Link
      </Button>
    </Flex>
  )
})
ProjectLink.displayName = 'ProjectLink'
