import {
  CaretDownIcon,
  Cross2Icon,
  GlobeIcon,
  Link2Icon,
  LockClosedIcon,
  PersonIcon,
} from '@radix-ui/react-icons'
import { Button, Dialog, DropdownMenu, Flex, IconButton, Separator, Text } from '@radix-ui/themes'
import { AnimatePresence, motion } from 'framer-motion'
import moment from 'moment'
import React from 'react'
import { useFetcherDataUnkown } from '../hooks/useFetcherData'
import { useFetcherWithOperation } from '../hooks/useFetcherWithOperation'
import { useProjectAccessMatchesSelectedCategory } from '../hooks/useProjectMatchingCategory'
import { sprinkles } from '../styles/sprinkles.css'
import {
  AccessLevel,
  AccessRequestStatus,
  asAccessLevel,
  operationApproveAccessRequest,
  operationChangeAccess,
  type ProjectAccessRequestWithUserDetails,
  type ProjectListing,
} from '../types'
import { useCopyProjectLinkToClipboard } from '../util/copyProjectLink'
import { isLikeApiError } from '../util/errors'
import { useProjectEditorLink } from '../util/links'
import { when } from '../util/react-conditionals'
import { Spinner } from './spinner'
import { UserAvatar } from './collaboratorAvatar'
import { useProjectsStore } from '../stores/projectsStore'

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

    return (
      <Dialog.Root open={sharingProjectId === project?.proj_id} onOpenChange={onOpenChange}>
        <Dialog.Content>
          <SharingDialog project={project} />
        </Dialog.Content>
      </Dialog.Root>
    )
  },
)
SharingDialogWrapper.displayName = 'SharingDialogWrapper'

function SharingDialog({ project }: { project: ProjectListing | null }) {
  const setSharingProjectId = useProjectsStore((store) => store.setSharingProjectId)
  const accessRequests = useProjectsStore((store) => store.sharingProjectAccessRequests)

  const projectAccessLevel = React.useMemo(() => {
    return asAccessLevel(project?.ProjectAccess?.access_level) ?? AccessLevel.PRIVATE
  }, [project])

  const [accessLevel, setAccessLevel] = React.useState<AccessLevel>(projectAccessLevel)

  const projectAccessMatchesSelectedCategory = useProjectAccessMatchesSelectedCategory(project)

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
    <Flex direction='column' style={{ gap: 20 }}>
      <Flex justify='between' align='center'>
        <Flex align={'center'} gap='2'>
          <Text size='3'>Project Sharing</Text>
          <AnimatePresence>
            {when(
              accessRequests.state === 'loading',
              <motion.div style={{ opacity: 0.1 }} exit={{ opacity: 0 }}>
                <Spinner className={sprinkles({ backgroundColor: 'black' })} />
              </motion.div>,
            )}
          </AnimatePresence>
        </Flex>
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
        accessLevel === AccessLevel.COLLABORATIVE || accessLevel === AccessLevel.PUBLIC,
        <ProjectLink projectId={project.proj_id} />,
      )}
      <AccessRequestsList projectId={project.proj_id} accessLevel={accessLevel} />
    </Flex>
  )
}

const AccessRequestsList = React.memo(
  ({ projectId, accessLevel }: { projectId: string; accessLevel: AccessLevel }) => {
    const myUser = useProjectsStore((store) => store.myUser)
    const accessRequests = useProjectsStore((store) => store.sharingProjectAccessRequests)

    const approveAccessRequestFetcher = useFetcherWithOperation(projectId, 'approveAccessRequest')

    const approveAccessRequest = React.useCallback(
      (tokenId: string) => {
        approveAccessRequestFetcher.submit(
          operationApproveAccessRequest(projectId, tokenId),
          { tokenId: tokenId },
          {
            method: 'POST',
            action: `/internal/projects/${projectId}/access/request/${tokenId}/approve`,
          },
        )
      },
      [approveAccessRequestFetcher, projectId],
    )

    const hasGonePrivate = React.useMemo(() => {
      return accessRequests.requests.length > 0 && accessLevel === AccessLevel.PRIVATE
    }, [accessLevel, accessRequests])

    return (
      <AnimatePresence>
        {when(
          accessRequests.state === 'ready' && accessRequests.requests.length > 0,
          <motion.div
            animate={{ height: 'auto', opacity: 1 }}
            initial={{ height: 0, opacity: 0 }}
            exit={{ height: 0, opacity: 0 }}
          >
            <Flex direction={'column'} gap='4'>
              <Separator size='4' />
              {when(
                hasGonePrivate,
                <Text size='1' style={{ opacity: 0.5 }}>
                  This project was changed to private, previous collaborators can no longer view it.
                </Text>,
              )}
              {myUser != null ? (
                <CollaboratorRow
                  disabled={false}
                  picture={myUser.picture}
                  name={`${myUser.name ?? myUser.email ?? myUser.id} (you)`}
                  starBadge={true}
                >
                  <Text size='1' style={{ cursor: 'default' }}>
                    Owner
                  </Text>
                </CollaboratorRow>
              ) : null}
              <AccessRequests
                projectId={projectId}
                projectAccessLevel={accessLevel}
                approveAccessRequest={approveAccessRequest}
                accessRequests={accessRequests.requests}
              />
            </Flex>
          </motion.div>,
        )}
      </AnimatePresence>
    )
  },
)
AccessRequestsList.displayName = 'AccessRequestsList'

function AccessRequests({
  projectId,
  projectAccessLevel,
  approveAccessRequest,
  accessRequests,
}: {
  projectId: string
  projectAccessLevel: AccessLevel
  approveAccessRequest: (projectId: string, tokenId: string) => void
  accessRequests: ProjectAccessRequestWithUserDetails[]
}) {
  const onApprove = React.useCallback(
    (token: string) => () => {
      approveAccessRequest(projectId, token)
    },
    [projectId, approveAccessRequest],
  )

  const isDisabled = React.useMemo(
    () => projectAccessLevel === AccessLevel.PRIVATE,
    [projectAccessLevel],
  )

  return accessRequests
    .sort((a, b) => {
      if (a.status !== b.status) {
        return a.status - b.status
      }
      return moment(a.updated_at).unix() - moment(b.updated_at).unix()
    })
    .map((request) => {
      const user = request.User
      if (user == null) {
        return null
      }

      const status = request.status
      return (
        <CollaboratorRow
          key={request.token}
          picture={user.picture}
          name={user.name ?? user.email ?? user.user_id}
          disabled={isDisabled}
        >
          {status === AccessRequestStatus.PENDING ? (
            <Button size='1' variant='ghost' onClick={onApprove(request.token)}>
              Approve
            </Button>
          ) : (
            <Text
              size='1'
              color={status === AccessRequestStatus.APPROVED ? 'green' : 'red'}
              style={{
                fontStyle: isDisabled ? 'italic' : 'normal',
                cursor: 'default',
              }}
            >
              {status === AccessRequestStatus.APPROVED ? 'Approved' : 'Rejected'}
            </Text>
          )}
        </CollaboratorRow>
      )
    })
}

const CollaboratorRow = React.memo(
  ({
    picture,
    name,
    children,
    disabled,
    starBadge,
  }: {
    picture: string | null
    name: string
    children: React.ReactNode
    disabled: boolean
    starBadge?: boolean
  }) => {
    return (
      <Flex
        justify='between'
        style={{ filter: disabled ? 'grayscale(1)' : undefined, opacity: disabled ? 0.5 : 1 }}
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
              // eslint-disable-next-line react/jsx-no-bind
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
