import { Button } from '@radix-ui/themes'
import { useFetcher, useLocation } from '@remix-run/react'
import cx from 'classnames'
import type { UserDetails } from 'prisma-client'
import React, { useMemo } from 'react'
import { colors } from '../styles/sprinkles.css'
import { Status } from '../util/statusCodes'
import * as styles from './projectNotFound.css'
import { useCDNLink } from '../util/cdnLink'
import { useIsDarkMode } from '../hooks/useIsDarkMode'

export default function ProjectNotFound({
  projectId,
  user,
  status,
}: {
  projectId: string | null
  user: UserDetails | null
  status: number
}) {
  const showNotFound = status === Status.NOT_FOUND || projectId == null || user == null

  return (
    <div
      style={{
        width: '100vw',
        height: '100vh',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        gap: '70px',
        fontFamily: 'Inter, sans-serif',
        lineHeight: 'initial',
      }}
    >
      {showNotFound ? (
        <NotFoundPage projectId={projectId} user={user} />
      ) : (
        <UnauthorizedPage projectId={projectId} user={user} />
      )}
    </div>
  )
}

function UnauthorizedPage({ projectId, user }: { projectId: string; user: UserDetails }) {
  const cdnLink = useCDNLink()
  const hmmPyramidLight = React.useMemo(() => {
    return cdnLink('/editor/hmmm_pyramid_light.png')
  }, [cdnLink])
  const hmmPyramidDark = React.useMemo(() => {
    return cdnLink('/editor/hmmm_pyramid_dark.png')
  }, [cdnLink])

  const isDarkMode = useIsDarkMode()

  const logoPic = React.useMemo(() => {
    return isDarkMode ? hmmPyramidDark : hmmPyramidLight
  }, [isDarkMode, hmmPyramidDark, hmmPyramidLight])

  const [accessRequested, setAccessRequested] = React.useState(false)

  const accessRequestsFetcher = useFetcher()
  const requestProjectAccess = React.useCallback(async () => {
    const action = `/internal/projects/${projectId}/access/request`
    accessRequestsFetcher.submit({}, { method: 'POST', action: action })
  }, [accessRequestsFetcher, projectId])

  const requestAccess = React.useCallback(async () => {
    if (projectId != null) {
      void requestProjectAccess()
      setAccessRequested(true)
    }
  }, [projectId, requestProjectAccess])

  return (
    <>
      <div className={styles.image} style={{ marginRight: 70 }}>
        <img src={logoPic} height='500px' alt='Utopia 403 Logo' />
      </div>
      <div className={styles.container}>
        <div style={{ fontSize: '100px', fontWeight: 600, fontStyle: 'italic' }}>Hmmm…</div>
        <div className={styles.runningText}>
          <span>Looks like you need permission to access this project. </span>
          <SignedInAs user={user} isDarkMode={isDarkMode} />
        </div>
        <div style={{ display: 'flex', gap: '20px' }}>
          {accessRequested ? (
            <>
              <Action text='Return Home' href={`/projects`} secondary />
              <Action text='Access Requested' disabled />
            </>
          ) : (
            <>
              <Action text='Sign Out' href={`/logout`} secondary />
              <Action text='Request Access' onClick={requestAccess} />
            </>
          )}
        </div>
      </div>
    </>
  )
}

function NotFoundPage({ user, projectId }: { user: UserDetails | null; projectId: string | null }) {
  const cdnLink = useCDNLink()

  const PyramidLight404 = React.useMemo(() => {
    return cdnLink('editor/404_pyramid_light.png')
  }, [cdnLink])

  const PyramidDark404 = React.useMemo(() => {
    return cdnLink('editor/404_pyramid_dark.png')
  }, [cdnLink])

  const isDarkMode = useIsDarkMode()

  const logoPic = React.useMemo(() => {
    return isDarkMode ? PyramidDark404 : PyramidLight404
  }, [isDarkMode, PyramidDark404, PyramidLight404])

  const location = useLocation()
  const onLoginRedirect = useMemo(() => {
    const queryParams = new URLSearchParams(location.search)
    const searchString = queryParams.toString() === '' ? '' : `?${queryParams.toString()}`
    return `/p/${projectId}${searchString}`
  }, [projectId, location.search])

  return (
    <>
      <div className={styles.image} style={{ marginRight: 50 }}>
        <img src={logoPic} height='500px' alt='Utopia 404 Logo' />
      </div>
      <div className={styles.container}>
        <div style={{ fontSize: '160px', fontWeight: 600, fontStyle: 'italic', height: 170 }}>
          404
        </div>
        <div style={{ fontSize: '42px' }}>Project not found.</div>
        <div className={styles.runningText}>
          <span>Either this project does not exist, or you need to be granted access to it. </span>
          <SignedInAs user={user} isDarkMode={isDarkMode} />
        </div>
        <div style={{ display: 'flex', gap: '20px' }}>
          {user?.user_id != null ? (
            <>
              <Action text='Sign Out' href={`/logout`} secondary />
              <Action text='Return Home' href={`/projects`} />
            </>
          ) : (
            <Action
              text='Sign In'
              href={`/login?redirectTo=${encodeURIComponent(onLoginRedirect)}`}
            />
          )}
        </div>
      </div>
    </>
  )
}

function Action({
  text,
  onClick,
  disabled,
  href,
  secondary,
}: {
  text: string
  onClick?: () => void
  disabled?: boolean
  href?: string
  secondary?: boolean
}) {
  if (href != null) {
    return (
      <a href={href} rel='noopener noreferrer' style={{ textDecoration: 'none' }}>
        <ActionButton text={text} secondary={secondary} />
      </a>
    )
  }
  return <ActionButton text={text} onClick={onClick} disabled={disabled} secondary={secondary} />
}

function ActionButton({
  text,
  onClick,
  disabled,
  secondary,
}: {
  text: string
  onClick?: () => void
  disabled?: boolean
  secondary?: boolean
}) {
  const colorStyle = secondary
    ? {
        backgroundColor: colors.secondary,
        color: colors.black,
      }
    : {
        backgroundColor: colors.primary,
        color: colors.white,
      }
  return (
    <Button
      disabled={disabled}
      onClick={onClick}
      radius='full'
      className={cx({ [styles.hover]: !disabled && !secondary })}
      style={{
        cursor: 'pointer',
        boxSizing: 'border-box',
        height: 'auto',
        fontSize: '18px',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        padding: '10px 30px',
        transition: 'background-color 0.3s',
        opacity: disabled ? 0.5 : 1,
        pointerEvents: disabled ? 'none' : 'initial',
        ...colorStyle,
      }}
    >
      {text}
    </Button>
  )
}

function SignedInAs({ user, isDarkMode }: { user: UserDetails | null; isDarkMode: boolean }) {
  return user?.user_id != null && user?.email != null ? (
    <>
      <span>You're signed in as </span>
      <a
        href='/projects'
        rel='noopener noreferrer'
        style={{ textDecoration: 'none', color: isDarkMode ? '#80CAFF' : '#0075F9' }}
      >
        {user?.email}
      </a>
      <span>.</span>
    </>
  ) : null
}
