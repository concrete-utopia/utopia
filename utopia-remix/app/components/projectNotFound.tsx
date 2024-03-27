import { Button } from '@radix-ui/themes'
import { useFetcher } from '@remix-run/react'
import cx from 'classnames'
import type { UserDetails } from 'prisma-client'
import React from 'react'
import { useAppStore } from '../stores/appStore'
import { colors } from '../styles/sprinkles.css'
import { when } from '../util/react-conditionals'
import { Status } from '../util/statusCodes'
import * as styles from './projectNotFound.css'

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
  const env = useAppStore((store) => store.env)
  const hmmPyramidLight = `${env?.UTOPIA_CDN_URL}/editor/hmmm_pyramid_light.png`
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
      <div className={styles.image}>
        <img src={hmmPyramidLight} height='500px' alt='Utopia 403 Logo' />
      </div>
      <div className={styles.container}>
        <div style={{ fontSize: '100px', fontWeight: 600, fontStyle: 'italic' }}>Hmmm…</div>
        <div className={styles.runningText}>
          <span>Looks like you need permission to access this project. </span>
          <span>You're signed in as </span>
          <a
            href='/projects'
            rel='noopener noreferrer'
            style={{ textDecoration: 'none', color: colors.primary }}
          >
            {user?.email}
          </a>
          <span>.</span>
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
  const env = useAppStore((store) => store.env)
  const PyramidLight404 = `${env?.UTOPIA_CDN_URL}/editor/404_pyramid_light.png`
  return (
    <>
      <div className={styles.image}>
        <img src={PyramidLight404} height='500px' alt='Utopia 404 Logo' />
      </div>
      <div className={styles.container}>
        <div style={{ fontSize: '160px', fontWeight: 600, fontStyle: 'italic' }}>404</div>
        <div style={{ fontSize: '42px' }}>Project not found.</div>
        <div className={styles.runningText}>
          <span>Either this project does not exist, or you need to be granted access to it. </span>
          {when(user?.user_id != null, () => (
            <>
              <span>You're signed in as </span>{' '}
              <a
                href='/projects'
                rel='noopener noreferrer'
                style={{ textDecoration: 'none', color: colors.primary }}
              >
                {user?.email}
              </a>
              <span>.</span>
            </>
          ))}
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
              href={`/login?redirectTo=${encodeURIComponent(`/p/${projectId}`)}`}
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
