import React from 'react'
import { when } from '../util/react-conditionals'
import { Button } from '@radix-ui/themes'
import { useFetcher } from '@remix-run/react'
import { useProjectsStore } from '../store'
import { colors } from '../styles/sprinkles.css'
import { hover } from './projectNotFound.css'
import cx from 'classnames'
import type { UserDetails } from 'prisma-client'

export default function ProjectNotFound({
  projectId,
  user,
}: {
  projectId: string | null
  user: UserDetails | null
}) {
  const env = useProjectsStore((store) => store.env)
  const PyramidLight404 = `${env?.UTOPIA_CDN_URL}/editor/404_pyramid_light.png`
  const hmmPyramidLight = `${env?.UTOPIA_CDN_URL}/editor/hmmm_pyramid_light.png`
  const loggedIn = user?.user_id != null
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
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          width: '320px',
        }}
      >
        <img src={PyramidLight404} height='500px' alt='Utopia 404 Logo' />
      </div>
      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          gap: '40px',
          alignItems: 'center',
          paddingBottom: '30px',
        }}
      >
        <div style={{ fontSize: '160px', fontWeight: 600, fontStyle: 'italic' }}>404</div>
        <div style={{ fontSize: '42px' }}>Project not found.</div>
        <div style={{ fontSize: '22px', width: '430px', textAlign: 'center', lineHeight: '40px' }}>
          <span>Either this project does not exist, or you need to be granted access to it. </span>
          {when(loggedIn, () => (
            <span>You're signed in as {user?.email}.</span>
          ))}
        </div>
        <div style={{ display: 'flex', gap: '20px' }}>
          <ActionButtonRow
            projectId={projectId}
            user={user}
            requestAccess={requestAccess}
            accessRequested={accessRequested}
          />
        </div>
      </div>
    </div>
  )
}

function ActionButtonRow({
  projectId,
  user,
  requestAccess,
  accessRequested,
}: {
  projectId: string | null
  user: UserDetails | null
  requestAccess: () => void
  accessRequested: boolean
}) {
  if (user?.user_id == null) {
    return (
      <Action
        text='Sign In'
        href={`/sign-in?redirect=${encodeURIComponent(`/project/${projectId}`)}`}
      />
    )
  }
  if (projectId == null) {
    return (
      <>
        <Action text='Sign Out' href={`/sign-out`} secondary />
        <Action text='Return Home' href={`/projects`} />
      </>
    )
  }
  if (accessRequested) {
    return (
      <>
        <Action text='Return Home' href={`/projects`} secondary />
        <Action text='Access Requested' disabled={true} />
      </>
    )
  }
  return (
    <>
      <Action text='Sign Out' href={`/sign-out`} secondary />
      <Action text='Request Access' onClick={requestAccess} disabled={accessRequested} />
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
  if (href !== null) {
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
      className={cx({ [hover]: !disabled && !secondary })}
      style={{
        boxSizing: 'border-box',
        height: 'auto',
        fontSize: '18px',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        padding: '10px 30px',
        transition: 'background-color 0.3s',
        ...colorStyle,
      }}
    >
      {text}
    </Button>
  )
}
