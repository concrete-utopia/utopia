/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { when } from '../util/react-conditionals'
import { Button } from '@radix-ui/themes'
import { useFetcher } from '@remix-run/react'
import { useProjectsStore } from '../store'

export default function ProjectNotFound({
  projectId,
  userId,
}: {
  projectId: string | null
  userId: string | null
}) {
  const env = useProjectsStore((store) => store.env)
  const PyramidLight404 = `${env?.UTOPIA_CDN_URL}/editor/404_pyramid_light.png`
  const loggedIn = userId != null
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
          Either this project does not exist, or you do not have access to it.
        </div>
        <div style={{ display: 'flex', gap: '20px' }}>
          <a href={`/projects`} rel='noopener noreferrer' style={{ textDecoration: 'none' }}>
            <ActionButton text='Return Home' />
          </a>
          {when(
            projectId != null && loggedIn,
            <ActionButton
              text={accessRequested ? 'Access Requested' : 'Request Access'}
              onClick={requestAccess}
              disabled={accessRequested}
            />,
          )}
        </div>
      </div>
    </div>
  )
}

function ActionButton({
  text,
  onClick,
  disabled,
}: {
  text: string
  onClick?: () => void
  disabled?: boolean
}) {
  return (
    <Button
      disabled={disabled}
      onClick={onClick}
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      css={{
        boxSizing: 'border-box',
        height: 'auto',
        fontSize: '18px',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        color: 'white',
        backgroundColor: '#0075f9',
        borderRadius: '90px',
        padding: '10px 30px',
        transition: 'background-color 0.3s',
        '&:hover': {
          backgroundColor: '#5DA9FF',
        },
      }}
    >
      {text}
    </Button>
  )
}
