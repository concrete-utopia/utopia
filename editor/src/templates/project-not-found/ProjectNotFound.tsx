/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { UTOPIA_BACKEND_BASE_URL } from '../../common/env-vars'
import { requestProjectAccess } from '../../components/editor/server'
import { when } from '../../utils/react-conditionals'
import { Button } from '../../uuiui/button'

const PyramidLight404 = `${process.env.UTOPIA_DOMAIN}/editor/404_pyramid_light.png?hash=${process.env.UTOPIA_SHA}`

export default function ProjectNotFound({
  projectId,
  loggedIn,
}: {
  projectId: string | null
  loggedIn: boolean
}) {
  const [accessRequested, setAccessRequested] = React.useState(false)
  const requestAccess = React.useCallback(async () => {
    if (projectId != null) {
      void requestProjectAccess(projectId)
      setAccessRequested(true)
    }
  }, [projectId])
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
          <a
            href={`${UTOPIA_BACKEND_BASE_URL}projects`}
            rel='noopener noreferrer'
            style={{ textDecoration: 'none' }}
          >
            <ActionButton text='Return Home' />
          </a>
          {when(
            projectId != null && loggedIn === true,
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
