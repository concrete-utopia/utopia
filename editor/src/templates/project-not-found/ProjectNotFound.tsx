/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { Button } from '../../uuiui/button'
import { UTOPIA_BACKEND_BASE_URL } from '../../common/env-vars'

const baseDomain = String(process.env.UTOPIA_DOMAIN)
const PyramidLight404 = `${baseDomain}/editor/404_pyramid_light.png?hash=${process.env.UTOPIA_SHA}`

export default function ProjectNotFound() {
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
        <a
          href={`${UTOPIA_BACKEND_BASE_URL}projects`}
          rel='noopener noreferrer'
          style={{ textDecoration: 'none' }}
        >
          <Button
            css={{
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
            Return Home
          </Button>
        </a>
      </div>
    </div>
  )
}
