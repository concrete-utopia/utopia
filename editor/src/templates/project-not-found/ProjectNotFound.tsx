import React from 'react'
import PyramidLight404 from './404_pyramid_light.png'
import { UTOPIA_BACKEND_BASE_URL } from '../../common/env-vars'

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
        <div
          style={{ fontSize: '160px', fontWeight: 600, fontStyle: 'italic', lineHeight: 'normal' }}
        >
          404
        </div>
        <div style={{ fontSize: '42px' }}>Project not found.</div>
        <div style={{ fontSize: '22px', width: '430px', textAlign: 'center', lineHeight: '40px' }}>
          Either this project does not exist, or you do not have access to it.
        </div>
        <a
          href={`${UTOPIA_BACKEND_BASE_URL}projects`}
          rel='noopener noreferrer'
          style={{ textDecoration: 'none' }}
        >
          <div
            style={{
              fontSize: '18px',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              color: 'white',
              backgroundColor: '#0075f9',
              borderRadius: '90px',
              padding: '10px 30px',
              transition: 'background-color 0.3s',
            }}
            onMouseOver={(e) => (e.currentTarget.style.backgroundColor = '#5DA9FF')}
            onMouseOut={(e) => (e.currentTarget.style.backgroundColor = '#0075f9')}
          >
            Return Home
          </div>
        </a>
      </div>
    </div>
  )
}
