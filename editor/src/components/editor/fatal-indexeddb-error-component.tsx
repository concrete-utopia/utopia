import React from 'react'

export const FatalIndexedDBErrorComponent = React.memo(() => {
  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#0000AA',
        position: 'relative',
        color: 'white',
        padding: 40,
        fontFamily: 'Courier',
        fontSize: '16px',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
      }}
    >
      <p>
        <span
          style={{
            background: 'grey',
            padding: '0px 16px',
          }}
        >
          Utopia
        </span>
      </p>
      <div style={{ textAlign: 'left', maxWidth: 800 }}>
        <p>
          A problem has been detected and Utopia has been shut down to prevent damage to your
          project.
        </p>
        <p>
          If this is the first time you've seen this error screen,{' '}
          <a href='' style={{ textDecoration: 'underline', color: 'white' }}>
            reload Utopia
          </a>
          . If this screen appears again, follow these steps:
        </p>
        <p>
          Check if your computer is out of disk space. Utopia uses your browser's database to store
          projects while you work on them, and your browser may delete that database if your
          computer is low on disk space.{' '}
        </p>
        <p>Check you've not logged out of Utopia in another tab. </p>
        <p>Technical information:</p>
        <p>***INDEXEDDB DISCOMBOBULATED 0xFFFFFFFFF</p>
        <p>
          Contact your Utopia administrator and{' '}
          <a
            href='https://discord.gg/NEEnPKCgzC'
            style={{ textDecoration: 'underline', color: 'white' }}
          >
            complain via Discord
          </a>{' '}
          for further assistance
        </p>
      </div>
    </div>
  )
})
