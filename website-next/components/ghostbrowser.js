export const EyeButton = (props) => (
  <div
    style={{
      borderRadius: '50%',
      height: 17,
      width: 17,
      background: 'white',
      border: '1px solid #383C4A',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center'
    }}
  >
    <div
      style={{
        borderRadius: '50%',
        height: 8,
        width: 8,
        background: '#383C4A',
      }}>
        <div
          style={{
            borderRadius: '50%',
            height: 2,
            width: 2,
            background: 'white',
            position: 'relative',
            left: 4,
            top: 3,
          }}
        />
      </div>
    </div>
)

export const GhostBrowser = (props) => (
  <div
    className={props.className}
    style={{
      display: 'flex',
      fontFamily: 'Inter',
      fontSize: 11,
      flexDirection: 'column',
      color: 'white',
      border: '1px solid #383C4A',
      borderRadius: 8,
      background: '#FFFFFF',
      // backdropFilter: 'blur(4px)',
      overflow: 'hidden',
    }}
  >
    <div
      style={{
        display: 'flex',
        alignItems: 'center',
        borderBottom: '1px solid #383C4A',
        paddingLeft: 8,
        paddingRight: 8,
        height: 32,
        minHeight: 32,
      }}
    >
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          flexGrow: 1,
          gap: 6,
          cursor: 'pointer',
        }}
      >
        <EyeButton />
        <EyeButton />
      </div>

      <div style={{ cursor: 'grab' }}>{props.title}</div>
    </div>

    <div style={{}}>{props.children}</div>
  </div>
)
