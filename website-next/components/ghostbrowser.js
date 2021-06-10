export const EyeButton = (props) => (
  <div
    style={{
      borderRadius: '50%',
      height: 6,
      width: 6,
      background: 'white',
      ...props.style,
    }}
  ></div>
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
      border: '1px solid white',
      borderRadius: 10,
      background: '#FFFFFF44',
      backdropFilter: 'blur(4px)',
      overflow: 'hidden',
    }}
  >
    <div
      style={{
        display: 'flex',
        alignItems: 'center',
        borderBottom: '1px solid white',
        paddingLeft: 8,
        paddingRight: 8,
        height: 24,
        minHeight: 24,
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
        <EyeButton style={{ backgroundColor: 'black' }} />
        <EyeButton style={{ backgroundColor: 'black' }} />
      </div>

      <div style={{ cursor: 'grab' }}>{props.title}</div>
    </div>

    <div style={{}}>{props.children}</div>
  </div>
)
