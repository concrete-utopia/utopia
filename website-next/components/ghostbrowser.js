export const EyeButton = (props) => (
  <div
    style={{
      borderRadius: '50%',
      height: 17,
      width: 17,
      background: props.dark ? '#181818' : 'white',
      border: '1px solid #383C4A',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
    }}
  >
    <div
      style={{
        borderRadius: '50%',
        height: 8,
        width: 8,
        background: props.dark ? '#181818' : '#383C4A',
      }}
    >
      <div
        style={{
          borderRadius: '50%',
          height: 2,
          width: 2,
          background: 'white',
          position: 'relative',
          left: 4,
          top: 3,
          display: props.dark ? 'none' : 'block',
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
      fontFamily: 'Moderat-Regular',
      fontSize: 11,
      flexDirection: 'column',
      color: 'white',
      border: '1px solid #383C4A',
      borderRadius: 8,
      background: props.dark ? '#383C4A' : '#FFFFFF',
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
        }}
      >
        <EyeButton dark={props.dark} />
        <EyeButton dark={props.dark} />
      </div>

      <div style={{ cursor: 'grab' }}>{props.title}</div>
    </div>

    <div style={{}}>{props.children}</div>
  </div>
)

export const GhostBrowserDark = (props) => <GhostBrowser {...props} dark />
