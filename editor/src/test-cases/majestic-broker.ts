export const MajesticBrokerTestCaseCode = `
import React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'
export var User = (props) => {
  return <span style={{ ...props.style, fontWeight: 600 }}>{props.children}</span>
}
User.propertyControls = {
  style: {
    type: 'style-controls',
  },
}
export var Comment = (props) => {
  return (
    <span style={{ ...props.style, fontWeight: 400, color: 'grey', marginLeft: 12 }}>
      {props.children}
    </span>
  )
}
Comment.propertyControls = {
  style: {
    type: 'style-controls',
  },
}
export var Card = (props) => {
  return (
    <div style={{ display: 'flex', flexDirection: 'column' }}>
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          fontWeight: 500,
          marginLeft: 20,
          marginRight: 20,
        }}
        layout={{ flexBasis: 58 }}
      >
        <div
          style={{
            width: 32,
            height: 32,
            borderRadius: 50,
            border: '1px solid red',
            boxShadow: 'inset 0px 0px 0px 2px white',
            background: \`url(\${props.url})\`,
            backgroundSize: '32px 32px',
          }}
        />
        <h3
          style={{ marginLeft: 16, flexGrow: 1, fontSize: 14, fontWeight: 600 }}
          layout={{ crossBasis: 49 }}
        >
          Zeus
          <View
            style={{ backgroundColor: '#aaaaaa33', left: 46, top: 11, width: 82, height: 62 }}
            layout={{ layoutSystem: 'pinSystem' }}
          />
        </h3>
        <button style={{ flexGrow: 0, border: 'none', backgroundColor: 'transparent' }}>
          . . .{' '}
        </button>
      </div>
      .
      <div
        data-label={'card'}
        style={{
          minWidth: 200,
          maxWidth: 375,
          minHeight: 400,
          boxShadow: '0px 0px 5px 2px rgba(0,0,0,.1)',
          backgroundRepeat: 'repeat',
          backgroundSize: '10px 10px',
          position: 'relative',
        }}
      >
        <img src={props.url} width={375} />
        <div style={{ width: '100%', marginLeft: 16, marginRight: 16 }}>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              cursor: 'pointer',
              gap: 20,
              height: 40,
            }}
          >
            <div style={{ fontSize: 25 }}>✩</div>
            <div style={{ fontSize: 25 }}>⌽</div>
            <div style={{ fontSize: 25 }}>〠</div>
          </div>
          <p className={'description'}>
            <span style={{ fontFamily: 'garamond', fontSize: 15 }}>
              Quando quegli stati, che si acquistano, sono abituati a vivere con le loro leggi e in
              libertà, a volerli mantenere, ci sono tre modi: il primo, raderne al suolo le città;
              l’altro, andarvi ad abitare personalmente; il terzo,{' '}
              <b>
                lasciarli vivere con le loro leggi, traendone un tributo e creandovi dentro
                un’amministrazione fatta di pochi individui che te li conservino amici.
              </b>
              <a style={{ color: 'black', textDecoration: 'none' }} href={''}>
                #macchiavelli
              </a>
              &nbsp;
              <a style={{ color: 'black', textDecoration: 'none' }} href={''}>
                #goodadvice
              </a>
              &nbsp;
              <a style={{ color: 'black', textDecoration: 'none' }} href={''}>
                #selfhelp
              </a>
              &nbsp;
            </span>
          </p>
          <p>
            <span style={{ fontWeight: 400 }}>
              <User>Apollo</User>
              <Comment>Uva uvam vivendo varia fit?</Comment>
            </span>
          </p>
          <p>
            <span style={{ fontWeight: 400 }}>
              <User>Hercules</User>
              <Comment>Ita est.</Comment>
            </span>
          </p>
          <p>
            <span style={{ fontWeight: 400 }}>
              <User>Xena</User>
              <Comment>In hoc signo vinces.</Comment>
            </span>
          </p>
        </div>
      </div>
    </div>
  )
}
export var Photo = (props) => {
  return (
    <div
      style={{
        ...props.style,
        backgroundSize: 'contain',
        backgroundSize: '20px 20px',
        borderRadius: 50,
        height: 50,
        width: 50,
        background: \`url(\${props.url})\`,
        transition: 'all .2s linear',
      }}
    />
  )
}
Photo.propertyControls = {
  style: {
    type: 'style-controls',
  },
}
export var ScrollablePhotoGrid = (props) => {
  return (
    <div style={{ ...props.style, overflowX: 'scroll', position: 'absolute', left: 0, right: 0 }}>
      <div
        style={{
          display: 'flex',
          gap: 20,
          width: 600,
          marginTop: 20,
          marginBottom: 20,
          padding: 8,
        }}
      >
        <div style={{ width: 300, height: 300, display: 'relative' }}>
          hi
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              top: 8,
              height: 78,
              width: 122,
              left: 38,
            }}
            layout={{ layoutSystem: 'pinSystem' }}
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                height: 40,
                width: 76,
                left: 6,
                top: 5,
                position: 'absolute',
              }}
              layout={{ layoutSystem: 'pinSystem' }}
            />
          </div>
        </div>
        <Photo
          url={
            'https://images.generated.photos/F3N3OT3PEQgszg8GLS1dfQf2vRkjoXCwff5cjs_l5Oc/rs:fit:512:512/Z3M6Ly9nZW5lcmF0/ZWQtcGhvdG9zL3Yz/XzAyMDY3NDEuanBn.jpg'
          }
        />
        <Photo
          url={
            'https://images.generated.photos/fqisoBYuCsobJnXNh6CAxBLv4ErstueHips0ddRMrD4/rs:fit:512:512/Z3M6Ly9nZW5lcmF0/ZWQtcGhvdG9zL3Yy/XzAzMDk2MjAuanBn.jpg'
          }
        />
        <Photo
          url={
            'https://images.generated.photos/GMbjdhAJrCKhDT8C7jMCJVHMjSc0S0A-nXcfNTPvCOo/rs:fit:512:512/Z3M6Ly9nZW5lcmF0/ZWQtcGhvdG9zL3Yy/XzAyNjMwOTEuanBn.jpg'
          }
        />
        <Photo
          url={
            'https://images.generated.photos/f0Uen3tXgTUbdKLBhgRC5Vk5m2lNGsUAxMz4Y8yOE9E/rs:fit:512:512/Z3M6Ly9nZW5lcmF0/ZWQtcGhvdG9zL3Yy/XzA1NDI0MTUuanBn.jpg'
          }
        />
        <Photo
          url={
            'https://images.generated.photos/gsx5n_96xTlUxl4YjkBFUwgXi3LLH23ifpiwisDoX-Q/rs:fit:512:512/Z3M6Ly9nZW5lcmF0/ZWQtcGhvdG9zL3Yy/XzAwODc5ODYuanBn.jpg'
          }
        />
        <Photo
          url={
            'https://images.generated.photos/LCOHlb4KV8SFHypgVUD1qDLn-n1q5eLckMfnVklfIUo/rs:fit:512:512/Z3M6Ly9nZW5lcmF0/ZWQtcGhvdG9zL3Yy/XzA1NDQxMDkuanBn.jpg'
          }
        />
        <Photo
          url={
            'https://images.generated.photos/LCOHlb4KV8SFHypgVUD1qDLn-n1q5eLckMfnVklfIUo/rs:fit:512:512/Z3M6Ly9nZW5lcmF0/ZWQtcGhvdG9zL3Yy/XzA1NDQxMDkuanBn.jpg'
          }
        />
        <Photo
          url={
            'https://images.generated.photos/LCOHlb4KV8SFHypgVUD1qDLn-n1q5eLckMfnVklfIUo/rs:fit:512:512/Z3M6Ly9nZW5lcmF0/ZWQtcGhvdG9zL3Yy/XzA1NDQxMDkuanBn.jpg'
          }
        />
        <Photo
          url={
            'https://images.generated.photos/LCOHlb4KV8SFHypgVUD1qDLn-n1q5eLckMfnVklfIUo/rs:fit:512:512/Z3M6Ly9nZW5lcmF0/ZWQtcGhvdG9zL3Yy/XzA1NDQxMDkuanBn.jpg'
          }
        />
      </div>
    </div>
  )
}
ScrollablePhotoGrid.propertyControls = {
  style: {
    type: 'style-controls',
  },
}
export var Content = (props) => {
  return (
    <div
      style={{
        ...props.style,
        display: 'flex',
        flexDirection: 'column',
        gap: 20,
        paddingTop: 20,
        paddingBottom: 40,
      }}
    >
      {props.children}
    </div>
  )
}
Content.propertyControls = {
  style: {
    type: 'style-controls',
  },
}
export var BottomMenu = (props) => {
  return (
    <div
      style={{
        ...props.style,
        display: 'flex',
        flexDirection: 'row',
        justifyContent: 'space-between',
      }}
    >
      {props.children}
    </div>
  )
}
BottomMenu.propertyControls = {
  style: {
    type: 'style-controls',
  },
}
export var App = () => {
  return (
    <div
      id={'sampleBody'}
      style={{ position: 'relative', background: 'url(./bg.jpg)', width: '100%', height: '100%' }}
    >
      <div
        style={{
          position: 'absolute',
          left: 0,
          right: 0,
          bottom: 0,
          top: 0,
          background: 'rgba(78,0,0,.3)',
          backdropFilter: 'blur (23px)',
        }}
      />
      <div
        data-label={'preview'}
        style={{
          position: 'absolute',
          width: 375,
          margin: 40,
          boxShadow: '0px 10px 35px 5px rgba(0,0,0,.5)',
          borderRadius: 5,
          background: 'rgb(10,30,50)',
          color: '#B2943D',
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'stretch',
          overflow: 'hidden',
        }}
      >
        <div
          style={{
            fontSize: 30,
            display: 'flex',
            flexDirection: 'row',
            justifyContent: 'space-between',
            flexGrow: 0.5,
          }}
        >
          <span>␐</span>
          <span style={{ fontWeight: 'bold' }}>␖</span>
          <span>␖</span>
          <span>␖</span>
          <span>␖</span>
        </div>
        <div id={'scrollView'} style={{ minHeight: 100, margin: 'auto', overflowX: 'hidden' }}>
          <ScrollablePhotoGrid />
        </div>
        <div
          style={{
            position: 'absolute',
            bottom: 0,
            zIndex: 99999,
            left: 0,
            right: 0,
            backgroundColor: 'rgb(30,60,80)',
            boxShadow: 'inset 0px 1px 0px 0px grey',
            display: 'grid',
            gridTemplateColumns: '60px 60px 120px 60px 60px',
            height: 40,
          }}
        >
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              fontSize: 22,
              fontWeight: 700,
              color: '#B2943D',
              justifyContent: 'center',
            }}
          >
            🃈
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              fontSize: 22,
              fontWeight: 700,
              color: '#B2943D',
              justifyContent: 'center',
            }}
          >
            🃌
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              fontSize: 22,
              fontWeight: 700,
              color: '#B2943D',
              justifyContent: 'center',
            }}
          >
            🀑
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              fontSize: 22,
              fontWeight: 700,
              color: '#B2943D',
              justifyContent: 'center',
            }}
          >
            🃈
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              fontSize: 22,
              fontWeight: 700,
              color: '#B2943D',
              justifyContent: 'center',
            }}
          >
            🀚
          </div>
        </div>
        <div
          id={'scrollView'}
          style={{ width: 375, height: 400, overflowY: 'scroll', position: 'relative' }}
        >
          <Content>
            {[
              'https://upload.wikimedia.org/wikipedia/commons/8/88/Bonifacio_bembo%2C_regina_di_spade.jpg',
              'https://upload.wikimedia.org/wikipedia/en/thumb/0/0d/Wands13.jpg/220px-Wands13.jpg',
              'https://upload.wikimedia.org/wikipedia/en/c/c3/RWS_Tarot_04_Emperor.jpg',
              'https://upload.wikimedia.org/wikipedia/en/0/0d/Wands13.jpg',
              'https://upload.wikimedia.org/wikipedia/en/3/33/Swords14.jpg',
            ].map((item) => (
              <Card url={item}>{item}</Card>
            ))}
          </Content>
          <BottomMenu />
        </div>
      </div>
    </div>
  )
}
export var storyboard = (
  <Storyboard>
    <Scene style={{ position: 'absolute', left: 0, top: 0, width: 522, height: 652 }}>
      <App />
    </Scene>
    <Scene data-label={'ScrollablePhotoGrid'} style={{ position: 'absolute', left: 561, top: 1, width: 437, height: 96 }}>
      <ScrollablePhotoGrid />
    </Scene>
    <Scene
      data-label={'Card'}
      style={{ position: 'absolute', left: 561, top: 123, width: 376, height: 1110 }}
    >
      <Card url='https://upload.wikimedia.org/wikipedia/commons/8/88/Bonifacio_bembo%2C_regina_di_spade.jpg' />
    </Scene>
  </Storyboard>
)
`
