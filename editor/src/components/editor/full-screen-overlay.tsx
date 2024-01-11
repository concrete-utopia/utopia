/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { css, jsx, keyframes } from '@emotion/react'
import React from 'react'

function handleEventNoop(e: React.MouseEvent | React.KeyboardEvent) {
  e.stopPropagation()
  e.preventDefault()
}

export const FullScreenOverlay = (
  props: React.PropsWithChildren<{ style?: React.CSSProperties }>,
) => {
  const anim = keyframes`
    from {
      opacity: 0;
    }
    to {
      opacity: 0.2;
    }
  `

  return (
    <div
      onMouseDown={handleEventNoop}
      onMouseUp={handleEventNoop}
      onClick={handleEventNoop}
      onKeyDown={handleEventNoop}
      onKeyUp={handleEventNoop}
      style={{
        position: 'fixed',
        top: 0,
        left: 0,
        right: 0,
        bottom: 0,
        backgroundColor: '#00000033',
        zIndex: 30,
        transition: 'all .1s ease-in-out',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'center',
        cursor: 'wait',
        ...props.style,
      }}
      css={css`
        animation: ${anim} 0.3s ease-in-out;
      `}
    >
      {props.children}
    </div>
  )
}
