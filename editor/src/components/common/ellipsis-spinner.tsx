/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx, css, keyframes } from '@emotion/react'
import React from 'react'

const ellipsis1Anim = keyframes`{
  0% {
    transform: scale(0);
  }
  100% {
    transform: scale(1);
  }
}`

const ellipsis2Anim = keyframes`{
  0% {
    transform: translate(0, 0);
  }
  100% {
    transform: translate(19px, 0);
  }
}`

const ellipsis3Anim = ellipsis2Anim

const ellipsis4Anim = keyframes`{
  0% {
    transform: scale(1);
  }
  100% {
    transform: scale(0);
  }
}`

// Don't even waste your time trying to type the `css` prop because it will haunt you forever
const Ellipsis = (props: any) => (
  <div
    css={{
      position: 'absolute',
      top: '27px',
      width: '11px',
      height: '11px',
      borderRadius: '50%',
      background: '#4400FF',
      animationTimingFunction: 'cubic-bezier(0, 1, 1, 0)',
    }}
    {...props}
  />
)

export const EllipsisSpinner = React.memo(() => {
  return (
    <div
      id='spinner'
      css={{
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        height: '100%',
        width: '100%',
      }}
    >
      <div
        id='lds-ellipsis'
        css={{
          position: 'relative',
          width: '64px',
          height: '64px',
        }}
      >
        <Ellipsis
          css={{
            left: '6px',
            animation: `${ellipsis1Anim} 0.6s infinite`,
          }}
        ></Ellipsis>

        <Ellipsis
          css={{
            left: '6px',
            animation: `${ellipsis2Anim} 0.6s infinite`,
          }}
        ></Ellipsis>

        <Ellipsis
          css={{
            left: '26px',
            animation: `${ellipsis3Anim} 0.6s infinite`,
          }}
        ></Ellipsis>

        <Ellipsis
          css={{
            left: '45px',
            animation: `${ellipsis4Anim} 0.6s infinite`,
          }}
        ></Ellipsis>
      </div>
    </div>
  )
})
