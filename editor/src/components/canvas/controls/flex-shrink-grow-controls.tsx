import * as React from 'react'
import styled from '@emotion/styled'

const arrowHeight = 5

const Arrowhead = styled.div((props: { color: 'green' | 'red' }) => ({
  display: 'inline-block',
  width: 0,
  height: 0,
  borderTop: `${arrowHeight}px solid transparent`,
  borderBottom: `${arrowHeight}px solid transparent`,
  borderRight: `${arrowHeight}px solid transparent`,
  borderLeft: `${arrowHeight}px solid ${props.color}`,
}))

const Arrowbase = styled.div((props: { length: number; color: 'green' | 'red' }) => ({
  display: 'inline-block',
  position: 'relative',
  width: props.length,
  height: arrowHeight,
  top: -arrowHeight / 2,
  backgroundColor: props.color,
}))

const Arrow: React.FunctionComponent<{
  direction: 'left' | 'up' | 'right' | 'down'
  color: 'green' | 'red'
}> = (props) => {
  const rotation = (() => {
    switch (props.direction) {
      case 'right':
        return 0
      case 'down':
        return 90
      case 'left':
        return 180
      case 'up':
        return 270
    }
  })()

  return (
    <div
      style={{
        transform: `rotate(${rotation}deg)`,
        transformOrigin: '0 50% 0',
      }}
    >
      <Arrowbase length={10} color={props.color} />
      <Arrowhead color={props.color} />
    </div>
  )
}

export const FlexGrowControl: React.FunctionComponent<{
  direction: 'row' | 'column'
  top: number
  left: number
}> = (props) => {
  return (
    <div
      style={{
        position: 'absolute',
        top: props.top,
        left: props.left,
      }}
    >
      <Arrow direction={props.direction === 'row' ? 'right' : 'down'} color={'green'} />
    </div>
  )
}

export const FlexShrinkControl: React.FunctionComponent<{
  direction: 'row' | 'column'
  top: number
  left: number
}> = (props) => {
  return (
    <div
      style={{
        position: 'absolute',
        top: props.top,
        left: props.left,
      }}
    >
      <Arrow direction={props.direction === 'row' ? 'left' : 'up'} color={'red'} />
    </div>
  )
}
