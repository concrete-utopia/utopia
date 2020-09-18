import * as React from 'react'
import styled from '@emotion/styled'

const arrowHeight = 5

const transparent = `${arrowHeight}px solid transparent`
const visible = `${arrowHeight}px solid blue`

const Arrowhead = styled.div((props: { direction: 'left' | 'up' | 'right' | 'down' }) => ({
  display: 'inline-block',
  width: 0,
  height: 0,
  borderTop: props.direction === 'down' ? visible : transparent,
  borderBottom: props.direction === 'up' ? visible : transparent,
  borderRight: props.direction === 'left' ? visible : transparent,
  borderLeft: props.direction === 'right' ? visible : transparent,
}))

const Arrowbase = styled.div((props: { length: number }) => ({
  display: 'inline-block',
  position: 'relative',
  width: props.length,
  height: arrowHeight,
  top: -arrowHeight / 2,
  backgroundColor: 'blue',
}))

const Arrow: React.FunctionComponent<any> = () => {
  return (
    <div>
      <Arrowbase length={10} />
      <Arrowhead direction={'right'} />
    </div>
  )
}

export const FlexGrowControl: React.FunctionComponent<{
  direction: 'row' | 'column'
}> = (props) => {
  return (
    <div>
      <Arrow direction='right' />
    </div>
  )
}
