/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'

export const H1 = styled.h1({
  transform: 'rotateZ(-1deg) skew(-1deg, 0deg)',
  color: 'black',
  padding: 8,
  fontSize: 30,
  fontWeight: 500,
  marginBottom: 44,
  marginTop: 44,
  width: 'fit-content',
})

export const CalloutPrimary = styled.h1({
  border: `3px solid #00ABFF`,
  borderRadius: 3,
  fontSize: 18,
  lineHeight: '28px',
  fontWeight: 500,
  color: '#00ABFF',
  padding: 24,
  transform: 'translateX(-13px)',
})

export const H2 = styled.h2({
  color: '#00ABFF',
  fontSize: 18,
  fontWeight: 500,
  lineHeight: '32px',
})

export const A = styled.a({
  fontStyle: 'normal',
  color: '#00ABFF',
  textDecoration: 'none',
  fontWeight: 600,
})

export const EM = styled.em({
  fontStyle: 'normal',
  color: 'black',
  textDecoration: 'none',
  fontWeight: 600,
})

export const PMT = styled.div({
  marginTop: '36px',
})

interface PrettyKeysProps {
  shortcut: string | string[]
}

export const PrettyKeys = (props: PrettyKeysProps) => {
  const keyStyle = {
    display: 'inline-block',
    backgroundColor: 'transparent',
    border: `1px solid black`,
    borderRadius: 3,
    fontWeight: 500,
    padding: 4,
    marginLeft: 8,
  }

  return (
    <span>
      {[...props.shortcut].map((keyb, index) => (
        <span key={keyb === ' ' ? `space-${index}` : keyb} style={{ ...keyStyle }}>
          {keyb}
        </span>
      ))}
    </span>
  )
}
