import { style } from '@vanilla-extract/css'

export const hover = style({
  ':hover': {
    backgroundColor: '#5DA9FF !important',
  },
})

export const image = style({
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'center',
  width: '320px',
})

export const container = style({
  display: 'flex',
  flexDirection: 'column',
  gap: '40px',
  alignItems: 'center',
  paddingBottom: '30px',
})

export const runningText = style({
  fontSize: '22px',
  width: '430px',
  textAlign: 'center',
  lineHeight: '40px',
})
