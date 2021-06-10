import styled from '@emotion/styled'

export const layout = {
  margins: {
    wide: 40,
    regular: 20,
  },
}

export const FlexRow = styled.div({
  display: 'flex',
  flexDirection: 'row',
  alignItems: 'center',
})

export const FlexColumn = styled.div({
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'center',
})

export const FlexCenter = styled.div({
  display: 'flex',
  flexDirection: 'row',
  alignItems: 'center',
  justifyContent: 'center',
})

export const FlexWrappingList = styled(FlexRow)({
  alignItems: 'flex-start',
  alignContent: 'flex-start',
  flexWrap: 'wrap',
})
