import React from 'react'
import { FlexColumn, FlexRow } from '../../../../uuiui'

interface DataSelectorColumnsProps {}

export const DataSelectorColumns = React.memo((props: DataSelectorColumnsProps) => {
  return (
    <FlexRow>
      <DataSelectorColumn />
    </FlexRow>
  )
})

interface DataSelectorColumnProps {}

const DataSelectorColumn = React.memo((props: DataSelectorColumnProps) => {
  return (
    <FlexColumn>
      <RowWithCartouche />
    </FlexColumn>
  )
})

interface RowWithCartoucheProps {}
const RowWithCartouche = React.memo((props: RowWithCartoucheProps) => {
  return <FlexRow>cica</FlexRow>
})
