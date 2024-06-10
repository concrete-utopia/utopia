import React from 'react'
import { FlexColumn, FlexRow } from '../../../../uuiui'
import type { DataPickerOption, ObjectPath } from './data-picker-utils'

interface DataSelectorColumnsProps {
  activeScope: Array<DataPickerOption>
  targetPathInsideScope: ObjectPath | null
}

export const DataSelectorColumns = React.memo((props: DataSelectorColumnsProps) => {
  return (
    <FlexRow>
      <DataSelectorColumn elementsToShow={props.activeScope} />
    </FlexRow>
  )
})

interface DataSelectorColumnProps {
  elementsToShow: Array<DataPickerOption>
}

const DataSelectorColumn = React.memo((props: DataSelectorColumnProps) => {
  const { elementsToShow } = props
  return (
    <FlexColumn>
      {elementsToShow.map((option) => {
        return <RowWithCartouche key={option.variableInfo.expression} />
      })}
    </FlexColumn>
  )
})

interface RowWithCartoucheProps {}
const RowWithCartouche = React.memo((props: RowWithCartoucheProps) => {
  return <FlexRow>cica</FlexRow>
})
