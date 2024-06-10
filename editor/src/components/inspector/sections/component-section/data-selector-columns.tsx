import React from 'react'
import { FlexColumn, FlexRow } from '../../../../uuiui'
import type { ArrayOption, DataPickerOption, ObjectOption, ObjectPath } from './data-picker-utils'
import { isPrefixOf, mapFirstApplicable } from '../../../../core/shared/array-utils'
import { when } from '../../../../utils/react-conditionals'

interface DataSelectorColumnsProps {
  activeScope: Array<DataPickerOption>
  targetPathInsideScope: ObjectPath
  onTargetPathChange: (newTargetPath: ObjectPath) => void
}

export const DataSelectorColumns = React.memo((props: DataSelectorColumnsProps) => {
  return (
    <FlexRow style={{ alignItems: 'flex-start' }}>
      <DataSelectorColumn
        activeScope={props.activeScope}
        targetPathInsideScope={props.targetPathInsideScope}
        onTargetPathChange={props.onTargetPathChange}
      />
    </FlexRow>
  )
})

interface DataSelectorColumnProps {
  activeScope: Array<DataPickerOption>
  targetPathInsideScope: ObjectPath
  onTargetPathChange: (newTargetPath: ObjectPath) => void
}

const DataSelectorColumn = React.memo((props: DataSelectorColumnProps) => {
  const { activeScope, targetPathInsideScope } = props
  const selectedElement = activeScope.find((option) =>
    isPrefixOf(option.valuePath, targetPathInsideScope),
  )
  const nextColumnScope =
    selectedElement != null &&
    (selectedElement.type === 'array' || selectedElement?.type === 'object')
      ? selectedElement
      : null

  return (
    <>
      <FlexColumn>
        {activeScope.map((option) => {
          return (
            <RowWithCartouche
              key={option.variableInfo.expression}
              data={option}
              selected={option === selectedElement}
              onTargetPathChange={props.onTargetPathChange}
            />
          )
        })}
      </FlexColumn>
      {nextColumnScope != null ? (
        <DataSelectorColumn
          activeScope={nextColumnScope.children}
          targetPathInsideScope={targetPathInsideScope}
          onTargetPathChange={props.onTargetPathChange}
        />
      ) : null}
    </>
  )
})

interface RowWithCartoucheProps {
  data: DataPickerOption
  selected: boolean
  onTargetPathChange: (newTargetPath: ObjectPath) => void
}
const RowWithCartouche = React.memo((props: RowWithCartoucheProps) => {
  const { onTargetPathChange } = props
  const targetPath = props.data.valuePath

  const onClick: React.MouseEventHandler<HTMLDivElement> = React.useCallback(
    (e) => {
      e.stopPropagation()
      onTargetPathChange(targetPath)
    },
    [targetPath, onTargetPathChange],
  )

  return <FlexRow onClick={onClick}>{props.data.variableInfo.expressionPathPart}</FlexRow>
})
