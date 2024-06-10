import React from 'react'
import { FlexColumn, FlexRow, colorTheme } from '../../../../uuiui'
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
    <FlexRow
      style={{
        alignItems: 'flex-start',
        overflowX: 'scroll',
        overflowY: 'hidden',
        scrollbarWidth: 'auto',
        scrollbarColor: 'gray transparent',
        gap: 4,
      }}
    >
      <DataSelectorColumn
        activeScope={props.activeScope}
        targetPathInsideScope={props.targetPathInsideScope}
        onTargetPathChange={props.onTargetPathChange}
        currentlyShowingScopeForArray={false}
      />
    </FlexRow>
  )
})

interface DataSelectorColumnProps {
  activeScope: Array<DataPickerOption>
  currentlyShowingScopeForArray: boolean
  targetPathInsideScope: ObjectPath
  onTargetPathChange: (newTargetPath: ObjectPath) => void
}

const DataSelectorColumn = React.memo((props: DataSelectorColumnProps) => {
  const { activeScope, targetPathInsideScope, currentlyShowingScopeForArray } = props

  const selectedElement: DataPickerOption | null =
    activeScope.find((option) => isPrefixOf(option.valuePath, targetPathInsideScope)) ?? null

  // if the current scope is an array, we want to show not only the array indexes, but also the contents of the first element of the array
  const pseudoSelectedElementForArray: DataPickerOption | null =
    currentlyShowingScopeForArray && activeScope.length > 0 ? activeScope[0] : null

  const nextColumnScope: ArrayOption | ObjectOption | null = (() => {
    const elementToUse = selectedElement ?? pseudoSelectedElementForArray
    if (elementToUse != null) {
      if (elementToUse.type === 'object' || elementToUse.type === 'array') {
        return elementToUse
      }
    }
    return null
  })()

  return (
    <>
      <FlexColumn style={{ width: 200, flexShrink: 0 }}>
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
          currentlyShowingScopeForArray={nextColumnScope.type === 'array'}
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

  return (
    <FlexRow
      onClick={onClick}
      style={{
        alignSelf: 'stretch',
        justifyContent: 'space-between',
        fontSize: 10,
        borderRadius: 4,
        height: 20,
        backgroundColor: props.selected ? colorTheme.primary.value : 'transparent',
        color: props.selected ? colorTheme.neutralInvertedForeground.value : colorTheme.fg0.value,
        padding: 5,
        cursor: 'pointer',
      }}
    >
      <span>{props.data.variableInfo.expressionPathPart}</span>
      <span>{'>'}</span>
    </FlexRow>
  )
})
