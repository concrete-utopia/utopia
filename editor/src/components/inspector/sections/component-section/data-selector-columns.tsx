import React from 'react'
import { FlexColumn, FlexRow, colorTheme } from '../../../../uuiui'
import type { ArrayOption, DataPickerOption, ObjectOption, ObjectPath } from './data-picker-utils'
import { isPrefixOf, mapFirstApplicable } from '../../../../core/shared/array-utils'
import { when } from '../../../../utils/react-conditionals'
import type { CartoucheSource, CartoucheUIProps } from './cartouche-ui'
import { CartoucheUI } from './cartouche-ui'
import {
  dataPathSuccess,
  traceDataFromVariableName,
} from '../../../../core/data-tracing/data-tracing'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { assertNever } from '../../../../core/shared/utils'

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
        originalDataForScope={null}
      />
    </FlexRow>
  )
})

interface DataSelectorColumnProps {
  activeScope: Array<DataPickerOption>
  originalDataForScope: DataPickerOption | null
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

  const dataSource = useVariableDataSource(props.originalDataForScope)

  return (
    <>
      <FlexColumn
        style={{
          width: 200,
          height: '100%',
          flexShrink: 0,
          overflowX: 'hidden',
          overflowY: 'scroll',
          scrollbarWidth: 'auto',
          scrollbarColor: 'gray transparent',
        }}
      >
        {activeScope.map((option) => {
          return (
            <RowWithCartouche
              key={option.variableInfo.expression}
              data={option}
              selected={option === selectedElement}
              onTargetPathChange={props.onTargetPathChange}
              forcedDataSource={dataSource}
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
          originalDataForScope={props.originalDataForScope ?? selectedElement}
        />
      ) : null}
    </>
  )
})

interface RowWithCartoucheProps {
  data: DataPickerOption
  selected: boolean
  forcedDataSource: CartoucheSource | null
  onTargetPathChange: (newTargetPath: ObjectPath) => void
}
const RowWithCartouche = React.memo((props: RowWithCartoucheProps) => {
  const { onTargetPathChange, data, forcedDataSource } = props
  const targetPath = data.valuePath

  const dataSource = useVariableDataSource(data)

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
        height: 24,
        backgroundColor: props.selected ? colorTheme.bg4.value : 'transparent',
        color: props.selected ? colorTheme.fg4.value : 'transparent',
        padding: 5,
        cursor: 'pointer',
      }}
    >
      <span>
        <CartoucheUI
          key={data.valuePath.toString()}
          source={forcedDataSource ?? dataSource ?? 'internal'}
          datatype={childTypeToCartoucheDataType(data.type)}
          selected={props.selected}
          role={'information'}
          testId={`data-selector-option-${data.variableInfo.expression}`}
        >
          {data.variableInfo.expressionPathPart}
        </CartoucheUI>
      </span>
      <span>{'>'}</span>
    </FlexRow>
  )
})

function useVariableDataSource(variable: DataPickerOption | null) {
  return useEditorState(
    Substores.projectContentsAndMetadata,
    (store) => {
      if (variable == null) {
        return null
      }
      const container = variable.variableInfo.insertionCeiling
      const trace = traceDataFromVariableName(
        container,
        variable.variableInfo.expression,
        store.editor.jsxMetadata,
        store.editor.projectContents,
        dataPathSuccess([]),
      )

      switch (trace.type) {
        case 'hook-result':
          return 'external'
        case 'literal-attribute':
        case 'literal-assignment':
          return 'literal'
        case 'component-prop':
        case 'element-at-scope':
        case 'failed':
          return 'internal'
          break
        default:
          assertNever(trace)
      }
    },
    'useVariableDataSource',
  )
}

function childTypeToCartoucheDataType(
  childType: DataPickerOption['type'],
): CartoucheUIProps['datatype'] {
  switch (childType) {
    case 'array':
      return 'array'
    case 'object':
      return 'object'
    case 'jsx':
    case 'primitive':
      return 'renderable'
    default:
      assertNever(childType)
  }
}
