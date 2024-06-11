import React from 'react'
import {
  dataPathSuccess,
  traceDataFromVariableName,
} from '../../../../core/data-tracing/data-tracing'
import { assertNever } from '../../../../core/shared/utils'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { MapCounterUi } from '../../../navigator/navigator-item/map-counter'
import type { CartoucheSource, CartoucheUIProps } from './cartouche-ui'
import { CartoucheUI } from './cartouche-ui'
import type { DataPickerOption } from './data-picker-utils'

export const DataPickerCartouche = React.memo(
  (props: {
    data: DataPickerOption
    forcedDataSource?: CartoucheSource | null // if the DataPickerOption is actually a child (of a child) of a variable, we need to provide the CartoucheSource that belongs to the original variable
    selected: boolean
  }) => {
    const { data, forcedDataSource, selected } = props
    const dataSource = useVariableDataSource(data)
    return (
      <CartoucheUI
        key={data.valuePath.toString()}
        source={forcedDataSource ?? dataSource ?? 'internal'}
        datatype={childTypeToCartoucheDataType(data.type)}
        selected={!data.disabled && selected}
        highlight={data.disabled ? 'disabled' : null}
        role={data.disabled ? 'information' : 'selection'}
        testId={`data-selector-option-${data.variableInfo.expression}`}
        badge={
          data.type === 'array' ? (
            <MapCounterUi
              counterValue={data.variableInfo.elements.length}
              overrideStatus='no-override'
              selectedStatus={selected}
            />
          ) : undefined
        }
      >
        {data.isChildOfArray ? (
          <>
            <span style={{ fontStyle: 'italic' }}>item </span>
            {data.variableInfo.expressionPathPart}
          </>
        ) : (
          data.variableInfo.expressionPathPart
        )}
      </CartoucheUI>
    )
  },
)

export function useVariableDataSource(variable: DataPickerOption | null) {
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
