import React from 'react'
import {
  dataPathSuccess,
  traceDataFromVariableName,
} from '../../../../core/data-tracing/data-tracing'
import { assertNever } from '../../../../core/shared/utils'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { MapCounterUi } from '../../../navigator/navigator-item/map-counter'
import type { CartoucheUIProps } from './cartouche-ui'
import { CartoucheUI } from './cartouche-ui'
import type { DataPickerOption } from './data-picker-utils'
import { variableMatches } from './variables-in-scope-utils'

interface DataPickerCartoucheProps {
  data: DataPickerOption
  selected: boolean
  forcedRole?: CartoucheUIProps['role']
  onClick?: CartoucheUIProps['onClick']
}

export const DataPickerCartouche = React.memo(
  (props: React.PropsWithChildren<DataPickerCartoucheProps>) => {
    const { data, selected, forcedRole: forceRole } = props
    const dataSource = useVariableDataSource(data)
    const children = props.children ?? data.variableInfo.expressionPathPart
    return (
      <CartoucheUI
        key={data.valuePath.toString()}
        tooltip={null}
        source={dataSource ?? 'internal'}
        datatype={childTypeToCartoucheDataType(data.type)}
        selected={variableMatches(data.variableInfo) && selected}
        highlight={variableMatches(data.variableInfo) ? null : 'disabled'}
        role={forceRole ?? (variableMatches(data.variableInfo) ? 'selection' : 'information')}
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
        onClick={props.onClick}
      >
        {data.isChildOfArray ? (
          <>
            <span style={{ fontStyle: 'italic', marginRight: 4 }}>item</span>
            {children}
          </>
        ) : (
          children
        )}
      </CartoucheUI>
    )
  },
)

export function useVariableDataSource(
  variable: DataPickerOption | null,
): CartoucheUIProps['source'] | null {
  return useEditorState(
    Substores.projectContentsAndMetadata,
    (store) => {
      if (variable == null) {
        return null
      }
      const container = variable.variableInfo.insertionCeiling
      const trace = traceDataFromVariableName(
        container,
        variable.valuePath[0].toString(),
        store.editor.jsxMetadata,
        store.editor.projectContents,
        dataPathSuccess(variable.valuePath.slice(1).map((x) => x.toString())),
      )

      switch (trace.type) {
        case 'hook-result':
          return 'external'
        case 'literal-attribute':
        case 'literal-assignment':
          return 'literal-assignment'
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
