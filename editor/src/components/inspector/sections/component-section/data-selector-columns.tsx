import styled from '@emotion/styled'
import React from 'react'
import {
  dataPathSuccess,
  traceDataFromVariableName,
} from '../../../../core/data-tracing/data-tracing'
import { isPrefixOf } from '../../../../core/shared/array-utils'
import { assertNever } from '../../../../core/shared/utils'
import { unless } from '../../../../utils/react-conditionals'
import { FlexColumn, FlexRow, colorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import type { CartoucheSource, CartoucheUIProps } from './cartouche-ui'
import { CartoucheUI } from './cartouche-ui'
import type { ArrayOption, DataPickerOption, ObjectOption, ObjectPath } from './data-picker-utils'

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

  const elementToUseForNextColumn = selectedElement ?? pseudoSelectedElementForArray

  const nextColumnScope: ArrayOption | ObjectOption | null = (() => {
    if (elementToUseForNextColumn != null) {
      if (
        elementToUseForNextColumn.type === 'object' ||
        elementToUseForNextColumn.type === 'array'
      ) {
        return elementToUseForNextColumn
      }
    }
    return null
  })()

  const nextColumnScopeValue =
    elementToUseForNextColumn?.type === 'primitive' || elementToUseForNextColumn?.type === 'jsx'
      ? elementToUseForNextColumn
      : null

  const dataSource = useVariableDataSource(props.originalDataForScope)

  const isLastColumn = nextColumnScope == null && nextColumnScopeValue == null
  const columnRef = useScrollIntoView(isLastColumn)

  return (
    <>
      <DataSelectorFlexColumn ref={columnRef}>
        {activeScope.map((option) => {
          return (
            <RowWithCartouche
              key={option.variableInfo.expression}
              data={option}
              isLeaf={nextColumnScope == null}
              selected={option === selectedElement}
              onTargetPathChange={props.onTargetPathChange}
              forcedDataSource={dataSource}
            />
          )
        })}
      </DataSelectorFlexColumn>
      {nextColumnScope != null ? (
        <DataSelectorColumn
          activeScope={nextColumnScope.children}
          targetPathInsideScope={targetPathInsideScope}
          onTargetPathChange={props.onTargetPathChange}
          currentlyShowingScopeForArray={nextColumnScope.type === 'array'}
          originalDataForScope={props.originalDataForScope ?? selectedElement}
        />
      ) : null}
      {nextColumnScopeValue != null ? <ValuePreviewColumn data={nextColumnScopeValue} /> : null}
    </>
  )
})

interface ValuePreviewColumnProps {
  data: DataPickerOption
}

const ValuePreviewColumn = React.memo((props: ValuePreviewColumnProps) => {
  const text = JSON.stringify(props.data.variableInfo.value, undefined, 2)
  const ref = useScrollIntoView(true)
  return (
    <DataSelectorFlexColumn ref={ref}>
      <div
        style={{
          padding: 4,
          border: '1px solid #ccc',
          borderRadius: 4,
          minHeight: 150,
          overflowX: 'hidden',
          overflowY: 'scroll',
          scrollbarWidth: 'auto',
          scrollbarColor: 'gray transparent',
          ['textWrap' as any]: 'balance', // I think we need to update some typings here?
        }}
      >
        {text}
      </div>
      <span>{variableTypeForInfo(props.data)}</span>
    </DataSelectorFlexColumn>
  )
})

function variableTypeForInfo(info: DataPickerOption): string {
  switch (info.type) {
    case 'array':
      return 'Array'
    case 'object':
      return 'Object'
    case 'primitive':
      return typeof info.variableInfo.value
    case 'jsx':
      return 'JSX'
    default:
      assertNever(info)
  }
}

interface RowWithCartoucheProps {
  data: DataPickerOption
  selected: boolean
  isLeaf: boolean
  forcedDataSource: CartoucheSource | null
  onTargetPathChange: (newTargetPath: ObjectPath) => void
}
const RowWithCartouche = React.memo((props: RowWithCartoucheProps) => {
  const { onTargetPathChange, data, forcedDataSource, isLeaf } = props
  const targetPath = data.valuePath

  const dataSource = useVariableDataSource(data)

  const onClick: React.MouseEventHandler<HTMLDivElement> = React.useCallback(
    (e) => {
      e.stopPropagation()
      onTargetPathChange(targetPath)
    },
    [targetPath, onTargetPathChange],
  )

  const ref = useScrollIntoView(props.selected)

  return (
    <FlexRow
      onClick={onClick}
      ref={ref}
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
          selected={!data.disabled && props.selected}
          showBackground={!data.disabled}
          highlight={data.disabled ? 'disabled' : null}
          role={'information'}
          testId={`data-selector-option-${data.variableInfo.expression}`}
        >
          {data.variableInfo.expressionPathPart}
        </CartoucheUI>
      </span>
      {unless(isLeaf, <span>{'>'}</span>)}
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

const DataSelectorFlexColumn = styled(FlexColumn)({
  width: 200,
  height: '100%',
  flexShrink: 0,
  overflowX: 'hidden',
  overflowY: 'scroll',
  scrollbarWidth: 'auto',
  scrollbarColor: 'gray transparent',
})

function useScrollIntoView(shouldScroll: boolean) {
  const elementRef = React.useRef<HTMLDivElement>(null)

  React.useEffect(() => {
    if (shouldScroll && elementRef.current != null) {
      elementRef.current.scrollIntoView({
        behavior: 'instant',
      })
    }
  }, [shouldScroll])

  return elementRef
}
