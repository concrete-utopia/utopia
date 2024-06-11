import styled from '@emotion/styled'
import React from 'react'
import {
  dataPathSuccess,
  traceDataFromVariableName,
} from '../../../../core/data-tracing/data-tracing'
import { isPrefixOf } from '../../../../core/shared/array-utils'
import { arrayEqualsByReference, assertNever } from '../../../../core/shared/utils'
import { unless, when } from '../../../../utils/react-conditionals'
import { FlexColumn, FlexRow, colorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import type { CartoucheSource, CartoucheUIProps } from './cartouche-ui'
import { CartoucheUI } from './cartouche-ui'
import type { ArrayOption, DataPickerOption, ObjectOption, ObjectPath } from './data-picker-utils'
import { MapCounterUi } from '../../../navigator/navigator-item/map-counter'

interface DataSelectorColumnsProps {
  activeScope: Array<DataPickerOption>
  targetPathInsideScope: ObjectPath
  onTargetPathChange: (newTargetPath: ObjectPath) => void
}

export const DataSelectorColumns = React.memo((props: DataSelectorColumnsProps) => {
  return (
    <FlexRow
      style={{
        flexGrow: 1,
        alignItems: 'flex-start',
        overflowX: 'scroll',
        overflowY: 'hidden',
        scrollbarWidth: 'auto',
        scrollbarColor: 'gray transparent',
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

  const elementOnSelectedPath: DataPickerOption | null =
    activeScope.find((option) => isPrefixOf(option.valuePath, targetPathInsideScope)) ?? null

  // if the current scope is an array, we want to show not only the array indexes, but also the contents of the first element of the array
  const pseudoSelectedElementForArray: DataPickerOption | null =
    elementOnSelectedPath == null && currentlyShowingScopeForArray && activeScope.length > 0
      ? activeScope[0]
      : null

  const nextColumnScope: ArrayOption | ObjectOption | null = (() => {
    const elementToUseForNextColumn = elementOnSelectedPath ?? pseudoSelectedElementForArray
    if (elementToUseForNextColumn != null) {
      if (
        (elementToUseForNextColumn.type === 'object' ||
          elementToUseForNextColumn.type === 'array') &&
        elementToUseForNextColumn.children.length > 0
      ) {
        return elementToUseForNextColumn
      }
    }
    return null
  })()

  const nextColumnScopeValue = nextColumnScope == null ? elementOnSelectedPath : null

  const dataSource = useVariableDataSource(props.originalDataForScope)

  const isLastColumn = nextColumnScope == null && nextColumnScopeValue == null
  const columnRef = useScrollIntoView(isLastColumn)

  return (
    <>
      <DataSelectorFlexColumn ref={columnRef}>
        {activeScope.map((option, index) => {
          const selected = arrayEqualsByReference(option.valuePath, targetPathInsideScope)
          const onActivePath = isPrefixOf(option.valuePath, targetPathInsideScope)
          return (
            <RowWithCartouche
              key={option.variableInfo.expression}
              data={option}
              currentlyShowingScopeForArray={currentlyShowingScopeForArray}
              isLeaf={nextColumnScope == null}
              selected={selected}
              onActivePath={onActivePath}
              forceShowArrow={pseudoSelectedElementForArray != null && index === 0}
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
          originalDataForScope={props.originalDataForScope ?? elementOnSelectedPath}
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
          width: 200,
          overflowWrap: 'break-word',
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
  currentlyShowingScopeForArray: boolean
  selected: boolean
  onActivePath: boolean
  forceShowArrow: boolean
  isLeaf: boolean
  forcedDataSource: CartoucheSource | null
  onTargetPathChange: (newTargetPath: ObjectPath) => void
}
const RowWithCartouche = React.memo((props: RowWithCartoucheProps) => {
  const {
    onTargetPathChange,
    data,
    currentlyShowingScopeForArray,
    forcedDataSource,
    isLeaf,
    selected,
    onActivePath,
    forceShowArrow,
  } = props
  const targetPath = data.valuePath

  const dataSource = useVariableDataSource(data)

  const onClick: React.MouseEventHandler<HTMLDivElement> = React.useCallback(
    (e) => {
      e.stopPropagation()
      onTargetPathChange(targetPath)
    },
    [targetPath, onTargetPathChange],
  )

  const ref = useScrollIntoView(selected)

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
        backgroundColor: onActivePath ? colorTheme.bg4.value : undefined,
        padding: 5,
        cursor: 'pointer',
      }}
    >
      <span>
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
          {currentlyShowingScopeForArray ? (
            <>
              <span style={{ fontStyle: 'italic' }}>item </span>
              {data.variableInfo.expressionPathPart}
            </>
          ) : (
            data.variableInfo.expressionPathPart
          )}
        </CartoucheUI>
      </span>
      <span
        style={{
          color: colorTheme.fg6.value,
          opacity: !isLeaf && (onActivePath || forceShowArrow) ? 1 : 0,
        }}
      >
        {'>'}
      </span>
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
  minWidth: 200,
  height: '100%',
  flexShrink: 0,
  overflowX: 'hidden',
  overflowY: 'scroll',
  scrollbarWidth: 'auto',
  scrollbarColor: 'gray transparent',
  paddingRight: 10, // to account for scrollbar
  paddingLeft: 6,
  paddingBottom: 10,
  borderRight: `1px solid ${colorTheme.subduedBorder.cssValue}`,
})

function useScrollIntoView(shouldScroll: boolean) {
  const elementRef = React.useRef<HTMLDivElement>(null)

  React.useEffect(() => {
    if (shouldScroll && elementRef.current != null) {
      elementRef.current.scrollIntoView({
        behavior: 'instant',
        block: 'nearest',
        inline: 'nearest',
      })
    }
  }, [shouldScroll])

  return elementRef
}
