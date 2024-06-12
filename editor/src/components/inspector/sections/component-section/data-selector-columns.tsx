import styled from '@emotion/styled'
import React from 'react'
import { isPrefixOf } from '../../../../core/shared/array-utils'
import { arrayEqualsByReference, assertNever } from '../../../../core/shared/utils'
import { FlexColumn, FlexRow, colorTheme } from '../../../../uuiui'
import type { CartoucheSource } from './cartouche-ui'
import type { ArrayOption, DataPickerOption, ObjectOption, ObjectPath } from './data-picker-utils'
import { DataPickerCartouche, useVariableDataSource } from './data-selector-cartouche'

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
    forcedDataSource,
    isLeaf,
    selected,
    onActivePath,
    forceShowArrow,
  } = props
  const targetPath = data.valuePath

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
        <DataPickerCartouche data={data} forcedDataSource={forcedDataSource} selected={selected} />
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
