import styled from '@emotion/styled'
import React from 'react'
import { isPrefixOf } from '../../../../core/shared/array-utils'
import { arrayEqualsByReference, assertNever } from '../../../../core/shared/utils'
import { FlexColumn, FlexRow, colorTheme } from '../../../../uuiui'
import type { ArrayOption, DataPickerOption, ObjectOption, ObjectPath } from './data-picker-utils'
import { DataPickerCartouche } from './data-selector-cartouche'

interface DataSelectorColumnsProps {
  activeScope: Array<DataPickerOption>
  targetPathInsideScope: ObjectPath
  onTargetPathChange: (newTargetVariable: DataPickerOption) => void
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
      />
    </FlexRow>
  )
})

interface DataSelectorColumnProps {
  activeScope: Array<DataPickerOption>
  currentlyShowingScopeForArray: boolean
  targetPathInsideScope: ObjectPath
  onTargetPathChange: (newTargetVariable: DataPickerOption) => void
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
  const text = safeJSONStringify(props.data.variableInfo.value)
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
  onTargetPathChange: (newTargetVariable: DataPickerOption) => void
}
const RowWithCartouche = React.memo((props: RowWithCartoucheProps) => {
  const { onTargetPathChange, data, isLeaf, selected, onActivePath, forceShowArrow } = props

  const onClick: React.MouseEventHandler<HTMLDivElement> = React.useCallback(
    (e) => {
      e.stopPropagation()
      onTargetPathChange(data)
    },
    [data, onTargetPathChange],
  )

  const ref = useScrollIntoView(selected)

  return (
    <DataPickerRow
      onClick={onClick}
      ref={ref}
      style={{
        backgroundColor: onActivePath ? colorTheme.bg4.value : undefined,
      }}
      disabled={data.variableInfo.matches !== 'matches'}
    >
      <span>
        <DataPickerCartouche data={data} selected={selected} />
      </span>
      <span
        style={{
          color: colorTheme.fg6.value,
          opacity: !isLeaf && (onActivePath || forceShowArrow) ? 1 : 0,
        }}
      >
        {'>'}
      </span>
    </DataPickerRow>
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
  paddingTop: 8,
  paddingRight: 10, // to account for scrollbar
  paddingLeft: 6,
  paddingBottom: 10,
  borderRight: `1px solid ${colorTheme.subduedBorder.cssValue}`,
})

const DataPickerRow = styled(FlexRow)((props: { disabled: boolean }) => ({
  alignSelf: 'stretch',
  justifyContent: 'space-between',
  fontSize: 10,
  borderRadius: 4,
  height: 24,
  padding: 5,
  cursor: !props.disabled ? 'pointer' : 'initial',
}))

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

function safeJSONStringify(value: unknown): string | null {
  try {
    return JSON.stringify(value, undefined, 2)
  } catch {
    return null
  }
}
