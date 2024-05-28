import React from 'react'
import {
  FlexColumn,
  FlexRow,
  Icn,
  PopupList,
  UtopiaStyles,
  UtopiaTheme,
  useColorTheme,
} from '../../../../uuiui'
import type { DataPickerCallback, VariableOption } from './data-picker-popup'
import { InspectorModal } from '../../widgets/inspector-modal'
import type { SelectOption } from '../../controls/select-control'
import { NO_OP, assertNever } from '../../../../core/shared/utils'
import { getControlStyles } from '../../common/control-styles'
import { last } from '../../../../core/shared/array-utils'
import { jsExpressionOtherJavaScriptSimple } from '../../../../core/shared/element-template'

export interface DataSelectorModalProps {
  closePopup: () => void
  style: React.CSSProperties
  variablesInScope: VariableOption[]
  onPropertyPicked: DataPickerCallback
}

const Separator = React.memo(
  ({
    color,
    spanGridColumns,
    margin,
  }: {
    color: string
    spanGridColumns?: number
    margin: number
  }) => {
    return (
      <div
        style={{
          width: '100%',
          height: 1,
          margin: `${margin}px 0px`,
          backgroundColor: color,
          gridColumn: spanGridColumns == null ? undefined : '1 / span 3',
        }}
      ></div>
    )
  },
)

function DataLabel({
  text,
  color,
  bgColor,
  icon,
  borderColor,
  borderRadius,
  onClick,
  onDoubleClick,
}: {
  text: string
  color?: string
  bgColor?: string
  borderRadius?: number
  borderColor?: string
  icon?: React.ReactElement
  onClick: ((e: React.MouseEvent) => void) | null
  onDoubleClick?: (e: React.MouseEvent) => void
}) {
  return (
    <FlexRow
      onClick={onClick ?? NO_OP}
      onDoubleClick={onDoubleClick}
      style={{
        padding: '4px 8px',
        color: color,
        gap: 4,
        fontWeight: 600,
        alignItems: 'baseline',
        borderRadius: borderRadius,
        border: borderColor == null ? undefined : `1px solid ${borderColor}`,
        backgroundColor: bgColor == null ? undefined : bgColor,
        width: 'max-content',
        height: 'max-content',
        cursor: onClick == null ? undefined : 'pointer',
      }}
    >
      {icon}
      <span>{text}</span>
    </FlexRow>
  )
}

const SIMPLE_CONTROL_STYLES = getControlStyles('simple')

function ElementSelector({
  total,
  selected,
  onSelect,
}: {
  total: number
  selected: number
  onSelect: (_: SelectOption) => void
}) {
  const options: SelectOption[] = React.useMemo(
    () =>
      Array(total)
        .fill(0)
        .map((_, i) => ({ value: i, label: `${i + 1}` })),
    [total],
  )

  return (
    <PopupList
      id={'data-selector-index-select'}
      value={{ label: `${selected + 1}`, value: selected }}
      options={options}
      onSubmitValue={onSelect}
      controlStyles={SIMPLE_CONTROL_STYLES}
      style={{ background: 'transparent' }}
    />
  )
}

const DEFAULT_SIZE: React.CSSProperties = {
  minWidth: 600,
  minHeight: 50,
  maxWidth: 700,
  maxHeight: 300,
}
const BRACES = '{}'
const CIRCLE = '◯'

export const DataSelectorModal = React.memo(
  React.forwardRef<HTMLDivElement, DataSelectorModalProps>(
    ({ style, closePopup, variablesInScope, onPropertyPicked }, forwardedRef) => {
      const colorTheme = useColorTheme()

      const [indexLookup, setIndexLookup] = React.useState<ValuePathLookup<number>>({})
      const updateIndexInLookup = React.useCallback(
        (valuePathString: string) => (option: SelectOption) =>
          setIndexLookup((lookup) => ({ ...lookup, [valuePathString]: option.value })),
        [],
      )

      const [focusedVariableChildren, setFocusedVariableChildren] =
        React.useState<VariableOption[]>(variablesInScope)
      const [currentValuePath, setCurrentValuePath] = React.useState<Array<string | number>>([])

      const optionLookup = useVariableOptionLookup(variablesInScope)

      const setCurrentValuePathCurried = React.useCallback(
        (path: VariableOption['valuePath']) => () => setCurrentValuePath(path),
        [],
      )

      const updateDisplayedValuesCurried = React.useCallback(
        (path: VariableOption['valuePath']) => (e: React.MouseEvent) => {
          e.stopPropagation()
          e.preventDefault()

          setCurrentValuePath(path)

          if (path.length === 0) {
            setFocusedVariableChildren(variablesInScope)
            return
          }

          const pathStringToSet = path.toString()
          const elementToSet = optionLookup[pathStringToSet]
          if (
            elementToSet != null &&
            (elementToSet.type === 'array' || elementToSet.type === 'object')
          ) {
            setFocusedVariableChildren(elementToSet.children)
          }
        },
        [optionLookup, variablesInScope],
      )

      const onBackClick = React.useCallback(
        (e: React.MouseEvent) => {
          e.stopPropagation()
          e.preventDefault()
          if (currentValuePath == null) {
            return
          }
          const path = currentValuePath.slice(0, -1)
          updateDisplayedValuesCurried(path)(e)
        },
        [currentValuePath, updateDisplayedValuesCurried],
      )

      const onApplyClick = React.useCallback(() => {
        if (currentValuePath == null) {
          return
        }
        const variable = optionLookup[currentValuePath.toString()]
        if (variable == null) {
          return
        }

        onPropertyPicked(
          jsExpressionOtherJavaScriptSimple(variable.variableInfo.expression, [
            variable.definedElsewhere,
          ]),
        )
        closePopup()
      }, [closePopup, currentValuePath, onPropertyPicked, optionLookup])

      const catchClick = React.useCallback((e: React.MouseEvent) => {
        e.stopPropagation()
        e.preventDefault()
      }, [])

      return (
        <InspectorModal
          offsetX={20}
          offsetY={0}
          closePopup={closePopup}
          style={{
            zIndex: 1,
          }}
          closePopupOnUnmount={false}
          outsideClickIgnoreClass={'ignore-react-onclickoutside-data-picker'}
        >
          <div // this entire wrapper div was made before using the InspectorModal, so it should be re-done
            style={{
              background: 'transparent',
              position: 'fixed',
              top: 0,
              left: 0,
              right: 0,
              bottom: 0,
              zIndex: 1, // so it's above the inspector
            }}
            onClick={closePopup}
          >
            <FlexColumn
              onClick={catchClick}
              ref={forwardedRef}
              style={{
                ...style,
                ...DEFAULT_SIZE,
                paddingTop: 16,
                paddingLeft: 16,
                paddingRight: 16,
                backgroundColor: colorTheme.inspectorBackground.value,
                color: colorTheme.fg1.value,
                overflow: 'hidden',
                borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
                boxShadow: UtopiaStyles.shadowStyles.highest.boxShadow,
                border: `1px solid ${colorTheme.fg0Opacity10.value}`,
              }}
            >
              {/* top bar */}
              <FlexRow style={{ justifyContent: 'space-between', alignItems: 'center' }}>
                <FlexRow style={{ gap: 8 }}>
                  <Icn
                    category='semantic'
                    type='icon-semantic-back'
                    width={18}
                    height={18}
                    style={{ cursor: 'pointer' }}
                    isDisabled={currentValuePath?.length === 0}
                    onClick={onBackClick}
                  />
                  {currentValuePath == null
                    ? null
                    : nonEmptyPrefixes(currentValuePath).map(({ segment, path }) => (
                        <DataLabel
                          key={segment}
                          text={segment.toString()}
                          borderRadius={4}
                          borderColor={colorTheme.fg0Opacity20.value}
                          onClick={updateDisplayedValuesCurried(path)}
                          // icon={
                          //   <span
                          //     style={{
                          //       color: colorTheme.brandNeonGreen.value,
                          //       fontWeight: 700,
                          //       fontSize: 14,
                          //     }}
                          //   >
                          //     {BRACES}
                          //   </span>
                          // }
                        />
                      ))}
                </FlexRow>
                <div
                  style={{
                    borderRadius: 4,
                    backgroundColor: colorTheme.primary.value,
                    color: 'white',
                    padding: '8px 12px',
                    fontSize: 14,
                    fontWeight: 500,
                    cursor: 'pointer',
                  }}
                  onClick={onApplyClick}
                >
                  Apply
                </div>
              </FlexRow>
              <Separator color={colorTheme.seperator.value} margin={12} />
              {/* detail view */}
              <div
                style={{
                  display: 'grid',
                  gridTemplateColumns: 'auto 40px 1fr',
                  gap: 8,
                  overflowY: 'scroll',
                  overflowX: 'hidden',
                  paddingBottom: 16,
                }}
              >
                {focusedVariableChildren.map((variable, idx) => (
                  <React.Fragment key={variable.valuePath.toString()}>
                    <DataLabel
                      text={
                        last(variable.valuePath)?.toString() ??
                        variable.variableInfo.expression.toString()
                      }
                      bgColor={colorTheme.selectionBlue10.value}
                      color={colorTheme.dynamicBlue.value}
                      borderRadius={4}
                      onClick={null}
                    />
                    {variable.type === 'array' ? (
                      <ElementSelector
                        total={variable.children.length}
                        selected={indexLookup[variable.valuePath.toString()] ?? 1}
                        onSelect={updateIndexInLookup(variable.valuePath.toString())}
                      />
                    ) : (
                      <div />
                    )}
                    {/* properties in scope */}
                    <FlexRow style={{ flexWrap: 'wrap', height: 'max-content' }}>
                      {childVars(variable, indexLookup).map((child) => (
                        <DataLabel
                          key={child.valuePath.toString()}
                          text={
                            last(child.valuePath)?.toString() ??
                            child.variableInfo.expression.toString()
                          }
                          icon={
                            <span
                              style={{
                                color: colorTheme.brandNeonGreen.value,
                                fontWeight: 700,
                                fontSize: 10,
                              }}
                            >
                              {CIRCLE}
                            </span>
                          }
                          onClick={setCurrentValuePathCurried(child.valuePath)}
                          onDoubleClick={updateDisplayedValuesCurried(child.valuePath)}
                        />
                      ))}
                    </FlexRow>
                    {idx < focusedVariableChildren.length - 1 ? (
                      <Separator
                        color={colorTheme.seperator.value}
                        spanGridColumns={3}
                        margin={4}
                      />
                    ) : null}
                  </React.Fragment>
                ))}
              </div>
            </FlexColumn>
          </div>
        </InspectorModal>
      )
    },
  ),
)

interface ValuePathLookup<T> {
  [valuePath: string]: T
}

function useVariableOptionLookup(options: VariableOption[]): ValuePathLookup<VariableOption> {
  return React.useMemo(() => {
    let lookup: ValuePathLookup<VariableOption> = {}
    function walk(option: VariableOption) {
      lookup[option.valuePath.toString()] = option
      switch (option.type) {
        case 'array':
        case 'object':
          option.children.forEach((c) => walk(c))
          return
        case 'jsx':
        case 'primitive':
          return
        default:
          assertNever(option)
      }
    }
    options.forEach((o) => walk(o))
    return lookup
  }, [options])
}

function childVars(option: VariableOption, indices: ValuePathLookup<number>): VariableOption[] {
  switch (option.type) {
    case 'object':
      return option.children
    case 'array':
      return childVars(option.children[indices[option.valuePath.toString()] ?? 0], indices)
    case 'jsx':
    case 'primitive':
      return []
    default:
      assertNever(option)
  }
}

export function nonEmptyPrefixes(
  ts: (string | number)[],
): Array<{ segment: string | number; path: (string | number)[] }> {
  let accumulator: Array<{ segment: string | number; path: (string | number)[] }> = []
  let current: (string | number)[] = []
  for (const t of ts) {
    current.push(t)
    accumulator.push({ segment: t, path: [...current] })
  }
  return accumulator
}
