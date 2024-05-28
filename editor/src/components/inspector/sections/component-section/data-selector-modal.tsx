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
import type {
  ArrayOption,
  DataPickerCallback,
  JSXOption,
  ObjectOption,
  PrimitiveOption,
  VariableOption,
} from './data-picker-popup'
import { InspectorModal } from '../../widgets/inspector-modal'
import type { SelectOption } from '../../controls/select-control'
import {
  arrayEqualsByReference,
  arrayEqualsByValue,
  assertNever,
} from '../../../../core/shared/utils'
import { getControlStyles } from '../../common/control-styles'
import { isPrefixOf, last } from '../../../../core/shared/array-utils'
import { jsExpressionOtherJavaScriptSimple } from '../../../../core/shared/element-template'
import type { CartoucheUIProps } from './cartouche-ui'
import { CartoucheUI } from './cartouche-ui'
import { when } from '../../../../utils/react-conditionals'

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
          gridColumn: spanGridColumns == null ? undefined : `1 / span ${spanGridColumns}`,
        }}
      ></div>
    )
  },
)

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

      const [navigatedToPath, setNavigatedToPath] = React.useState<Array<string | number>>([])

      // TODO invariant: currentValuePath should be a prefix of currentSelectedPath, we should enforce this
      const [currentSelectedPath, setCurrentSelectedPath] = React.useState<Array<string | number>>(
        [],
      )

      // TODO invariant: currentValuePath should be a prefix of currentHoveredPath, we should enforce this
      const [currentHoveredPath, setCurrentHoveredPath] = React.useState<Array<string | number>>([])

      const [indexLookup, setIndexLookup] = React.useState<ValuePathLookup<number>>({})
      const updateIndexInLookup = React.useCallback(
        (valuePathString: string) => (option: SelectOption) =>
          setIndexLookup((lookup) => ({ ...lookup, [valuePathString]: option.value })),
        [],
      )

      const optionLookup = useVariableOptionLookup(variablesInScope)

      const focusedVariableChildren = React.useMemo(() => {
        if (navigatedToPath.length === 0) {
          return variablesInScope
        }

        const elementToSet = optionLookup[navigatedToPath.toString()]
        if (
          elementToSet == null ||
          (elementToSet.type !== 'array' && elementToSet.type !== 'object')
        ) {
          return [] // TODO this should never happen!
        }
        return elementToSet.children
      }, [navigatedToPath, optionLookup, variablesInScope])

      const { primitiveVars, folderVars } = React.useMemo(() => {
        let primitives: Array<PrimitiveOption | JSXOption> = []
        let folders: Array<ArrayOption | ObjectOption> = []

        for (const option of focusedVariableChildren) {
          switch (option.type) {
            case 'array':
            case 'object':
              folders.push(option)
              break
            case 'jsx':
            case 'primitive':
              primitives.push(option)
              break
            default:
              assertNever(option)
          }
        }

        return { primitiveVars: primitives, folderVars: folders }
      }, [focusedVariableChildren])

      const setCurrentSelectedPathCurried = React.useCallback(
        (path: VariableOption['valuePath']) => () => {
          if (!isPrefixOf(navigatedToPath, path)) {
            // if navigatedToPath is not a prefix of path, we don't update the selection
            return
          }
          setCurrentSelectedPath(path)
        },
        [navigatedToPath],
      )

      const setNavigatedToPathCurried = React.useCallback(
        (path: VariableOption['valuePath']) => (e: React.MouseEvent) => {
          e.stopPropagation()
          e.preventDefault()

          setNavigatedToPath(path)
          setCurrentSelectedPath([])
        },
        [],
      )

      const onBackClick = React.useCallback(
        (e: React.MouseEvent) => {
          e.stopPropagation()
          e.preventDefault()
          if (navigatedToPath == null) {
            return
          }
          const path = navigatedToPath.slice(0, -1)
          setNavigatedToPathCurried(path)(e)
        },
        [navigatedToPath, setNavigatedToPathCurried],
      )

      const onApplyClick = React.useCallback(() => {
        if (navigatedToPath == null) {
          return
        }
        const variable = optionLookup[navigatedToPath.toString()]
        if (variable == null) {
          return
        }

        onPropertyPicked(
          jsExpressionOtherJavaScriptSimple(variable.variableInfo.expression, [
            variable.definedElsewhere,
          ]),
        )
        closePopup()
      }, [closePopup, navigatedToPath, onPropertyPicked, optionLookup])

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
                    isDisabled={navigatedToPath?.length === 0}
                    onClick={onBackClick}
                  />
                  {currentSelectedPath == null
                    ? null
                    : nonEmptyPathPrefixes(currentSelectedPath, optionLookup).map(
                        ({ segment, path, role }) => (
                          <CartoucheUI
                            key={segment}
                            onClick={setNavigatedToPathCurried(path)}
                            tooltip={segment.toString()}
                            source={'internal'}
                            inverted={false}
                            selected={false}
                            role={role}
                            testId={`data-selector-top-bar-segment-${segment}`}
                          >
                            {segment.toString()}
                          </CartoucheUI>
                        ),
                      )}
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
                {when(
                  primitiveVars.length > 0,
                  <>
                    <FlexRow
                      style={{
                        gridColumn: '3',
                        flexWrap: 'wrap',
                        height: 'max-content',
                        gap: 4,
                      }}
                    >
                      {primitiveVars.map((variable) => (
                        <CartoucheUI
                          key={variable.valuePath.toString()}
                          tooltip={variableNameFromPath(variable)}
                          source={'internal'}
                          inverted={false}
                          selected={false}
                          role={cartoucheFolderOrInfo(variable)}
                          testId={`data-selector-primitive-values-${variableNameFromPath(
                            variable,
                          )}`}
                          onClick={setCurrentSelectedPathCurried(variable.valuePath)}
                        >
                          {variableNameFromPath(variable)}
                        </CartoucheUI>
                      ))}
                    </FlexRow>
                    <Separator color={colorTheme.seperator.value} spanGridColumns={3} margin={4} />
                  </>,
                )}
                {folderVars.map((variable, idx) => (
                  <React.Fragment key={variable.valuePath.toString()}>
                    <CartoucheUI
                      tooltip={variableNameFromPath(variable)}
                      source={'internal'}
                      inverted={false}
                      selected={false}
                      role='selection'
                      testId={`data-selector-left-section-${variableNameFromPath(variable)}`}
                    >
                      {variableNameFromPath(variable)}
                    </CartoucheUI>
                    {variable.type === 'array' ? (
                      <ElementSelector
                        total={variable.children.length}
                        selected={indexLookup[variable.valuePath.toString()] ?? 0}
                        onSelect={updateIndexInLookup(variable.valuePath.toString())}
                      />
                    ) : (
                      <div />
                    )}
                    {/* properties in scope */}
                    <FlexRow style={{ flexWrap: 'wrap', height: 'max-content', gap: 4 }}>
                      {childVars(variable, indexLookup).map((child) => (
                        <CartoucheUI
                          key={child.valuePath.toString()}
                          tooltip={variableNameFromPath(child)}
                          source={'internal'}
                          inverted={false}
                          selected={arrayEqualsByReference(currentSelectedPath, child.valuePath)}
                          role={cartoucheFolderOrInfo(child)}
                          testId={`data-selector-right-section-${variableNameFromPath(child)}`}
                          onClick={setCurrentSelectedPathCurried(child.valuePath)}
                          onDoubleClick={setNavigatedToPathCurried(child.valuePath)}
                        >
                          {variableNameFromPath(child)}
                        </CartoucheUI>
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

export function nonEmptyPathPrefixes(
  valuePath: VariableOption['valuePath'],
  lookup: ValuePathLookup<VariableOption>,
): Array<{ segment: string | number; path: (string | number)[]; role: CartoucheUIProps['role'] }> {
  let accumulator = []
  let current: (string | number)[] = []
  for (const segment of valuePath) {
    current.push(segment)
    const optionFromLookup = lookup[current.toString()]

    accumulator.push({
      segment: segment,
      path: [...current],
      role: cartoucheFolderOrInfo(optionFromLookup),
    })
  }
  return accumulator
}

function variableNameFromPath(variable: VariableOption): string {
  return last(variable.valuePath)?.toString() ?? variable.variableInfo.expression.toString()
}

function cartoucheFolderOrInfo(option: VariableOption): CartoucheUIProps['role'] {
  switch (option.type) {
    case 'array':
    case 'object':
      return 'folder'
    case 'jsx':
    case 'primitive':
      return 'information'
    default:
      assertNever(option)
  }
}
