import React from 'react'
import {
  FlexColumn,
  FlexRow,
  Icons,
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
import { arrayEqualsByReference, assertNever } from '../../../../core/shared/utils'
import { getControlStyles } from '../../common/control-styles'
import { isPrefixOf, last } from '../../../../core/shared/array-utils'
import { jsExpressionOtherJavaScriptSimple } from '../../../../core/shared/element-template'
import type { CartoucheUIProps, HoverHandlers } from './cartouche-ui'
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
const SLASH = 'X'

export const DataSelectorModal = React.memo(
  React.forwardRef<HTMLDivElement, DataSelectorModalProps>(
    ({ style, closePopup, variablesInScope, onPropertyPicked }, forwardedRef) => {
      const colorTheme = useColorTheme()

      const [navigatedToPath, setNavigatedToPath] = React.useState<Array<string | number>>([])
      const [hoveredPath, setHoveredPath] = React.useState<VariableOption['valuePath'] | null>(null)

      const onHover = React.useCallback(
        (path: VariableOption['valuePath']): HoverHandlers => ({
          onMouseEnter: () => setHoveredPath(path),
          onMouseLeave: () => setHoveredPath(null),
        }),
        [],
      )

      // TODO invariant: currentValuePath should be a prefix of currentSelectedPath, we should enforce this
      const [currentSelectedPath, setCurrentSelectedPath] = React.useState<Array<
        string | number
      > | null>(null)

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
          setCurrentSelectedPath(null)
        },
        [],
      )

      const onHomeClick = React.useCallback(
        (e: React.MouseEvent) => {
          e.stopPropagation()
          e.preventDefault()

          setNavigatedToPathCurried([])(e)
        },
        [setNavigatedToPathCurried],
      )

      const pathInTopBar = currentSelectedPath ?? navigatedToPath
      const pathInTopBarIncludingHover = hoveredPath ?? pathInTopBar

      const onApplyClick = React.useCallback(() => {
        const variable = optionLookup[pathInTopBar.toString()]
        if (variable == null) {
          return
        }

        onPropertyPicked(
          jsExpressionOtherJavaScriptSimple(variable.variableInfo.expression, [
            variable.definedElsewhere,
          ]),
        )
        closePopup()
      }, [closePopup, onPropertyPicked, optionLookup, pathInTopBar])

      const catchClick = React.useCallback((e: React.MouseEvent) => {
        e.stopPropagation()
        e.preventDefault()

        setCurrentSelectedPath(null)
      }, [])

      const valuePreviewText = (() => {
        const variable = optionLookup[pathInTopBarIncludingHover.toString()]
        if (variable == null) {
          return null
        }

        return JSON.stringify(variable.variableInfo.value, undefined, 2)
      })()

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
              <FlexRow style={{ justifyContent: 'space-between', alignItems: 'center', gap: 8 }}>
                <FlexRow style={{ gap: 8, flexWrap: 'wrap', flexGrow: 1 }}>
                  <FlexRow
                    style={{
                      flexGrow: 1,
                      borderStyle: 'solid',
                      borderWidth: 1,
                      borderColor: colorTheme.fg7.value,
                      borderRadius: 4,
                      fontSize: 11,
                      fontWeight: 400,
                      height: 33,
                      padding: '0px 6px',
                    }}
                  >
                    <FlexRow style={{ flexWrap: 'wrap', flexGrow: 1 }}>
                      {nonEmptyPathPrefixes(pathInTopBarIncludingHover, optionLookup).map(
                        ({ segment, path }, idx) => (
                          <span key={path.toString()}>
                            {idx === 0 ? segment : pathSegmentToString(segment)}
                          </span>
                        ),
                      )}
                    </FlexRow>
                    <div
                      style={{
                        ...disabledButtonStyles(pathInTopBar.length === 0),
                        fontWeight: 400,
                        fontSize: 12,
                      }}
                      onClick={onHomeClick}
                    >
                      <Icons.Cross />
                    </div>
                  </FlexRow>
                </FlexRow>
                <div
                  style={{
                    borderRadius: 4,
                    backgroundColor: colorTheme.primary.value,
                    color: 'white',
                    padding: '8px 12px',
                    fontSize: 14,
                    fontWeight: 500,
                    ...disabledButtonStyles(pathInTopBar.length === 0),
                  }}
                  onClick={onApplyClick}
                >
                  Apply
                </div>
              </FlexRow>
              <FlexRow
                style={{
                  gridColumn: '3',
                  flexWrap: 'wrap',
                  gap: 4,
                  overflowX: 'scroll',
                  opacity: 0.8,
                  fontSize: 10,
                  height: 20,
                }}
              >
                {valuePreviewText}
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
                          datatype={childTypeToCartoucheDataType(variable.type)}
                          inverted={false}
                          selected={
                            currentSelectedPath == null
                              ? false
                              : arrayEqualsByReference(currentSelectedPath, variable.valuePath)
                          }
                          role={cartoucheFolderOrInfo(variable, 'no-folder')}
                          testId={`data-selector-primitive-values-${variableNameFromPath(
                            variable,
                          )}`}
                          onHover={onHover(variable.valuePath)}
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
                      datatype={childTypeToCartoucheDataType(variable.type)}
                      source={'internal'}
                      inverted={false}
                      selected={
                        currentSelectedPath == null
                          ? false
                          : arrayEqualsByReference(currentSelectedPath, variable.valuePath)
                      }
                      role={cartoucheFolderOrInfo(variable, 'no-folder')}
                      testId={`data-selector-left-section-${variableNameFromPath(variable)}`}
                      onClick={setCurrentSelectedPathCurried(variable.valuePath)}
                      onHover={onHover(variable.valuePath)}
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
                          datatype={childTypeToCartoucheDataType(child.type)}
                          selected={
                            currentSelectedPath == null
                              ? false
                              : arrayEqualsByReference(currentSelectedPath, child.valuePath)
                          }
                          role={cartoucheFolderOrInfo(child, 'can-be-folder')}
                          testId={`data-selector-right-section-${variableNameFromPath(child)}`}
                          onClick={setCurrentSelectedPathCurried(child.valuePath)}
                          onDoubleClick={setNavigatedToPathCurried(child.valuePath)}
                          onHover={onHover(child.valuePath)}
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

function childTypeToCartoucheDataType(
  childType: VariableOption['type'],
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
): Array<{
  segment: string | number
  path: (string | number)[]
  role: CartoucheUIProps['role']
  type: CartoucheUIProps['datatype']
}> {
  let accumulator = []
  let current: (string | number)[] = []
  for (const segment of valuePath) {
    current.push(segment)
    const optionFromLookup = lookup[current.toString()]

    accumulator.push({
      segment: segment,
      path: [...current],
      role: cartoucheFolderOrInfo(optionFromLookup, 'can-be-folder'),
      type: childTypeToCartoucheDataType(optionFromLookup.type),
    })
  }
  return accumulator
}

function variableNameFromPath(variable: VariableOption): string {
  return last(variable.valuePath)?.toString() ?? variable.variableInfo.expression.toString()
}

function cartoucheFolderOrInfo(
  option: VariableOption,
  canBeFolder: 'no-folder' | 'can-be-folder',
): CartoucheUIProps['role'] {
  if (option.variableInfo.matches) {
    return 'selection'
  }
  switch (option.type) {
    case 'object':
      return canBeFolder === 'can-be-folder' ? 'folder' : 'information'
    case 'array':
    case 'jsx':
    case 'primitive':
      return 'information'
    default:
      assertNever(option)
  }
}

function isPreviewSegment(
  pathInTopBar: VariableOption['valuePath'],
  pathUpToSegment: VariableOption['valuePath'],
) {
  return !isPrefixOf(pathUpToSegment, pathInTopBar)
}

function disabledButtonStyles(disabled: boolean): React.CSSProperties {
  return {
    opacity: disabled ? 0.5 : 1,
    cursor: disabled ? undefined : 'pointer',
  }
}

function pathSegmentToString(segment: string | number) {
  if (typeof segment === 'string') {
    return `.${segment}`
  }
  return `[${segment}]`
}
