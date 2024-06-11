import React from 'react'
import { groupBy, isPrefixOf, last } from '../../../../core/shared/array-utils'
import { jsExpressionOtherJavaScriptSimple } from '../../../../core/shared/element-template'
import {
  CanvasContextMenuPortalTargetID,
  arrayEqualsByReference,
  assertNever,
} from '../../../../core/shared/utils'
import { unless } from '../../../../utils/react-conditionals'
import {
  FlexColumn,
  FlexRow,
  Icons,
  LargerIcons,
  PopupList,
  UtopiaStyles,
  UtopiaTheme,
  useColorTheme,
} from '../../../../uuiui'
import { getControlStyles } from '../../common/control-styles'
import type { SelectOption } from '../../controls/select-control'
import { InspectorModal } from '../../widgets/inspector-modal'
import type { CartoucheUIProps, HoverHandlers } from './cartouche-ui'
import {
  type ArrayOption,
  type DataPickerCallback,
  type JSXOption,
  type ObjectOption,
  type PrimitiveOption,
  type DataPickerOption,
  type ObjectPath,
  getEnclosingScopes,
  childTypeToCartoucheDataType,
  cartoucheFolderOrInfo,
} from './data-picker-utils'
import {
  dataPathSuccess,
  traceDataFromVariableName,
} from '../../../../core/data-tracing/data-tracing'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import * as EP from '../../../../core/shared/element-path'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { FileRootPath } from '../../../canvas/ui-jsx-canvas'
import { insertionCeilingToString, insertionCeilingsEqual } from '../../../canvas/ui-jsx-canvas'
import { DataSelectorSearch } from './data-selector-search'
import { DataSelectorColumns } from './data-selector-columns'

export const DataSelectorPopupBreadCrumbsTestId = 'data-selector-modal-top-bar'

export interface DataSelectorModalProps {
  closePopup: () => void
  style: React.CSSProperties
  variablesInScope: DataPickerOption[]
  onPropertyPicked: DataPickerCallback
  startingSelectedValuePath: ObjectPath | null
  lowestInsertionCeiling: ElementPath | null
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

function ArrayIndexSelector({
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

interface ProcessedVariablesInScope {
  [valuePath: string]: DataPickerOption
}

interface ArrayIndexLookup {
  [valuePath: string]: number
}

export const DataSelectorModal = React.memo(
  React.forwardRef<HTMLDivElement, DataSelectorModalProps>(
    (
      {
        style,
        closePopup,
        variablesInScope: allVariablesInScope,
        onPropertyPicked,
        startingSelectedValuePath,
        lowestInsertionCeiling,
      },
      forwardedRef,
    ) => {
      const colorTheme = useColorTheme()

      const scopeBuckets = React.useMemo(
        () => putVariablesIntoScopeBuckets(allVariablesInScope),
        [allVariablesInScope],
      )

      const lowestMatchingScope: ElementPath | FileRootPath = React.useMemo(() => {
        if (lowestInsertionCeiling == null) {
          return { type: 'file-root' }
        }

        const scopeFromStartedSelectedValuePath = optionalMap(
          (s) => getSelectedScopeFromBuckets(s, scopeBuckets),
          startingSelectedValuePath,
        )

        if (scopeFromStartedSelectedValuePath != null) {
          return scopeFromStartedSelectedValuePath
        }

        const matchingScope = findClosestMatchingScope(lowestInsertionCeiling, scopeBuckets)
        return matchingScope
      }, [lowestInsertionCeiling, startingSelectedValuePath, scopeBuckets])

      const [selectedScope, setSelectedScope] = React.useState<ElementPath | FileRootPath>(
        lowestMatchingScope,
      )
      const setSelectedScopeCurried = React.useCallback(
        (name: ElementPath, hasContent: boolean) => () => {
          if (hasContent) {
            setSelectedScope(name)
            setSelectedPath([])
            setHoveredPath(null)
            setNavigatedToPath([])
          }
        },
        [],
      )

      const { filteredVariablesInScope } = useFilterVariablesInScope(
        allVariablesInScope,
        scopeBuckets,
        selectedScope,
      )

      const processedVariablesInScope = useProcessVariablesInScope(allVariablesInScope)

      const searchBoxRef = React.useRef<HTMLInputElement>(null)

      const elementLabelsWithScopes = useEditorState(
        Substores.fullStore,
        (store) => {
          const scopes = getEnclosingScopes(
            store.editor.jsxMetadata,
            store.editor.allElementProps,
            store.editor.elementPathTree,
            store.editor.projectContents,
            Object.keys(scopeBuckets),
            lowestInsertionCeiling ?? EP.emptyElementPath,
          )
          return scopes.map(({ insertionCeiling, label, hasContent }) => ({
            label: label,
            scope: insertionCeiling,
            hasContent: hasContent,
          }))
        },
        'DataSelectorModal elementLabelsWithScopes',
      )

      const [navigatedToPath, setNavigatedToPath] = React.useState<ObjectPath>(
        findFirstObjectPathToNavigateTo(processedVariablesInScope, startingSelectedValuePath) ?? [],
      )

      const [selectedPath, setSelectedPath] = React.useState<ObjectPath>(
        startingSelectedValuePath ?? [],
      )

      const [searchTerm, setSearchTerm] = React.useState<string | null>(null)
      const onStartSearch = React.useCallback(() => {
        searchBoxRef.current?.focus()
        setSearchTerm('')
      }, [])

      const onSearchFieldValueChange = React.useCallback(
        (e: React.ChangeEvent<HTMLInputElement>) => {
          e.stopPropagation()
          e.preventDefault()
          if (e.target.value.length === 0) {
            setSearchTerm(null)
          } else {
            setSearchTerm(e.target.value)
          }
        },
        [],
      )

      const [hoveredPath, setHoveredPath] = React.useState<ObjectPath | null>(null)

      const setSelectedPathFromColumns = React.useCallback((newPath: ObjectPath) => {
        setSelectedPath(newPath)
        setHoveredPath(null)
        setNavigatedToPath([])
      }, [])

      const setNavigatedToPathCurried = React.useCallback(
        (path: DataPickerOption['valuePath']) => (e: React.MouseEvent) => {
          e.stopPropagation()
          e.preventDefault()

          setNavigatedToPath(path)
          setSelectedPath([])
          setHoveredPath(null)
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

      const catchClick = React.useCallback((e: React.MouseEvent) => {
        e.stopPropagation()
        e.preventDefault()
      }, [])

      const onHover = React.useCallback(
        (path: DataPickerOption['valuePath']): HoverHandlers => ({
          onMouseEnter: () => setHoveredPath(path),
          onMouseLeave: () => setHoveredPath(null),
        }),
        [],
      )

      const [indexLookup, setIndexLookup] = React.useState<ArrayIndexLookup>({})
      const updateIndexInLookup = React.useCallback(
        (valuePathString: string) => (option: SelectOption) =>
          setIndexLookup((lookup) => ({ ...lookup, [valuePathString]: option.value })),
        [],
      )

      const focusedVariableChildren = React.useMemo(() => {
        if (navigatedToPath.length === 0) {
          return filteredVariablesInScope
        }

        const innerScopeToShow = processedVariablesInScope[navigatedToPath.toString()]
        if (
          innerScopeToShow == null ||
          (innerScopeToShow.type !== 'array' && innerScopeToShow.type !== 'object')
        ) {
          return [] // TODO this should never happen!
        }
        return innerScopeToShow.children
      }, [navigatedToPath, processedVariablesInScope, filteredVariablesInScope])

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

      const metadata = useEditorState(
        Substores.metadata,
        (store) => store.editor.jsxMetadata,
        'DataSelectorModal metadata',
      )
      const projectContents = useEditorState(
        Substores.projectContents,
        (store) => store.editor.projectContents,
        'DataSelectorModal projectContents',
      )

      const variableSources = React.useMemo(() => {
        let result: { [valuePath: string]: CartoucheUIProps['source'] } = {}
        for (const variable of allVariablesInScope) {
          const container = variable.variableInfo.insertionCeiling
          const trace = traceDataFromVariableName(
            container,
            variable.variableInfo.expression,
            metadata,
            projectContents,
            dataPathSuccess([]),
          )

          switch (trace.type) {
            case 'hook-result':
              result[variable.valuePath.toString()] = 'external'
              break
            case 'literal-attribute':
            case 'literal-assignment':
              result[variable.valuePath.toString()] = 'literal'
              break
            case 'component-prop':
            case 'element-at-scope':
            case 'failed':
              result[variable.valuePath.toString()] = 'internal'
              break
            default:
              assertNever(trace)
          }
        }
        return result
      }, [allVariablesInScope, metadata, projectContents])

      const setCurrentSelectedPathCurried = React.useCallback(
        (path: DataPickerOption['valuePath']) => () => {
          if (!isPrefixOf(navigatedToPath, path)) {
            // if navigatedToPath is not a prefix of path, we don't update the selection
            return
          }
          setSelectedPath(path)
        },
        [navigatedToPath],
      )

      const activeTargetPath = selectedPath ?? navigatedToPath
      const pathInTopBarIncludingHover = hoveredPath ?? activeTargetPath

      const applyVariable = React.useCallback(
        (path: ObjectPath) => {
          const variable = processedVariablesInScope[path.toString()]
          if (variable == null) {
            return
          }
          if (variable.disabled) {
            return
          }
          onPropertyPicked(
            jsExpressionOtherJavaScriptSimple(variable.variableInfo.expression, [
              variable.definedElsewhere,
            ]),
          )
          closePopup()
        },
        [closePopup, onPropertyPicked, processedVariablesInScope],
      )

      const onApplyClick = React.useCallback(
        () => applyVariable(activeTargetPath),
        [applyVariable, activeTargetPath],
      )

      const navigateToSearchResult = React.useCallback(
        (path: ObjectPath) => {
          setNavigatedToPath(
            findFirstObjectPathToNavigateTo(processedVariablesInScope, path) ?? path,
          )
          setSelectedPath(path)
        },
        [processedVariablesInScope],
      )

      const valuePreviewText = (() => {
        const variable = processedVariablesInScope[pathInTopBarIncludingHover.toString()]
        if (variable == null) {
          return null
        }

        return JSON.stringify(variable.variableInfo.value, undefined, 2)
      })()

      const searchFocused = searchTerm != null

      return (
        <InspectorModal
          offsetX={20}
          offsetY={0}
          closePopup={closePopup}
          style={{
            zIndex: 1,
          }}
          closePopupOnUnmount={false}
          portalTarget={document.getElementById(CanvasContextMenuPortalTargetID) as HTMLElement}
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
              ref={forwardedRef}
              onClick={catchClick}
              style={{
                width: 700,
                height: 300,
                paddingTop: 16,
                paddingLeft: 16,
                paddingRight: 16,
                backgroundColor: colorTheme.inspectorBackground.value,
                color: colorTheme.fg1.value,
                overflow: 'hidden',
                borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
                boxShadow: UtopiaStyles.shadowStyles.highest.boxShadow,
                border: `1px solid ${colorTheme.fg0Opacity10.value}`,
                ...style,
              }}
            >
              {/* top bar */}
              <FlexRow style={{ justifyContent: 'flex-end', alignItems: 'center', gap: 8 }}>
                <FlexRow
                  style={{
                    gap: 8,
                    flexGrow: 25,
                    opacity: searchFocused ? 0 : 1,
                    pointerEvents: searchFocused ? 'none' : undefined,
                  }}
                >
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
                    <FlexRow
                      data-testid={DataSelectorPopupBreadCrumbsTestId}
                      style={{ flexWrap: 'wrap', flexGrow: 1 }}
                    >
                      {pathBreadcrumbs(pathInTopBarIncludingHover, processedVariablesInScope).map(
                        ({ segment, path }, idx) => (
                          <span key={path.toString()}>
                            {idx === 0 ? segment : pathSegmentToString(segment)}
                          </span>
                        ),
                      )}
                    </FlexRow>
                    <div
                      style={{
                        ...disabledButtonStyles(activeTargetPath.length === 0),
                        fontWeight: 400,
                        fontSize: 12,
                      }}
                      onClick={onHomeClick}
                    >
                      <Icons.Cross />
                    </div>
                  </FlexRow>
                </FlexRow>
                <FlexRow
                  style={{
                    alignItems: 'flex-start',
                    justifyContent: 'flex-start',
                    gap: 8,
                    flexGrow: 1,
                  }}
                >
                  <FlexRow
                    style={{
                      padding: '6px 8px',
                      borderRadius: 16,
                      gap: searchFocused ? 0 : 8,
                      border: `1px solid ${colorTheme.fg7.value}`,
                      flexGrow: 1,
                    }}
                    onClick={onStartSearch}
                  >
                    <LargerIcons.MagnifyingGlass />
                    <input
                      ref={searchBoxRef}
                      value={searchTerm ?? ''}
                      onChange={onSearchFieldValueChange}
                      data-testId='data-selector-modal-search-input'
                      placeholder='Search'
                      style={{
                        outline: 'none',
                        border: 'none',
                        width: searchFocused ? 125 : 0,
                        transition: 'width 0.2s',
                      }}
                    />
                  </FlexRow>
                  {unless(
                    searchFocused,
                    <FlexRow
                      style={{
                        borderRadius: 4,
                        backgroundColor: colorTheme.primary.value,
                        color: 'white',
                        padding: '8px 12px',
                        fontSize: 14,
                        fontWeight: 500,
                        flexGrow: 1,
                        alignItems: 'center',
                        justifyContent: 'center',
                        ...disabledButtonStyles(activeTargetPath.length === 0),
                      }}
                      onClick={onApplyClick}
                    >
                      Apply
                    </FlexRow>,
                  )}
                </FlexRow>
              </FlexRow>
              <FlexColumn style={{ flexGrow: 1, overflow: 'hidden', contain: 'content' }}>
                {searchTerm == null ? (
                  <DataSelectorColumns
                    activeScope={filteredVariablesInScope}
                    targetPathInsideScope={selectedPath}
                    onTargetPathChange={setSelectedPathFromColumns}
                  />
                ) : (
                  <DataSelectorSearch
                    setSearchTerm={setSearchTerm}
                    searchTerm={searchTerm}
                    applyVariable={applyVariable}
                    setNavigatedToPath={navigateToSearchResult}
                    allVariablesInScope={allVariablesInScope}
                    variableSources={variableSources}
                  />
                )}
              </FlexColumn>
              {/* Scope Selector Breadcrumbs */}
              <FlexRow style={{ gap: 2, paddingTop: 16, paddingBottom: 16, opacity: 0.5 }}>
                {elementLabelsWithScopes.map(({ label, scope, hasContent }, idx, a) => (
                  <React.Fragment key={`label-${idx}`}>
                    <div
                      onClick={setSelectedScopeCurried(scope, hasContent)}
                      style={{
                        width: 'max-content',
                        padding: '2px 4px',
                        borderRadius: 4,
                        cursor: hasContent ? 'pointer' : undefined,
                        color: hasContent
                          ? colorTheme.neutralForeground.value
                          : colorTheme.subduedForeground.value,
                        fontSize: 12,
                        fontWeight: insertionCeilingsEqual(selectedScope, scope) ? 800 : undefined,
                      }}
                    >
                      {label}
                    </div>
                    {idx < a.length - 1 ? (
                      <span style={{ width: 'max-content', padding: '2px 4px' }}>{'/'}</span>
                    ) : null}
                  </React.Fragment>
                ))}
              </FlexRow>
            </FlexColumn>
          </div>
        </InspectorModal>
      )
    },
  ),
)

type ScopeBuckets = {
  [insertionCeiling: string]: Array<DataPickerOption>
}

function findClosestMatchingScope(
  targetScope: ElementPath | FileRootPath,
  scopeBuckets: ScopeBuckets,
): ElementPath | FileRootPath {
  if (targetScope.type === 'elementpath') {
    const allPaths = EP.allPathsInsideComponent(targetScope)
    for (const path of allPaths) {
      if (scopeBuckets[EP.toString(path)] != null) {
        return path
      }
    }
  }

  return { type: 'file-root' }
}

function putVariablesIntoScopeBuckets(options: DataPickerOption[]): ScopeBuckets {
  const buckets: { [insertionCeiling: string]: Array<DataPickerOption> } = groupBy(
    (o) => insertionCeilingToString(o.insertionCeiling),
    options,
  )

  return buckets
}

function useFilterVariablesInScope(
  options: DataPickerOption[],
  scopeBuckets: ScopeBuckets,
  scopeToShow: ElementPath | FileRootPath | 'do-not-filter',
): {
  filteredVariablesInScope: Array<DataPickerOption>
} {
  return React.useMemo(() => {
    if (scopeToShow === 'do-not-filter') {
      return {
        filteredVariablesInScope: options,
      }
    }

    const matchingScope = findClosestMatchingScope(scopeToShow, scopeBuckets)
    const filteredOptions: Array<DataPickerOption> =
      scopeBuckets[insertionCeilingToString(matchingScope)] ?? []
    return {
      filteredVariablesInScope: filteredOptions,
    }
  }, [scopeBuckets, options, scopeToShow])
}

function useProcessVariablesInScope(options: DataPickerOption[]): ProcessedVariablesInScope {
  return React.useMemo(() => {
    let lookup: ProcessedVariablesInScope = {}
    function walk(option: DataPickerOption) {
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

function childVars(option: DataPickerOption, indices: ArrayIndexLookup): DataPickerOption[] {
  switch (option.type) {
    case 'object':
      return option.children
    case 'array':
      return option.children.length === 0
        ? []
        : childVars(option.children[indices[option.valuePath.toString()] ?? 0], indices)
    case 'jsx':
    case 'primitive':
      return []
    default:
      assertNever(option)
  }
}

function pathBreadcrumbs(
  valuePath: DataPickerOption['valuePath'],
  processedVariablesInScope: ProcessedVariablesInScope,
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
    const optionFromLookup = processedVariablesInScope[current.toString()]

    if (optionFromLookup == null) {
      continue
    }

    accumulator.push({
      segment: segment,
      path: [...current],
      role: cartoucheFolderOrInfo(optionFromLookup, 'can-be-folder'),
      type: childTypeToCartoucheDataType(optionFromLookup.type),
    })
  }
  return accumulator
}

function variableNameFromPath(variable: DataPickerOption): string {
  return last(variable.valuePath)?.toString() ?? variable.variableInfo.expression.toString()
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

function getSelectedScopeFromBuckets(
  startingSelectedValuePath: ObjectPath,
  scopeBuckets: ScopeBuckets,
): ElementPath | null {
  for (const [pathString, options] of Object.entries(scopeBuckets)) {
    const anyOptionHasMatchingValuePath = options.some((o) =>
      arrayEqualsByReference(o.valuePath, startingSelectedValuePath),
    )
    if (anyOptionHasMatchingValuePath) {
      return EP.fromString(pathString)
    }
  }

  return null
}

function findFirstObjectPathToNavigateTo(
  processedVariablesInScope: ProcessedVariablesInScope,
  selectedValuePath: ObjectPath | null,
): ObjectPath | null {
  if (selectedValuePath == null) {
    return null
  }

  let currentPath = selectedValuePath
  while (currentPath.length > 0) {
    const parentPath = currentPath.slice(0, -1)
    const parentOption = processedVariablesInScope[parentPath.toString()]
    const grandParentPath = currentPath.slice(0, -2)
    const grandParentOption = processedVariablesInScope[grandParentPath.toString()]
    if (grandParentOption != null && grandParentOption.type === 'array') {
      return grandParentPath.slice(0, -1)
    }
    if (parentOption != null && parentOption.type === 'object') {
      return parentPath.slice(0, -1)
    }
    currentPath = currentPath.slice(0, -1)
  }

  return null
}
