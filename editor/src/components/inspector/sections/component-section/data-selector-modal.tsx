import React from 'react'
import { groupBy, mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import { jsExpressionOtherJavaScriptSimple } from '../../../../core/shared/element-template'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import {
  CanvasContextMenuPortalTargetID,
  arrayEqualsByReference,
  assertNever,
} from '../../../../core/shared/utils'
import { when } from '../../../../utils/react-conditionals'
import {
  FlexColumn,
  FlexRow,
  Icons,
  LargerIcons,
  UtopiaStyles,
  UtopiaTheme,
  useColorTheme,
} from '../../../../uuiui'
import type { FileRootPath } from '../../../canvas/ui-jsx-canvas'
import { insertionCeilingToString } from '../../../canvas/ui-jsx-canvas'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { InspectorModal } from '../../widgets/inspector-modal'
import {
  getEnclosingScopes,
  type DataPickerCallback,
  type DataPickerOption,
  type ObjectPath,
} from './data-picker-utils'
import { DataSelectorColumns } from './data-selector-columns'
import { DataSelectorLeftSidebar } from './data-selector-left-sidebar'
import { DataSelectorSearch } from './data-selector-search'
import { stopPropagation } from '../../common/inspector-utils'
import { variableOrChildMatches } from './variables-in-scope-utils'

export const DataSelectorPopupBreadCrumbsTestId = 'data-selector-modal-top-bar'

export interface DataSelectorModalProps {
  closePopup: () => void
  style: React.CSSProperties
  variablesInScope: DataPickerOption[]
  onPropertyPicked: DataPickerCallback
  startingSelectedValuePath: ObjectPath | null
  lowestInsertionCeiling: ElementPath | null
}

interface ProcessedVariablesInScope {
  [valuePath: string]: DataPickerOption
}

export const DataSelectorModal = React.memo(
  React.forwardRef<HTMLDivElement, DataSelectorModalProps>(
    (
      {
        style,
        closePopup,
        variablesInScope,
        onPropertyPicked,
        startingSelectedValuePath,
        lowestInsertionCeiling,
      },
      forwardedRef,
    ) => {
      const allVariablesInScope = React.useMemo(
        () => filterKeepMatchingOnly(variablesInScope),
        [variablesInScope],
      )
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
      const setSelectedScopeAndResetSelection = React.useCallback(
        (scope: ElementPath | FileRootPath) => {
          setSelectedScope(scope)
          setSelectedPath([])
          setHoveredPath(null)
          setNavigatedToPath([])
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

      const cancelSearch = React.useCallback(() => {
        setSearchTerm(null)
        searchBoxRef.current?.blur()
      }, [])

      const onSearchFieldValueChange = React.useCallback(
        (e: React.ChangeEvent<HTMLInputElement>) => {
          e.stopPropagation()
          e.preventDefault()
          setSearchTerm(e.target.value)
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

      const activeTargetPath = selectedPath ?? navigatedToPath
      const pathInTopBarIncludingHover = hoveredPath ?? activeTargetPath

      const applyVariable = React.useCallback(
        (path: ObjectPath) => {
          const variable = processedVariablesInScope[path.toString()]
          if (variable == null) {
            return
          }
          if (variable.variableInfo.matches !== 'matches') {
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

      const valuePreviewText = (() => {
        const variable = processedVariablesInScope[pathInTopBarIncludingHover.toString()]
        if (variable == null) {
          return null
        }
        return JSON.stringify(variable.variableInfo.value, undefined, 2)
      })()

      const navigateToSearchResult = React.useCallback(
        (path: ObjectPath) => {
          setSearchTerm(null)
          setSelectedPathFromColumns(path)
        },
        [setSelectedPathFromColumns],
      )

      const searchNullOrEmpty = searchTerm == null || searchTerm.length < 1

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
            <FlexRow
              ref={forwardedRef}
              style={{
                minWidth: 850,
                height: 300,
                backgroundColor: colorTheme.inspectorBackground.value,
                color: colorTheme.fg1.value,
                overflow: 'hidden',
                borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
                boxShadow: UtopiaStyles.shadowStyles.highest.boxShadow,
                border: `1px solid ${colorTheme.fg0Opacity10.value}`,
                ...style,
              }}
            >
              <FlexColumn
                onClick={stopPropagation}
                style={{
                  alignSelf: 'stretch',
                  minWidth: 150,
                  padding: 8,
                  paddingTop: 16,
                  borderRight: `1px solid ${colorTheme.subduedBorder.cssValue}`,
                  contain: 'layout',
                }}
              >
                <FlexRow
                  style={{
                    height: 24,
                    marginBottom: 16,
                    padding: '6px 8px',
                    borderRadius: 16,
                    gap: 8,
                    border: `1px solid ${colorTheme.fg7.value}`,
                  }}
                >
                  <LargerIcons.MagnifyingGlass style={{ zoom: 0.6 }} />
                  <input
                    onClick={onStartSearch}
                    onFocus={onStartSearch}
                    onChange={onSearchFieldValueChange}
                    ref={searchBoxRef}
                    value={searchTerm ?? ''}
                    data-testId='data-selector-modal-search-input'
                    placeholder='Search'
                    style={{
                      outline: 'none',
                      border: 'none',
                      paddingRight: 14,
                    }}
                  />
                  {when(
                    searchTerm != null,
                    <Icons.CrossInTranslucentCircle
                      style={{ cursor: 'pointer', position: 'fixed', right: 0, marginRight: 14 }}
                      onClick={cancelSearch}
                    />,
                  )}
                </FlexRow>
                <DataSelectorLeftSidebar
                  scopes={elementLabelsWithScopes}
                  activeScope={selectedScope}
                  setSelectedScope={setSelectedScopeAndResetSelection}
                />
              </FlexColumn>
              <FlexColumn
                onClick={catchClick}
                style={{
                  alignSelf: 'stretch',
                  flexGrow: 1,
                  paddingLeft: 8,
                  paddingTop: 16,
                  paddingRight: 16,
                  overflow: 'hidden',
                  width: 700,
                }}
              >
                {when(
                  searchNullOrEmpty,
                  <>
                    {/* top bar */}
                    <FlexRow
                      style={{ justifyContent: 'space-between', alignItems: 'center', gap: 8 }}
                    >
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
                          <FlexRow
                            data-testid={DataSelectorPopupBreadCrumbsTestId}
                            style={{ flexWrap: 'wrap', flexGrow: 1 }}
                          >
                            {pathBreadcrumbs(
                              pathInTopBarIncludingHover,
                              processedVariablesInScope,
                            ).map(({ segment, path }, idx) => (
                              <span key={path.toString()}>
                                {idx === 0 ? segment : pathSegmentToString(segment)}
                              </span>
                            ))}
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
                      <div
                        style={{
                          borderRadius: 4,
                          backgroundColor: colorTheme.primary.value,
                          color: 'white',
                          padding: '8px 12px',
                          fontSize: 14,
                          fontWeight: 500,
                          ...disabledButtonStyles(activeTargetPath.length === 0),
                        }}
                        onClick={onApplyClick}
                      >
                        Apply
                      </div>
                    </FlexRow>
                    {/* Value preview */}
                    <FlexRow
                      style={{
                        flexShrink: 0,
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
                  </>,
                )}
                <FlexColumn style={{ flexGrow: 1, overflow: 'hidden', contain: 'content' }}>
                  {searchNullOrEmpty ? (
                    <DataSelectorColumns
                      activeScope={filteredVariablesInScope}
                      targetPathInsideScope={selectedPath}
                      onTargetPathChange={setSelectedPathFromColumns}
                    />
                  ) : (
                    <DataSelectorSearch
                      searchTerm={searchTerm}
                      setNavigatedToPath={navigateToSearchResult}
                      allVariablesInScope={allVariablesInScope}
                    />
                  )}
                </FlexColumn>
              </FlexColumn>
            </FlexRow>
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

function pathBreadcrumbs(
  valuePath: DataPickerOption['valuePath'],
  processedVariablesInScope: ProcessedVariablesInScope,
): Array<{
  segment: string | number
  path: (string | number)[]
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
    })
  }
  return accumulator
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

function filterVariableOption(option: DataPickerOption): DataPickerOption | null {
  switch (option.type) {
    case 'array':
    case 'object':
      return {
        ...option,
        children: filterKeepMatchingOnly(option.children),
      }
    case 'jsx':
    case 'primitive':
      return variableOrChildMatches(option.variableInfo) ? option : null
    default:
      assertNever(option)
  }
}

function filterKeepMatchingOnly(options: DataPickerOption[]): DataPickerOption[] {
  return mapDropNulls((o) => filterVariableOption(o), options)
}
