import React from 'react'
import { groupBy } from '../../../../core/shared/array-utils'
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
import { DataPickerCartouche } from './data-selector-cartouche'

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
      const setSelectedScopeAndResetSelection = React.useCallback(
        (scope: ElementPath | FileRootPath) => {
          setSelectedScope(scope)
          setSelectedPath([])
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

      const scopeLabels = useEditorState(
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
        'DataSelectorModal scopeLabels',
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

      const setSelectedPathFromColumns = React.useCallback((newPath: ObjectPath) => {
        setSelectedPath(newPath)
      }, [])

      const catchClick = React.useCallback((e: React.MouseEvent) => {
        e.stopPropagation()
        e.preventDefault()
      }, [])

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
        () => applyVariable(selectedPath),
        [applyVariable, selectedPath],
      )

      const navigateToSearchResult = React.useCallback(
        (path: ObjectPath) => {
          setSearchTerm(null)
          setSelectedPathFromColumns(path)
        },
        [setSelectedPathFromColumns],
      )

      const searchNullOrEmpty = searchTerm == null || searchTerm.length < 1

      const pickedVariable = processedVariablesInScope[
        selectedPath.toString()
      ] as DataPickerOption | null

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
              onClick={catchClick}
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
              <DataSelectorLeftSidebar
                scopes={scopeLabels}
                activeScope={selectedScope}
                setSelectedScope={setSelectedScopeAndResetSelection}
              />
              <FlexColumn
                style={{
                  alignSelf: 'stretch',
                  flexGrow: 1,
                  overflow: 'hidden',
                  width: 700,
                }}
              >
                <FlexRow style={{ padding: '8px 8px 5px' }}>
                  <FlexRow
                    style={{
                      flexGrow: 1,
                      height: 24,
                      padding: '6px 8px',
                      borderRadius: 4,
                      gap: 8,
                      border: `1px solid ${colorTheme.subduedBorder.value}`,
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
                      placeholder='Search data'
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
                </FlexRow>
                <FlexColumn
                  style={{
                    flexGrow: 1,
                    overflow: 'hidden',
                    contain: 'content',
                    borderTop: `1px solid ${colorTheme.subduedBorder.cssValue}`,
                  }}
                >
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
                {when(
                  searchNullOrEmpty,
                  <>
                    {/* bottom bar */}
                    <FlexRow
                      style={{
                        justifyContent: 'space-between',
                        alignItems: 'center',
                        gap: 8,
                        borderTop: `1px solid ${colorTheme.subduedBorder.cssValue}`,
                        paddingTop: 10,
                        paddingBottom: 10,
                        paddingRight: 16,
                      }}
                    >
                      <FlexRow style={{ gap: 8, paddingRight: 8, flexWrap: 'wrap', flexGrow: 1 }}>
                        <FlexRow
                          style={{
                            flexGrow: 1,
                            fontSize: 11,
                            fontWeight: 500,
                            padding: '0px 6px',
                            alignItems: 'center',
                          }}
                        >
                          <FlexRow
                            data-testid={DataSelectorPopupBreadCrumbsTestId}
                            style={{ flexWrap: 'wrap', flexGrow: 1 }}
                          >
                            <span>Selection:</span>
                            {pickedVariable != null ? (
                              <DataPickerCartouche
                                data={pickedVariable}
                                selected={false}
                                forceRole='information'
                              />
                            ) : null}
                          </FlexRow>
                        </FlexRow>
                      </FlexRow>
                      <div
                        style={{
                          borderRadius: 4,
                          backgroundColor: colorTheme.white.value,
                          color: colorTheme.fg0.value,
                          border: `1px solid ${colorTheme.subduedBorder.value}`,
                          padding: 3,
                          fontSize: 11,
                          fontWeight: 400,
                          height: 24,
                          width: 81,
                          textAlign: 'center',
                        }}
                        onClick={closePopup}
                      >
                        Cancel
                      </div>
                      <div
                        style={{
                          borderRadius: 4,
                          backgroundColor: colorTheme.black.value,
                          color: 'white',
                          padding: 3,
                          fontSize: 11,
                          fontWeight: 400,
                          height: 24,
                          width: 81,
                          textAlign: 'center',
                          ...disabledButtonStyles(pickedVariable?.disabled ?? true),
                        }}
                        onClick={onApplyClick}
                      >
                        Apply
                      </div>
                    </FlexRow>
                  </>,
                )}
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

function disabledButtonStyles(disabled: boolean): React.CSSProperties {
  return {
    opacity: disabled ? 0.5 : 1,
    cursor: disabled ? undefined : 'pointer',
  }
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
