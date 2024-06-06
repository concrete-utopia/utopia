import React from 'react'
import * as EP from '../../../core/shared/element-path'
import { Icons, Tooltip, useColorTheme } from '../../../uuiui'
import { useDispatch } from '../../editor/store/dispatch-context'
import type { DataReferenceNavigatorEntry, NavigatorEntry } from '../../editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import type { CondensedNavigatorRow } from '../navigator-row'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getNavigatorEntryLabel, labelSelector } from './navigator-item-wrapper'
import { BasePaddingUnit, elementWarningsSelector } from './navigator-item'
import { setHighlightedViews, toggleCollapse } from '../../editor/actions/action-creators'
import { selectComponents } from '../../editor/actions/meta-actions'
import type { ElementPath } from 'utopia-shared/src/types'
import { unless, when } from '../../../utils/react-conditionals'
import { ExpandableIndicator } from './expandable-indicator'
import { LayoutIcon } from './layout-icon'
import { DataReferenceCartoucheControl } from '../../inspector/sections/component-section/data-reference-cartouche'

function useEntryLabel(entry: NavigatorEntry) {
  const labelForTheElement = useEditorState(
    Substores.projectContentsAndMetadata,
    (store) => labelSelector(store, entry),
    'CondensedEntry labelSelector',
  )

  const entryLabel = React.useMemo(() => {
    return getNavigatorEntryLabel(entry, labelForTheElement)
  }, [entry, labelForTheElement])

  return entryLabel
}

export const CondensedEntryItemWrapper = React.memo(
  (props: { windowStyle: React.CSSProperties; navigatorRow: CondensedNavigatorRow }) => {
    const colorTheme = useColorTheme()

    const selectedViews = useEditorState(
      Substores.selectedViews,
      (store) => store.editor.selectedViews,
      'CondensedEntryItemWrapper selectedViews',
    )

    const hasSelection = React.useMemo(() => {
      return selectedViews.some((path) =>
        props.navigatorRow.entries.some(
          (entry) => entry.type !== 'DATA_REFERENCE' && EP.pathsEqual(path, entry.elementPath),
        ),
      )
    }, [selectedViews, props.navigatorRow])

    const wholeRowInsideSelection = React.useMemo(() => {
      return selectedViews.some((path) => {
        return props.navigatorRow.entries.every((entry) => {
          return EP.isDescendantOfOrEqualTo(entry.elementPath, path)
        })
      })
    }, [selectedViews, props.navigatorRow])

    const isCollapsed = useEditorState(
      Substores.navigator,
      (store) =>
        store.editor.navigator.collapsedViews.some((path) =>
          props.navigatorRow.entries.some((entry) => EP.pathsEqual(path, entry.elementPath)),
        ),
      'CondensedEntryItemWrapper isCollapsed',
    )

    const isDataReferenceRow = React.useMemo(() => {
      return props.navigatorRow.entries.every(
        (entry, idx) =>
          (idx === 0 && entry.type === 'REGULAR') || (idx > 0 && entry.type === 'DATA_REFERENCE'),
      )
    }, [props.navigatorRow])

    const rowContainsSelection = React.useMemo(() => {
      return props.navigatorRow.entries.some((entry) =>
        selectedViews.some((view) => EP.pathsEqual(view, entry.elementPath)),
      )
    }, [selectedViews, props.navigatorRow])

    const rowRootSelected = React.useMemo(() => {
      return selectedViews.some((view) =>
        EP.pathsEqual(view, props.navigatorRow.entries[0].elementPath),
      )
    }, [selectedViews, props.navigatorRow])

    return (
      <div
        style={{
          ...props.windowStyle,
          display: 'flex',
          alignItems: 'center',
          backgroundColor: rowRootSelected
            ? colorTheme.selectionBlue.value
            : hasSelection || wholeRowInsideSelection
            ? colorTheme.childSelectionBlue.value
            : 'transparent',
          borderTopLeftRadius: rowContainsSelection ? 5 : 0,
          borderTopRightRadius: rowContainsSelection ? 5 : 0,
          borderBottomLeftRadius:
            rowContainsSelection && (isCollapsed || isDataReferenceRow) ? 5 : 0,
          borderBottomRightRadius:
            rowContainsSelection && (isCollapsed || isDataReferenceRow) ? 5 : 0,
          overflowX: 'auto',
        }}
      >
        {props.navigatorRow.entries.map((entry, idx) => {
          const showSeparator =
            props.navigatorRow.variant === 'trunk' && idx < props.navigatorRow.entries.length - 1

          return (
            <CondensedEntryItem
              navigatorRow={props.navigatorRow}
              isDataReferenceRow={isDataReferenceRow}
              showExpandableIndicator={idx === 0}
              key={EP.toString(entry.elementPath)}
              entry={entry}
              showSeparator={showSeparator}
              wholeRowInsideSelection={wholeRowInsideSelection}
              rowContainsSelection={rowContainsSelection}
              rowRootSelected={rowRootSelected}
            />
          )
        })}
      </div>
    )
  },
)
CondensedEntryItemWrapper.displayName = 'CondensedEntryItemWrapper'

const CondensedEntryItem = React.memo(
  (props: {
    entry: NavigatorEntry
    navigatorRow: CondensedNavigatorRow
    isDataReferenceRow: boolean
    rowContainsSelection: boolean
    rowRootSelected: boolean
    wholeRowInsideSelection: boolean
    showSeparator: boolean
    showExpandableIndicator: boolean
  }) => {
    const colorTheme = useColorTheme()

    const selectedViews = useEditorState(
      Substores.selectedViews,
      (store) => store.editor.selectedViews,
      'CondensedEntryItem selectedViews',
    )

    const rowEntriesBeforeThisOne = React.useMemo(() => {
      let entries: NavigatorEntry[] = []
      for (const entry of props.navigatorRow.entries) {
        if (EP.pathsEqual(entry.elementPath, props.entry.elementPath)) {
          break
        }
        entries.push(entry)
      }
      return entries
    }, [props.entry, props.navigatorRow])

    const isChildOfSelected = React.useMemo(() => {
      return selectedViews.some((path) => {
        return rowEntriesBeforeThisOne.some((other) => EP.pathsEqual(other.elementPath, path))
      })
    }, [selectedViews, rowEntriesBeforeThisOne])

    const selectionIsDataReference = React.useMemo(() => {
      const entries = props.navigatorRow.entries.filter((entry) =>
        selectedViews.some((path) => EP.pathsEqual(path, entry.elementPath)),
      )
      return entries.some((entry) => entry.type === 'DATA_REFERENCE')
    }, [props.navigatorRow, selectedViews])

    const isSelected = React.useMemo(() => {
      return selectedViews.some((path) => EP.pathsEqual(path, props.entry.elementPath))
    }, [selectedViews, props.entry])

    const indentation = React.useMemo(() => {
      if (!props.showExpandableIndicator) {
        return 0
      }
      return BasePaddingUnit * props.navigatorRow.indentation
    }, [props.showExpandableIndicator, props.navigatorRow.indentation])

    const backgroundColor = React.useMemo((): string => {
      if (props.wholeRowInsideSelection) {
        return 'transparent'
      } else if (!selectionIsDataReference && (isSelected || isChildOfSelected)) {
        return colorTheme.childSelectionBlue.value
      } else {
        return colorTheme.bg1.value
      }
    }, [
      props.wholeRowInsideSelection,
      colorTheme,
      selectionIsDataReference,
      isSelected,
      isChildOfSelected,
    ])

    return (
      <React.Fragment>
        <CondensedEntryItemContent
          entry={props.entry}
          wholeRowInsideSelection={props.wholeRowInsideSelection}
          selected={isSelected}
          isChildOfSelected={isChildOfSelected}
          showExpandableIndicator={props.showExpandableIndicator}
          isDataReferenceRow={props.isDataReferenceRow}
          indentation={indentation}
          rowRootSelected={props.rowRootSelected}
        />
        {when(
          props.showSeparator,
          <CondensedEntryTrunkSeparator
            backgroundColor={backgroundColor}
            selected={props.rowRootSelected}
          />,
        )}
      </React.Fragment>
    )
  },
)
CondensedEntryItem.displayName = 'CondensedEntryItem'

const CondensedEntryItemContent = React.memo(
  (props: {
    entry: NavigatorEntry
    wholeRowInsideSelection: boolean
    isChildOfSelected: boolean
    selected: boolean
    rowRootSelected: boolean
    showExpandableIndicator: boolean
    isDataReferenceRow: boolean
    indentation: number
  }) => {
    const dispatch = useDispatch()
    const colorTheme = useColorTheme()

    const highlightedViews = useEditorState(
      Substores.highlightedHoveredViews,
      (store) => store.editor.highlightedViews,
      'CondensedEntryItemContent highlightedViews',
    )

    const isCollapsed = useEditorState(
      Substores.navigator,
      (store) =>
        store.editor.navigator.collapsedViews.some((path) =>
          EP.pathsEqual(path, props.entry.elementPath),
        ),
      'CondensedEntryItemContent isCollapsed',
    )

    const showLabel = useEditorState(
      Substores.metadata,
      (store) => {
        return (
          MetadataUtils.isProbablyScene(store.editor.jsxMetadata, props.entry.elementPath) ||
          MetadataUtils.isProbablyRemixScene(store.editor.jsxMetadata, props.entry.elementPath)
        )
      },
      'CondensedEntryItemContent isScene',
    )

    const isDataReference = React.useMemo(() => {
      return props.entry.type === 'DATA_REFERENCE'
    }, [props.entry])

    const onClick = React.useCallback(
      (e: React.MouseEvent) => {
        e.preventDefault()
        e.stopPropagation()
        dispatch(selectComponents([props.entry.elementPath], false))
      },
      [dispatch, props.entry],
    )

    const onMouseOver = React.useCallback(() => {
      dispatch([setHighlightedViews([props.entry.elementPath])])
    }, [props.entry, dispatch])

    const onMouseOut = React.useCallback(() => {
      dispatch([
        setHighlightedViews(
          highlightedViews.filter((path) => !EP.pathsEqual(path, props.entry.elementPath)),
        ),
      ])
    }, [props.entry, dispatch, highlightedViews])

    return (
      <div
        style={{
          minWidth: 29,
          height: 29,
          display: 'flex',
          flexShrink: 0,
          alignItems: 'center',
          justifyContent: 'center',
          backgroundColor:
            !props.wholeRowInsideSelection && !props.isChildOfSelected
              ? colorTheme.bg1.value
              : undefined,
          borderTopRightRadius: props.selected ? 5 : 0,
          borderBottomRightRadius: props.selected ? 5 : 0,
          marginRight: !props.showExpandableIndicator && props.isDataReferenceRow ? 4 : 0,
        }}
        onClick={onClick}
        onMouseOver={onMouseOver}
        onMouseOut={onMouseOut}
      >
        <div
          style={{
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            borderRadius: 5,
            backgroundColor:
              props.selected && !isDataReference ? colorTheme.selectionBlue.value : undefined,
            width: '100%',
            height: '100%',
            padding: props.showExpandableIndicator ? '0px 5px' : 0,
            paddingLeft: props.indentation,
          }}
        >
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
            }}
          >
            {when(
              props.showExpandableIndicator,
              <WrappedExpandableIndicator
                elementPath={props.entry.elementPath}
                selected={props.selected}
                collapsed={isCollapsed}
                disabled={props.isDataReferenceRow}
                invisible={props.isDataReferenceRow && props.entry.type === 'REGULAR'}
              />,
            )}
            {unless(
              isDataReference,
              <WrappedLayoutIcon
                entry={props.entry}
                hideTooltip={isDataReference || showLabel}
                selected={props.selected || props.rowRootSelected}
              />,
            )}
          </div>
          {when(showLabel, <WrappedLabel selected={props.selected} entry={props.entry} />)}
          {when(
            isDataReference,
            <DataReferenceCartoucheControl
              {...(props.entry as DataReferenceNavigatorEntry)}
              selected={props.selected}
              highlight={
                props.rowRootSelected ? 'strong' : props.wholeRowInsideSelection ? 'subtle' : null
              }
              hideTooltip={true}
            />,
          )}
        </div>
      </div>
    )
  },
)
CondensedEntryItemContent.displayName = 'CondensedEntryItemContent'

type CondensedEntryTrunkSeparatorProps = {
  backgroundColor: string
  selected: boolean
}

const CondensedEntryTrunkSeparator = React.memo((props: CondensedEntryTrunkSeparatorProps) => {
  const colorTheme = useColorTheme()

  return (
    <div
      style={{
        backgroundColor: props.backgroundColor,
        height: '100%',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <div
        style={{
          width: 12,
          height: 12,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          color: colorTheme.fg6.value,
        }}
      >
        <Icons.NarrowExpansionArrowRight color={props.selected ? 'white' : 'main'} />
      </div>
    </div>
  )
})

CondensedEntryTrunkSeparator.displayName = 'CondensedEntryTrunkSeparator'

const WrappedExpandableIndicator = React.memo(
  (props: {
    elementPath: ElementPath
    selected: boolean
    collapsed: boolean
    disabled: boolean
    invisible: boolean
  }) => {
    const dispatch = useDispatch()

    const onClickCollapse = React.useCallback(
      (elementPath: ElementPath) => (e: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
        if (props.disabled) {
          return
        }
        e.stopPropagation()
        dispatch([toggleCollapse(elementPath)], 'leftpane')
      },
      [dispatch, props.disabled],
    )

    return (
      <div
        style={{
          width: 22,
          height: 29,
          marginLeft: 5,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
        onClick={onClickCollapse(props.elementPath)}
      >
        {when(props.invisible, <div style={{ width: 12, height: 12 }} />)}
        {unless(
          props.disabled || props.invisible,
          <ExpandableIndicator
            visible={true}
            collapsed={props.collapsed}
            selected={false}
            iconColor={props.selected ? 'white' : 'main'}
            style={{
              opacity: 'var(--paneHoverOpacity)',
            }}
          />,
        )}
      </div>
    )
  },
)
WrappedExpandableIndicator.displayName = 'WrappedExpandableIndicator'

const WrappedLayoutIcon = React.memo(
  (props: { entry: NavigatorEntry; hideTooltip: boolean; selected: boolean }) => {
    const entryLabel = useEntryLabel(props.entry)

    const iconOverride = useEditorState(
      Substores.propertyControlsInfo,
      (store) =>
        MetadataUtils.getIconOfComponent(
          props.entry.elementPath,
          store.editor.propertyControlsInfo,
          store.editor.projectContents,
        ),
      'WrappedLayoutIcon iconOverride',
    )

    const elementWarnings = useEditorState(
      Substores.derived,
      (store) => elementWarningsSelector(store, props.entry),
      'WrappedLayoutIcon elementWarningsSelector',
    )

    return (
      <Tooltip title={entryLabel} disabled={props.hideTooltip}>
        <div
          style={{
            width: '100%',
            height: '100%',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
          }}
        >
          <LayoutIcon
            navigatorEntry={props.entry}
            override={iconOverride}
            color={props.selected ? 'white' : 'main'}
            elementWarnings={elementWarnings}
          />
        </div>
      </Tooltip>
    )
  },
)
WrappedLayoutIcon.displayName = 'WrappedLayoutIcon'

const WrappedLabel = React.memo((props: { selected: boolean; entry: NavigatorEntry }) => {
  const entryLabel = useEntryLabel(props.entry)

  return (
    <span
      style={{
        color: props.selected ? 'white' : undefined,
        padding: '0 4px',
      }}
    >
      {entryLabel}
    </span>
  )
})
WrappedLabel.displayName = 'WrappedLabel'
