import type { CSSProperties } from 'react'
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
      return selectedViews.some((path) =>
        props.navigatorRow.entries.every((entry) => EP.isDescendantOf(entry.elementPath, path)),
      )
    }, [selectedViews, props.navigatorRow])

    return (
      <div
        style={{
          ...props.windowStyle,
          display: 'flex',
          alignItems: 'center',
          backgroundColor:
            hasSelection || wholeRowInsideSelection
              ? colorTheme.childSelectionBlue.value
              : 'transparent',
          borderTopLeftRadius: wholeRowInsideSelection ? 0 : 5,
          borderTopRightRadius: wholeRowInsideSelection ? 0 : 5,
          overflowX: 'auto',
        }}
      >
        {props.navigatorRow.entries.map((entry, idx) => {
          const showSeparator =
            props.navigatorRow.variant === 'trunk' && idx < props.navigatorRow.entries.length - 1

          return (
            <CondensedEntryItem
              navigatorRow={props.navigatorRow}
              showExpandableIndicator={idx === 0}
              key={EP.toString(entry.elementPath)}
              entry={entry}
              showSeparator={showSeparator}
              wholeRowInsideSelection={wholeRowInsideSelection}
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

    const isDataReferenceRow = React.useMemo(() => {
      return props.navigatorRow.entries.every(
        (entry, idx) =>
          (idx === 0 && entry.type === 'REGULAR') || (idx > 0 && entry.type === 'DATA_REFERENCE'),
      )
    }, [props.navigatorRow])

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
          isDataReferenceRow={isDataReferenceRow}
          indentation={indentation}
        />
        {when(
          props.showSeparator,
          <CondensedEntryTrunkSeparator backgroundColor={backgroundColor} />,
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
            gap: 5,
            borderRadius: 5,
            backgroundColor:
              props.selected && !isDataReference ? colorTheme.selectionBlue.value : undefined,
            width: '100%',
            height: '100%',
            padding: props.showExpandableIndicator ? '0px 5px' : 0,
            paddingLeft: props.indentation,
          }}
        >
          {when(
            props.showExpandableIndicator,
            <WrappedExpandableIndicator
              elementPath={props.entry.elementPath}
              selected={props.selected}
              collapsed={isCollapsed}
              disabled={props.isDataReferenceRow}
            />,
          )}
          {unless(
            isDataReference,
            <WrappedLayoutIcon
              entry={props.entry}
              disabled={isDataReference || showLabel}
              selected={props.selected}
            />,
          )}
          {when(showLabel, <WrappedLabel selected={props.selected} entry={props.entry} />)}
          {when(
            isDataReference && props.entry.type === 'DATA_REFERENCE',
            <WrappedCartouche
              selected={props.selected}
              entry={props.entry as DataReferenceNavigatorEntry}
            />,
          )}
        </div>
      </div>
    )
  },
)
CondensedEntryItemContent.displayName = 'CondensedEntryItemContent'

const CondensedEntryTrunkSeparator = React.memo((props: { backgroundColor: string }) => {
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
        <Icons.NarrowExpansionArrowRight />
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
  }) => {
    const dispatch = useDispatch()

    const onClickCollapse = React.useCallback(
      (elementPath: ElementPath) => (e: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
        e.stopPropagation()
        dispatch([toggleCollapse(elementPath)], 'leftpane')
      },
      [dispatch],
    )

    return (
      <div
        style={{
          width: 12,
          height: 29,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
        onClick={onClickCollapse(props.elementPath)}
      >
        {unless(
          props.disabled,
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
  (props: { entry: NavigatorEntry; disabled: boolean; selected: boolean }) => {
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
      <Tooltip title={entryLabel} disabled={props.disabled}>
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

const WrappedCartouche = React.memo(
  (props: { selected: boolean; entry: DataReferenceNavigatorEntry }) => {
    return (
      <div style={{ paddingLeft: 6 }}>
        <DataReferenceCartoucheControl selected={props.selected} {...props.entry} />
      </div>
    )
  },
)
WrappedCartouche.displayName = 'WrappedCartouche'
