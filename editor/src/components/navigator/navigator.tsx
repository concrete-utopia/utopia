/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import type { Size } from 'react-virtualized-auto-sizer'
import AutoSizer from 'react-virtualized-auto-sizer'
import type { ListChildComponentProps } from 'react-window'
import { VariableSizeList } from 'react-window'
import { last, safeIndex } from '../../core/shared/array-utils'
import * as EP from '../../core/shared/element-path'
import type { ElementPath } from '../../core/shared/project-file-types'
import { getSelectedNavigatorEntries } from '../../templates/editor-navigator'
import { useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import Utils from '../../utils/utils'
import { FlexColumn, Section, SectionBodyArea } from '../../uuiui'
import { setFocus } from '../common/actions'
import {
  clearHighlightedViews,
  clearSelection,
  showContextMenu,
} from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import type { NavigatorEntry } from '../editor/store/editor-state'
import {
  isRegularNavigatorEntry,
  navigatorEntryToKey,
  navigatorEntriesEqual,
} from '../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { ElementContextMenu } from '../element-context-menu'
import { getItemHeight } from './navigator-item/navigator-item'
import { NavigatorDragLayer } from './navigator-drag-layer'
import { NavigatorItemWrapper } from './navigator-item/navigator-item-wrapper'

interface ItemProps extends ListChildComponentProps {}

function navigatorEntriesContainTarget(entries: NavigatorEntry[], target: NavigatorEntry): boolean {
  return !entries.some((t) => t.elementPath === target.elementPath && t.type === target.type)
}

const Item = React.memo(({ index, style }: ItemProps) => {
  const visibleNavigatorTargets = useEditorState(
    Substores.derived,
    (store) => {
      return store.derived.visibleNavigatorTargets
    },
    'Item visibleNavigatorTargets',
  )
  const editorSliceRef = useRefEditorState((store) => {
    const currentlySelectedNavigatorEntries = getSelectedNavigatorEntries(
      store.editor.selectedViews,
      store.derived.navigatorTargets,
    )
    return {
      selectedViews: store.editor.selectedViews,
      navigatorTargets: store.derived.navigatorTargets,
      visibleNavigatorTargets: store.derived.visibleNavigatorTargets,
      currentlySelectedNavigatorEntries: currentlySelectedNavigatorEntries,
    }
  })

  const getCurrentlySelectedNavigatorEntries = React.useCallback((): Array<NavigatorEntry> => {
    return editorSliceRef.current.currentlySelectedNavigatorEntries
  }, [editorSliceRef])

  const visibleTargetIndexToRegularIndex = React.useCallback(
    (visibleTargetIndex: number) => {
      const visibleNavigatorEntry =
        editorSliceRef.current.visibleNavigatorTargets[visibleTargetIndex]
      if (visibleNavigatorEntry == null) {
        return null
      } else {
        const targetIndex = editorSliceRef.current.navigatorTargets.findIndex((target) =>
          navigatorEntriesEqual(target, visibleNavigatorEntry),
        )
        if (targetIndex >= 0) {
          return targetIndex
        } else {
          return null
        }
      }
    },
    [editorSliceRef],
  )

  // Used to determine the views that will be selected by starting with the last selected item
  // and selecting everything from there to `targetIndex`.
  const getSelectedViewsInRange = React.useCallback(
    (visibleTargetIndex: number): Array<ElementPath> => {
      const selectedItemIndexes = editorSliceRef.current.selectedViews
        .map((selection) =>
          editorSliceRef.current.navigatorTargets.findIndex(
            (entry) =>
              isRegularNavigatorEntry(entry) && EP.pathsEqual(entry.elementPath, selection),
          ),
        )
        .sort((a, b) => a - b)
      // As we're primarily operating on visible navigator targets, we need to convert the index.
      const targetIndex = visibleTargetIndexToRegularIndex(visibleTargetIndex)
      if (targetIndex == null) {
        return []
      }
      const lastSelectedItemIndex = last(selectedItemIndexes)
      if (lastSelectedItemIndex == null) {
        const lastSelectedItem = editorSliceRef.current.navigatorTargets[targetIndex]
        if (isRegularNavigatorEntry(lastSelectedItem)) {
          return [lastSelectedItem.elementPath]
        } else {
          return []
        }
      } else {
        let start = 0
        let end = 0
        if (targetIndex > lastSelectedItemIndex) {
          start = selectedItemIndexes[0]
          end = targetIndex
        } else if (targetIndex < lastSelectedItemIndex && targetIndex > selectedItemIndexes[0]) {
          start = selectedItemIndexes[0]
          end = targetIndex
        } else {
          start = targetIndex
          end = lastSelectedItemIndex
        }
        let selectedViewTargets: Array<ElementPath> = editorSliceRef.current.selectedViews
        Utils.fastForEach(editorSliceRef.current.navigatorTargets, (item, itemIndex) => {
          if (itemIndex >= start && itemIndex <= end && isRegularNavigatorEntry(item)) {
            selectedViewTargets = EP.addPathIfMissing(item.elementPath, selectedViewTargets)
          }
        })
        return selectedViewTargets
      }
    },
    [editorSliceRef, visibleTargetIndexToRegularIndex],
  )

  const targetEntry = visibleNavigatorTargets[index]
  const componentKey = navigatorEntryToKey(targetEntry)
  const deepKeptStyle = useKeepReferenceEqualityIfPossible(style)

  if (navigatorEntriesContainTarget(visibleNavigatorTargets, targetEntry)) {
    return null
  }

  return (
    <NavigatorItemWrapper
      key={componentKey}
      index={index}
      targetComponentKey={componentKey}
      navigatorEntry={targetEntry}
      getCurrentlySelectedEntries={getCurrentlySelectedNavigatorEntries}
      getSelectedViewsInRange={getSelectedViewsInRange}
      windowStyle={deepKeptStyle}
    />
  )
})

export const NavigatorContainerId = 'navigator'

export const NavigatorComponent = React.memo(() => {
  const dispatch = useDispatch()
  const { minimised, visibleNavigatorTargets, selectionIndex } = useEditorState(
    Substores.fullStore,
    (store) => {
      const selectedViews = store.editor.selectedViews
      const innerVisibleNavigatorTargets = store.derived.visibleNavigatorTargets
      const innerSelectionIndex =
        selectedViews == null
          ? -1
          : innerVisibleNavigatorTargets.findIndex((entry) => {
              return (
                isRegularNavigatorEntry(entry) && EP.pathsEqual(entry.elementPath, selectedViews[0])
              )
            })
      return {
        minimised: store.editor.navigator.minimised,
        visibleNavigatorTargets: innerVisibleNavigatorTargets,
        selectionIndex: innerSelectionIndex,
      }
    },
    'NavigatorComponent',
  )

  const itemListRef = React.createRef<VariableSizeList>()

  React.useEffect(() => {
    if (selectionIndex >= 0) {
      itemListRef.current?.scrollToItem(selectionIndex, 'smart')
    }
  }, [selectionIndex, itemListRef])

  React.useEffect(() => {
    /**
     * VariableSizeList caches the item sizes returned by itemSize={getItemSize}
     * When a reorder happens, the items are offset, and the cached sizes are not applied to the right items anymore
     * resetAfterIndex(0, false) clears the cached size of all items, and false means it does not force a re-render
     *
     * as a first approximation, this useEffect runs on any change to visibleNavigatorTargets
     */
    itemListRef.current?.resetAfterIndex(0, false)
  }, [visibleNavigatorTargets, itemListRef])

  const onFocus = React.useCallback(
    (e: React.FocusEvent<HTMLElement>) => {
      dispatch([setFocus('navigator')])
    },
    [dispatch],
  )

  const onMouseLeave = React.useCallback(
    (e: React.MouseEvent<HTMLElement>) => {
      dispatch([clearHighlightedViews()], 'everyone')
    },
    [dispatch],
  )

  const onContextMenu = React.useCallback(
    (event: React.MouseEvent<HTMLElement>) => {
      dispatch([showContextMenu('context-menu-navigator', event.nativeEvent)], 'everyone')
    },
    [dispatch],
  )

  const getItemSize = React.useCallback(
    (entryIndex: number) => {
      const navigatorTarget = safeIndex(visibleNavigatorTargets, entryIndex)
      if (navigatorTarget == null) {
        throw new Error(`Could not find navigator entry at index ${entryIndex}`)
      } else {
        return getItemHeight(navigatorTarget)
      }
    },
    [visibleNavigatorTargets],
  )

  const ItemList = (size: Size) => {
    if (size.height == null) {
      return null
    } else {
      return (
        <VariableSizeList
          ref={itemListRef}
          width={'100%'}
          height={size.height}
          itemSize={getItemSize}
          itemCount={visibleNavigatorTargets.length}
          layout={'vertical'}
          style={{ overflowX: 'hidden' }}
        >
          {Item}
        </VariableSizeList>
      )
    }
  }

  const containerClick = React.useCallback(
    (mouseEvent: React.MouseEvent<HTMLElement>) => {
      // Ensure this is a left click.
      if (mouseEvent.button === 0) {
        dispatch([clearSelection()])
      }
    },
    [dispatch],
  )

  return (
    <Section
      data-name='Navigator'
      onFocus={onFocus}
      onMouseLeave={onMouseLeave}
      onContextMenu={onContextMenu}
      id={NavigatorContainerId}
      data-testid={NavigatorContainerId}
      tabIndex={-1}
      css={{
        flexGrow: 1,
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'stretch',
        justifyContent: 'stretch',
        overscrollBehavior: 'contain',
        '--paneHoverOpacity': 0,
        '&:hover': {
          '--paneHoverOpacity': 1,
        },
        padding: 5,
        height: 0,
      }}
      onClick={containerClick}
    >
      <SectionBodyArea
        minimised={minimised}
        flexGrow={1}
        style={{
          flexGrow: 1,
          overscrollBehavior: 'contain',
          display: 'flex',
          alignItems: 'stretch',
          justifyContent: 'stretch',
        }}
      >
        <ElementContextMenu contextMenuInstance={'context-menu-navigator'} />
        <FlexColumn
          style={{
            flexGrow: 1,
            flexShrink: 1,
            flexBasis: '100%',
            overflowX: 'hidden',
          }}
        >
          <NavigatorDragLayer />
          <AutoSizer
            disableWidth={true}
            style={{
              overscrollBehavior: 'contain',
              overflowX: 'hidden',
              height: '100%',
            }}
          >
            {ItemList}
          </AutoSizer>
        </FlexColumn>
      </SectionBodyArea>
    </Section>
  )
})
NavigatorComponent.displayName = 'NavigatorComponent'
