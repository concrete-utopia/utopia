/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import AutoSizer, { Size } from 'react-virtualized-auto-sizer'
import { ListChildComponentProps, VariableSizeList } from 'react-window'
import { last, safeIndex } from '../../core/shared/array-utils'
import * as EP from '../../core/shared/element-path'
import { ElementPath } from '../../core/shared/project-file-types'
import { getSelectedNavigatorEntries } from '../../templates/editor-navigator'
import { useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import Utils from '../../utils/utils'
import { FlexColumn, Section, SectionBodyArea } from '../../uuiui'
import { setFocus } from '../common/actions'
import { clearHighlightedViews, showContextMenu } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import {
  NavigatorEntry,
  isRegularNavigatorEntry,
  navigatorEntryToKey,
} from '../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { ElementContextMenu } from '../element-context-menu'
import { getItemHeight } from './navigator-item/navigator-item'
import { NavigatorItemWrapper } from './navigator-item/navigator-item-wrapper'

interface ItemProps extends ListChildComponentProps {}

function navigatorEntriesContainTarget(entries: NavigatorEntry[], target: NavigatorEntry): boolean {
  return !entries.some((t) => t.elementPath === target.elementPath && t.type === target.type)
}

const Item = React.memo(({ index, style }: ItemProps) => {
  const navigatorTargets = useEditorState(
    Substores.derived,
    (store) => {
      return store.derived.navigatorTargets
    },
    'Item navigatorTargets',
  )
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
    )
    return {
      selectedViews: store.editor.selectedViews,
      navigatorTargets: store.derived.navigatorTargets,
      currentlySelectedNavigatorEntries: currentlySelectedNavigatorEntries,
    }
  })

  const getCurrentlySelectedNavigatorEntries = React.useCallback((): Array<NavigatorEntry> => {
    return editorSliceRef.current.currentlySelectedNavigatorEntries
  }, [editorSliceRef])

  // Used to determine the views that will be selected by starting with the last selected item
  // and selecting everything from there to `targetIndex`.
  const getSelectedViewsInRange = React.useCallback(
    (targetIndex: number): Array<ElementPath> => {
      const selectedItemIndexes = editorSliceRef.current.selectedViews
        .map((selection) =>
          editorSliceRef.current.navigatorTargets.findIndex(
            (entry) =>
              isRegularNavigatorEntry(entry) && EP.pathsEqual(entry.elementPath, selection),
          ),
        )
        .sort((a, b) => a - b)
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
    [editorSliceRef],
  )

  const targetEntry = navigatorTargets[index]
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
  const { minimised, navigatorTargets, visibleNavigatorTargets, selectionIndex } = useEditorState(
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
        navigatorTargets: store.derived.navigatorTargets,
        visibleNavigatorTargets: innerVisibleNavigatorTargets,
        selectionIndex: innerSelectionIndex,
      }
    },
    'NavigatorComponent',
  )

  const itemListRef = React.createRef<VariableSizeList>()

  React.useEffect(() => {
    if (selectionIndex > 0) {
      itemListRef.current?.scrollToItem(selectionIndex)
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
  }, [navigatorTargets, itemListRef])

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
      const navigatorTarget = safeIndex(navigatorTargets, entryIndex)
      if (navigatorTarget == null) {
        throw new Error(`Could not find navigator entry at index ${entryIndex}`)
      } else if (navigatorEntriesContainTarget(visibleNavigatorTargets, navigatorTarget)) {
        return 0
      } else {
        return getItemHeight(navigatorTarget)
      }
    },
    [navigatorTargets, visibleNavigatorTargets],
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
          itemCount={navigatorTargets.length}
          layout={'vertical'}
          style={{ overflowX: 'hidden' }}
        >
          {Item}
        </VariableSizeList>
      )
    }
  }

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
        zIndex: 1,
        flexGrow: 1,
        height: '100%',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'stretch',
        justifyContent: 'stretch',
        overscrollBehavior: 'contain',
        '--paneHoverOpacity': 0,
        '&:hover': {
          '--paneHoverOpacity': 1,
        },
      }}
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
