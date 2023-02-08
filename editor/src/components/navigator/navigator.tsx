/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import * as EP from '../../core/shared/element-path'
import Utils from '../../utils/utils'
import { setFocus } from '../common/actions'
import { ElementPath } from '../../core/shared/project-file-types'
import { clearHighlightedViews, showContextMenu } from '../editor/actions/action-creators'
import { DragSelection } from './navigator-item/navigator-item-dnd-container'
import { NavigatorItemWrapper } from './navigator-item/navigator-item-wrapper'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { ElementContextMenu } from '../element-context-menu'
import { createDragSelections } from '../../templates/editor-navigator'
import { FixedSizeList, ListChildComponentProps } from 'react-window'
import AutoSizer, { Size } from 'react-virtualized-auto-sizer'
import { Section, SectionBodyArea, FlexColumn, FlexRow, useColorTheme } from '../../uuiui'
import { last } from '../../core/shared/array-utils'
import { UtopiaTheme } from '../../uuiui/styles/theme/utopia-theme'
import { useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import { useDispatch } from '../editor/store/dispatch-context'
import { useDragLayer } from 'react-dnd'
import { NavigatorRowLabel } from './navigator-item/navigator-item'
import { NO_OP } from '../../core/shared/utils'

interface ItemProps extends ListChildComponentProps {}

const Item = React.memo(({ index, style }: ItemProps) => {
  const visibleNavigatorTargets = useEditorState(
    Substores.derived,
    (store) => {
      return store.derived.visibleNavigatorTargets
    },
    'Item visibleNavigatorTargets',
  )
  const editorSliceRef = useRefEditorState((store) => {
    const dragSelections = createDragSelections(
      store.derived.navigatorTargets,
      store.editor.selectedViews,
    )
    return {
      selectedViews: store.editor.selectedViews,
      navigatorTargets: store.derived.navigatorTargets,
      dragSelections: dragSelections,
    }
  })
  const getDistanceFromAncestorWhereImTheLastLeaf = React.useCallback(
    (componentId: string, distance: number): number => {
      // TODO FIXME HOLY SHIT THIS IS STUCK IN OLDE WORLDE
      console.error('FIX getDistanceFromAncestorWhereImTheLastLeaf')
      return distance
    },
    [],
  )

  const getDragSelections = React.useCallback((): Array<DragSelection> => {
    return editorSliceRef.current.dragSelections
  }, [editorSliceRef])

  const getSelectedViewsInRange = React.useCallback(
    (targetIndex: number): Array<ElementPath> => {
      const selectedItemIndexes = editorSliceRef.current.selectedViews
        .map((selection) =>
          editorSliceRef.current.navigatorTargets.findIndex((tp) => EP.pathsEqual(tp, selection)),
        )
        .sort((a, b) => a - b)
      const lastSelectedItemIndex = last(selectedItemIndexes)
      if (lastSelectedItemIndex == null) {
        return [editorSliceRef.current.navigatorTargets[targetIndex]]
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
          if (itemIndex >= start && itemIndex <= end) {
            selectedViewTargets = EP.addPathIfMissing(item, selectedViewTargets)
          }
        })
        return selectedViewTargets
      }
    },
    [editorSliceRef],
  )

  const targetPath = visibleNavigatorTargets[index]
  const componentKey = EP.toComponentId(targetPath)
  const deepKeptStyle = useKeepReferenceEqualityIfPossible(style)
  return (
    <NavigatorItemWrapper
      key={componentKey}
      index={index}
      targetComponentKey={componentKey}
      elementPath={targetPath}
      getMaximumDistance={getDistanceFromAncestorWhereImTheLastLeaf}
      getDragSelections={getDragSelections}
      getSelectedViewsInRange={getSelectedViewsInRange}
      windowStyle={deepKeptStyle}
    />
  )
})

const CustomDragLayer = React.memo(() => {
  const { isDragging, currentOffset } = useDragLayer((monitor) => ({
    item: monitor.getItem(),
    itemType: monitor.getItemType(),
    currentOffset: monitor.getClientOffset(),
    isDragging: monitor.isDragging(),
  }))

  const colorTheme = useColorTheme()

  return (
    <div
      data-testid='draglayer'
      style={{
        pointerEvents: 'none',
        position: 'absolute',
        width: '100%',
        height: '100%',
        top: 0,
        left: 0,
      }}
    >
      {isDragging && (
        <FlexRow
          style={{
            width: 100,
            height: 20,
            borderRadius: 4,
            opacity: 0.5,
            backgroundColor: colorTheme.secondaryBlue.value,
            color: 'white',
            position: 'absolute',
            transform: `translate3d(${(currentOffset?.x ?? 0) - 550}px, ${
              // TODO: figure out why DOM is like it is
              (currentOffset?.y ?? 0) - 50
            }px, 0)`,
          }}
        >
          <NavigatorRowLabel
            elementPath={EP.elementPath([[]])}
            iconColor='on-highlight-main'
            warningText={null}
            renamingTarget={null}
            selected={true}
            label={'div'}
            isDynamic={false}
            dispatch={NO_OP}
          />
        </FlexRow>
      )}
    </div>
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
          : innerVisibleNavigatorTargets.findIndex((path) => EP.pathsEqual(path, selectedViews[0]))
      return {
        minimised: store.editor.navigator.minimised,
        visibleNavigatorTargets: innerVisibleNavigatorTargets,
        selectionIndex: innerSelectionIndex,
      }
    },
    'NavigatorComponent',
  )

  const itemListRef = React.createRef<FixedSizeList>()

  React.useEffect(() => {
    if (selectionIndex > 0) {
      itemListRef.current?.scrollToItem(selectionIndex)
    }
  }, [selectionIndex, itemListRef])

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

  const ItemList = (size: Size) => {
    if (size.height == null) {
      return null
    } else {
      return (
        <FixedSizeList
          ref={itemListRef}
          width={'100%'}
          height={size.height}
          itemSize={UtopiaTheme.layout.rowHeight.smaller}
          itemCount={visibleNavigatorTargets.length}
          layout={'vertical'}
          style={{ overflowX: 'hidden' }}
        >
          {Item}
        </FixedSizeList>
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
      style={{
        zIndex: 1,
        flexGrow: 1,
        height: '100%',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'stretch',
        justifyContent: 'stretch',
        overscrollBehavior: 'contain',
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
        <CustomDragLayer data-testid='draglayer' />
      </SectionBodyArea>
    </Section>
  )
})
NavigatorComponent.displayName = 'NavigatorComponent'
