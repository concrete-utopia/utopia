/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import * as EP from '../../core/shared/element-path'
import Utils from '../../utils/utils'
import { setFocus } from '../common/actions'
import { ElementPath } from '../../core/shared/project-file-types'
import * as EditorActions from '../editor/actions/action-creators'
import { clearHighlightedViews, showContextMenu } from '../editor/actions/action-creators'
import { DragSelection } from './navigator-item/navigator-item-dnd-container'
import { NavigatorItemWrapper } from './navigator-item/navigator-item-wrapper'
import { useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { ElementContextMenu } from '../element-context-menu'
import { createDragSelections } from '../../templates/editor-navigator'
import { FixedSizeList, ListChildComponentProps } from 'react-window'
import AutoSizer, { Size } from 'react-virtualized-auto-sizer'
import {
  Section,
  SectionTitleRow,
  FlexRow,
  Title,
  SectionBodyArea,
  FlexColumn,
  InspectorSectionHeader,
} from '../../uuiui'
import { last } from '../../core/shared/array-utils'
import { UtopiaTheme } from '../../uuiui/styles/theme/utopia-theme'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { when } from '../../utils/react-conditionals'
import { codeOutlineModel, CodeOutlineView } from './code-outline'

interface ItemProps extends ListChildComponentProps {}

const Item = React.memo(({ index, style }: ItemProps) => {
  const visibleNavigatorTargets = useEditorState((store) => {
    return store.derived.visibleNavigatorTargets
  }, 'Item visibleNavigatorTargets')
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
  return (
    <NavigatorItemWrapper
      key={componentKey}
      index={index}
      targetComponentKey={componentKey}
      elementPath={targetPath}
      getMaximumDistance={getDistanceFromAncestorWhereImTheLastLeaf}
      getDragSelections={getDragSelections}
      getSelectedViewsInRange={getSelectedViewsInRange}
      windowStyle={style}
    />
  )
})

export const NavigatorContainerId = 'navigator'

export const NavigatorComponent = React.memo(() => {
  const { dispatch, minimised, visibleNavigatorTargets, selectionIndex } = useEditorState(
    (store) => {
      const selectedViews = store.editor.selectedViews
      const innerVisibleNavigatorTargets = store.derived.visibleNavigatorTargets
      const innerSelectionIndex =
        selectedViews == null
          ? -1
          : innerVisibleNavigatorTargets.findIndex((path) => EP.pathsEqual(path, selectedViews[0]))
      return {
        dispatch: store.dispatch,
        minimised: store.editor.navigator.minimised,
        visibleNavigatorTargets: innerVisibleNavigatorTargets,
        selectionIndex: innerSelectionIndex,
      }
    },
    'NavigatorComponent',
  )

  const projectContents = useEditorState(
    (store) => store.editor.projectContents,
    'NavigatorComponent projectContents',
  )
  const outlineModel = codeOutlineModel(0, projectContents)

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

  const toggleTwirler = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('navigator')])
  }, [dispatch])

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

  const CodeOutlineSized = (size: Size) => {
    if (size.height == null) {
      return null
    } else {
      return outlineModel.map((entry) => <CodeOutlineView key={entry.key} entry={entry} />)
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
      <SectionTitleRow minimised={minimised} toggleMinimised={toggleTwirler}>
        <FlexRow flexGrow={1}>
          <Title>Structure</Title>
        </FlexRow>
      </SectionTitleRow>
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
      {when(
        isFeatureEnabled('Code outline'),
        <React.Fragment>
          <SectionTitleRow minimised={minimised} toggleMinimised={toggleTwirler}>
            Code Outline
          </SectionTitleRow>
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
                {CodeOutlineSized}
              </AutoSizer>
            </FlexColumn>
          </SectionBodyArea>
        </React.Fragment>,
      )}
    </Section>
  )
})
NavigatorComponent.displayName = 'NavigatorComponent'
