/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as R from 'ramda'
import * as React from 'react'
import * as TP from '../../core/shared/template-path'
import Utils from '../../utils/utils'
import { setFocus } from '../common/actions'
import { TemplatePath } from '../../core/shared/project-file-types'
import * as EditorActions from '../editor/actions/action-creators'
import { clearHighlightedViews, showContextMenu } from '../editor/actions/action-creators'
import { DragSelection } from './navigator-item/navigator-item-dnd-container'
import { NavigatorItemWrapper } from './navigator-item/navigator-item-wrapper'
import { useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { ElementContextMenu } from '../element-context-menu'
import { createDragSelections } from '../../templates/editor-navigator'
import { FixedSizeList, ListChildComponentProps } from 'react-window'
import { Size } from 'react-virtualized-auto-sizer'
import { UtopiaTheme, Section, SectionTitleRow, FlexRow, Title, SectionBodyArea } from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'
// There's some weirdness between the types and the results in the two module systems.
// This is to effectively massage the result so that if it is loaded in the browser or in
// node it should end up with the right thing.
const AutoSizer = require('react-virtualized-auto-sizer')
const AutoSizerComponent: typeof AutoSizer =
  (AutoSizer as any)['default'] == null ? AutoSizer : (AutoSizer as any)['default']

export interface DropTargetHint {
  target: TemplatePath | null
  type: DropTargetType
}

export type DropTargetType = 'before' | 'after' | 'reparent' | null

const NavigatorContainerId = 'navigator'

export const NavigatorComponent = betterReactMemo(
  'NavigatorComponent',
  ({ style: navigatorStyle }) => {
    const editorSliceRef = useRefEditorState((store) => {
      const dragSelections = createDragSelections(
        store.editor.jsxMetadata,
        store.derived.navigatorTargets,
        store.editor.selectedViews,
      )
      return {
        selectedViews: store.editor.selectedViews,
        navigatorTargets: store.derived.navigatorTargets,
        dragSelections: dragSelections,
      }
    })

    const { dispatch, minimised, visibleNavigatorTargets } = useEditorState((store) => {
      return {
        dispatch: store.dispatch,
        minimised: store.editor.navigator.minimised,
        visibleNavigatorTargets: store.derived.visibleNavigatorTargets,
      }
    }, 'NavigatorComponent')

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
        dispatch([showContextMenu('context-menu-navigator', event.nativeEvent, null)], 'everyone')
      },
      [dispatch],
    )

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
      (index: number): Array<TemplatePath> => {
        const selectedItemIndexes = editorSliceRef.current.selectedViews
          .map((selection) =>
            editorSliceRef.current.navigatorTargets.findIndex((tp) => TP.pathsEqual(tp, selection)),
          )
          .sort((a, b) => a - b)
        const lastSelectedItemIndex = R.last(selectedItemIndexes)
        if (lastSelectedItemIndex == null) {
          return [editorSliceRef.current.navigatorTargets[index]]
        } else {
          let start = 0
          let end = 0
          if (index > lastSelectedItemIndex) {
            start = selectedItemIndexes[0]
            end = index
          } else if (index < lastSelectedItemIndex && index > selectedItemIndexes[0]) {
            start = selectedItemIndexes[0]
            end = index
          } else {
            start = index
            end = lastSelectedItemIndex
          }
          let selectedViewTargets: Array<TemplatePath> = editorSliceRef.current.selectedViews
          Utils.fastForEach(editorSliceRef.current.navigatorTargets, (item, itemIndex) => {
            if (itemIndex >= start && itemIndex <= end) {
              selectedViewTargets = TP.addPathIfMissing(item, selectedViewTargets)
            }
          })
          return selectedViewTargets
        }
      },
      [editorSliceRef],
    )

    const toggleTwirler = React.useCallback(() => {
      dispatch([EditorActions.togglePanel('navigator')])
    }, [dispatch])

    const Item = betterReactMemo('Item', ({ index, style }: ListChildComponentProps) => {
      const targetPath = visibleNavigatorTargets[index]
      const componentKey = TP.toComponentId(targetPath)
      return (
        <NavigatorItemWrapper
          key={componentKey}
          index={index}
          targetComponentKey={componentKey}
          templatePath={targetPath}
          getMaximumDistance={getDistanceFromAncestorWhereImTheLastLeaf}
          getDragSelections={getDragSelections}
          getSelectedViewsInRange={getSelectedViewsInRange}
          windowStyle={style}
        />
      )
    })

    const ItemList = (size: Size) => {
      if (size.height == null) {
        return null
      } else {
        return (
          <FixedSizeList
            width={'100%'}
            height={size.height}
            itemSize={UtopiaTheme.layout.rowHeight.smaller}
            itemCount={visibleNavigatorTargets.length}
            layout={'vertical'}
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
        tabIndex={-1}
        style={{
          ...navigatorStyle,
          backgroundColor: UtopiaTheme.color.leftPaneBackground.o(80).value,
          backdropFilter: 'blur(7px)',
          overflowX: 'scroll',
        }}
      >
        <SectionTitleRow minimised={minimised} toggleMinimised={toggleTwirler}>
          <FlexRow flexGrow={1}>
            <Title>Elements</Title>
          </FlexRow>
        </SectionTitleRow>
        <SectionBodyArea minimised={minimised} flexGrow={1}>
          <ElementContextMenu contextMenuInstance={'context-menu-navigator'} />
          <div style={{ flex: '1 1 auto' }}>
            <AutoSizerComponent disableWidth={true}>{ItemList}</AutoSizerComponent>
          </div>
        </SectionBodyArea>
      </Section>
    )
  },
)
NavigatorComponent.displayName = 'NavigatorComponent'
