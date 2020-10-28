/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as R from 'ramda'
import * as React from 'react'
import * as TP from '../../core/shared/template-path'
import Utils from '../../utils/utils'
import { setFocus } from '../common/actions'
import { Title } from 'uuiui'
import { FlexRow } from 'uuiui'
import { Section, SectionBodyArea, SectionTitleRow } from 'uuiui'
import { TemplatePath } from '../../core/shared/project-file-types'
import * as EditorActions from '../editor/actions/actions'
import { clearHighlightedViews, showContextMenu } from '../editor/actions/actions'
import { DragSelection } from './navigator-item/navigator-item-dnd-container'
import { NavigatorItemWrapper } from './navigator-item/navigator-item-wrapper'
import { useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { ElementContextMenu } from '../element-context-menu'
import { createDragSelections } from '../../templates/editor-navigator'
import { betterReactMemo } from 'uuiui-deps'

export interface DropTargetHint {
  target: TemplatePath | null
  type: DropTargetType
}

export type DropTargetType = 'before' | 'after' | 'reparent' | null

const NavigatorContainerId = 'navigator'

export const getChildrenOfCollapsedViews = (
  templatePaths: TemplatePath[],
  collapsedViews: Array<TemplatePath>,
): Array<TemplatePath> => {
  return Utils.flatMapArray((view) => {
    return Utils.stripNulls(
      templatePaths.map((childPath) => {
        return TP.isAncestorOf(childPath, view) && !TP.pathsEqual(view, childPath)
          ? childPath
          : null
      }),
    )
  }, collapsedViews)
}

export const NavigatorComponent = betterReactMemo('NavigatorComponent', () => {
  const editorSliceRef = useRefEditorState((store) => {
    const dragSelections = createDragSelections(
      store.editor.jsxMetadataKILLME,
      store.derived.navigatorTargets,
      store.editor.selectedViews,
    )
    return {
      selectedViews: store.editor.selectedViews,
      navigatorTargets: store.derived.navigatorTargets,
      dragSelections: dragSelections,
    }
  })

  const { dispatch, focusedPanel, minimised, navigatorTargets } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      focusedPanel: store.editor.focusedPanel,
      minimised: store.editor.navigator.minimised,
      navigatorTargets: store.derived.navigatorTargets,
    }
  }, 'NavigatorComponent')

  const onFocus = React.useCallback(
    (e: React.FocusEvent<HTMLElement>) => {
      if (focusedPanel !== 'navigator') {
        dispatch([setFocus('navigator')])
      }
    },
    [dispatch, focusedPanel],
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

  return (
    <Section
      data-name='Navigator'
      onFocus={onFocus}
      onMouseLeave={onMouseLeave}
      onContextMenu={onContextMenu}
      id={NavigatorContainerId}
      tabIndex={-1}
    >
      <SectionTitleRow minimised={minimised} toggleMinimised={toggleTwirler}>
        <FlexRow flexGrow={1}>
          <Title>Elements</Title>
        </FlexRow>
      </SectionTitleRow>
      <SectionBodyArea minimised={minimised}>
        <ElementContextMenu contextMenuInstance={'context-menu-navigator'} />
        {navigatorTargets.map((targetPath, index) => {
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
            />
          )
        })}
      </SectionBodyArea>
    </Section>
  )
})
NavigatorComponent.displayName = 'NavigatorComponent'
