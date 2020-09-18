import * as R from 'ramda'
import * as React from 'react'
import { TemplatePath } from '../../../core/shared/project-file-types'
import * as TP from '../../../core/shared/template-path'
import { createDragSelections } from '../../../templates/editor-navigator'
import { FlexColumn } from '../../../uuiui'
import { Utils } from '../../../uuiui-deps'
import { setFocus } from '../../common/actions'
import { clearHighlightedViews } from '../../editor/actions/actions'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { DragSelection } from '../../navigator/navigator-item/navigator-item-dnd-container'
import { NavigatorItemWrapper } from '../../navigator/navigator-item/navigator-item-wrapper'

interface MiniNavigatorProps {
  style: React.CSSProperties
}

export const MiniNavigator: React.FunctionComponent<MiniNavigatorProps> = ({ style }) => {
  const { dispatch, focusedPanel, navigatorTargets } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      focusedPanel: store.editor.focusedPanel,
      navigatorTargets: store.derived.navigatorTargets,
    }
  })

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

  const onFocus = React.useCallback(() => {
    if (focusedPanel !== 'mininavigator') {
      dispatch([setFocus('mininavigator')])
    }
  }, [dispatch, focusedPanel])

  const onMouseLeave = React.useCallback(() => {
    dispatch([clearHighlightedViews()], 'everyone')
  }, [dispatch])

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

  return (
    <FlexColumn style={style}>
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
    </FlexColumn>
  )
}
