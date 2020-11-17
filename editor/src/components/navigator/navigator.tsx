/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as R from 'ramda'
import * as React from 'react'
import * as TP from '../../core/shared/template-path'
import Utils from '../../utils/utils'
import { setFocus } from '../common/actions'
import { Title, UtopiaTheme } from 'uuiui'
import { FlexRow } from 'uuiui'
import { Section, SectionBodyArea, SectionTitleRow } from 'uuiui'
import { foldParsedTextFile, TemplatePath } from '../../core/shared/project-file-types'
import * as EditorActions from '../editor/actions/actions'
import { clearHighlightedViews, showContextMenu } from '../editor/actions/actions'
import { DragSelection } from './navigator-item/navigator-item-dnd-container'
import { NavigatorItemWrapper } from './navigator-item/navigator-item-wrapper'
import { useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { ElementContextMenu } from '../element-context-menu'
import { createDragSelections } from '../../templates/editor-navigator'
import { betterReactMemo } from 'uuiui-deps'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import {
  getOpenImportsFromState,
  getOpenUIJSFile,
  getOpenUtopiaJSXComponentsByName,
} from '../editor/store/editor-state'
import { getJSXElementNameAsString, isJSXElement } from '../../core/shared/element-template'
import { fastForEach, NO_OP } from '../../core/shared/utils'
import { isRight } from '../../core/shared/either'
import { SelectedComponentNavigator } from './navigator-item/component-navigator'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { FixedSizeList, ListChildComponentProps } from 'react-window'
import { Size } from 'react-virtualized-auto-sizer'
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

const NavigatorOverflowScrollId = 'navigator-overflow-scroll'

export const NavigatorComponentWrapper = betterReactMemo('NavigatorComponent', () => {
  // get selected views
  // if one view selected, check if it is component
  // if it is a component, check if it is a first party component
  // if it is a first party component, render split pane
  const editorSliceRef = useRefEditorState((store) => {
    return {
      componentsByName: getOpenUtopiaJSXComponentsByName(store.editor),
      imports:
        store.derived.canvas.transientState.fileState == null
          ? getOpenImportsFromState(store.editor)
          : store.derived.canvas.transientState.fileState.imports,
      metadata: store.editor.jsxMetadataKILLME,
    }
  })

  const { dispatch, selectedViews } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      selectedViews: store.editor.selectedViews,
    }
  }, 'NavigatorComponentWrapper')

  const onMouseDown = React.useCallback(
    (mouseEvent: React.MouseEvent<HTMLDivElement>) => {
      if (mouseEvent.target instanceof HTMLDivElement) {
        if (mouseEvent.target.id === NavigatorOverflowScrollId) {
          dispatch([EditorActions.clearSelection()])
        }
      }
    },
    [dispatch],
  )

  if (isFeatureEnabled('Component Navigator') && selectedViews.length === 1) {
    const selectedView = selectedViews[0]
    if (TP.isInstancePath(selectedView)) {
      const selectedViewMetadata = MetadataUtils.getElementByInstancePathMaybe(
        editorSliceRef.current.metadata.elements,
        selectedView,
      )

      if (selectedViewMetadata != null) {
        const element = selectedViewMetadata.element
        if (isRight(element) && isJSXElement(element.value)) {
          const elementName = getJSXElementNameAsString(element.value.name)
          const matchingComponent = editorSliceRef.current.componentsByName[elementName]
          if (matchingComponent != null) {
            return (
              <React.Fragment>
                <div
                  id={NavigatorOverflowScrollId}
                  style={{
                    height: '50%',
                    overflowY: 'scroll',
                    flexGrow: 1,
                  }}
                  onMouseDown={onMouseDown}
                >
                  <NavigatorComponent />
                </div>
                <div
                  style={{
                    height: '50%',
                    overflowY: 'scroll',
                    flexGrow: 1,
                  }}
                >
                  <SelectedComponentNavigator
                    selectedComponent={matchingComponent}
                    imports={editorSliceRef.current.imports}
                    utopiaComponentNames={Object.keys(editorSliceRef.current.componentsByName)}
                  />
                </div>
              </React.Fragment>
            )
          }
        }
      }
    }
  }

  return (
    <div
      id={NavigatorOverflowScrollId}
      style={{
        height: '100%',
        overflowY: 'scroll',
        flexGrow: 1,
      }}
      onMouseDown={onMouseDown}
    >
      <NavigatorComponent />
    </div>
  )
})

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

  const { dispatch, focusedPanel, minimised, visibleNavigatorTargets } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      focusedPanel: store.editor.focusedPanel,
      minimised: store.editor.navigator.minimised,
      visibleNavigatorTargets: store.derived.visibleNavigatorTargets,
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
      style={{ height: '100%' }}
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
})
NavigatorComponent.displayName = 'NavigatorComponent'
