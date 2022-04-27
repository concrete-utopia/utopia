/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React, { useRef } from 'react'
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
  UtopiaTheme,
  Section,
  SectionTitleRow,
  FlexRow,
  Title,
  SectionBodyArea,
  useColorTheme,
  FlexColumn,
  InspectorSectionHeader,
  SquareButton,
  FunctionIcons,
} from '../../uuiui'
import { last } from '../../core/shared/array-utils'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import * as PP from '../../core/shared/property-path'
import { jsxSimpleAttributeToValue } from '../../core/shared/jsx-attributes'
import { omit } from '../../core/shared/object-utils'

export const NavigatorContainerId = 'navigator'

export const NavigatorComponent = React.memo(() => {
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
  const colorTheme = useColorTheme()
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
    (index: number): Array<ElementPath> => {
      const selectedItemIndexes = editorSliceRef.current.selectedViews
        .map((selection) =>
          editorSliceRef.current.navigatorTargets.findIndex((tp) => EP.pathsEqual(tp, selection)),
        )
        .sort((a, b) => a - b)
      const lastSelectedItemIndex = last(selectedItemIndexes)
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

  const toggleTwirler = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('navigator')])
  }, [dispatch])

  const Item = React.memo(({ index, style }: ListChildComponentProps) => {
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
          style={{ overflowX: 'hidden' }}
        >
          {Item}
        </FixedSizeList>
      )
    }
  }

  return (
    <React.Fragment>
      <Section
        data-name='Navigator'
        onFocus={onFocus}
        onMouseLeave={onMouseLeave}
        onContextMenu={onContextMenu}
        id={NavigatorContainerId}
        tabIndex={-1}
        style={{
          zIndex: 1,
          flexGrow: 1,
          height: '50%',
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
      </Section>
      <ConversionSection />
    </React.Fragment>
  )
})
NavigatorComponent.displayName = 'NavigatorComponent'

const ConversionSection = React.memo(() => {
  const conversionPropertyChanges = useEditorState(
    (store) => store.editor.conversionPropertyChanges,
    'ConversionSection conversionPropertyChanges',
  )
  const colorTheme = useColorTheme()
  const dispatch = useEditorState((store) => store.dispatch, 'ConversionSection dispatch')
  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  if (Object.keys(conversionPropertyChanges).length === 0) {
    return null
  }
  return (
    <div
      style={{
        height: '50%',
        display: 'flex',
        flexDirection: 'column',
        borderTop: '1px solid hsl(0,0%,95%)',
      }}
    >
      <Title>New style properties from layout switch</Title>
      {Object.keys(conversionPropertyChanges).map((pathAsString) => {
        const elementPath = EP.fromString(pathAsString)
        const elementName = MetadataUtils.getElementLabel(elementPath, metadataRef.current)
        const valuesAtPath = conversionPropertyChanges[pathAsString]
        return (
          <div
            key={pathAsString}
            css={{
              display: 'flex',
              flexDirection: 'column',
              ':hover': { backgroundColor: colorTheme.bg3.value },
            }}
            onMouseOver={() => {
              dispatch([EditorActions.setHighlightedView(elementPath)], 'everyone')
            }}
          >
            <div
              style={{
                display: 'flex',
                flexDirection: 'row',
                justifyContent: 'space-between',
                alignItems: 'center',
              }}
            >
              <div>{elementName}</div>
              <SquareButton
                highlight
                /* eslint-disable-next-line react/jsx-no-bind */
                onClick={() => {
                  const changesUpdated = omit([pathAsString], conversionPropertyChanges)
                  dispatch(
                    [
                      ...valuesAtPath.map((valueAtPath) =>
                        EditorActions.unsetProperty(elementPath, valueAtPath.path),
                      ),
                      EditorActions.updateConversionPropertyChanges(changesUpdated),
                    ],
                    'everyone',
                  )
                }}
              >
                <FunctionIcons.Delete />
              </SquareButton>
            </div>
            {valuesAtPath.map((valueAtPath) => {
              return (
                <div
                  key={PP.toString(valueAtPath.path)}
                  style={{ display: 'flex', flexDirection: 'row' }}
                >
                  <span style={{ paddingLeft: 20, paddingRight: 10 }}>
                    {PP.lastPartToString(valueAtPath.path)}
                  </span>
                  {JSON.stringify(jsxSimpleAttributeToValue(valueAtPath.value).value)}
                </div>
              )
            })}
          </div>
        )
      })}
    </div>
  )
})
