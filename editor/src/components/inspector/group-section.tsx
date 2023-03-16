/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { FlexColumn, FlexRow, InspectorSectionHeader, PopupList, useColorTheme } from '../../uuiui'
import { createSelector } from 'reselect'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import { MetadataSubstate } from '../editor/store/store-hook-substore-types'
import {
  ContentAffectingType,
  getElementContentAffectingType,
} from '../canvas/canvas-strategies/strategies/group-like-helpers'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { assertNever } from '../../core/shared/utils'
import { getControlStyles, SelectOption } from '../../uuiui-deps'
import {
  ElementInstanceMetadataMap,
  emptyComments,
  isJSXElementLike,
  jsxAttributesFromMap,
  jsxAttributeValue,
  JSXElementChild,
  jsxElementName,
  jsxFragment,
} from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { applyCommandsAction } from '../editor/actions/action-creators'
import { isLeft } from '../../core/shared/either'
import { parentPath } from '../../core/shared/element-path'
import { deleteElement } from '../canvas/commands/delete-element-command'
import { addElement } from '../canvas/commands/add-element-command'
import { useDispatch } from '../editor/store/dispatch-context'
import { CanvasCommand } from '../canvas/commands/commands'

function wrapInElement(
  wrapIn: 'fragment' | 'sizeless-div',
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const { children, uid } = instance.element.value

  const newElement = (): JSXElementChild => {
    switch (wrapIn) {
      case 'sizeless-div':
        return {
          type: 'JSX_ELEMENT',
          name: jsxElementName('div', []),
          props: jsxAttributesFromMap({
            'data-uid': jsxAttributeValue(uid, emptyComments),
          }),
          children: children,
          uid: uid,
        }
      case 'fragment':
        return jsxFragment(uid, children, true)
      default:
        assertNever(wrapIn)
    }
  }

  const elementToBeAdded = newElement()

  const parent = parentPath(elementPath)

  return [deleteElement('always', elementPath), addElement('always', parent, elementToBeAdded)]
}

const simpleControlStyles = getControlStyles('simple')

const selectedElementGrouplikeTypeSelector = createSelector(
  metadataSelector,
  (store: MetadataSubstate) => store.editor.allElementProps,
  selectedViewsSelector,
  (metadata, allElementProps, selectedViews) => {
    if (selectedViews.length === 0) {
      return null
    }
    const allTypes = selectedViews.map((elementPath) =>
      getElementContentAffectingType(metadata, allElementProps, elementPath),
    )
    const allElementsGrouplike = allTypes.every((t) => t != null)
    if (!allElementsGrouplike) {
      return null
    }
    return allTypes[0]
  },
)

const FragmentOption: SelectOption = { value: 'fragment', label: 'fragment' }
const DivOption = { value: 'sizeless-div', label: 'Div' }

const Options: Array<SelectOption> = [FragmentOption, DivOption]

export const GroupSection = React.memo(() => {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const selectedElementGrouplikeType = useEditorState(
    Substores.metadata,
    selectedElementGrouplikeTypeSelector,
    'GroupSection allSelectedElementGrouplike',
  )

  const currentValue = React.useMemo(() => {
    if (selectedElementGrouplikeType === 'fragment') {
      return FragmentOption
    }
    if (selectedElementGrouplikeType === 'sizeless-div') {
      return DivOption
    }
    return undefined
  }, [selectedElementGrouplikeType])

  const onChange = React.useCallback(
    ({ value }: SelectOption) => {
      const mode = value as ContentAffectingType
      if (mode === 'fragment' || mode === 'sizeless-div') {
        const commands = selectedViewsRef.current.flatMap((elementPath) =>
          wrapInElement(mode, metadataRef.current, elementPath),
        )
        dispatch([applyCommandsAction(commands)])
      }
    },
    [dispatch, metadataRef, selectedViewsRef],
  )

  if (selectedElementGrouplikeType == null || selectedElementGrouplikeType === 'conditional') {
    return null
  }

  return (
    <FlexColumn>
      <InspectorSectionHeader
        css={{
          transition: 'color .1s ease-in-out',
          color: colorTheme.fg1.value,
          '--buttonContentOpacity': 0.3,
          '&:hover': {
            color: colorTheme.fg1.value,
            '--buttonContentOpacity': 1,
          },
        }}
      >
        <FlexRow
          style={{
            flexGrow: 1,
            alignSelf: 'stretch',
            gap: 8,
          }}
        >
          <span style={{ textTransform: 'capitalize', fontSize: '11px' }}>Group</span>
        </FlexRow>
      </InspectorSectionHeader>
      <FlexRow style={{ padding: 4 }}>
        <PopupList
          value={currentValue}
          options={Options}
          onSubmitValue={onChange}
          controlStyles={simpleControlStyles}
        />
      </FlexRow>
    </FlexColumn>
  )
})
