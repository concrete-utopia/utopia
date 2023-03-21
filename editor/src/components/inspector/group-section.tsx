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
  detectBestWrapperElement,
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
import { optionalMap } from '../../core/shared/optional-utils'

interface PositioningProps {
  width: number
  height: number
  top: number
  left: number
}

type Wrapper =
  | { type: 'fragment' }
  | { type: 'sizeless-div' }
  | {
      type: 'div'
      props: PositioningProps
    }

type WrapperType = Wrapper['type']

function wrapInElement(
  wrapper: Wrapper,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const { children, uid } = instance.element.value

  const newElement = (): JSXElementChild => {
    switch (wrapper.type) {
      case 'fragment':
        return jsxFragment(uid, children, true)
      case 'sizeless-div': {
        const { element } = detectBestWrapperElement(metadata, elementPath, () => uid)
        return {
          ...element,
          children: children,
        }
      }
      case 'div': {
        const { element, style } = detectBestWrapperElement(metadata, elementPath, () => uid)
        return {
          ...element,
          props: jsxAttributesFromMap({
            'data-uid': jsxAttributeValue(uid, emptyComments),
            style: jsxAttributeValue(
              {
                ...style,
                width: wrapper.props.width,
                height: wrapper.props.height,
              },
              emptyComments,
            ),
          }),
          children: children,
        }
      }
      default:
        assertNever(wrapper)
    }
  }

  const elementToBeAdded = newElement()

  const parent = parentPath(elementPath)

  return [deleteElement('always', elementPath), addElement('always', parent, elementToBeAdded)]
}

function getWrapperPositioningProps(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): PositioningProps | null {
  const frame = MetadataUtils.getChildrenBoundingBox(metadata, elementPath)
  const offset = MetadataUtils.getChildrenOffset(metadata, elementPath)
  if (frame == null || offset == null) {
    return null
  }
  return { top: offset.top, left: offset.left, width: frame.width, height: frame.height }
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

const FragmentOption: SelectOption = { value: 'fragment', label: 'Fragment' }
const GroupOption = { value: 'sizeless-div', label: 'Group' }
const DivOption = { value: 'div', label: 'Div' }

const Options: Array<SelectOption> = [FragmentOption, GroupOption, DivOption]

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
      return GroupOption
    }
    return DivOption
  }, [selectedElementGrouplikeType])

  const onChange = React.useCallback(
    ({ value }: SelectOption) => {
      const mode = value as WrapperType
      const commands = selectedViewsRef.current.flatMap((elementPath) => {
        switch (mode) {
          case 'fragment':
            return wrapInElement({ type: 'fragment' }, metadataRef.current, elementPath)
          case 'sizeless-div':
            return wrapInElement({ type: 'sizeless-div' }, metadataRef.current, elementPath)
          case 'div':
            return (
              optionalMap(
                (props) =>
                  wrapInElement({ type: 'div', props: props }, metadataRef.current, elementPath),
                getWrapperPositioningProps(metadataRef.current, elementPath),
              ) ?? []
            )
          default:
            assertNever(mode)
        }
      })
      dispatch([applyCommandsAction(commands)])
    },
    [dispatch, metadataRef, selectedViewsRef],
  )

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
