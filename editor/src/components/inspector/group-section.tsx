/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { FlexColumn, FlexRow, InspectorSectionHeader, PopupList, useColorTheme } from '../../uuiui'
import { createSelector } from 'reselect'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import { MetadataSubstate } from '../editor/store/store-hook-substore-types'
import {
  detectBestWrapperElement,
  getElementContentAffectingType,
  treatElementAsContentAffecting,
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
  jsxFragment,
} from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { applyCommandsAction } from '../editor/actions/action-creators'
import { isLeft } from '../../core/shared/either'
import * as EP from '../../core/shared/element-path'
import { deleteElement } from '../canvas/commands/delete-element-command'
import { addElement } from '../canvas/commands/add-element-command'
import { useDispatch } from '../editor/store/dispatch-context'
import { CanvasCommand } from '../canvas/commands/commands'
import { optionalMap } from '../../core/shared/optional-utils'
import { canvasPoint } from '../../core/shared/math-utils'
import { absolute } from '../../utils/utils'
import { AllElementProps } from '../editor/store/editor-state'
import {
  getChildFrameAdjustCommands,
  getChildrenOffsets,
  getWrapperPositioningProps,
  isDescendantOfGroups,
  isStoryboardOrGroupChild,
  PositioningProps,
} from './group-section-utils'

type Wrapper =
  | { type: 'fragment' }
  | { type: 'sizeless-div' }
  | {
      type: 'div'
      props: PositioningProps
    }

export type WrapperType = Wrapper['type']

// TODO: abstract out common code here

function wrapInFragment(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): CanvasCommand[] {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const { children, uid } = instance.element.value
  const commands = [
    deleteElement('always', elementPath),
    addElement(
      'always',
      parentPath,
      jsxFragment(uid, children, true),
      absolute(MetadataUtils.getIndexInParent(metadata, elementPath)),
    ),
  ]

  const isCurrentElementContentAffecting = treatElementAsContentAffecting(
    metadata,
    allElementProps,
    elementPath,
  )

  const storyboardOrGroupChild = isStoryboardOrGroupChild(metadata, allElementProps, elementPath)

  // if current wrapper is grouplike, children won't need adjusting since positions are not affected
  if (!storyboardOrGroupChild && isCurrentElementContentAffecting) {
    return commands
  }

  const offset = instance.specialSizeMeasurements.offset

  // however if current wrapper is not grouplike, children will have to be shifted by minus
  // the offset of the current parent

  return [
    ...commands,
    ...getChildFrameAdjustCommands(
      metadata,
      allElementProps,
      elementPath,
      canvasPoint({ x: -offset.x, y: -offset.y }),
    ),
  ]
}

function wrapInSizelessDiv(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): CanvasCommand[] {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const { children, uid } = instance.element.value
  const { element } = detectBestWrapperElement(metadata, elementPath, () => uid)

  const commands = [
    deleteElement('always', elementPath),
    addElement(
      'always',
      parentPath,
      {
        ...element,
        children: children,
      },
      absolute(MetadataUtils.getIndexInParent(metadata, elementPath)),
    ),
  ]

  const isCurrentElementContentAffecting = treatElementAsContentAffecting(
    metadata,
    allElementProps,
    elementPath,
  )

  const storyboardOrGroupChild = isStoryboardOrGroupChild(metadata, allElementProps, elementPath)

  // if current wrapper is grouplike, children won't need adjusting since positions are not affected
  if (!storyboardOrGroupChild && isCurrentElementContentAffecting) {
    return commands
  }

  // however if current wrapper is not grouplike, children will have to be shifted by minus
  // the offset of the current parent

  const offset = instance.specialSizeMeasurements.offset
  return [
    ...commands,
    ...getChildFrameAdjustCommands(
      metadata,
      allElementProps,
      elementPath,
      canvasPoint({ x: -offset.x, y: -offset.y }),
    ),
  ]
}

function wrapInDiv(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
  positioningProps: PositioningProps,
): CanvasCommand[] {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  const offset = getChildrenOffsets(metadata, allElementProps, elementPath)

  if (
    instance == null ||
    isLeft(instance.element) ||
    !isJSXElementLike(instance.element.value) ||
    offset == null
  ) {
    return []
  }

  const { children, uid } = instance.element.value
  const { element, style } = detectBestWrapperElement(metadata, elementPath, () => uid)

  const { width, height } = positioningProps

  const props: Record<string, any> = { 'data-uid': jsxAttributeValue(uid, emptyComments) }
  const newStyle = {
    ...style,
    width: width,
    height: height,
  }

  const storyboardOrGroupChild = isDescendantOfGroups(metadata, allElementProps, elementPath)

  if (storyboardOrGroupChild || EP.isStoryboardChild(elementPath)) {
    newStyle.position = 'absolute'
    newStyle.top = offset.top
    newStyle.left = offset.left
  }

  props.style = jsxAttributeValue(newStyle, emptyComments)

  const newElement: JSXElementChild = {
    ...element,
    props: jsxAttributesFromMap(props),
    children: children,
  }

  return [
    deleteElement('always', elementPath),
    addElement(
      'always',
      parentPath,
      newElement,
      absolute(MetadataUtils.getIndexInParent(metadata, elementPath)),
    ),
    ...getChildFrameAdjustCommands(
      metadata,
      allElementProps,
      elementPath,
      canvasPoint({ x: offset.left, y: offset.top }),
    ),
  ]
}

const simpleControlStyles = getControlStyles('simple')

const selectedElementGrouplikeTypeSelector = createSelector(
  metadataSelector,
  (store: MetadataSubstate) => store.editor.allElementProps,
  selectedViewsSelector,
  (metadata, allElementProps, selectedViews) => {
    if (selectedViews.length !== 1) {
      return null
    }
    return getElementContentAffectingType(metadata, allElementProps, selectedViews[0])
  },
)

export function groupSectionOption(wrapperType: WrapperType): SelectOption {
  switch (wrapperType) {
    case 'div':
      return { value: 'div', label: 'Div' }
    case 'fragment':
      return { value: 'fragment', label: 'Fragment' }
    case 'sizeless-div':
      return { value: 'sizeless-div', label: 'Group' }
    default:
      assertNever(wrapperType)
  }
}

const FragmentOption = groupSectionOption('fragment')
const GroupOption = groupSectionOption('sizeless-div')
const DivOption = groupSectionOption('div')

const Options: Array<SelectOption> = [FragmentOption, GroupOption, DivOption]

export const GroupSection = React.memo(() => {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const selectedViews = useEditorState(
    Substores.selectedViews,
    selectedViewsSelector,
    'GroupSection selectedViews',
  )

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
            return wrapInFragment(metadataRef.current, allElementPropsRef.current, elementPath)
          case 'sizeless-div':
            return wrapInSizelessDiv(metadataRef.current, allElementPropsRef.current, elementPath)
          case 'div':
            return (
              optionalMap(
                (props) =>
                  wrapInDiv(metadataRef.current, allElementPropsRef.current, elementPath, props),
                getWrapperPositioningProps(metadataRef.current, elementPath),
              ) ?? []
            )
          default:
            assertNever(mode)
        }
      })
      dispatch([applyCommandsAction(commands)])
    },
    [allElementPropsRef, dispatch, metadataRef, selectedViewsRef],
  )

  if (selectedViews.length !== 1) {
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
