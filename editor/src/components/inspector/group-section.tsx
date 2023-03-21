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
import { mapDropNulls } from '../../core/shared/array-utils'
import {
  canvasPoint,
  CanvasPoint,
  isInfinityRectangle,
  LocalRectangle,
} from '../../core/shared/math-utils'
import { setCssLengthProperty } from '../canvas/commands/set-css-length-command'
import { styleP } from './inspector-common'
import { cssNumber } from './common/css-utils'
import { absolute } from '../../utils/utils'

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

// TODO: abstract out common code here

function wrapInFragment(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): CanvasCommand[] {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const { children, uid } = instance.element.value

  return [
    deleteElement('always', elementPath),
    addElement(
      'always',
      parentPath,
      jsxFragment(uid, children, true),
      absolute(MetadataUtils.getIndexInParent(metadata, elementPath)),
    ),
  ]
}

function wrapInSizelessDiv(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): CanvasCommand[] {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const { children, uid } = instance.element.value
  const { element } = detectBestWrapperElement(metadata, elementPath, () => uid)

  return [
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
}

function wrapInDiv(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
  positioningProps: PositioningProps,
): CanvasCommand[] {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const { children, uid } = instance.element.value
  const { element, style } = detectBestWrapperElement(metadata, elementPath, () => uid)

  const isInFlowLayout = MetadataUtils.isPositionedByFlow(
    MetadataUtils.findElementByElementPath(metadata, elementPath),
  )

  const { top, left, width, height } = positioningProps

  const props: Record<string, any> = { 'data-uid': jsxAttributeValue(uid, emptyComments) }
  if (!isInFlowLayout) {
    props.style = jsxAttributeValue(
      {
        ...style,
        position: 'absolute',
        top: top,
        left: left,
        width: width,
        height: height,
      },
      emptyComments,
    )
  }

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
    ...getChildFrameAdjustCommands(metadata, elementPath, canvasPoint({ x: left, y: top })),
  ]
}

function wrapInElement(
  wrapper: Wrapper,
  originalWrapperType: WrapperType,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const wrapperCommands = () => {
    switch (wrapper.type) {
      case 'div':
        return wrapInDiv(metadata, elementPath, wrapper.props)
      case 'fragment':
        return wrapInFragment(metadata, elementPath)
      case 'sizeless-div':
        return wrapInSizelessDiv(metadata, elementPath)
      default:
        assertNever(wrapper)
    }
  }

  if (originalWrapperType === 'fragment' || originalWrapperType === 'sizeless-div') {
    return wrapperCommands()
  }

  if (wrapper.type === 'div') {
    // NOOP
    return []
  }

  // here we're converting from an absolutely positioned parent with offset to one
  // that doesn't provide offsets for the children, so child frames have to be adjusted

  const parentFrame = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (parentFrame == null || isInfinityRectangle(parentFrame)) {
    return []
  }

  const aaaa = getChildFrameAdjustCommands(
    metadata,
    elementPath,
    canvasPoint({ x: -parentFrame.x, y: -parentFrame.y }),
  )

  return [...wrapperCommands(), ...aaaa]
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

function getChildFrameAdjustCommands(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
  parentOffset: CanvasPoint,
): CanvasCommand[] {
  const childGlobalFrames = mapDropNulls((childPath): [LocalRectangle, ElementPath] | null => {
    const childInstance = MetadataUtils.findElementByElementPath(metadata, childPath)
    if (childInstance == null || !MetadataUtils.isPositionAbsolute(childInstance)) {
      return null
    }

    const frame = childInstance.localFrame
    if (frame == null || isInfinityRectangle(frame)) {
      return null
    }

    return [frame, childPath]
  }, MetadataUtils.getChildrenPathsUnordered(metadata, elementPath))

  const adjustTopCommands: CanvasCommand[] = childGlobalFrames.map(([frame, targetPath]) =>
    setCssLengthProperty(
      'always',
      targetPath,
      styleP('top'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(frame.y - parentOffset.y, null) },
      null,
    ),
  )

  const adjustLeftCommands: CanvasCommand[] = childGlobalFrames.map(([frame, targetPath]) =>
    setCssLengthProperty(
      'always',
      targetPath,
      styleP('left'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(frame.x - parentOffset.x, null) },
      null,
    ),
  )

  return [...adjustTopCommands, ...adjustLeftCommands]
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

const FragmentOption: SelectOption = { value: 'fragment', label: 'Fragment' }
const GroupOption = { value: 'sizeless-div', label: 'Group' }
const DivOption = { value: 'div', label: 'Div' }

const Options: Array<SelectOption> = [FragmentOption, GroupOption, DivOption]

export const GroupSection = React.memo(() => {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

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
      const originalMode = currentValue.value as WrapperType
      const mode = value as WrapperType
      const commands = selectedViewsRef.current.flatMap((elementPath) => {
        switch (mode) {
          case 'fragment':
            return wrapInElement(
              { type: 'fragment' },
              originalMode,
              metadataRef.current,
              elementPath,
            )
          case 'sizeless-div':
            return wrapInElement(
              { type: 'sizeless-div' },
              originalMode,
              metadataRef.current,
              elementPath,
            )
          case 'div':
            return (
              optionalMap(
                (props) =>
                  wrapInElement(
                    { type: 'div', props: props },
                    originalMode,
                    metadataRef.current,
                    elementPath,
                  ),
                getWrapperPositioningProps(metadataRef.current, elementPath),
              ) ?? []
            )
          default:
            assertNever(mode)
        }
      })
      dispatch([applyCommandsAction(commands)])
    },
    [currentValue.value, dispatch, metadataRef, selectedViewsRef],
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
