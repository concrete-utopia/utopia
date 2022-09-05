import React from 'react'
import { FlexDirection, FlexWrap } from 'utopia-api/core'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { emptyComments, jsxAttributeValue } from '../../../core/shared/element-template'
import { when } from '../../../utils/react-conditionals'
import { Icn, IcnProps, PopupList, useColorTheme } from '../../../uuiui'
import { SelectOption } from '../../../uuiui-deps'
import { InlineLink } from '../../../uuiui/inline-button'
import { setProp_UNSAFE } from '../../editor/actions/action-creators'
import { useEditorState } from '../../editor/store/store-hook'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import {
  alignItemsOptions,
  flexDirectionOptions,
  getDirectionAwareLabels,
} from '../../inspector/sections/layout-section/flex-container-subsection/flex-container-controls'

const getFlexDirectionIcon = (
  flexWrap: FlexWrap | null,
  flexDirection: FlexDirection | null,
): IcnProps | null => {
  if (flexWrap == null) {
    return null
  }
  return (
    flexDirectionOptions(flexWrap).find((option) => option.value === flexDirection)?.icon ?? null
  )
}

export const LayoutParentControl = React.memo((): JSX.Element | null => {
  const colorTheme = useColorTheme()

  const dispatch = useEditorState((store) => store.dispatch, 'LayoutParentControl dispatch')

  const { canvasOffset, scale } = useEditorState((store) => {
    return {
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
      scale: store.editor.canvas.scale,
    }
  }, 'LayoutParentControl canvas')
  const { parentTarget, parentLayout, parentFrame, flexWrap, flexDirection, alignItems } =
    useEditorState((store) => {
      if (
        store.editor.canvas.controls.flexReparentTargetLines != null ||
        store.editor.selectedViews.length !== 1 ||
        store.editor.selectedViews.some((path) => EP.isStoryboardChild(path))
      ) {
        return {
          parentTarget: null,
          parentLayout: null,
          parentFrame: null,
          flexWrap: null,
          flexDirection: null,
          alignItems: null,
        }
      }
      const parentElement = MetadataUtils.getParent(
        store.editor.jsxMetadata,
        store.editor.selectedViews[0],
      )
      const elementProps =
        parentElement == null
          ? {}
          : store.editor.allElementProps[EP.toString(parentElement.elementPath)]
      return {
        parentTarget: parentElement?.elementPath,
        parentLayout: parentElement?.specialSizeMeasurements.layoutSystemForChildren,
        parentFrame: parentElement?.globalFrame,
        flexWrap: elementProps?.style?.flexWrap ?? 'nowrap',
        flexDirection: elementProps?.style?.flexDirection ?? 'row',
        alignItems: elementProps?.style?.alignItems ?? 'flex-start',
      }
    }, 'LayoutParentControl')

  const {
    justifyFlexStart,
    justifyFlexEnd,
    alignDirection,
    alignItemsFlexStart,
    alignItemsFlexEnd,
    alignContentFlexStart,
    alignContentFlexEnd,
  } = getDirectionAwareLabels(flexWrap, flexDirection)
  const allAlignmentOptions = alignItemsOptions(
    alignDirection,
    alignItemsFlexStart,
    alignItemsFlexEnd,
  )
  const selectedAlignment = allAlignmentOptions.find((option) => option.value === alignItems)

  const flexDirectionIcon = getFlexDirectionIcon(flexWrap, flexDirection)
  const flexDirectionClicked = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      if (parentTarget != null) {
        const newValue = flexDirection === 'row' ? 'column' : 'row'
        dispatch(
          [
            setProp_UNSAFE(
              parentTarget,
              stylePropPathMappingFn('flexDirection', ['style']),
              jsxAttributeValue(newValue, emptyComments),
            ),
          ],
          'canvas',
        )
      }
    },
    [parentTarget, flexDirection, dispatch],
  )

  const onSubmitValueAlignment = React.useCallback(
    (option: SelectOption) => {
      if (parentTarget != null) {
        dispatch(
          [
            setProp_UNSAFE(
              parentTarget,
              stylePropPathMappingFn('alignItems', ['style']),
              jsxAttributeValue(option.value, emptyComments),
            ),
          ],
          'canvas',
        )
      }
    },
    [parentTarget, dispatch],
  )

  const onControlMouseDown = React.useCallback((event: React.MouseEvent<HTMLDivElement>) => {
    event.nativeEvent.stopPropagation()
    event.nativeEvent.stopImmediatePropagation()
    event.stopPropagation()
  }, [])

  if (parentFrame == null) {
    return null
  }

  return (
    <div
      style={{
        position: 'absolute',
        left: parentFrame.x + canvasOffset.x,
        top: parentFrame.y + canvasOffset.y - 16 / scale,
        transform: `${scale < 1 ? `scale(${1 / scale})` : ''}`,
      }}
    >
      <div
        style={{
          right: 42,
          zoom: scale >= 1 ? 1 / scale : 1,
          position: 'absolute',
          borderRadius: 2,
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'stretch',
          justifyContent: 'flex-start',
          boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor.o(20).value} 0px 0px 1px, ${
            colorTheme.canvasControlsSizeBoxShadowColor.o(21).value
          } 0px 1px 2px 1px`,
          backgroundColor: colorTheme.inspectorBackground.value,
          textTransform: 'capitalize',
        }}
        onMouseDown={onControlMouseDown}
      >
        <svg
          style={{ position: 'absolute', left: 'calc(100% + 8px)', top: 2 }}
          width='30px'
          height='11px'
          viewBox='0 0 30 11'
        >
          <g stroke='none' strokeWidth='1' fill='none' fillRule='evenodd' strokeLinecap='round'>
            <polyline stroke='#979797' points='0.5 0.5 15.5 0.5 30 10'></polyline>
          </g>
        </svg>
        <div style={{ display: 'flex' }}>
          <span style={{ padding: '0px 4px', fontSize: 8, color: '#007aff' }}>Parent</span>
        </div>
        <div style={{ display: 'flex', alignItems: 'center', gap: 4, padding: '0px 4px' }}>
          <span style={{ fontSize: 10, fontWeight: 500, color: colorTheme.fg4.value }}>
            {parentLayout}
          </span>
          {when(
            flexDirectionIcon != null && parentLayout === 'flex',
            <Icn onClick={flexDirectionClicked} {...(flexDirectionIcon as IcnProps)} />,
          )}
          {when(
            flexDirectionIcon != null && parentLayout === 'flex',
            <div>
              {selectedAlignment ? (
                <PopupList
                  options={allAlignmentOptions}
                  value={selectedAlignment}
                  onSubmitValue={onSubmitValueAlignment}
                />
              ) : null}
            </div>,
          )}
        </div>
      </div>
    </div>
  )
})
