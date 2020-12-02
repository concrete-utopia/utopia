import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ControlProps } from './new-canvas-controls'
import * as TP from '../../../core/shared/template-path'
import { InstancePath } from '../../../core/shared/project-file-types'
import { colorTheme, Icn, PopupList } from '../../../uuiui'
import {
  alignItemsOptions,
  flexDirectionOptions,
  getDirectionAwareLabels,
  justifyContentOptions,
} from '../../inspector/sections/layout-section/flex-container-subsection/flex-container-controls'
import { FlexAlignment, FlexDirection, FlexJustifyContent, FlexWrap } from 'utopia-api'
import { setProp_UNSAFE } from '../../editor/actions/actions'
import { createLayoutPropertyPath } from '../../../core/layout/layout-helpers-new'
import { ElementInstanceMetadata, jsxAttributeValue } from '../../../core/shared/element-template'
import { CanvasRectangle } from '../../../core/shared/math-utils'
import { SelectOption } from '../../../uuiui-deps'
import { useRecoilState } from 'recoil'
import { layoutHoveredState } from '../../../core/shared/inspector-recoil'
import { isFeatureEnabled } from '../../../utils/feature-switches'

interface FlexParentControlProps extends ControlProps {
  frame: CanvasRectangle
  parentTarget: InstancePath
  parentElement: ElementInstanceMetadata
}

const FlexParentControl = (props: FlexParentControlProps): JSX.Element => {
  const { parentElement, dispatch, parentTarget, frame } = props
  const flexWrap: FlexWrap = parentElement.props?.style?.flexWrap ?? 'nowrap'
  const flexDirection: FlexDirection = parentElement.props?.style?.flexDirection ?? 'row'
  const alignItems: FlexAlignment = parentElement.props?.style?.alignItems ?? 'flex-start'
  const justifyContent: FlexJustifyContent =
    parentElement.props?.style?.justifyContent ?? 'flex-start'

  const {
    justifyFlexStart,
    justifyFlexEnd,
    alignDirection,
    alignItemsFlexStart,
    alignItemsFlexEnd,
    alignContentFlexStart,
    alignContentFlexEnd,
  } = getDirectionAwareLabels(flexWrap, flexDirection)
  const flexDirectionIcon = flexDirectionOptions(flexWrap).find(
    (option) => option.value === flexDirection,
  )?.icon
  const allAlignmentOptions = alignItemsOptions(
    alignDirection,
    alignItemsFlexStart,
    alignItemsFlexEnd,
  )
  const selectedAlignment = allAlignmentOptions.find((option) => option.value === alignItems)

  const allJustifyContentOptions = justifyContentOptions(
    flexDirection,
    justifyFlexStart,
    justifyFlexEnd,
  )
  const selectedJustifyContent = allJustifyContentOptions.find(
    (option) => option.value === justifyContent,
  )

  const [, setLayoutHovered] = useRecoilState(layoutHoveredState)

  const flexDirectionClicked = React.useCallback(() => {
    const newValue = flexDirection === 'row' ? 'column' : 'row'
    dispatch(
      [
        setProp_UNSAFE(
          parentTarget,
          createLayoutPropertyPath('flexDirection'),
          jsxAttributeValue(newValue),
        ),
      ],
      'canvas',
    )
  }, [parentTarget, flexDirection, dispatch])

  const onSubmitValueAlignment = React.useCallback(
    (option: SelectOption) => {
      dispatch(
        [
          setProp_UNSAFE(
            parentTarget,
            createLayoutPropertyPath('alignItems'),
            jsxAttributeValue(option.value),
          ),
        ],
        'canvas',
      )
    },
    [parentTarget, dispatch],
  )

  const onSubmitValueJustify = React.useCallback(
    (option: SelectOption) => {
      dispatch(
        [
          setProp_UNSAFE(
            parentTarget,
            createLayoutPropertyPath('justifyContent'),
            jsxAttributeValue(option.value),
          ),
        ],
        'canvas',
      )
    },
    [parentTarget, dispatch],
  )

  return (
    <div
      style={{
        borderRadius: 5,
        boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor.o(50).value} 0px 0px ${
          1 / props.scale
        }px, ${colorTheme.canvasControlsSizeBoxShadowColor.o(21).value} 0px ${1 / props.scale}px ${
          2 / props.scale
        }px ${1 / props.scale}px`,
        height: 25,
        backgroundColor: '#f5f5f5',
        position: 'absolute',
        left: frame.x + props.canvasOffset.x,
        top: frame.y + props.canvasOffset.y - 25 - 8,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'flex-start',
      }}
      onMouseOver={() => {
        setLayoutHovered(true)
      }}
      onMouseOut={() => {
        setLayoutHovered(false)
      }}
    >
      <span
        style={{
          padding: '0 5px',
          color: colorTheme.primary.value,
        }}
      >
        {' '}
        Flex
      </span>
      <div
        style={{
          padding: '0 5px',
        }}
        onClick={flexDirectionClicked}
      >
        {flexDirectionIcon ? <Icn {...flexDirectionIcon} /> : flexDirection}
      </div>
      <div
        style={{
          padding: '0 5px',
        }}
      >
        {selectedAlignment ? (
          <PopupList
            options={allAlignmentOptions}
            value={selectedAlignment}
            onSubmitValue={onSubmitValueAlignment}
          />
        ) : null}
      </div>
      <div
        style={{
          padding: '0 5px',
        }}
      >
        {selectedJustifyContent ? (
          <PopupList
            options={allJustifyContentOptions}
            value={selectedJustifyContent}
            onSubmitValue={onSubmitValueJustify}
          />
        ) : null}
      </div>
    </div>
  )
}

export const ParentControls = (props: ControlProps): JSX.Element | null => {
  if (!isFeatureEnabled('Flex Container Tools')) {
    return null
  }

  const everyThingIsYogaLayouted =
    props.selectedViews.length > 0 &&
    props.selectedViews.every((selectedView) => {
      return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        selectedView,
        props.componentMetadata,
      )
    })

  const parentTarget = props.selectedViews.length > 0 ? TP.parentPath(props.selectedViews[0]) : null
  const frame =
    parentTarget != null
      ? MetadataUtils.getFrameInCanvasCoords(parentTarget, props.componentMetadata)
      : null
  if (
    !everyThingIsYogaLayouted ||
    parentTarget == null ||
    TP.isScenePath(parentTarget) ||
    frame == null
  ) {
    return null
  }
  const parentElement = MetadataUtils.getElementByInstancePathMaybe(
    props.componentMetadata.elements,
    parentTarget,
  )
  if (parentElement == null) {
    return null
  }
  return (
    <FlexParentControl
      {...props}
      parentElement={parentElement}
      parentTarget={parentTarget}
      frame={frame}
    />
  )
}
