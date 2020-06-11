import * as React from 'react'
import { animated, SpringValue } from 'react-spring'
import { Icn, ChainedNumberInput, UtopiaTheme } from 'uuiui'
import { InspectorSubsectionHeader } from 'uuiui'
import { FlexRow } from 'uuiui'
import { BooleanControl } from '../../../controls/boolean-control'
import { ColorControl } from '../../../controls/color-control'
import { ControlStatus, ControlStyles } from '../../../common/control-status'
import { useArraySuperControl } from '../../../controls/array-supercontrol'
import {
  CSSBoxShadow,
  CSSBoxShadowAndBorder,
  CSSBoxShadows,
  CSSColor,
  defaultBoxShadow,
  isCSSUnknownFunctionParameters,
  CSSBoxShadowAndBorderOrUnknown,
  isCSSBoxShadowAndBorder,
  CSSNumber,
  EmptyInputValue,
  fallbackOnEmptyInputValueToCSSEmptyValue,
  fallbackOnEmptyInputValueToCSSDefaultEmptyValue,
} from '../../../common/css-utils'
import {
  useInspectorStyleInfo,
  useIsSubSectionVisible,
  UseSubmitValueFactory,
} from '../../../common/property-path-hooks'
import { stopPropagation } from '../../../common/inspector-utils'
import { FakeUnknownArrayItem } from '../../../controls/unknown-array-item'
import { useGetSubsectionHeaderStyle } from '../../../common/inspector-utils'
import { SquareButton } from 'uuiui'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { addOnUnsetValues, removeRow } from '../../../common/context-menu-items'
import utils from '../../../../../utils/utils'
import { betterReactMemo } from 'uuiui-deps'
import { GridRow } from '../../../widgets/grid-row'
import { ContextMenuItem } from '../../../../context-menu-items'

function getIndexedToggleShadowEnabled(index: number) {
  return function indexedUpdateShadowEnabled(
    newValue: boolean,
    oldValue: CSSBoxShadows,
  ): CSSBoxShadows {
    let newBoxShadows = [...oldValue]
    newBoxShadows[index].enabled = newValue
    return newBoxShadows
  }
}

function getIndexedUpdateShadowColor(index: number) {
  return function indexedUpdateShadowColor(
    newValue: CSSColor,
    oldValue: CSSBoxShadows,
  ): CSSBoxShadows {
    let newBoxShadows = [...oldValue]
    newBoxShadows[index] = { ...newBoxShadows[index], color: newValue }
    return newBoxShadows
  }
}

function getIndexedUpdateShadowOffsetX(index: number) {
  return function indexedUpdateShadowOffsetX(
    newOffsetX: CSSNumber | EmptyInputValue,
    oldValue: CSSBoxShadows,
  ): CSSBoxShadows {
    let newBoxShadows = [...oldValue]
    newBoxShadows[index] = {
      ...newBoxShadows[index],
      offsetX: fallbackOnEmptyInputValueToCSSEmptyValue(defaultBoxShadow.offsetX, newOffsetX),
    }
    return newBoxShadows
  }
}

function getIndexedUpdateShadowOffsetY(index: number) {
  return function indexedUpdateShadowOffsetY(
    newOffsetY: CSSNumber | EmptyInputValue,
    oldValue: CSSBoxShadows,
  ): CSSBoxShadows {
    let newBoxShadows = [...oldValue]
    newBoxShadows[index] = {
      ...newBoxShadows[index],
      offsetY: fallbackOnEmptyInputValueToCSSEmptyValue(defaultBoxShadow.offsetY, newOffsetY),
    }
    return newBoxShadows
  }
}

function getIndexedUpdateShadowBlurRadius(index: number) {
  return function indexedUpdateShadowBlurRadius(
    newBlurRadius: CSSNumber | EmptyInputValue,
    oldValue: CSSBoxShadows,
  ): CSSBoxShadows {
    let newBoxShadows = [...oldValue]
    newBoxShadows[index] = {
      ...newBoxShadows[index],
      blurRadius: fallbackOnEmptyInputValueToCSSDefaultEmptyValue(
        defaultBoxShadow.blurRadius,
        newBlurRadius,
      ),
    }
    return newBoxShadows
  }
}

function getIndexedUpdateShadowSpreadRadius(index: number) {
  return function indexedUpdateShadowSpreadRadius(
    newSpreadRadius: CSSNumber | EmptyInputValue,
    oldValue: CSSBoxShadows,
  ): CSSBoxShadows {
    let newBoxShadows = [...oldValue]
    newBoxShadows[index] = {
      ...newBoxShadows[index],
      spreadRadius: fallbackOnEmptyInputValueToCSSDefaultEmptyValue(
        defaultBoxShadow.spreadRadius,
        newSpreadRadius,
      ),
    }
    return newBoxShadows
  }
}

function updateInsertShadow(_: any, oldValue: CSSBoxShadows): CSSBoxShadows {
  return [...oldValue, { ...defaultBoxShadow }]
}

function getIndexedSpliceShadow(index: number) {
  return function (_: any, oldValue: CSSBoxShadows) {
    let newCSSBoxShadow = [...oldValue]
    newCSSBoxShadow.splice(index, 1)
    return newCSSBoxShadow
  }
}

interface ShadowItemProps {
  value: CSSBoxShadow
  shadowsLength: number
  onUnsetValues: () => void
  index: number
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  useSubmitValueFactory: UseSubmitValueFactory<CSSBoxShadows>
  contextMenuItems: Array<ContextMenuItem<null>>
}

const rowHeight = UtopiaTheme.layout.gridRowHeight.tall

const ShadowItem = betterReactMemo<ShadowItemProps>('ShadowItem', (props) => {
  const [enabledSubmitValue] = props.useSubmitValueFactory(
    getIndexedToggleShadowEnabled(props.index),
  )
  const [colorSubmitValue, colorTransientSubmitValue] = props.useSubmitValueFactory(
    getIndexedUpdateShadowColor(props.index),
  )
  const [offsetXSubmitValue, offsetXTransientSubmitValue] = props.useSubmitValueFactory(
    getIndexedUpdateShadowOffsetX(props.index),
  )
  const [offsetYSubmitValue, offsetYTransientSubmitValue] = props.useSubmitValueFactory(
    getIndexedUpdateShadowOffsetY(props.index),
  )
  const [blurRadiusSubmitValue, blurRadiusTransientSubmitValue] = props.useSubmitValueFactory(
    getIndexedUpdateShadowBlurRadius(props.index),
  )
  const [spreadRadiusSubmitValue, spreadRadiusTransientSubmitValue] = props.useSubmitValueFactory(
    getIndexedUpdateShadowSpreadRadius(props.index),
  )

  const [onSubmitIndexedSpliceValue] = props.useSubmitValueFactory(
    getIndexedSpliceShadow(props.index),
  )

  const { shadowsLength, onUnsetValues } = props
  const removeShadow = React.useMemo(() => {
    return removeRow(() => {
      if (shadowsLength > 1) {
        onSubmitIndexedSpliceValue(null)
      } else {
        onUnsetValues()
      }
    })
  }, [shadowsLength, onUnsetValues, onSubmitIndexedSpliceValue])

  return (
    <InspectorContextMenuWrapper
      id={`shadow-row-context-menu-${props.index}`}
      items={[removeShadow, ...props.contextMenuItems]}
      data={null}
    >
      <GridRow tall padded={true} alignItems='start' type='<---1fr--->|------172px-------|'>
        <GridRow tall padded={false} alignItems='start' type='<-auto-><----------1fr--------->'>
          <BooleanControl
            id='boxShadow-enable-disable'
            key='boxShadow-enable-disable'
            value={props.value.enabled}
            onMouseDown={stopPropagation}
            onSubmitValue={enabledSubmitValue}
            controlStatus={props.controlStatus}
            controlStyles={props.controlStyles}
          />
          <ColorControl
            id='boxShadow-Color'
            key='boxShadow-Color'
            value={props.value.color}
            onSubmitValue={colorSubmitValue}
            onTransientSubmitValue={colorTransientSubmitValue}
            pickerOffset={{ x: -45, y: 0 }}
            controlStatus={props.controlStatus}
            controlStyles={props.controlStyles}
          />
        </GridRow>
        <ChainedNumberInput
          idPrefix='boxShadow'
          propsArray={[
            {
              value: props.value.offsetX,
              labelBelow: 'x',
              onSubmitValue: offsetXSubmitValue,
              onTransientSubmitValue: offsetXTransientSubmitValue,
              controlStatus: props.controlStatus,
              numberType: 'Length',
              defaultUnitToHide: 'px',
            },
            {
              value: props.value.offsetY,
              labelBelow: 'y',
              onSubmitValue: offsetYSubmitValue,
              onTransientSubmitValue: offsetYTransientSubmitValue,
              controlStatus: props.controlStatus,
              numberType: 'Length',
              defaultUnitToHide: 'px',
            },
            {
              value: props.value.blurRadius.value,
              labelBelow: 'blur',
              onSubmitValue: blurRadiusSubmitValue,
              onTransientSubmitValue: blurRadiusTransientSubmitValue,
              controlStatus: props.controlStatus,
              numberType: 'Length',
              defaultUnitToHide: 'px',
            },
            {
              value: props.value.spreadRadius.value,
              labelBelow: 'spread',
              onSubmitValue: spreadRadiusSubmitValue,
              onTransientSubmitValue: spreadRadiusTransientSubmitValue,
              controlStatus: props.controlStatus,
              numberType: 'Length',
              defaultUnitToHide: 'px',
            },
          ]}
        />
      </GridRow>
    </InspectorContextMenuWrapper>
  )
})

export const ShadowSubsection = betterReactMemo('ShadowSubsection', () => {
  const isVisible = useIsSubSectionVisible('shadow')
  const {
    value,
    onUnsetValues,
    onSubmitValue,
    controlStatus,
    controlStyles,
    propertyStatus,
    useSubmitValueFactory,
  } = useInspectorStyleInfo('boxShadow')
  const headerStyle = useGetSubsectionHeaderStyle(controlStatus)

  const [insertShadowValue] = useSubmitValueFactory(updateInsertShadow)

  const { springs, bind } = useArraySuperControl(value, onSubmitValue, rowHeight)

  const contextMenuItems = utils.stripNulls([
    value.length > 0 ? addOnUnsetValues(['boxShadow parameter'], onUnsetValues) : null,
  ])

  if (!isVisible) {
    return null
  }
  return (
    <InspectorContextMenuWrapper
      id='shadow-subsection-context-menu'
      items={contextMenuItems}
      data={null}
    >
      <InspectorSubsectionHeader style={headerStyle}>
        <FlexRow
          style={{
            flexGrow: 1,
          }}
        >
          Shadow
        </FlexRow>
        {propertyStatus.overwritable ? (
          <SquareButton highlight onMouseDown={insertShadowValue}>
            <Icn
              onMouseDown={insertShadowValue}
              style={{ paddingTop: 1 }}
              category='semantic'
              type='plus'
              color={propertyStatus.controlled ? 'blue' : 'darkgray'}
              width={16}
              height={16}
            />
          </SquareButton>
        ) : null}
      </InspectorSubsectionHeader>
      <div
        style={{
          height: rowHeight * springs.length,
        }}
      >
        {isCSSUnknownFunctionParameters(value) ? (
          <FakeUnknownArrayItem controlStatus={controlStatus} />
        ) : (
          springs.map((springStyle: { [x: string]: SpringValue<any> }, index: number) => {
            const boxShadow = value[index]
            if (boxShadow != null) {
              return (
                <animated.div
                  {...bind(index)}
                  key={index}
                  style={{
                    ...springStyle,
                    width: '100%',
                    position: 'absolute',
                    height: rowHeight,
                  }}
                >
                  <ShadowItem
                    value={boxShadow}
                    shadowsLength={value.length}
                    useSubmitValueFactory={useSubmitValueFactory}
                    onUnsetValues={onUnsetValues}
                    index={index}
                    key={index}
                    controlStatus={controlStatus}
                    controlStyles={controlStyles}
                    contextMenuItems={contextMenuItems}
                  />
                </animated.div>
              )
            } else {
              throw new Error(`No shadow exists at index ${index}`)
            }
          })
        )}
      </div>
    </InspectorContextMenuWrapper>
  )
})
ShadowSubsection.displayName = 'ShadowSubsection'
