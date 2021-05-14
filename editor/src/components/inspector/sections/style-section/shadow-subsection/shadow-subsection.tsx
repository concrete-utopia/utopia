import * as React from 'react'
import { animated, SpringValue } from 'react-spring'
import utils from '../../../../../utils/utils'
import {
  UtopiaTheme,
  useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue,
  CheckboxInput,
  ChainedNumberInput,
  InspectorSubsectionHeader,
  FlexRow,
  SquareButton,
  Icn,
} from '../../../../../uuiui'
import { betterReactMemo } from '../../../../../uuiui-deps'
import { ContextMenuItem } from '../../../../context-menu-items'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { addOnUnsetValues, removeRow } from '../../../common/context-menu-items'
import { ControlStatus, ControlStyles } from '../../../common/control-status'
import {
  CSSBoxShadow,
  CSSBoxShadows,
  CSSColor,
  CSSNumber,
  defaultBoxShadow,
  EmptyInputValue,
  fallbackOnEmptyInputValueToCSSDefaultEmptyValue,
  fallbackOnEmptyInputValueToCSSEmptyValue,
  cssPixelLength,
  cssDefault,
  toggleShadowEnabled,
} from '../../../common/css-utils'
import { useGetSubsectionHeaderStyle } from '../../../common/inspector-utils'
import {
  useInspectorStyleInfo,
  useIsSubSectionVisible,
  UseSubmitValueFactory,
} from '../../../common/property-path-hooks'
import { useArraySuperControl } from '../../../controls/array-supercontrol'
import { ColorControl } from '../../../controls/color-control'
import { UIGridRow } from '../../../widgets/ui-grid-row'

export function getIndexedToggleShadowEnabled(index: number) {
  return function (_: null, oldValue: CSSBoxShadows): CSSBoxShadows {
    let newBoxShadows = [...oldValue]
    newBoxShadows[index] = toggleShadowEnabled(newBoxShadows[index])
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
      offsetX: fallbackOnEmptyInputValueToCSSEmptyValue(cssPixelLength(0), newOffsetX),
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
      offsetY: fallbackOnEmptyInputValueToCSSEmptyValue(cssPixelLength(0), newOffsetY),
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
        cssDefault(cssPixelLength(0)),
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
        cssDefault(cssPixelLength(0)),
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

const rowHeight = UtopiaTheme.layout.rowHeight.max

const ShadowItem = betterReactMemo<ShadowItemProps>('ShadowItem', (props) => {
  const [enabledSubmitValueToggle] = props.useSubmitValueFactory(
    getIndexedToggleShadowEnabled(props.index),
  )
  const [colorSubmitValue, colorTransientSubmitValue] = props.useSubmitValueFactory(
    getIndexedUpdateShadowColor(props.index),
  )
  const [
    offsetXSubmitValue,
    offsetXTransientSubmitValue,
  ] = useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
    props.useSubmitValueFactory(getIndexedUpdateShadowOffsetX(props.index)),
  )
  const [
    offsetYSubmitValue,
    offsetYTransientSubmitValue,
  ] = useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
    props.useSubmitValueFactory(getIndexedUpdateShadowOffsetY(props.index)),
  )
  const [
    blurRadiusSubmitValue,
    blurRadiusTransientSubmitValue,
  ] = useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
    props.useSubmitValueFactory(getIndexedUpdateShadowBlurRadius(props.index)),
  )
  const [
    spreadRadiusSubmitValue,
    spreadRadiusTransientSubmitValue,
  ] = useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
    props.useSubmitValueFactory(getIndexedUpdateShadowSpreadRadius(props.index)),
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
  }, [shadowsLength, onSubmitIndexedSpliceValue, onUnsetValues])

  const enabledSubmitValue = React.useCallback(() => {
    enabledSubmitValueToggle(null)
  }, [enabledSubmitValueToggle])

  return (
    <InspectorContextMenuWrapper
      id={`shadow-row-context-menu-${props.index}`}
      items={[removeShadow, ...props.contextMenuItems]}
      data={null}
    >
      <UIGridRow tall padded={true} alignItems='start' variant='<---1fr--->|------172px-------|'>
        <UIGridRow
          tall
          padded={false}
          alignItems='start'
          variant='<-auto-><----------1fr--------->'
        >
          <CheckboxInput
            id='boxShadow-enable-disable'
            checked={props.value.enabled}
            onChange={enabledSubmitValue}
            controlStatus={props.controlStatus}
          />
          <ColorControl
            id='boxShadow-Color'
            key='boxShadow-Color'
            testId='boxShadow-Color'
            value={props.value.color}
            onSubmitValue={colorSubmitValue}
            onTransientSubmitValue={colorTransientSubmitValue}
            pickerOffset={{ x: -45, y: 0 }}
            controlStatus={props.controlStatus}
            controlStyles={props.controlStyles}
          />
        </UIGridRow>
        <ChainedNumberInput
          idPrefix='boxShadow'
          propsArray={[
            {
              value: props.value.offsetX,
              DEPRECATED_labelBelow: 'x',
              onSubmitValue: offsetXSubmitValue,
              onTransientSubmitValue: offsetXTransientSubmitValue,
              controlStatus: props.controlStatus,
              numberType: 'Length',
              defaultUnitToHide: 'px',
              testId: 'boxShadow-x',
            },
            {
              value: props.value.offsetY,
              DEPRECATED_labelBelow: 'y',
              onSubmitValue: offsetYSubmitValue,
              onTransientSubmitValue: offsetYTransientSubmitValue,
              controlStatus: props.controlStatus,
              numberType: 'Length',
              defaultUnitToHide: 'px',
              testId: 'boxShadow-y',
            },
            {
              value: props.value.blurRadius.value,
              DEPRECATED_labelBelow: 'blur',
              onSubmitValue: blurRadiusSubmitValue,
              onTransientSubmitValue: blurRadiusTransientSubmitValue,
              controlStatus: props.controlStatus,
              numberType: 'Length',
              defaultUnitToHide: 'px',
              testId: 'boxShadow-blur',
            },
            {
              value: props.value.spreadRadius.value,
              DEPRECATED_labelBelow: 'spread',
              onSubmitValue: spreadRadiusSubmitValue,
              onTransientSubmitValue: spreadRadiusTransientSubmitValue,
              controlStatus: props.controlStatus,
              numberType: 'Length',
              defaultUnitToHide: 'px',
              testId: 'boxShadow-spread',
            },
          ]}
        />
      </UIGridRow>
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
    value.length > 0 ? addOnUnsetValues(['boxShadow'], onUnsetValues) : null,
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
        {springs.map((springStyle: { [x: string]: SpringValue<any> }, index: number) => {
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
        })}
      </div>
    </InspectorContextMenuWrapper>
  )
})
ShadowSubsection.displayName = 'ShadowSubsection'
