import * as R from 'ramda'
import * as React from 'react'
import { animated, SpringValue } from 'react-spring'
import { Icn } from 'uuiui'
import { NumberInput } from 'uuiui'
import { InspectorSubsectionHeader } from 'uuiui'
import { FlexRow } from 'uuiui'
import { BooleanControl } from '../../../controls/boolean-control'
import { ColorControl } from '../../../controls/color-control'
import { ControlStatus, ControlStyles } from '../../../widgets/control-status'
import { PropertyRow, PropertyRowHeightWithLabel } from '../../../widgets/property-row'
import { useArraySuperControl } from '../../../controls/array-supercontrol'
import {
  CSSColor,
  CSSTextShadow,
  CSSTextShadows,
  defaultTextShadow,
  CSSNumber,
  cssNumber,
  EmptyInputValue,
  fallbackOnEmptyInputValueToCSSEmptyValue,
  fallbackOnEmptyInputValueToCSSDefaultEmptyValue,
} from '../../../new-inspector/css-utils'
import {
  useInspectorStyleInfo,
  useIsSubSectionVisible,
} from '../../../new-inspector/new-inspector-hooks'
import { stopPropagation } from '../../../utils'
import { FakeUnknownArrayItem } from '../../../controls/unknown-array-item'
import { SquareButton } from 'uuiui'
import { NewInspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { addOnUnsetValues } from '../../../new-inspector/context-menu-items'
import utils from '../../../../../utils/utils'
import { betterReactMemo } from 'uuiui-deps'

function getIndexedToggleTextShadowEnabled(index: number) {
  return function indexedToggleTextShadowEnabled(
    enabled: boolean,
    cssTextShadows: CSSTextShadows,
  ): CSSTextShadows {
    let newTextShadow = [...cssTextShadows]
    newTextShadow[index].enabled = enabled
    return newTextShadow
  }
}

function getIndexedUpdateTextShadowColor(index: number) {
  return function updateTextShadowColor(
    newColor: CSSColor,
    cssTextShadows: CSSTextShadows,
  ): CSSTextShadows {
    let newTextShadows = [...cssTextShadows]
    let newTextShadow: CSSTextShadow = { ...newTextShadows[index] }
    newTextShadow.color = newColor
    newTextShadows[index] = newTextShadow
    return newTextShadows
  }
}

function getIndexedUpdateTextShadowOffsetX(index: number) {
  return function updateTextShadowOffsetX(
    newOffsetX: CSSNumber | EmptyInputValue,
    cssTextShadows: CSSTextShadows,
  ): CSSTextShadows {
    let newTextShadows = [...cssTextShadows]
    let newTextShadow: CSSTextShadow = { ...newTextShadows[index] }
    newTextShadow.offsetX = fallbackOnEmptyInputValueToCSSEmptyValue(
      {
        ...defaultTextShadow.offsetX,
      },
      newOffsetX,
    )
    newTextShadows[index] = newTextShadow
    return newTextShadows
  }
}

function getIndexedUpdateTextShadowOffsetY(index: number) {
  return function updateTextShadowOffsetY(
    newOffsetY: CSSNumber | EmptyInputValue,
    cssTextShadows: CSSTextShadows,
  ): CSSTextShadows {
    let newTextShadows = [...cssTextShadows]
    let newTextShadow: CSSTextShadow = { ...newTextShadows[index] }
    newTextShadow.offsetY = fallbackOnEmptyInputValueToCSSEmptyValue(
      {
        ...defaultTextShadow.offsetY,
      },
      newOffsetY,
    )
    newTextShadows[index] = newTextShadow
    return newTextShadows
  }
}

function getIndexedUpdateTextShadowBlurRadius(index: number) {
  return function updateTextShadowBlurRadius(
    newBlurRadius: CSSNumber | EmptyInputValue,
    cssTextShadows: CSSTextShadows,
  ): CSSTextShadows {
    let newTextShadows = [...cssTextShadows]
    let newTextShadow: CSSTextShadow = { ...newTextShadows[index] }
    newTextShadow.blurRadius = fallbackOnEmptyInputValueToCSSDefaultEmptyValue(
      defaultTextShadow.blurRadius,
      newBlurRadius,
    )
    newTextShadows[index] = newTextShadow
    return newTextShadows
  }
}

function getIndexedSpliceTextShadow(index: number) {
  return function spliceTextShadow(_: any, cssTextShadows: CSSTextShadows): CSSTextShadows {
    return R.remove(index, 1, [...cssTextShadows])
  }
}

function insertTextShadow(_: any, cssTextShadows: CSSTextShadows): CSSTextShadows {
  return [...cssTextShadows, defaultTextShadow]
}

type TextShadowItemProps = {
  value: CSSTextShadow
  index: number
  textShadowsLength: number
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  useSubmitValueFactory: <NT>(
    transform: (newValue: NT, oldValue: CSSTextShadows) => CSSTextShadows,
  ) => [(newValue: NT) => void, (newValue: NT) => void]
}

const zeroBlurRadius = cssNumber(0, 'px')

const TextShadowItem = betterReactMemo<TextShadowItemProps>('TextShadowItem', (props) => {
  const [enabledSubmitValue] = props.useSubmitValueFactory(
    getIndexedToggleTextShadowEnabled(props.index),
  )
  const [colorSubmitValue, colorTransientSubmitValue] = props.useSubmitValueFactory(
    getIndexedUpdateTextShadowColor(props.index),
  )
  const [offsetXSubmitValue, offsetXTransientSubmitValue] = props.useSubmitValueFactory(
    getIndexedUpdateTextShadowOffsetX(props.index),
  )
  const [offsetYSubmitValue, offsetYTransientSubmitValue] = props.useSubmitValueFactory(
    getIndexedUpdateTextShadowOffsetY(props.index),
  )
  const [blurRadiusSubmitValue, blurRadiusTransientSubmitValue] = props.useSubmitValueFactory(
    getIndexedUpdateTextShadowBlurRadius(props.index),
  )
  const [deleteTextShadowItemSubmitValue] = props.useSubmitValueFactory(
    getIndexedSpliceTextShadow(props.index),
  )

  const removeShadow = React.useCallback(
    (e: React.MouseEvent<HTMLImageElement>) => {
      e.stopPropagation()
      deleteTextShadowItemSubmitValue(null)
    },
    [deleteTextShadowItemSubmitValue],
  )

  return (
    <PropertyRow
      key={props.index}
      style={{
        gridTemplateColumns: '12px 28px repeat(3, 1fr) 20px',
        gridColumnGap: 8,
      }}
    >
      <BooleanControl
        style={{ gridColumn: '1 / span 1' }}
        id='textShadow-enable-disable'
        key='textShadow-enable-disable'
        value={props.value.enabled}
        onSubmitValue={enabledSubmitValue}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
        onMouseDown={stopPropagation}
      />
      <ColorControl
        style={{ gridColumn: '2 / span 1' }}
        id='textShadow-Color'
        key='textShadow-Color'
        value={props.value.color}
        onSubmitValue={colorSubmitValue}
        onTransientSubmitValue={colorTransientSubmitValue}
        pickerOffset={{ x: -45, y: 0 }}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
      />
      <NumberInput
        style={{ gridColumn: '3 / span 1' }}
        value={props.value.offsetX}
        labelBelow='x'
        id={`textShadow-offsetX-${props.index}`}
        onSubmitValue={offsetXSubmitValue}
        onTransientSubmitValue={offsetXTransientSubmitValue}
        controlStatus={props.controlStatus}
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='Length'
        defaultUnitToHide='px'
      />
      <NumberInput
        style={{ gridColumn: '4 / span 1' }}
        value={props.value.offsetY}
        labelBelow='y'
        id={`textShadow-offsetY-${props.index}`}
        onSubmitValue={offsetYSubmitValue}
        onTransientSubmitValue={offsetYTransientSubmitValue}
        controlStatus={props.controlStatus}
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='Length'
        defaultUnitToHide='px'
      />
      <NumberInput
        style={{ gridColumn: '5 / span 1' }}
        value={props.value.blurRadius == null ? zeroBlurRadius : props.value.blurRadius.value}
        labelBelow='blur'
        id={`textShadow-blurRadius-${props.index}`}
        onSubmitValue={blurRadiusSubmitValue}
        onTransientSubmitValue={blurRadiusTransientSubmitValue}
        controlStatus={props.controlStatus}
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='Length'
        defaultUnitToHide='px'
      />
      <SquareButton highlight onMouseDown={removeShadow} style={{ marginTop: 1 }}>
        <Icn category='semantic' type='minus' color='darkgray' width={16} height={16} />
      </SquareButton>
    </PropertyRow>
  )
})

export const TextShadowSubsection = betterReactMemo('TextShadowSubsection', () => {
  const {
    value: textShadowsValue,
    controlStatus,
    controlStyles,
    onUnsetValues,
    onSubmitValue,
    useSubmitValueFactory,
  } = useInspectorStyleInfo('textShadow', undefined, (transformedType: CSSTextShadows) => {
    if (Array.isArray(transformedType) && transformedType.length === 0) {
      return {}
    } else {
      return {
        textShadow: transformedType,
      }
    }
  })

  const [insertTextShadowItemSubmitValue] = useSubmitValueFactory(insertTextShadow)

  const isVisible = useIsSubSectionVisible('textShadow')

  const { springs, bind } = useArraySuperControl(
    textShadowsValue,
    onSubmitValue,
    PropertyRowHeightWithLabel,
  )

  const unsetTextShadows = React.useCallback(() => {
    onUnsetValues()
  }, [onUnsetValues])

  const contextMenuItems = utils.stripNulls([
    textShadowsValue.length > 0 ? addOnUnsetValues(['textShadow'], onUnsetValues) : null,
  ])

  const length = textShadowsValue.length

  const insertShadow = React.useCallback(() => {
    insertTextShadowItemSubmitValue(null)
  }, [insertTextShadowItemSubmitValue])

  if (isVisible) {
    return (
      <NewInspectorContextMenuWrapper
        id='text-shadow-subsection-context-menu'
        items={contextMenuItems}
        data={null}
      >
        <InspectorSubsectionHeader>
          <FlexRow
            style={{
              flexGrow: 1,
            }}
          >
            Text Shadow
          </FlexRow>
          <SquareButton highlight onMouseDown={insertShadow}>
            <Icn
              style={{ paddingTop: 1 }}
              category='semantic'
              type='plus'
              color='darkgray'
              width={16}
              height={16}
            />
          </SquareButton>
        </InspectorSubsectionHeader>
        <div
          style={{
            height: PropertyRowHeightWithLabel * springs.length,
          }}
        >
          {controlStyles.unknown ? (
            <FakeUnknownArrayItem controlStatus={controlStatus} />
          ) : (
            springs.map((springStyle: { [x: string]: SpringValue<any> }, index: number) => {
              const value = textShadowsValue[index]
              if (value != null) {
                return (
                  <animated.div
                    {...bind(index)}
                    key={index}
                    style={{
                      ...springStyle,
                      position: 'absolute',
                      height: PropertyRowHeightWithLabel,
                    }}
                  >
                    <TextShadowItem
                      key={index}
                      value={value}
                      textShadowsLength={length}
                      index={index}
                      controlStatus={controlStatus}
                      controlStyles={controlStyles}
                      useSubmitValueFactory={useSubmitValueFactory}
                    />
                  </animated.div>
                )
              } else {
                throw new Error(`No text shadow exists at index ${index}`)
              }
            })
          )}
        </div>
      </NewInspectorContextMenuWrapper>
    )
  } else {
    return null
  }
})
TextShadowSubsection.displayName = 'TextShadowSubsection'
