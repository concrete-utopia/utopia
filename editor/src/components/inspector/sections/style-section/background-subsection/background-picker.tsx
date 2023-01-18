import fastDeepEquals from 'fast-deep-equal'
import React from 'react'
import { jsxAttributeValue, emptyComments } from '../../../../../core/shared/element-template'
import { create } from '../../../../../core/shared/property-path'
import {
  useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue,
  useColorTheme,
  FlexRow,
  Icn,
  NumberInput,
  PopupList,
  UtopiaStyles,
  UIRow,
  Icons,
} from '../../../../../uuiui'
import { pickColorWithEyeDropper } from '../../../../canvas/canvas-utils'
import { setProperty } from '../../../../editor/actions/action-creators'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../../../editor/store/store-hook'
import { ControlStatus } from '../../../common/control-status'
import {
  CSSBackgroundLayer,
  CSSBackgroundLayers,
  CSSBackgroundLayerType,
  CSSColor,
  CSSConicGradientBackgroundLayer,
  CSSGradientBackgroundLayer,
  CSSGradientStop,
  CSSLinearGradientBackgroundLayer,
  CSSNumber,
  CSSRadialGradientBackgroundLayer,
  CSSSolidBackgroundLayer,
  CSSURLFunctionBackgroundLayer,
  defaultConicGradientBackgroundLayer,
  defaultCSSColor,
  defaultCSSRadialGradientSize,
  EmptyInputValue,
  fallbackOnEmptyInputValueToCSSDefaultEmptyValue,
  isCSSBackgroundImageLayer,
  isCSSBackgroundLayerWithBGSize,
  isCSSConicGradientBackgroundLayer,
  isCSSGradientBackgroundLayer,
  isCSSImageURLBackgroundLayer,
  isCSSRadialGradientBackgroundLayer,
  isCSSSolidBackgroundLayer,
  orderStops,
} from '../../../common/css-utils'
import {
  TransformedStateAndPropsEqualityTest,
  stopPropagation,
  useHandleCloseOnESCOrEnter,
  useModelControlledTransformableState,
} from '../../../common/inspector-utils'
import { UseSubmitValueFactory } from '../../../common/property-path-hooks'
import { BGSizeMetadataControl } from '../../../controls/bg-size-metadata-control'
import { ColorPickerInner, colorPickerWidth } from '../../../controls/color-picker'
import { URLBackgroundLayerMetadataControls } from '../../../controls/url-background-layer-metadata-controls'
import { InspectorModal } from '../../../widgets/inspector-modal'
import {
  backgroundLayerTypeSelectOptions,
  conicGradientSelectOption,
  CSSBackgroundLayerTypeSelectOption,
  getIndexedOnCSSBackgroundLayerTypeSelectSubmitValue,
  getIndexedUpdateRadialOrConicGradientCenterX,
  getIndexedUpdateRadialOrConicGradientCenterY,
  imageSelectOption,
  linearGradientSelectOption,
  radialGradientSelectOption,
  solidSelectOption,
} from './background-layer-helpers'
import { GradientStopsEditor } from './gradient-stop-editor'
import { getIndexedUpdateCSSBackgroundLayerLinearGradientAngle } from './linear-gradient-layer'
import { PickerImagePreview } from './picker-image-preview'

const backgroundLayerOptionsByValue: {
  [key in CSSBackgroundLayerType]: CSSBackgroundLayerTypeSelectOption
} = {
  'solid-background-layer': solidSelectOption,
  'linear-gradient-background-layer': linearGradientSelectOption,
  'radial-gradient-background-layer': radialGradientSelectOption,
  'conic-gradient-background-layer': conicGradientSelectOption,
  'url-function-background-layer': imageSelectOption,
}

interface BackgroundPickerProps {
  value: CSSSolidBackgroundLayer | CSSGradientBackgroundLayer | CSSURLFunctionBackgroundLayer
  closePopup: () => void
  portalTarget?: HTMLElement
  useSubmitValueFactory: UseSubmitValueFactory<CSSBackgroundLayers>
  offsetX: number
  offsetY: number
  id: string
  testId: string
  backgroundLayerIndex: number
  controlStatus: ControlStatus
}

function getIndexedUpdateCSSBackgroundLayerStop(index: number, backgroundLayerIndex: number) {
  return function indexedUpdateCSSBackgroundLayerStop(
    newValue: CSSColor,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    const oldBackgroundLayer = oldValue[backgroundLayerIndex]
    if (oldBackgroundLayer != null) {
      const newBackgroundLayers = [...oldValue]
      if (isCSSGradientBackgroundLayer(oldBackgroundLayer)) {
        const newStopsArray = [...oldBackgroundLayer.stops]
        newStopsArray[index].color = { ...newValue }
        newBackgroundLayers[backgroundLayerIndex] = { ...oldBackgroundLayer, stops: newStopsArray }
      } else if (isCSSSolidBackgroundLayer(oldBackgroundLayer)) {
        newBackgroundLayers[backgroundLayerIndex] = {
          ...oldBackgroundLayer,
          color: newValue,
        }
      }
      return newBackgroundLayers
    }
    return oldValue
  }
}

export const inspectorEdgePadding = 8

export interface BackgroundLayerControlsProps {
  value: CSSBackgroundLayer
  useSubmitValueFactory: UseSubmitValueFactory<CSSBackgroundLayers>
  controlStatus: ControlStatus
  index: number
}

interface LinearGradientControlsProps extends BackgroundLayerControlsProps {
  value: CSSLinearGradientBackgroundLayer
}

export const MetadataControlsStyle: React.CSSProperties = {
  padding: `0 ${inspectorEdgePadding}px 6px`,
  display: 'grid',
  gridTemplateColumns: '1fr 8px 1fr 20px 1fr 8px 1fr',
  columnGap: 0,
  gridRowGap: 8,
}

const LinearGradientControls: React.FunctionComponent<
  React.PropsWithChildren<LinearGradientControlsProps>
> = (props) => {
  const [gradientAngleSubmitValue, gradientAngleTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(
        getIndexedUpdateCSSBackgroundLayerLinearGradientAngle(props.index),
      ),
    )
  return (
    <div style={MetadataControlsStyle}>
      <NumberInput
        id='background-layer-gradient-angle'
        testId='background-layer-gradient-angle'
        value={props.value.angle.value}
        onSubmitValue={gradientAngleSubmitValue}
        onTransientSubmitValue={gradientAngleTransientSubmitValue}
        controlStatus={props.controlStatus}
        labelInner={{
          category: 'layout/systems',
          type: 'transform-rotate',
          color: 'secondary',
          width: 10,
          height: 10,
        }}
        DEPRECATED_labelBelow='angle'
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='AnglePercent'
        defaultUnitToHide={null}
      />
    </div>
  )
}

interface RadialGradientControlsProps extends BackgroundLayerControlsProps {
  value: CSSRadialGradientBackgroundLayer
}

function getIndexedUpdateRadialGradientWidth(index: number) {
  return function updateRadialGradientWidth(
    newValue: CSSNumber | EmptyInputValue,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    const newCssBackgroundImagesizing = [...oldValue]
    const workingRadialGradient = { ...newCssBackgroundImagesizing[index] }
    if (isCSSRadialGradientBackgroundLayer(workingRadialGradient)) {
      workingRadialGradient.gradientSize.width = fallbackOnEmptyInputValueToCSSDefaultEmptyValue(
        defaultCSSRadialGradientSize.width,
        newValue,
      )
    }
    newCssBackgroundImagesizing[index] = workingRadialGradient
    return newCssBackgroundImagesizing
  }
}

function getIndexedUpdateRadialGradientHeight(index: number) {
  return function updateRadialGradientHeight(
    newValue: CSSNumber | EmptyInputValue,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    const newCssBackgroundImagesizing = [...oldValue]
    const workingRadialGradient = { ...newCssBackgroundImagesizing[index] }
    if (isCSSRadialGradientBackgroundLayer(workingRadialGradient)) {
      workingRadialGradient.gradientSize.height = fallbackOnEmptyInputValueToCSSDefaultEmptyValue(
        defaultCSSRadialGradientSize.height,
        newValue,
      )
    }
    newCssBackgroundImagesizing[index] = workingRadialGradient
    return newCssBackgroundImagesizing
  }
}

const RadialGradientControls: React.FunctionComponent<
  React.PropsWithChildren<RadialGradientControlsProps>
> = (props) => {
  const [gradientWidthSubmitValue, gradientWidthTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(getIndexedUpdateRadialGradientWidth(props.index)),
    )
  const [gradientHeightSubmitValue, gradientHeightTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(getIndexedUpdateRadialGradientHeight(props.index)),
    )
  const [gradientCenterXSubmitValue, gradientCenterXTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(getIndexedUpdateRadialOrConicGradientCenterX(props.index)),
    )
  const [gradientCenterYSubmitValue, gradientCenterYTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(getIndexedUpdateRadialOrConicGradientCenterY(props.index)),
    )

  const radialBackgroundLayerWidth = props.value.gradientSize.width.value
  const radialBackgroundLayerHeight = props.value.gradientSize.height.value

  return (
    <div style={MetadataControlsStyle}>
      <NumberInput
        style={{
          gridColumn: '1 / span 1',
        }}
        id='background-gradient-center-x'
        testId='background-gradient-center-x'
        value={props.value.center.x.value}
        onSubmitValue={gradientCenterXSubmitValue}
        onTransientSubmitValue={gradientCenterXTransientSubmitValue}
        controlStatus={props.controlStatus}
        DEPRECATED_labelBelow='center x'
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='LengthPercent'
        defaultUnitToHide={null}
      />
      <NumberInput
        style={{
          gridColumn: '3 / span 1',
        }}
        id='background-gradient-center-y'
        testId='background-gradient-center-y'
        value={props.value.center.y.value}
        onSubmitValue={gradientCenterYSubmitValue}
        onTransientSubmitValue={gradientCenterYTransientSubmitValue}
        controlStatus={props.controlStatus}
        DEPRECATED_labelBelow='center y'
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='LengthPercent'
        defaultUnitToHide={null}
      />
      <NumberInput
        style={{
          gridColumn: '5 / span 1',
        }}
        id='background-gradient-width'
        testId='background-gradient-width'
        value={radialBackgroundLayerWidth}
        onSubmitValue={gradientWidthSubmitValue}
        onTransientSubmitValue={gradientWidthTransientSubmitValue}
        controlStatus={props.controlStatus}
        DEPRECATED_labelBelow='width'
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='LengthPercent'
        defaultUnitToHide={null}
      />
      <NumberInput
        style={{
          gridColumn: '7 / span 1',
        }}
        id='background-gradient-height'
        testId='background-gradient-height'
        value={radialBackgroundLayerHeight}
        onSubmitValue={gradientHeightSubmitValue}
        onTransientSubmitValue={gradientHeightTransientSubmitValue}
        controlStatus={props.controlStatus}
        DEPRECATED_labelBelow='height'
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='LengthPercent'
        defaultUnitToHide={null}
      />
    </div>
  )
}

interface ConicGradientControlsProps extends BackgroundLayerControlsProps {
  value: CSSConicGradientBackgroundLayer
}

function getIndexedUpdateConicGradientFromAngle(index: number) {
  return function updateConicGradientFromAngle(
    newValue: CSSNumber | EmptyInputValue,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    const newCssBackgroundImagesizing = [...oldValue]
    const workingConicGradient = { ...newCssBackgroundImagesizing[index] }
    if (isCSSConicGradientBackgroundLayer(workingConicGradient)) {
      workingConicGradient.fromAngle = fallbackOnEmptyInputValueToCSSDefaultEmptyValue(
        defaultConicGradientBackgroundLayer.fromAngle,
        newValue,
      )
    }
    newCssBackgroundImagesizing[index] = workingConicGradient
    return newCssBackgroundImagesizing
  }
}

const ConicGradientControls: React.FunctionComponent<
  React.PropsWithChildren<ConicGradientControlsProps>
> = (props) => {
  const [gradientCenterXSubmitValue, gradientCenterXTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(getIndexedUpdateRadialOrConicGradientCenterX(props.index)),
    )
  const [gradientCenterYSubmitValue, gradientCenterYTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(getIndexedUpdateRadialOrConicGradientCenterY(props.index)),
    )

  const [gradientFromAngleSubmitValue, gradientFromAngleTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(getIndexedUpdateConicGradientFromAngle(props.index)),
    )

  return (
    <div style={MetadataControlsStyle}>
      <NumberInput
        style={{ gridColumn: '1 / span 1' }}
        id='background-gradient-center-x'
        testId='background-gradient-center-x'
        value={props.value.center.x.value}
        onSubmitValue={gradientCenterXSubmitValue}
        onTransientSubmitValue={gradientCenterXTransientSubmitValue}
        controlStatus={props.controlStatus}
        DEPRECATED_labelBelow='x'
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='LengthPercent'
        defaultUnitToHide={null}
      />
      <NumberInput
        style={{ gridColumn: '3 / span 1' }}
        id='background-gradient-center-y'
        testId='background-gradient-center-y'
        value={props.value.center.y.value}
        onSubmitValue={gradientCenterYSubmitValue}
        onTransientSubmitValue={gradientCenterYTransientSubmitValue}
        controlStatus={props.controlStatus}
        DEPRECATED_labelBelow='y'
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='LengthPercent'
        defaultUnitToHide={null}
      />
      <NumberInput
        style={{ gridColumn: '5 / span 1' }}
        id='background-gradient-from-angle'
        testId='background-gradient-from-angle'
        value={props.value.fromAngle.value}
        onSubmitValue={gradientFromAngleSubmitValue}
        onTransientSubmitValue={gradientFromAngleTransientSubmitValue}
        controlStatus={props.controlStatus}
        DEPRECATED_labelBelow='angle'
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='AnglePercent'
        defaultUnitToHide={null}
      />
    </div>
  )
}

const doesPropsEqualStateStops: TransformedStateAndPropsEqualityTest<Array<CSSGradientStop>> = (
  newStateValue,
  newPropsValue,
) => fastDeepEquals(orderStops(newStateValue), newPropsValue)

function setColor(
  stopIndex: number,
  newValue: CSSColor,
  oldValue: Array<CSSGradientStop>,
): Array<CSSGradientStop> {
  const workingStops = [...oldValue]
  const workingStop = { ...workingStops[stopIndex] }
  workingStop.color = newValue
  workingStops[stopIndex] = workingStop
  return workingStops
}

export const BackgroundPicker: React.FunctionComponent<
  React.PropsWithChildren<BackgroundPickerProps>
> = (props) => {
  const colorTheme = useColorTheme()
  useHandleCloseOnESCOrEnter(props.closePopup)
  const [showSettings, setShowSettings] = React.useState(false)
  const toggleSettings = React.useCallback(
    () => setShowSettings((value) => !value),
    [setShowSettings],
  )
  const [selectedStopUnorderedIndex, setSelectedStopUnorderedIndex] = React.useState(0)
  const dispatch = useDispatch()
  const selectedViewsFromStore = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'BackgroundPicker selectedViews',
  )

  const [onSubmitColorValue, onTransientSubmitColorValue] = props.useSubmitValueFactory(
    getIndexedUpdateCSSBackgroundLayerStop(selectedStopUnorderedIndex, props.backgroundLayerIndex),
  )
  const [onSubmitBackgroundLayerType] = props.useSubmitValueFactory(
    getIndexedOnCSSBackgroundLayerTypeSelectSubmitValue(props.backgroundLayerIndex),
  )

  const { backgroundLayerIndex } = props
  const updateStops = React.useCallback(
    (newStops: Array<CSSGradientStop>, oldValue: CSSBackgroundLayers) => {
      const oldBackgroundLayer = oldValue[backgroundLayerIndex]
      if (oldBackgroundLayer != null) {
        if (isCSSGradientBackgroundLayer(oldBackgroundLayer)) {
          const newBackgroundLayers = [...oldValue]
          newBackgroundLayers[backgroundLayerIndex] = {
            ...oldBackgroundLayer,
            stops: newStops,
          }
          return newBackgroundLayers
        }
      }
      return oldValue
    },
    [backgroundLayerIndex],
  )

  const [onSubmitValueStops, onTransientSubmitValueStops] = props.useSubmitValueFactory(updateStops)

  const [stops, onSubmitValueAndUpdateLocalStops] = useModelControlledTransformableState<
    Array<CSSGradientStop>
  >(
    isCSSGradientBackgroundLayer(props.value) ? props.value.stops : [],
    doesPropsEqualStateStops,
    onSubmitValueStops,
    onTransientSubmitValueStops,
  )

  const onSubmitValueAndUpdateLocalColor = React.useCallback(
    (newValue: CSSColor) =>
      onSubmitValueAndUpdateLocalStops(
        setColor(selectedStopUnorderedIndex, newValue, stops),
        'dragEnd',
      ),
    [stops, onSubmitValueAndUpdateLocalStops, selectedStopUnorderedIndex],
  )

  const onTransientSubmitValueAndUpdateLocalColor = React.useCallback(
    (newValue: CSSColor) =>
      onSubmitValueAndUpdateLocalStops(
        setColor(selectedStopUnorderedIndex, newValue, stops),
        'drag',
      ),
    [stops, onSubmitValueAndUpdateLocalStops, selectedStopUnorderedIndex],
  )

  const closePopup = props.closePopup

  const dispatchEyeDropper = React.useCallback(() => {
    const selectedViews = selectedViewsFromStore
    if (selectedViews.length === 0) {
      return
    }
    closePopup()
    void pickColorWithEyeDropper()
      .then(({ sRGBHex }) => {
        dispatch(
          selectedViews.map((view) =>
            setProperty(
              view,
              create(['style', 'backgroundColor']),
              jsxAttributeValue(sRGBHex, emptyComments),
            ),
          ),
        )
      })
      .catch((e) => console.error(e))
  }, [dispatch, closePopup, selectedViewsFromStore])

  const MetadataControls: React.ReactNode = (() => {
    switch (props.value.type) {
      case 'linear-gradient-background-layer': {
        return (
          <LinearGradientControls
            value={props.value}
            index={props.backgroundLayerIndex}
            useSubmitValueFactory={props.useSubmitValueFactory}
            controlStatus={props.controlStatus}
          />
        )
      }
      case 'radial-gradient-background-layer': {
        return (
          <RadialGradientControls
            value={props.value}
            index={props.backgroundLayerIndex}
            useSubmitValueFactory={props.useSubmitValueFactory}
            controlStatus={props.controlStatus}
          />
        )
      }
      case 'conic-gradient-background-layer': {
        return (
          <ConicGradientControls
            value={props.value}
            index={props.backgroundLayerIndex}
            useSubmitValueFactory={props.useSubmitValueFactory}
            controlStatus={props.controlStatus}
          />
        )
      }
      case 'url-function-background-layer': // these are displayed by default
      case 'solid-background-layer': {
        return null
      }
      default: {
        const _exhaustiveCheck: never = props.value
        throw new Error(`Unhandled background layer type ${props.value}`)
      }
    }
  })()

  return (
    <InspectorModal
      offsetX={props.offsetX - colorPickerWidth}
      offsetY={props.offsetY}
      closePopup={props.closePopup}
      style={{
        zIndex: 1,
      }}
      closePopupOnUnmount={false}
      outsideClickIgnoreClass={`ignore-react-onclickoutside-${props.id}`}
    >
      <div
        id={props.id}
        className='colorPicker-wrapper'
        style={{
          width: colorPickerWidth,
          position: 'absolute',
          overflow: 'hidden',
          zIndex: 2,
          marginBottom: 32,
          ...UtopiaStyles.popup,
        }}
        onMouseDown={stopPropagation}
      >
        <UIRow
          padded={true}
          style={{
            borderBottom: `1px solid ${colorTheme.neutralBorder.value}`,
          }}
        >
          <div
            style={{
              flexGrow: 1,
            }}
          >
            <PopupList
              id='colorPicker-background-layer-type-selector'
              value={backgroundLayerOptionsByValue[props.value.type]}
              options={backgroundLayerTypeSelectOptions}
              onSubmitValue={onSubmitBackgroundLayerType}
              containerMode='noBorder'
            />
          </div>
          {isCSSBackgroundImageLayer(props.value) ? <Icons.Gear onClick={toggleSettings} /> : null}
          <FlexRow>
            <Icn
              type='pipette'
              color='secondary'
              width={18}
              height={18}
              onClick={dispatchEyeDropper}
              style={{ marginLeft: 8 }}
            />
            <Icn
              type='cross-large'
              color='secondary'
              width={16}
              height={16}
              onClick={props.closePopup}
              style={{ marginLeft: 8 }}
            />
          </FlexRow>
        </UIRow>
        {isCSSImageURLBackgroundLayer(props.value) ? (
          <PickerImagePreview value={props.value} />
        ) : (
          <>
            {isCSSGradientBackgroundLayer(props.value) ? (
              <GradientStopsEditor
                stops={stops}
                onSubmitValueAndUpdateLocalStops={onSubmitValueAndUpdateLocalStops}
                selectedStopUnorderedIndex={selectedStopUnorderedIndex}
                setSelectedStopUnorderedIndex={setSelectedStopUnorderedIndex}
                useSubmitValueFactory={props.useSubmitValueFactory}
              />
            ) : null}
            {isCSSSolidBackgroundLayer(props.value) || isCSSGradientBackgroundLayer(props.value) ? (
              isCSSSolidBackgroundLayer(props.value) ? (
                <ColorPickerInner
                  value={props.value.color}
                  onSubmitValue={onSubmitColorValue}
                  onTransientSubmitValue={onTransientSubmitColorValue}
                  offsetX={props.offsetX}
                  offsetY={props.offsetY}
                  id={props.id}
                  testId={props.testId}
                />
              ) : (
                <ColorPickerInner
                  value={stops[selectedStopUnorderedIndex]?.color ?? { ...defaultCSSColor }}
                  onSubmitValue={onSubmitValueAndUpdateLocalColor}
                  onTransientSubmitValue={onTransientSubmitValueAndUpdateLocalColor}
                  offsetX={props.offsetX}
                  offsetY={props.offsetY}
                  id={props.id}
                  testId={props.testId}
                />
              )
            ) : null}
          </>
        )}
        {showSettings ? MetadataControls : null}
        {isCSSImageURLBackgroundLayer(props.value) ? (
          <URLBackgroundLayerMetadataControls
            value={props.value}
            useSubmitValueFactory={props.useSubmitValueFactory}
            controlStatus={props.controlStatus}
            index={props.backgroundLayerIndex}
          />
        ) : null}
        {showSettings && isCSSBackgroundLayerWithBGSize(props.value) ? (
          <BGSizeMetadataControl
            value={props.value}
            useSubmitValueFactory={props.useSubmitValueFactory}
            controlStatus={props.controlStatus}
            index={props.backgroundLayerIndex}
          />
        ) : null}
      </div>
    </InspectorModal>
  )
}
