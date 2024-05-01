import React from 'react'
import { animated } from 'react-spring'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { useArraySuperControl } from '../../../controls/array-supercontrol'
import { FakeUnknownArrayItem, UnknownArrayItem } from '../../../controls/unknown-array-item'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import type {
  CSSBackgroundLayer,
  CSSBackgroundLayers,
  CSSBackgrounds,
  CSSBackgroundSize,
  CSSBGSize,
  CSSSolidColor,
  CSSDefault,
} from '../../../common/css-utils'
import {
  cssBackgroundLayerToCSSBackground,
  cssBackgroundToCSSBackgroundLayer,
  cssSolidBackgroundLayer,
  cssSolidColor,
  defaultBGSize,
  defaultLinearGradientBackgroundLayer,
  defaultSolidBackgroundLayer,
  isCSSBackgroundLayerWithBGSize,
  isCSSSolidBackgroundLayer,
  cssDefault,
} from '../../../common/css-utils'
import { useGetSubsectionHeaderStyle } from '../../../common/inspector-utils'
import {
  InspectorPropsContext,
  stylePropPathMappingFn,
  useInspectorContext,
  useInspectorInfo,
  useIsSubSectionVisible,
} from '../../../common/property-path-hooks'
import { ConicGradientBackgroundLayer } from './conic-gradient-layer'
import { LinearGradientBackgroundLayer } from './linear-gradient-layer'
import { RadialGradientBackgroundLayer } from './radial-gradient-layer'
import { SolidBackgroundLayer } from './solid-background-layer'
import { URLBackgroundLayer } from './url-background-layer'
import {
  UtopiaTheme,
  InspectorSubsectionHeader,
  FlexRow,
  SquareButton,
  Icn,
  Icons,
} from '../../../../../uuiui'
import { useContextSelector } from 'use-context-selector'
import { BackgroundPrefixedProperties } from '../../../../../core/properties/css-properties'
import * as PP from '../../../../../core/shared/property-path'

function insertBackgroundLayer(
  cssBackgroundLayers: CSSBackgroundLayers,
  wasUnset: boolean,
  onSubmitValue: (newValue: CSSBackgroundLayers) => void,
): void {
  if (cssBackgroundLayers.length === 0 || wasUnset) {
    onSubmitValue([{ ...defaultSolidBackgroundLayer }])
  } else {
    onSubmitValue([...cssBackgroundLayers, { ...defaultLinearGradientBackgroundLayer }])
  }
}

function cssBackgroundLayerToCSSBGSizeOrDefault(v: CSSBackgroundLayer): CSSBGSize {
  if (isCSSBackgroundLayerWithBGSize(v)) {
    return { ...v.bgSize, enabled: v.enabled }
  } else {
    return { ...defaultBGSize }
  }
}

function getBackgroundSizeOrUndefinedIfDefault(
  layers: CSSBackgroundLayers,
): { backgroundSize: CSSBackgroundSize } | undefined {
  const shouldItBePrinted = !layers.every(
    (layer) => isCSSBackgroundLayerWithBGSize(layer) && layer.bgSize.size.default,
  )
  return shouldItBePrinted
    ? { backgroundSize: layers.map(cssBackgroundLayerToCSSBGSizeOrDefault).reverse() }
    : undefined
}

export function cssBackgroundLayerArrayToBackgroundImagesAndColor(
  cssBackgroundLayers: CSSBackgroundLayers,
): {
  backgroundColor?: CSSDefault<CSSSolidColor>
  backgroundImage?: CSSBackgrounds
  backgroundSize?: CSSBackgroundSize
} {
  switch (cssBackgroundLayers.length) {
    case 0: {
      return {}
    }
    case 1: {
      const zerothBackgroundLayer = cssBackgroundLayers[0]
      if (zerothBackgroundLayer == null) {
        return {}
      } else {
        if (isCSSSolidBackgroundLayer(zerothBackgroundLayer)) {
          return {
            backgroundColor: cssDefault(
              cssSolidColor(zerothBackgroundLayer.color, zerothBackgroundLayer.enabled),
              false,
            ),
          }
        } else {
          return {
            backgroundImage: cssBackgroundLayers.map(cssBackgroundLayerToCSSBackground).reverse(),
            ...getBackgroundSizeOrUndefinedIfDefault(cssBackgroundLayers),
          }
        }
      }
    }
    default: {
      const zerothBackgroundLayer = cssBackgroundLayers[0]
      if (zerothBackgroundLayer == null) {
        return {}
      } else {
        if (isCSSSolidBackgroundLayer(zerothBackgroundLayer)) {
          const newCSSBackgroundLayers = cssBackgroundLayers.slice(1)
          return {
            backgroundColor: cssDefault(
              cssSolidColor(zerothBackgroundLayer.color, zerothBackgroundLayer.enabled),
              false,
            ),
            backgroundImage: newCSSBackgroundLayers
              .map(cssBackgroundLayerToCSSBackground)
              .reverse(),
            ...getBackgroundSizeOrUndefinedIfDefault(newCSSBackgroundLayers),
          }
        } else {
          return {
            backgroundImage: cssBackgroundLayers.map(cssBackgroundLayerToCSSBackground).reverse(),
            ...getBackgroundSizeOrUndefinedIfDefault(cssBackgroundLayers),
          }
        }
      }
    }
  }
}

export function backgroundImagesAndColorToCSSBackgroundLayerArray(values: {
  backgroundColor: CSSDefault<CSSSolidColor>
  backgroundImage: CSSBackgrounds
  backgroundSize: CSSBackgroundSize
}): CSSBackgroundLayers {
  const backgroundLayers = values.backgroundImage
    .map((bgImage, i) => {
      const bgSize = values.backgroundSize[i] ?? { ...defaultBGSize }
      return cssBackgroundToCSSBackgroundLayer(bgImage, bgSize)
    })
    .reverse()
  if (values.backgroundColor.default) {
    return backgroundLayers
  } else {
    return [cssSolidBackgroundLayer(values.backgroundColor.value), ...backgroundLayers]
  }
}

export const backgroundLonghandPaths: Array<
  'backgroundColor' | 'backgroundImage' | 'backgroundSize'
> = ['backgroundColor', 'backgroundImage', 'backgroundSize']

const rowHeight = UtopiaTheme.layout.rowHeight.max

export const BackgroundSubsection = React.memo(() => {
  const [openPopup, setOpenPopup] = React.useState<number | undefined>(undefined)
  const {
    controlStatus,
    controlStyles,
    propertyStatus,
    value,
    onSubmitValue,
    useSubmitValueFactory,
  } = useInspectorInfo(
    backgroundLonghandPaths,
    backgroundImagesAndColorToCSSBackgroundLayerArray,
    cssBackgroundLayerArrayToBackgroundImagesAndColor,
    stylePropPathMappingFn,
  )

  const isVisible = useIsSubSectionVisible('background')

  const headerStyle = useGetSubsectionHeaderStyle(controlStatus)

  const { springs, bind } = useArraySuperControl(value, onSubmitValue, rowHeight, true)

  const { onContextUnsetValue } = useInspectorContext()

  const targetPath = useContextSelector(InspectorPropsContext, (context) => context.targetPath)

  const onUnsetSubsectionValues = React.useCallback(() => {
    onContextUnsetValue(
      BackgroundPrefixedProperties.map((prop) => {
        return PP.createFromArray([...targetPath, prop])
      }),
      false,
    )
  }, [onContextUnsetValue, targetPath])

  const unsetContextMenuItem = React.useMemo(() => {
    return [addOnUnsetValues(['all background properties'], onUnsetSubsectionValues)]
  }, [onUnsetSubsectionValues])

  const backgroundLayerArrayForDisplay = value.map((backgroundLayer, index) => {
    switch (backgroundLayer.type) {
      case 'linear-gradient-background-layer': {
        return (
          <LinearGradientBackgroundLayer
            index={index}
            value={backgroundLayer}
            controlStatus={controlStatus}
            controlStyles={controlStyles}
            useSubmitTransformedValuesFactory={useSubmitValueFactory}
            popupOpen={openPopup === index}
            setOpenPopup={setOpenPopup}
            unsetContextMenuItem={[]}
          />
        )
      }
      case 'radial-gradient-background-layer': {
        return (
          <RadialGradientBackgroundLayer
            key={index}
            index={index}
            value={backgroundLayer}
            controlStatus={controlStatus}
            controlStyles={controlStyles}
            useSubmitTransformedValuesFactory={useSubmitValueFactory}
            popupOpen={openPopup === index}
            setOpenPopup={setOpenPopup}
            unsetContextMenuItem={[]}
          />
        )
      }
      case 'conic-gradient-background-layer': {
        return (
          <ConicGradientBackgroundLayer
            key={index}
            index={index}
            value={backgroundLayer}
            controlStatus={controlStatus}
            controlStyles={controlStyles}
            useSubmitTransformedValuesFactory={useSubmitValueFactory}
            popupOpen={openPopup === index}
            setOpenPopup={setOpenPopup}
            unsetContextMenuItem={[]}
          />
        )
      }
      case 'solid-background-layer': {
        return (
          <SolidBackgroundLayer
            key={index}
            index={index}
            value={backgroundLayer}
            useSubmitTransformedValuesFactory={useSubmitValueFactory}
            controlStatus={controlStatus}
            controlStyles={controlStyles}
            popupOpen={openPopup === index}
            setOpenPopup={setOpenPopup}
            unsetContextMenuItem={[]}
          />
        )
      }
      case 'url-function-background-layer': {
        return (
          <URLBackgroundLayer
            key={index}
            index={index}
            value={backgroundLayer}
            useSubmitTransformedValuesFactory={useSubmitValueFactory}
            controlStatus={controlStatus}
            controlStyles={controlStyles}
            popupOpen={openPopup === index}
            setOpenPopup={setOpenPopup}
            unsetContextMenuItem={[]}
          />
        )
      }
      case 'unknown-array-item': {
        return (
          <UnknownArrayItem
            index={index}
            key={`unknown-${index}`}
            value={backgroundLayer}
            controlStatus={controlStatus}
            controlStyles={controlStyles}
            useSubmitTransformedValuesFactory={useSubmitValueFactory}
          />
        )
      }
      default: {
        const _exhaustiveCheck: never = backgroundLayer
        throw new Error(`Unhandled background layer type ${JSON.stringify(backgroundLayer)}`)
      }
    }
  })

  const insertBackgroundLayerMouseDown = React.useCallback(() => {
    insertBackgroundLayer([...value], controlStatus === 'unset', onSubmitValue)
  }, [onSubmitValue, value, controlStatus])

  if (!isVisible) {
    return null
  }

  const rowsToRender =
    controlStatus === 'unset' ? null : (
      <div
        style={{
          height: rowHeight * springs.length,
        }}
      >
        {springs.map((springStyle, index) => {
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
              {backgroundLayerArrayForDisplay[index]}
            </animated.div>
          )
        })}
      </div>
    )

  return (
    <InspectorContextMenuWrapper
      id='background-layer-subsection-context-menu'
      items={unsetContextMenuItem}
      data={null}
    >
      <InspectorSubsectionHeader style={headerStyle}>
        <FlexRow style={{ flexGrow: 1, gap: 8 }}>
          <span>Background</span>
        </FlexRow>
        {propertyStatus.overwritable ? (
          <FlexRow style={{ gap: 4 }}>
            <SquareButton
              highlight
              onMouseDown={onUnsetSubsectionValues}
              data-testid={'inspector-background-remove-all'}
              style={{ width: 12 }}
            >
              <Icn category='semantic' type='cross' width={12} height={12} />
            </SquareButton>
            <SquareButton
              highlight
              onMouseDown={insertBackgroundLayerMouseDown}
              style={{ width: 12 }}
            >
              <Icn category='semantic' type='plus' width={12} height={12} />
            </SquareButton>
          </FlexRow>
        ) : null}
      </InspectorSubsectionHeader>
      {controlStyles.unknown ? (
        <FakeUnknownArrayItem controlStatus={controlStatus} />
      ) : (
        rowsToRender
      )}
    </InspectorContextMenuWrapper>
  )
})
BackgroundSubsection.displayName = 'BackgroundSubsection'
