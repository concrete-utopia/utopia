/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { mapArrayToDictionary } from '../../../../../core/shared/array-utils'
import { foldEither } from '../../../../../core/shared/either'
import { InspectorContextMenuItems } from '../../../../../uuiui-deps'
import { SubduedBorderRadiusControl } from '../../../../canvas/controls/select-mode/subdued-border-radius-control'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { CSSNumber } from '../../../common/css-utils'
import {
  useInspectorContext,
  useInspectorLayoutInfo,
  useInspectorStyleInfo,
} from '../../../common/property-path-hooks'
import { PropertyLabel } from '../../../widgets/property-label'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import {
  longhandShorthandEventHandler,
  SplitChainedNumberInput,
} from '../../layout-section/layout-system-subsection/split-chained-number-input'

export const RadiusRow = React.memo(() => {
  const { value: borderRadiusValue, onUnsetValues } = useInspectorStyleInfo('borderRadius')
  const contextMenuItems = InspectorContextMenuItems.optionalAddOnUnsetValues(
    borderRadiusValue != null,
    ['borderRadius'],
    onUnsetValues,
  )
  const contextMenuLabel = React.useMemo(() => ['border radius'], [])
  return (
    <InspectorContextMenuWrapper
      id='borderRadius-subsection-context-menu'
      items={contextMenuItems}
      data={null}
    >
      <UIGridRow tall padded={true} variant='<---1fr--->|------172px-------|'>
        <PropertyLabel
          target={[]}
          propNamesToUnset={contextMenuLabel}
          style={{
            paddingBottom: 20,
          }}
        >
          Radius
        </PropertyLabel>
        <BorderRadiusControl />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

export const BorderRadiusControl = React.memo(() => {
  const borderRadius = useInspectorStyleInfo('borderRadius')

  const shorthand = useInspectorLayoutInfo('borderRadius')

  const dispatch = useDispatch()

  const { selectedViewsRef } = useInspectorContext()

  const tl: CSSNumber = React.useMemo(
    () =>
      foldEither(
        (n) => n,
        (i) => i.tl,
        borderRadius.value,
      ),
    [borderRadius],
  )
  const tr: CSSNumber = React.useMemo(
    () =>
      foldEither(
        (n) => n,
        (i) => i.tr,
        borderRadius.value,
      ),
    [borderRadius],
  )
  const bl: CSSNumber = React.useMemo(
    () =>
      foldEither(
        (n) => n,
        (i) => i.bl,
        borderRadius.value,
      ),
    [borderRadius],
  )
  const br: CSSNumber = React.useMemo(
    () =>
      foldEither(
        (n) => n,
        (i) => i.br,
        borderRadius.value,
      ),
    [borderRadius],
  )

  const canvasControlsForSides = React.useMemo(() => {
    return mapArrayToDictionary(
      ['top'],
      (k) => k,
      (side) => ({
        onHover: {
          control: SubduedBorderRadiusControl,
          props: {
            hoveredOrFocused: 'hovered',
          },
          key: `subdued-border-radius-control-hovered`,
        },
        onFocus: {
          control: SubduedBorderRadiusControl,
          props: {
            hoveredOrFocused: 'focused',
          },
          key: `subdued-border-radius-control-focused`,
        },
      }),
    )
  }, [])

  return (
    <SplitChainedNumberInput
      labels={{
        top: 'TL',
        bottom: 'BR',
        left: 'BL',
        right: 'TR',
      }}
      tooltips={{
        oneValue: 'Radius',
        perSide: 'Radius per corner',
      }}
      controlModeOrder={['one-value', 'per-side']}
      numberType={'LengthPercent'}
      selectedViews={selectedViewsRef.current}
      name='radius'
      defaultMode='one-value'
      top={{
        ...borderRadius,
        value: tl,
      }}
      left={{
        ...borderRadius,
        value: bl,
      }}
      bottom={{
        ...borderRadius,
        value: br,
      }}
      right={{
        ...borderRadius,
        value: tr,
      }}
      shorthand={shorthand}
      canvasControls={canvasControlsForSides}
      eventHandler={longhandShorthandEventHandler(
        'borderRadius',
        {
          T: 'borderTopLeftRadius',
          R: 'borderTopRightRadius',
          B: 'borderBottomRightRadius',
          L: 'borderBottomLeftRadius',
        },
        selectedViewsRef.current[0],
        dispatch,
      )}
    />
  )
})
