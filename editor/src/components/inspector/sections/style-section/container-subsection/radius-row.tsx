/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { foldEither, isRight, right } from '../../../../../core/shared/either'
import { InspectorContextMenuItems } from '../../../../../uuiui-deps'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import {
  CSSBorderRadius,
  CSSBorderRadiusIndividual,
  CSSNumber,
  cssNumber,
  defaultBorderRadiusIndividual,
  EmptyInputValue,
  fallbackOnEmptyInputValueToCSSEmptyValue,
} from '../../../common/css-utils'
import {
  useInspectorContext,
  useInspectorLayoutInfo,
  useInspectorStyleInfo,
} from '../../../common/property-path-hooks'
import { PropertyLabel } from '../../../widgets/property-label'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import {
  Sides,
  SplitChainedNumberInput,
} from '../../layout-section/layout-system-subsection/split-chained-number-input'

function updateBorderRadiusTL(
  newBorderRadiusTLValue: CSSNumber | EmptyInputValue,
  borderRadius: CSSBorderRadius,
): CSSBorderRadius {
  const safeNewValue = fallbackOnEmptyInputValueToCSSEmptyValue(
    cssNumber(0),
    newBorderRadiusTLValue,
  )
  if (isRight(borderRadius)) {
    const newBorderRadius: CSSBorderRadiusIndividual = {
      ...borderRadius.value,
      tl: safeNewValue,
    }
    return right(newBorderRadius)
  } else {
    return right({ ...defaultBorderRadiusIndividual, tl: safeNewValue })
  }
}

function updateBorderRadiusTR(
  newBorderRadiusTRValue: CSSNumber | EmptyInputValue,
  borderRadius: CSSBorderRadius,
): CSSBorderRadius {
  const safeNewValue = fallbackOnEmptyInputValueToCSSEmptyValue(
    cssNumber(0),
    newBorderRadiusTRValue,
  )
  if (isRight(borderRadius)) {
    const newBorderRadius: CSSBorderRadiusIndividual = {
      ...borderRadius.value,
      tr: safeNewValue,
    }
    return right(newBorderRadius)
  } else {
    return right({ ...defaultBorderRadiusIndividual, tr: safeNewValue })
  }
}

function updateBorderRadiusBL(
  newBorderRadiusBLValue: CSSNumber | EmptyInputValue,
  borderRadius: CSSBorderRadius,
): CSSBorderRadius {
  const safeNewValue = fallbackOnEmptyInputValueToCSSEmptyValue(
    cssNumber(0),
    newBorderRadiusBLValue,
  )
  if (isRight(borderRadius)) {
    const newBorderRadius: CSSBorderRadiusIndividual = {
      ...borderRadius.value,
      bl: safeNewValue,
    }
    return right(newBorderRadius)
  } else {
    return right({ ...defaultBorderRadiusIndividual, bl: safeNewValue })
  }
}

function updateBorderRadiusBR(
  newBorderRadiusBRValue: CSSNumber | EmptyInputValue,
  borderRadius: CSSBorderRadius,
): CSSBorderRadius {
  const safeNewValue = fallbackOnEmptyInputValueToCSSEmptyValue(
    cssNumber(0),
    newBorderRadiusBRValue,
  )
  if (isRight(borderRadius)) {
    const newBorderRadius: CSSBorderRadiusIndividual = {
      ...borderRadius.value,
      br: safeNewValue,
    }
    return right(newBorderRadius)
  } else {
    return right({ ...defaultBorderRadiusIndividual, br: safeNewValue })
  }
}

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

  const updateShorthand = React.useCallback(
    (sides: Sides, transient?: boolean) => {
      const { top: tl, bottom: br, left: bl, right: tr } = sides
      shorthand.onSubmitValue(
        {
          type: 'RIGHT',
          value: {
            tl: tl,
            tr: tr,
            br: br,
            bl: bl,
          },
        },
        transient,
      )
    },
    [shorthand],
  )

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

  const onSubmitValueTL = React.useCallback(() => {
    const [update] = borderRadius.useSubmitValueFactory((newValue: CSSNumber) =>
      updateBorderRadiusTL(newValue, borderRadius.value),
    )
    return update
  }, [borderRadius])
  const onTransientSubmitValueTL = React.useCallback(() => {
    const [, update] = borderRadius.useSubmitValueFactory((newValue: CSSNumber) =>
      updateBorderRadiusTL(newValue, borderRadius.value),
    )
    return update
  }, [borderRadius])

  const onSubmitValueTR = React.useCallback(() => {
    const [update] = borderRadius.useSubmitValueFactory((newValue: CSSNumber) =>
      updateBorderRadiusTR(newValue, borderRadius.value),
    )
    return update
  }, [borderRadius])
  const onTransientSubmitValueTR = React.useCallback(() => {
    const [, update] = borderRadius.useSubmitValueFactory((newValue: CSSNumber) =>
      updateBorderRadiusTR(newValue, borderRadius.value),
    )
    return update
  }, [borderRadius])

  const onSubmitValueBL = React.useCallback(() => {
    const [update] = borderRadius.useSubmitValueFactory((newValue: CSSNumber) =>
      updateBorderRadiusBL(newValue, borderRadius.value),
    )
    return update
  }, [borderRadius])
  const onTransientSubmitValueBL = React.useCallback(() => {
    const [, update] = borderRadius.useSubmitValueFactory((newValue: CSSNumber) =>
      updateBorderRadiusBL(newValue, borderRadius.value),
    )
    return update
  }, [borderRadius])

  const onSubmitValueBR = React.useCallback(() => {
    const [update] = borderRadius.useSubmitValueFactory((newValue: CSSNumber) =>
      updateBorderRadiusBR(newValue, borderRadius.value),
    )
    return update
  }, [borderRadius])
  const onTransientSubmitValueBR = React.useCallback(() => {
    const [, update] = borderRadius.useSubmitValueFactory((newValue: CSSNumber) =>
      updateBorderRadiusBR(newValue, borderRadius.value),
    )
    return update
  }, [borderRadius])

  return (
    <SplitChainedNumberInput
      labels={{
        top: 'TL',
        bottom: 'BR',
        left: 'BL',
        right: 'TR',
      }}
      controlModeOrder={['one-value', 'per-side']}
      selectedViews={selectedViewsRef.current}
      name='radius'
      defaultMode='one-value'
      top={{
        ...borderRadius,
        value: tl,
        onSubmitValue: onSubmitValueTL(),
        onTransientSubmitValue: onTransientSubmitValueTL(),
      }}
      left={{
        ...borderRadius,
        value: bl,
        onSubmitValue: onSubmitValueBL(),
        onTransientSubmitValue: onTransientSubmitValueBL(),
      }}
      bottom={{
        ...borderRadius,
        value: br,
        onSubmitValue: onSubmitValueBR(),
        onTransientSubmitValue: onTransientSubmitValueBR(),
      }}
      right={{
        ...borderRadius,
        value: tr,
        onSubmitValue: onSubmitValueTR(),
        onTransientSubmitValue: onTransientSubmitValueTR(),
      }}
      shorthand={shorthand}
      updateShorthand={updateShorthand}
    />
  )
})
