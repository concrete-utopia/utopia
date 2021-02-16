import * as React from 'react'
import { usePopper } from 'react-popper'
import { identity } from '../../../../../core/shared/utils'
import utils from '../../../../../utils/utils'
import { FlexRow, UtopiaTheme, Icons, fontSize, fontWeight } from '../../../../../uuiui'
import { betterReactMemo } from '../../../../../uuiui-deps'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import { emptyValues } from '../../../common/css-utils'
import {
  ParsedValues,
  stylePropPathMappingFn,
  useInspectorInfoNoDefaults,
} from '../../../common/property-path-hooks'
import { PropertyRow } from '../../../widgets/property-row'
import { FontFamilySelectPopup } from './font-family-select-popup'

export function transformFontFamily(
  values: Partial<ParsedValues<'fontFamily' | 'fontStyle' | 'fontWeight'>> = {},
): Partial<ParsedValues<'fontFamily' | 'fontStyle' | 'fontWeight'>> {
  return values
}

export const FontFamilySelect = betterReactMemo('FontFamilySelect', () => {
  const [referenceElement, setReferenceElement] = React.useState<HTMLDivElement | null>(null)
  const [popperElement, setPopperElement] = React.useState<HTMLDivElement | null>(null)
  const { styles, attributes } = usePopper(referenceElement, popperElement, {
    modifiers: [
      {
        name: 'offset',
        options: {
          offset: [0, 8],
        },
      },
    ],
  })

  const [popupIsOpen, setPopupIsOpen] = React.useState(false)
  const togglePopup = React.useCallback(() => setPopupIsOpen((v) => !v), [])
  const closePopup = React.useCallback(() => setPopupIsOpen(false), [])

  const onMouseDown = React.useCallback(
    (e: React.MouseEvent) => {
      togglePopup()
      e.stopPropagation()
    },
    [togglePopup],
  )

  const { value, useSubmitValueFactory, onUnsetValues, controlStyles } = useInspectorInfoNoDefaults(
    ['fontFamily', 'fontStyle', 'fontWeight'],
    transformFontFamily,
    identity,
    stylePropPathMappingFn,
  )

  const fontFamilyContextMenuItems = utils.stripNulls([
    controlStyles.unsettable ? addOnUnsetValues(['fontFamily'], onUnsetValues) : null,
  ])

  const fontFamilyValue: ParsedValues<'fontFamily' | 'fontStyle' | 'fontWeight'> = {
    fontFamily: value?.fontFamily ?? emptyValues['fontFamily'],
    fontStyle: value?.fontStyle ?? emptyValues['fontStyle'],
    fontWeight: value?.fontWeight ?? emptyValues['fontWeight'],
  }

  return (
    <PropertyRow>
      <InspectorContextMenuWrapper
        id='fontFamily-context-menu'
        items={fontFamilyContextMenuItems}
        data={null}
        style={{ marginBottom: 8, gridColumn: '1 / span 6' }}
      >
        {popupIsOpen ? (
          <FontFamilySelectPopup
            {...attributes.popper}
            style={styles.popper}
            ref={setPopperElement}
            value={fontFamilyValue}
            onUnsetValues={onUnsetValues}
            controlStyles={controlStyles}
            closePopup={closePopup}
            useSubmitFontVariantFactory={useSubmitValueFactory}
          />
        ) : null}
        <FlexRow
          ref={setReferenceElement}
          onMouseDown={onMouseDown}
          style={{
            boxShadow: `0 0 0 1px ${controlStyles.borderColor} inset`,
            padding: 4,
            fontSize: 14,
            height: 30,
            borderRadius: UtopiaTheme.inputBorderRadius,
          }}
        >
          <div style={{ flexGrow: 1 }}>{fontFamilyValue.fontFamily[0]}</div>
          <Icons.ExpansionArrowDown />
        </FlexRow>
      </InspectorContextMenuWrapper>
    </PropertyRow>
  )
})
