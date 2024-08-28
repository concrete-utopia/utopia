/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { usePopper } from 'react-popper'
import { identity } from '../../../../../core/shared/utils'
import utils from '../../../../../utils/utils'
import { FlexRow, UtopiaTheme, colorTheme, SmallerIcons } from '../../../../../uuiui'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import { stylePropPathMappingFn, useInspectorInfo } from '../../../common/property-path-hooks'
import { FontFamilySelectPopup } from './font-family-select-popup'

export const FontFamilySelect = React.memo(() => {
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

  const { value, useSubmitValueFactory, onUnsetValues, controlStyles } = useInspectorInfo(
    ['fontFamily', 'fontStyle', 'fontWeight'],
    identity,
    identity,
    stylePropPathMappingFn,
  )

  const fontFamilyContextMenuItems = utils.stripNulls([
    controlStyles.unsettable ? addOnUnsetValues(['fontFamily'], onUnsetValues) : null,
  ])

  return (
    <FlexRow
      style={{
        padding: '0 8px 0 4px',
        minHeight: UtopiaTheme.layout.rowHeight.normal,
        alignItems: 'center',
      }}
    >
      <InspectorContextMenuWrapper
        id='fontFamily-context-menu'
        items={fontFamilyContextMenuItems}
        data={null}
      >
        {popupIsOpen ? (
          <FontFamilySelectPopup
            {...attributes.popper}
            style={styles.popper}
            ref={setPopperElement}
            value={value}
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
            color: controlStyles.mainColor,
            padding: '0 8px',
            height: 22,
            borderRadius: UtopiaTheme.inputBorderRadius,
            gap: 5,
          }}
          css={{
            '&:hover': {
              boxShadow: `inset 0px 0px 0px 1px ${colorTheme.fg7.value}`,
              justifyContent: 'space-between',
            },
            '&:focus-within': {
              boxShadow: `inset 0px 0px 0px 1px ${colorTheme.dynamicBlue.value}`,
              justifyContent: 'space-between',
            },
          }}
        >
          <div>{value.fontFamily[0]}</div>
          <SmallerIcons.ExpansionArrowDown />
        </FlexRow>
      </InspectorContextMenuWrapper>
    </FlexRow>
  )
})
