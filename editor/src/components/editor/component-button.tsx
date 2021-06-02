/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import { BreadcrumbTrail } from '../canvas/controls/breadcrumb-trail'
import {
  FlexRow,
  Icn,
  IcnColor,
  Icons,
  OnClickOutsideHOC,
  SmallerIcons,
  UtopiaStyles,
  UtopiaTheme,
} from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'
import { RenderAsRow } from '../canvas/controls/render-as'
import { useEditorState } from './store/store-hook'
import * as EP from '../../core/shared/element-path'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { setFocusedElement } from './actions/action-creators'

export const ComponentOrInstanceIndicator = betterReactMemo('ComponentOrInstanceIndicator', () => {
  const { metadata, focusedElementPath, selectedViews } = useEditorState((store) => {
    return {
      metadata: store.editor.jsxMetadata,
      focusedElementPath: store.editor.focusedElementPath,
      selectedViews: store.editor.selectedViews,
    }
  }, 'Component-button')

  const dispatch = useEditorState((state) => state.dispatch, 'ComponentOrInstanceIndicator')

  const popupEnabled = selectedViews.length > 0

  const [isOpen, setIsOpen] = React.useState(false)
  const toggleOpen = React.useCallback(() => {
    setIsOpen(!isOpen)
  }, [isOpen])

  const closeAndEatEvent = React.useCallback(
    (e: MouseEvent) => {
      setIsOpen(false)
      e.stopPropagation()
      e.preventDefault()
    },
    [setIsOpen],
  )

  const target = selectedViews[0]

  const isFocused = target == null ? false : EP.isFocused(focusedElementPath, target)

  const toggleFocusMode = React.useCallback(() => {
    dispatch([setFocusedElement(isFocused() ? null : target)])
  }, [dispatch, isFocused, target])

  const isComponent = React.useCallback(() => {
    return MetadataUtils.isFocusableComponent(target, metadata)
  }, [target, metadata])

  const getEditContextStyle = (): React.CSSProperties => {
    if (target != null) {
      if (MetadataUtils.isFocusableComponent(target, metadata) && isFocused() == false) {
        return {
          color: UtopiaTheme.color.component.value,
          backgroundColor: UtopiaTheme.color.component.shade(10).value,
          stroke: UtopiaTheme.color.component.fileNameFragment,
        }
      } else if (isFocused() && MetadataUtils.isFocusableComponent(target, metadata)) {
        return {
          color: UtopiaTheme.color.componentChild.value,
          backgroundColor: UtopiaTheme.color.componentChild.shade(10).value,
          stroke: UtopiaTheme.color.componentChild.fileNameFragment,
        }
      } else {
        return {
          background: UtopiaTheme.color.secondaryBackground.value,
          color: UtopiaTheme.color.neutralForeground.value,
          stroke: 'black',
          opacity: 0.5,
          pointerEvents: 'none',
        }
      }
    } else {
      return {
        background: UtopiaTheme.color.secondaryBackground.value,
        color: UtopiaTheme.color.neutralForeground.value,
        stroke: 'black',
        opacity: 0.5,
        pointerEvents: 'none',
      }
    }
  }

  return (
    <div
      role='compositeButton'
      style={{
        position: 'relative',
        display: 'flex',
        alignItems: 'stretch',
        height: UtopiaTheme.layout.inputHeight.default,
        flexBasis: 38,
      }}
    >
      <FlexRow
        role='button'
        // TODO replace me with the right function call from above
        onClick={toggleFocusMode}
        css={{
          flexGrow: 1,
          flexShrink: 1,
          overflow: 'hidden',
          borderTopLeftRadius: UtopiaTheme.inputBorderRadius,
          borderBottomLeftRadius: UtopiaTheme.inputBorderRadius,
          gap: 8,
          paddingLeft: 4,
          cursor: 'pointer',
          transition: 'background-color .1s ease-in-out',
          ...getEditContextStyle(),
          '&:hover': {
            filter: 'brightness(1.02  )',
          },
          '&:active': {
            filter: 'brightness(1.03)',
          },
        }}
      >
        {/* TODO replace me with the real icon */}
        {isComponent() ? (
          <Icons.Component color={getEditContextStyle().stroke as IcnColor} />
        ) : (
          <Icn
            category='element'
            type='ghost'
            width={18}
            height={18}
            color={getEditContextStyle().stroke as IcnColor}
          />
        )}
      </FlexRow>

      <div
        className='ignore-react-onclickoutside'
        role='expansionButton'
        css={{
          pointerEvents: popupEnabled ? 'initial' : 'none',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          flexGrow: 0,
          flexShrink: 0,
          flexBasis: 14,
          transition: 'background-color .1s ease-in-out',
          ...getEditContextStyle(),
          // slightly darker than the button next to it
          filter: 'brightness(.99)',
          borderLeft: `1px dashed ${UtopiaTheme.color.secondaryBorder.value}`,
          cursor: 'pointer',
          borderTopRightRadius: UtopiaTheme.inputBorderRadius,
          borderBottomRightRadius: UtopiaTheme.inputBorderRadius,
          '&:hover': {
            filter: 'brightness(1.01)',
          },
          '&:active > *': {
            transform: 'translateY(1px)',
          },
          '&:active': {
            filter: 'brightness(1.03)',
          },
        }}
        onClick={toggleOpen}
      >
        <SmallerIcons.ExpansionArrowDown
          isDisabled={!popupEnabled}
          color={getEditContextStyle().stroke as IcnColor}
          style={{
            flexGrow: 0,
            flexShrink: 0,
            transform: isOpen ? 'rotate(180deg)' : undefined,
          }}
        />
      </div>

      {isOpen ? (
        <OnClickOutsideHOC onClickOutside={closeAndEatEvent}>
          <div
            tabIndex={0}
            style={{
              position: 'absolute',
              left: 0,
              top: 30,
              zIndex: 1,
              width: UtopiaTheme.layout.inspectorWidth,
              height: 100,
              ...UtopiaStyles.popup,
              display: 'flex',
              flexDirection: 'column',
              alignContent: 'flex-start',
            }}
            onClick={(e) => e.stopPropagation()}
            onMouseDown={(e) => e.stopPropagation()}
            onMouseUp={(e) => e.stopPropagation()}
          >
            <BreadcrumbTrail />
            <RenderAsRow />
          </div>
        </OnClickOutsideHOC>
      ) : null}
    </div>
  )
})
