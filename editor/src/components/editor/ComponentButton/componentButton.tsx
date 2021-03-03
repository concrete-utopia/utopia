import * as React from 'react'
import BreadcrumbSeparator from 'antd/lib/breadcrumb/BreadcrumbSeparator'
import onClickOutside from 'react-onclickoutside'
import { BreadcrumbTrail } from '../../canvas/controls/breadcrumb-trail'
import { colorTheme, Icons, OnClickOutsideHOC, UtopiaTheme } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import {
  NameRow,
  NameRowInnerProps,
} from '../../../components/inspector/sections/header-section/name-row'
import {
  LayoutWrapperRow,
  LayoutWrapperCoreProps,
  LayoutWrapperRowProps,
} from '../../../components/inspector/sections/header-section/layout-wrapper-section'
import { JSXElementName } from '../../../core/shared/element-template'
import { NameRowCrumbs } from '../../../components/canvas/controls/namerowcrumbs'

interface ComponentOrInstanceIndicatorProps
  extends NameRowInnerProps,
  LayoutWrapperCoreProps,
  LayoutWrapperRowProps {
  gap: 8,
  label: string
  component: boolean
  instance: boolean
}

export const ComponentOrInstanceIndicator = betterReactMemo(
  'ComponentOrInstanceIndicator',
  (props: ComponentOrInstanceIndicatorProps) => {
    const [isOpen, setIsOpen] = React.useState(false)
    const toggleOpen = React.useCallback(() => {
      setIsOpen(!isOpen)
    }, [isOpen])

    const closePopup = React.useCallback(() => setIsOpen(false), [])

    return (
      <div
        id={'ComponentView'}
        onClick={toggleOpen}
        style={{
          position: 'relative',
          display: 'flex',
          gap: 8,
          maxWidth: 130,
          padding: '5px 8px',
          borderRadius: UtopiaTheme.inputBorderRadius,
          borderColor: UtopiaTheme.color.aboveNeutralBackground.value,
          // make the menu button slightly darker if the menu is open
          background: isOpen ? '#4842EE' : UtopiaTheme.color.brandPurple.value,
          cursor: 'pointer',
          boxShadow: `0px 0px 0px 1px ${props.component ? '#' : props.instance ? '#5852FE' : '#888'
            }`,
        }}
      >
        <Icons.Component color="white" />
        <span
          style={{
            whiteSpace: 'nowrap',
            overflow: 'hidden',
            textOverflow: 'Ellipsis',
            color: 'white', //props.component ? 'white' : props.instance ? '#5852FE' : 'inherit',
            paddingTop: 2
          }}
        >
          {props.label}
          ComponentName
        </span>
        {/* rotate the expansion arrow up when the menu is open */}
        <Icons.ExpansionArrow style={{ transform: isOpen ? 'rotate(180deg)' : undefined }} />
        {isOpen ? (
          <OnClickOutsideHOC onClickOutside={closePopup}>
            <div
              style={{
                position: 'absolute',
                left: 0,
                top: 30,
                borderRadius: 5,
                zIndex: 99999,
                width: 270,
                height: 100,
                border: '1px solid grey',
                background: UtopiaTheme.color.neutralBackground.value,
                boxShadow: 'inset 0px 0px 0px .5px lightgrey, 0px 2px 5px 0px lightgrey',
                display: 'flex',
                flexDirection: 'column',
                color: '#333',
                overflow: 'ellipsis',
                whiteSpace: 'nowrap',
                alignContent: 'flex-start',
                pointerEvents: 'none'
              }}
            >
              <BreadcrumbTrail />
              <NameRowCrumbs />
              <LayoutWrapperRow
                onWrap={props.onWrap}
                onUnwrap={props.onUnwrap}
                value={props.value}
              />
            </div>
          </OnClickOutsideHOC>
        ) : null}
      </div>
    )
  },
)
