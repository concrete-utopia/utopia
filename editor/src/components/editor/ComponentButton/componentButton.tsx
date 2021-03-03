/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import BreadcrumbSeparator from 'antd/lib/breadcrumb/BreadcrumbSeparator'
import onClickOutside from 'react-onclickoutside'
import { BreadcrumbTrail } from '../../canvas/controls/breadcrumb-trail'
import { colorTheme, FlexRow, Icons, OnClickOutsideHOC, UtopiaTheme } from '../../../uuiui'
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
  gap: 8
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

    const closeAndEatEvent = React.useCallback(
      (e: MouseEvent) => {
        setIsOpen(false)
        e.stopPropagation()
        e.preventDefault()
      },
      [setIsOpen],
    )

    return (
      <div
        role='compositeButton'
        style={{
          position: 'relative',
          display: 'flex',
          alignItems: 'stretch',
          height: 22,
          borderRadius: UtopiaTheme.inputBorderRadius,

          maxWidth: 130,
          // overflow: 'hidden',
          boxShadow: '0px 0px 0px 1px #ccc',
        }}
      >
        <FlexRow
          css={{
            flexGrow: 1,
            flexShrink: 1,
            overflow: 'hidden',
            gap: 8,
            paddingLeft: 8,
            cursor: 'pointer',
            background: UtopiaTheme.color.neutralBackground.value,
            '&:hover': {
              filter: 'brightness(.98)',
            },
            '&:active': {
              filter: 'brightness(.95)',
            },
          }}
        >
          <Icons.Component color='purple' style={{ flexGrow: 1, flexShrink: 0 }} />
          <span
            style={{
              flexGrow: 1,
              flexShrink: 1,
              textOverflow: 'ellipsis',
              overflow: 'hidden',
              whiteSpace: 'nowrap',
            }}
          >
            TODO REPLACE ME WITH PROPS LABEL
          </span>
        </FlexRow>

        <div
          role='expansionButton'
          css={{
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            flexGrow: 0,
            flexShrink: 0,
            flexBasis: '20',
            background: UtopiaTheme.color.neutralBackground.value,
            '&:hover': {
              filter: 'brightness(.98)',
            },
            '&:active': {
              filter: 'brightness(.95)',
            },
          }}
          onClick={toggleOpen}
        >
          <Icons.ExpansionArrow style={{ transform: isOpen ? 'rotate(180deg)' : undefined }} />
        </div>

        {isOpen ? (
          <OnClickOutsideHOC onClickOutside={closeAndEatEvent}>
            <div
              style={{
                position: 'absolute',
                left: 0,
                top: 30,
                borderRadius: 2,
                zIndex: 99999,
                width: UtopiaTheme.layout.inspectorWidth,
                height: 100,
                background: UtopiaTheme.color.neutralBackground.value,
                boxShadow: 'inset 0px 0px 0px .5px hsl(0,0%,80%), 0px 2px 5px 0px hsl(0,0%,80%)',
                display: 'flex',
                flexDirection: 'column',
                alignContent: 'flex-start',
              }}
              onClick={(e) => e.stopPropagation()}
              onMouseDown={(e) => e.stopPropagation()}
              onMouseUp={(e) => e.stopPropagation()}
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
