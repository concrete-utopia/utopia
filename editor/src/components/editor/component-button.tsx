/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import { BreadcrumbTrail } from '../canvas/controls/breadcrumb-trail'
import {
  background,
  color,
  FlexRow,
  Icn,
  IcnColor,
  OnClickOutsideHOC,
  SmallerIcons,
  stroke,
  UtopiaTheme,
} from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'
import { RenderAsRow } from '../canvas/controls/render-as'
import { string } from 'fast-check/*'
import { useEditorState } from './store/store-hook'

export const ComponentOrInstanceIndicator = betterReactMemo('ComponentOrInstanceIndicator', () => {
  const { selectedViews } = useEditorState((store) => {
    return { selectedViews: store.editor.selectedViews }
  }, 'TopMenuContextProvider')

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

  enum editContext {
    ELEMENT = 0,
    COMPONENT_INSTANCE,
    COMPONENT,
    COMPONENT_CHILD,
  }

  const [currentEditContext, setCurrentEditContext] = React.useState(editContext.COMPONENT)

  // TODO replace this with a function that enters focus mode
  const TODOREPLACEMEcycleEditContext = () => {
    const nextEditContext = (currentEditContext + 1) % 4
    setCurrentEditContext(nextEditContext)
  }

  const getEditContextStyle = (): React.CSSProperties => {
    if (currentEditContext === editContext.COMPONENT) {
      return {
        color: UtopiaTheme.color.component.value,
        backgroundColor: UtopiaTheme.color.component.shade(10).value,
        stroke: UtopiaTheme.color.component.fileNameFragment,
      }
    } else if (currentEditContext === editContext.COMPONENT_CHILD) {
      return {
        color: UtopiaTheme.color.componentChild.value,
        backgroundColor: UtopiaTheme.color.componentChild.shade(10).value,
        stroke: UtopiaTheme.color.componentChild.fileNameFragment,
      }
    } else if (currentEditContext === editContext.COMPONENT_INSTANCE) {
      return {
        background: UtopiaTheme.color.secondaryBackground.value,
        color: UtopiaTheme.color.neutralForeground.value,
        stroke: 'black',
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
        flexBasis: 130,
      }}
    >
      <FlexRow
        role='button'
        // TODO replace me with the right function call from above
        onClick={TODOREPLACEMEcycleEditContext}
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
        <Icn
          category='element'
          type='ghost'
          width={18}
          height={18}
          color={getEditContextStyle().stroke as IcnColor}
        />
        <span
          style={{
            flexGrow: 1,
            flexShrink: 1,
            textOverflow: 'ellipsis',
            overflow: 'hidden',
            whiteSpace: 'nowrap',
            fontWeight: 500,
            paddingRight: 8,
          }}
        >
          {/* TODO replace me with the real label */}
          {selectedViews.length}
        </span>
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
              borderRadius: 2,
              zIndex: 1,
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
            <RenderAsRow />
          </div>
        </OnClickOutsideHOC>
      ) : null}
    </div>
  )
})
