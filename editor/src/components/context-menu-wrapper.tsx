/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { Item, Menu, Submenu as SubmenuComponent, useContextMenu } from 'react-contexify'
import { colorTheme, Icons, OnClickOutsideHOC, UtopiaStyles } from '../uuiui'
import { getControlStyles } from '../uuiui-deps'
import type { ContextMenuItem } from './context-menu-items'
import type { EditorDispatch } from './editor/action-types'
import type { WindowPoint } from '../core/shared/math-utils'
import { windowPoint } from '../core/shared/math-utils'
import { addOpenMenuId, removeOpenMenuId } from '../core/shared/menu-state'
import { createPortal } from 'react-dom'
import { CanvasContextMenuPortalTargetID } from '../core/shared/utils'

interface Submenu<T> {
  items: Item<T>[]
  label: string | React.ReactNode
  type: 'submenu'
}

interface SimpleItem<T> {
  item: Item<T>
  type: 'simple'
}

type MenuItem<T> = Submenu<T> | SimpleItem<T> | null

export type Item<T> = ContextMenuItem<T> | null

export type ContextMenuData<T> = Item<T>[] | null

export interface ContextMenuWrapperProps<T> {
  children?: React.ReactNode
  className?: string
  data: T
  dispatch?: EditorDispatch
  forwardRef?: React.RefObject<HTMLDivElement>
  id: string
  innerClassName?: string
  items: ContextMenuItem<T>[]
  providerStyle?: React.CSSProperties
  renderTag?: string
  style?: React.CSSProperties
  testId?: string
}

type ContextMenuItems<T> = Array<ContextMenuItem<T>>
export interface ContextMenuProps<T> {
  dispatch?: EditorDispatch
  getData: () => T
  id: string
  items: ContextMenuItems<T> | (() => ContextMenuItems<T>)
}

export const ContextMenu = <T,>({ dispatch, getData, id, items }: ContextMenuProps<T>) => {
  const [menuIsVisible, setMenuIsVisible] = React.useState(false)

  const onVisibilityChange = React.useCallback(
    (isVisible: boolean) => {
      setMenuIsVisible(isVisible)
      if (isVisible) {
        addOpenMenuId(id)
      } else {
        removeOpenMenuId(id)
      }
    },
    [id],
  )

  const splitItems = React.useMemo(() => {
    const tempItems: MenuItem<T>[] = []

    if (menuIsVisible) {
      const menuItems = typeof items === 'function' ? items() : items
      for (const item of menuItems) {
        if (item?.submenuName != null) {
          const alreadyAdded = tempItems.find(
            (alreadySplit) =>
              alreadySplit?.type === 'submenu' && alreadySplit.label === item.submenuName,
          )
          if (alreadyAdded != null && alreadyAdded.type === 'submenu') {
            alreadyAdded.items.push(item)
          } else {
            tempItems.push({
              type: 'submenu',
              label: item.submenuName,
              items: [item],
            })
          }
        } else {
          tempItems.push({
            type: 'simple',
            item: item,
          })
        }
      }
    }
    return tempItems
  }, [items, menuIsVisible])

  const isHidden = React.useCallback(
    (item: Item<T>): (() => boolean) => {
      return () => {
        if (typeof item?.isHidden === 'function') {
          return item.isHidden(getData())
        }
        return item?.isHidden ?? false
      }
    },
    [getData],
  )

  const isDisabled = React.useCallback(
    (item: Item<T>): (() => boolean) =>
      () => {
        return typeof item?.enabled === 'function' ? !item.enabled(getData?.()) : !item?.enabled
      },
    [getData],
  )

  const renderItem = React.useCallback(
    (item: Item<T>, index: number) => {
      return (
        <Item
          key={`context-menu-${index}-item`}
          disabled={isDisabled(item)}
          // eslint-disable-next-line react/jsx-no-bind
          onClick={({ event, triggerEvent }) => {
            event.stopPropagation()
            const rightClickCoordinate: WindowPoint | null = (() => {
              if (!(triggerEvent instanceof MouseEvent)) {
                return null
              }
              return windowPoint({ x: triggerEvent.clientX, y: triggerEvent.clientY })
            })()
            item?.action(getData(), dispatch, rightClickCoordinate, event)
          }}
          hidden={isHidden(item)}
          style={{
            ...UtopiaStyles.flexRow,
            height: item?.isSeparator ? 9 : undefined,
            borderRadius: UtopiaStyles.popup.borderRadius,
          }}
        >
          <span style={{ flexGrow: 1, flexShrink: 0 }} className='react-contexify-span'>
            {item?.name}
          </span>
          <span style={{ flexGrow: 0, flexShrink: 0, opacity: 0.6 }} className='shortcut'>
            {item?.shortcut}
          </span>
        </Item>
      )
    },
    [getData, dispatch, isDisabled, isHidden],
  )

  const onMouseDown = React.useCallback((event: React.MouseEvent<HTMLDivElement>) => {
    event.stopPropagation()
  }, [])

  return (
    <Menu
      key={id}
      id={id}
      animation={false}
      onVisibilityChange={onVisibilityChange}
      onMouseDown={onMouseDown}
    >
      {splitItems.map((item, index) => {
        if (item?.type === 'submenu') {
          return (
            <SubmenuComponent
              key={`context-menu-${index}`}
              label={<span style={{ display: 'flex', alignItems: 'center' }}>{item.label}</span>}
              arrow={<Icons.ExpansionArrowRightWhite style={{ marginLeft: 8 }} />}
            >
              {item.items.map(renderItem)}
            </SubmenuComponent>
          )
        } else {
          if (item === null) {
            return null
          }
          return renderItem(item.item, index)
        }
      })}
    </Menu>
  )
}

/** @deprecated use ContextMenuWrapper instead, which is Portaled */
export const ContextMenuWrapper_DEPRECATED = <T,>({
  children,
  className = '',
  data,
  dispatch,
  forwardRef,
  id,
  items,
  providerStyle,
  style,
}: ContextMenuWrapperProps<T>) => {
  const name = `${id}-context-menu-wrapper`

  const stopPropagation = React.useCallback(
    (event: React.MouseEvent<HTMLElement>) => event.stopPropagation(),
    [],
  )

  const getData = React.useCallback(() => data, [data])

  return (
    <div
      key={name}
      className={[name, className].join(' ')}
      style={style}
      onMouseDown={stopPropagation}
      onMouseUp={stopPropagation}
      onClick={stopPropagation}
      ref={forwardRef}
    >
      <MenuProvider id={id} itemsLength={items.length} key={`${id}-provider`} style={providerStyle}>
        {children}
      </MenuProvider>
      <ContextMenu dispatch={dispatch} getData={getData} id={id} items={items} key={id} />
    </div>
  )
}

export const InspectorRowHoverCSS = {
  '--control-styles-interactive-unset-main-color': colorTheme.fg7.value,
  '--control-styles-interactive-unset-secondary-color': colorTheme.fg7.value,
  '--control-styles-interactive-unset-track-color': colorTheme.bg5.value,
  '--control-styles-interactive-unset-rail-color': colorTheme.bg3.value,
  '&:hover': {
    '--control-styles-interactive-unset-main-color': getControlStyles('simple').mainColor,
    '--control-styles-interactive-unset-secondary-color': getControlStyles('simple').secondaryColor,
    '--control-styles-interactive-unset-track-color': getControlStyles('simple').trackColor,
    '--control-styles-interactive-unset-rail-color': getControlStyles('simple').railColor,
  },
  '&:focus-within': {
    '--control-styles-interactive-unset-main-color': getControlStyles('simple').mainColor,
    '--control-styles-interactive-unset-secondary-color': getControlStyles('simple').secondaryColor,
    '--control-styles-interactive-unset-track-color': getControlStyles('simple').trackColor,
    '--control-styles-interactive-unset-rail-color': getControlStyles('simple').railColor,
  },
}

export const InspectorContextMenuWrapper = <T,>({
  children,
  className = '',
  data,
  id,
  items,
  style,
  testId,
}: ContextMenuWrapperProps<T>) => {
  const name = `${id}-context-menu-wrapper`

  const getData = React.useCallback(() => data, [data])

  return (
    <div
      key={name}
      data-testid={testId}
      className={[name, className].join(' ')}
      css={{
        width: '100%',
        ...(style as Record<string, string>),
        ...InspectorRowHoverCSS,
      }}
    >
      <MenuProvider
        key={`${id}-provider`}
        id={id}
        itemsLength={items.length}
        style={{ width: '100%', height: '100%' }}
        localXHack_KILLME='local-x-coord-KILLME'
      >
        {children}
      </MenuProvider>
      <ContextMenu getData={getData} id={id} items={items} key={id} />
    </div>
  )
}

interface MenuProviderProps {
  children: React.ReactNode
  id: string
  itemsLength: number
  style?: React.CSSProperties
  localXHack_KILLME?: 'local-x-coord-KILLME' | 'default' // FIXME: remove this, this is just here because react-contexify positions the context menu to the global position of the mouse, so ContextMenu should in the root
}

export const MenuProvider = ({
  children,
  id,
  itemsLength,
  localXHack_KILLME,
  style,
}: MenuProviderProps) => {
  const { show } = useContextMenu({ id })

  const onContextMenu = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      event.stopPropagation()
      if (itemsLength <= 0) {
        return
      }

      show({
        event: event,
        position:
          localXHack_KILLME === 'local-x-coord-KILLME'
            ? { x: event.nativeEvent.offsetX, y: event.nativeEvent.pageY }
            : undefined,
      })
    },
    [itemsLength, localXHack_KILLME, show],
  )

  return (
    <div style={style} onContextMenu={onContextMenu}>
      {children}
    </div>
  )
}

export const ContextMenuWrapper = <T,>({
  children,
  data,
  dispatch,
  id,
  items,
  style,
}: {
  children?: React.ReactNode
  data: T
  dispatch?: EditorDispatch
  id: string
  items: ContextMenuItem<T>[]
  style?: React.CSSProperties
}) => {
  const { hideAll } = useContextMenu({ id })

  const getData = React.useCallback(() => data, [data])

  const portalTarget = document.getElementById(CanvasContextMenuPortalTargetID)

  return (
    <React.Fragment>
      <MenuProvider id={id} itemsLength={items.length} key={`${id}-provider`} style={style}>
        {children}
      </MenuProvider>
      {portalTarget != null
        ? createPortal(
            <OnClickOutsideHOC onClickOutside={hideAll}>
              <ContextMenu dispatch={dispatch} getData={getData} id={id} items={items} key={id} />
            </OnClickOutsideHOC>,
            portalTarget,
          )
        : null}
    </React.Fragment>
  )
}
