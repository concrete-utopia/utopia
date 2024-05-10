/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import fastDeepEquals from 'fast-deep-equal'
import React, {
  type CSSProperties,
  Fragment,
  type MouseEvent,
  type RefObject,
  type ReactNode,
  memo,
  useCallback,
  useMemo,
} from 'react'
import {
  contextMenu,
  Item,
  Menu,
  Submenu as SubmenuComponent,
  useContextMenu,
} from 'react-contexify'
import { colorTheme, Icons } from '../uuiui'
import { getControlStyles } from '../uuiui-deps'
import type { ContextMenuItem } from './context-menu-items'
import type { EditorDispatch } from './editor/action-types'
import type { WindowPoint } from '../core/shared/math-utils'
import { windowPoint } from '../core/shared/math-utils'
import { BodyMenuOpenClass } from '../core/shared/utils'

interface Submenu<T> {
  items: Item<T>[]
  label: string | ReactNode
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
  children?: ReactNode
  className?: string
  data: T
  dispatch?: EditorDispatch
  forwardRef?: RefObject<HTMLDivElement>
  id: string
  innerClassName?: string
  items: ContextMenuItem<T>[]
  providerStyle?: CSSProperties
  renderTag?: string
  style?: CSSProperties
  testId?: string
}

export interface ContextMenuProps<T> {
  dispatch?: EditorDispatch
  getData: () => T
  id: string
  items: ContextMenuItem<T>[]
}

const onShown = () => document.body.classList.add(BodyMenuOpenClass)
const onHidden = () => document.body.classList.remove(BodyMenuOpenClass)

export const MomentumContextMenu = memo(
  <T,>({ dispatch, getData, id, items }: ContextMenuProps<T>) => {
    const splitItems = useMemo(() => {
      const tempItems: MenuItem<T>[] = []

      for (const item of items) {
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
      return tempItems
    }, [items])

    const isHidden = useCallback(
      (item: Item<T>): boolean => {
        if (typeof item?.isHidden === 'function') {
          return item.isHidden(getData())
        }
        return item?.isHidden ?? false
      },
      [getData],
    )

    const isDisabled = useCallback(
      (item: Item<T>): boolean =>
        typeof item?.enabled === 'function' ? !item.enabled(getData?.()) : !item?.enabled,
      [getData],
    )

    const renderItem = useCallback(
      (item: Item<T>, index: number) => {
        return (
          <Item
            key={`context-menu-${index}-item`}
            disabled={isDisabled(item)}
            // eslint-disable-next-line react/jsx-no-bind
            onClick={({ event, triggerEvent }) => {
              event.stopPropagation()
              const rightClickCoordinate: WindowPoint | null = (() => {
                if (!(triggerEvent instanceof MouseEvent)) return null
                return windowPoint({ x: triggerEvent.clientX, y: triggerEvent.clientY })
              })()
              item?.action(getData(), dispatch, rightClickCoordinate, event)
              contextMenu.hideAll()
            }}
            hidden={isHidden(item)}
            style={{
              height: item?.isSeparator ? 9 : 28,
              display: 'flex',
              alignItems: 'center',
              borderRadius: 4,
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

    return (
      <Menu key={id} id={id} animation={false} onShown={onShown} onHidden={onHidden}>
        {splitItems.map((item, index) => {
          if (item?.type === 'submenu') {
            return (
              <SubmenuComponent
                key={`context-menu-${index}`}
                label={
                  <span style={{ height: 28, display: 'flex', alignItems: 'center' }}>
                    {item.label}
                  </span>
                }
                arrow={<Icons.ExpansionArrowRightWhite style={{ marginLeft: 8 }} />}
              >
                {item.items.map(renderItem)}
              </SubmenuComponent>
            )
          } else {
            if (item === null) return null
            return renderItem(item.item, index)
          }
        })}
      </Menu>
    )
  },
  (props, nextProps) => fastDeepEquals(props.items, nextProps.items),
)

export const ContextMenuWrapper = <T,>({
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

  const stopPropagation = useCallback(
    (event: MouseEvent<HTMLElement>) => event.stopPropagation(),
    [],
  )

  const getData = useCallback(() => data, [data])

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
      <MomentumContextMenu
        dispatch={dispatch}
        getData={getData}
        id={id}
        items={items as ContextMenuItem<unknown>[]}
        key={id}
      />
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

  const getData = useCallback(() => data, [data])

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
      <MomentumContextMenu
        getData={getData}
        id={id}
        items={items as ContextMenuItem<unknown>[]}
        key={id}
      />
    </div>
  )
}

interface MenuProviderProps {
  children: ReactNode
  id: string
  itemsLength: number
  style?: CSSProperties
  localXHack_KILLME?: 'local-x-coord-KILLME' | 'default' // FIXME: remove this, this is just here because react-contexify positions the context menu to the global position of the mouse, so MomentumContextMenu should in the root
}

export const MenuProvider = ({
  children,
  id,
  itemsLength,
  localXHack_KILLME,
  style,
}: MenuProviderProps) => {
  const { show } = useContextMenu({ id })

  const onContextMenu = useCallback(
    (event: MouseEvent<HTMLDivElement>) => {
      if (itemsLength <= 0) return

      show(
        event,
        localXHack_KILLME === 'local-x-coord-KILLME'
          ? { position: { x: event.nativeEvent.offsetX, y: event.nativeEvent.pageY } }
          : undefined,
      )
    },
    [itemsLength, localXHack_KILLME, show],
  )

  return (
    <div style={style} onContextMenu={onContextMenu}>
      {children}
    </div>
  )
}
