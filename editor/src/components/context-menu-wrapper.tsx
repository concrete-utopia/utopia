/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import fastDeepEquals from 'fast-deep-equal'
import React, { Component as ReactComponent } from 'react'
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

export interface ContextMenuWrapperProps<T> {
  id: string
  items: Array<ContextMenuItem<T>>
  dispatch?: EditorDispatch
  data: T
  renderTag?: string
  className?: string
  innerClassName?: string
  style?: React.CSSProperties
  providerStyle?: React.CSSProperties
  testId?: string
}

export interface ContextMenuProps<T> {
  id: string
  items: Array<ContextMenuItem<T>>
  dispatch?: EditorDispatch
  getData: () => T
}

interface Submenu<T> {
  type: 'submenu'
  label: string | React.ReactNode
  items: Array<ContextMenuItem<T>>
}

interface SimpleItem<T> {
  type: 'simple'
  item: ContextMenuItem<T>
}

export class MomentumContextMenu<T> extends ReactComponent<ContextMenuProps<T>> {
  shouldComponentUpdate(nextProps: ContextMenuProps<T>) {
    const result = !fastDeepEquals(this.props.items, nextProps.items)
    return result
  }

  splitItemsForSubmenu(items: Array<ContextMenuItem<T>>): Array<Submenu<T> | SimpleItem<T>> {
    let splitItems: Array<Submenu<T> | SimpleItem<T>> = []
    for (const item of items) {
      if (item.submenuName != null) {
        const alreadyAdded = splitItems.find(
          (alreadySplit: any) =>
            alreadySplit.type === 'submenu' && alreadySplit.label === item.submenuName,
        )
        if (alreadyAdded != null && alreadyAdded.type === 'submenu') {
          alreadyAdded.items.push(item)
        } else {
          splitItems.push({
            type: 'submenu',
            label: item.submenuName!,
            items: [item],
          })
        }
      } else {
        splitItems.push({
          type: 'simple',
          item: item,
        })
      }
    }
    return splitItems
  }

  isHidden = (item: ContextMenuItem<T>): (() => boolean) => {
    return () => {
      if (item.isHidden == null) {
        return false
      } else if (typeof item.isHidden === 'function') {
        return item.isHidden(this.props.getData())
      } else {
        return item.isHidden
      }
    }
  }

  isDisabled = (item: ContextMenuItem<T>): (() => boolean) => {
    return () => {
      if (typeof item.enabled === 'function') {
        return !item.enabled(this.props.getData())
      } else {
        return !item.enabled
      }
    }
  }

  renderItem(item: ContextMenuItem<T>, index: number) {
    return (
      <Item
        key={`context-menu-${index}-item`}
        disabled={this.isDisabled(item)}
        // eslint-disable-next-line react/jsx-no-bind
        onClick={({ event, triggerEvent }) => {
          event.stopPropagation()
          const rightClickCoordinate: WindowPoint | null = (() => {
            if (!(triggerEvent instanceof MouseEvent)) {
              return null
            }
            return windowPoint({ x: triggerEvent.clientX, y: triggerEvent.clientY })
          })()
          item.action(this.props.getData(), this.props.dispatch, rightClickCoordinate, event)
          contextMenu.hideAll()
        }}
        hidden={this.isHidden(item)}
        style={{
          height: item.isSeparator ? 9 : 28,
          display: 'flex',
          alignItems: 'center',
          borderRadius: 4,
        }}
      >
        <span style={{ flexGrow: 1, flexShrink: 0 }} className='react-contexify-span'>
          {item.name}
        </span>
        <span style={{ flexGrow: 0, flexShrink: 0, opacity: 0.6 }} className='shortcut'>
          {item.shortcut}
        </span>
      </Item>
    )
  }

  onShown = () => {
    document.body.classList.add(BodyMenuOpenClass)
  }
  onHidden = () => {
    document.body.classList.remove(BodyMenuOpenClass)
  }

  render() {
    const { id } = this.props
    const items = this.splitItemsForSubmenu(this.props.items)
    return (
      <Menu key={id} id={id} animation={false} onShown={this.onShown} onHidden={this.onHidden}>
        {items.map((item: Submenu<T> | SimpleItem<T>, index: number) => {
          if (item.type === 'submenu') {
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
                {item.items.map((submenuItem, submenuIndex) =>
                  this.renderItem(submenuItem, submenuIndex),
                )}
              </SubmenuComponent>
            )
          } else {
            return this.renderItem(item.item, index)
          }
        })}
      </Menu>
    )
  }
}

export class ContextMenuWrapper<T> extends ReactComponent<
  ContextMenuWrapperProps<T> & { dispatch?: EditorDispatch; children?: React.ReactNode }
> {
  getData = () => this.props.data
  wrapperStopPropagation = (event: React.MouseEvent<HTMLElement>) => {
    event.stopPropagation()
  }
  render() {
    const name = `${this.props.id}-context-menu-wrapper`
    return (
      <div
        key={name}
        className={name + ' ' + (this.props.className ?? '')}
        style={this.props.style}
        onMouseDown={this.wrapperStopPropagation}
        onMouseUp={this.wrapperStopPropagation}
        onClick={this.wrapperStopPropagation}
      >
        <MenuProvider
          key={`${this.props.id}-provider`}
          id={this.props.id}
          style={this.props.providerStyle}
          itemsLength={this.props.items.length}
        >
          {this.props.children}
        </MenuProvider>
        <MomentumContextMenu
          key={`${this.props.id}`}
          id={this.props.id}
          items={this.props.items}
          dispatch={this.props.dispatch}
          getData={this.getData}
        />
      </div>
    )
  }
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
export class InspectorContextMenuWrapper<T> extends ReactComponent<
  React.PropsWithChildren<ContextMenuWrapperProps<T>>
> {
  getData = () => this.props.data
  render() {
    const name = `${this.props.id}-context-menu-wrapper`
    return (
      <div
        key={name}
        data-testid={this.props.testId}
        className={name + ' ' + (this.props.className ?? '')}
        css={{
          width: '100%',
          ...(this.props.style as any), // TODO Emotion and React 18 types don't like each other
          ...InspectorRowHoverCSS,
        }}
      >
        <React.Fragment>
          <MenuProvider
            key={`${this.props.id}-provider`}
            id={this.props.id}
            itemsLength={this.props.items.length}
            style={{
              width: '100%',
              height: '100%',
            }}
          >
            {this.props.children}
          </MenuProvider>
          <MomentumContextMenu
            key={`${this.props.id}`}
            id={this.props.id}
            items={this.props.items}
            getData={this.getData}
          />
        </React.Fragment>
      </div>
    )
  }
}

interface MenuProviderProps {
  id: string
  itemsLength: number
  style?: React.CSSProperties
}

export const MenuProvider: React.FunctionComponent<React.PropsWithChildren<MenuProviderProps>> = (
  props,
) => {
  const { show } = useContextMenu({ id: props.id })
  const onContextMenu = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      if (props.itemsLength > 0) {
        show(event)
      }
    },
    [props.itemsLength, show],
  )

  return (
    <div style={props.style} onContextMenu={onContextMenu}>
      {props.children}
    </div>
  )
}
