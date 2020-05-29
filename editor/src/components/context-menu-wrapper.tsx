import * as React from 'react'
import { Component as ReactComponent } from 'react'
import { Menu, MenuProvider, Item, Submenu as SubmenuComponent, contextMenu } from 'react-contexify'
import RU from '../utils/react-utils'
import { ContextMenuItem } from './context-menu-items'
import { EditorDispatch } from './editor/action-types'
import * as fastDeepEquals from 'fast-deep-equal'

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
}

export function openMenu(id: string, nativeEvent: MouseEvent) {
  // this function directly opens the react-contexify menu, without going through the prescribed route of
  // letting react-contexify open itself. in order to have symmetric functionality,
  // we preventDefault() here, replicataing what happens inside 'react-contexify' as well
  nativeEvent.preventDefault()
  contextMenu.show({
    id: id,
    event: nativeEvent,
  })
}

export interface ContextMenuProps<T> {
  id: string
  items: Array<ContextMenuItem<T>>
  dispatch?: EditorDispatch
  getData: () => T
}

interface Submenu<T> {
  type: 'submenu'
  label: string
  items: Array<ContextMenuItem<T>>
}

interface SimpleItem<T> {
  type: 'simple'
  item: ContextMenuItem<T>
}

export class MomentumContextMenu<T> extends ReactComponent<ContextMenuProps<T>, {}> {
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

  renderItem(item: ContextMenuItem<T>, index: number) {
    return (
      <Item
        key={`context-menu-${index}-item`}
        disabled={!item.enabled}
        // eslint-disable-next-line react/jsx-no-bind
        onClick={({
          event,
          clickEvent,
        }: {
          event: MouseEvent
          clickEvent: React.MouseEvent<HTMLDivElement>
        }) => {
          clickEvent.stopPropagation()
          event.stopPropagation()
          item.action(this.props.getData(), this.props.dispatch, event)
          contextMenu.hideAll()
        }}
      >
        <span className='react-contexify-span'>{item.name}</span>
        <span className='shortcut'>{item.shortcut}</span>
      </Item>
    )
  }

  render() {
    const { id } = this.props
    const items = this.splitItemsForSubmenu(this.props.items)
    return (
      <Menu key={id} id={id}>
        {items.map((item: Submenu<T> | SimpleItem<T>, index: number) => {
          if (item.type === 'submenu') {
            return (
              <SubmenuComponent key={`context-menu-${index}`} label={item.label}>
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
  ContextMenuWrapperProps<T> & { dispatch: EditorDispatch }
> {
  getData = () => this.props.data
  render() {
    const name = `${this.props.id}-context-menu-wrapper`
    return (
      <div
        key={name}
        className={name + ' ' + (this.props.className || '')}
        style={this.props.style}
        onContextMenu={(e) => {
          e.stopPropagation()
          e.preventDefault()
        }}
      >
        <MenuProvider
          key={`${this.props.id}-provider`}
          id={this.props.id}
          style={this.props.providerStyle}
          storeRef={false}
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

export class InspectorContextMenuWrapper<T> extends ReactComponent<ContextMenuWrapperProps<T>> {
  getData = () => this.props.data
  render() {
    const name = `${this.props.id}-context-menu-wrapper`
    return (
      <div
        key={name}
        className={name + ' ' + (this.props.className || '')}
        style={{
          width: '100%',
          ...this.props.style,
        }}
        onContextMenu={(e) => {
          e.stopPropagation()
          e.preventDefault()
        }}
      >
        {this.props.items.length > 0 ? (
          <>
            <MenuProvider
              key={`${this.props.id}-provider`}
              id={this.props.id}
              style={{
                width: '100%',
                height: '100%',
              }}
              storeRef={false}
            >
              {this.props.children}
            </MenuProvider>
            <MomentumContextMenu
              key={`${this.props.id}`}
              id={this.props.id}
              items={this.props.items}
              getData={this.getData}
            />
          </>
        ) : (
          this.props.children
        )}
      </div>
    )
  }
}
