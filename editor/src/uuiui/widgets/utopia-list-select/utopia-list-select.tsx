import React from 'react'
import { Keyboard } from '../../../utils/keyboard'

export interface UtopiaListOption {
  key: string
  name: string
  label: string
  subtitle: string
}

export interface UtopiaListSelectProps {
  options: UtopiaListOption[] // it's a Key and DisplayName pair
  focusedKey: string | null
  onFocusChange: (focusedKey: string) => void
  onSelect: (selectedKey: string) => void
  extraClasses: string[]
  extraListItemClasses: string[]
}

export class UtopiaListSelect extends React.Component<UtopiaListSelectProps> {
  onListItemMouseOver = (key: string) => {
    this.props.onFocusChange(key)
  }

  onListItemSelect = (key: string) => {
    this.props.onSelect(key)
  }

  onKeyDown = (event: React.KeyboardEvent<HTMLDivElement>) => {
    const options = this.props.options
    const focusedIndex = options.findIndex((item) => item.key === this.props.focusedKey)

    switch (Keyboard.keyCharacterForCode(event.keyCode)) {
      case 'up':
        if (focusedIndex === -1) {
          if (options.length > 0) {
            const option = options[options.length - 1]
            if (option !== undefined) {
              this.props.onFocusChange(option.key)
            }
          }
        } else {
          if (focusedIndex > 0) {
            const option = options[focusedIndex - 1]
            if (option !== undefined) {
              this.props.onFocusChange(option.key)
            }
          }
        }
        break
      case 'down':
        if (focusedIndex === -1) {
          if (options.length > 0) {
            const option = options[options.length - 1]
            if (option !== undefined) {
              this.props.onFocusChange(option.key)
            }
          }
        } else {
          if (focusedIndex < options.length) {
            const option = options[focusedIndex + 1]
            if (option !== undefined) {
              this.props.onFocusChange(option.key)
            }
          }
        }
        break
      default:
        break
    }
  }

  createListItem = (item: UtopiaListOption) => {
    return (
      <UtopiaListItem
        key={item.key}
        itemKey={item.key}
        name={item.name}
        label={item.label}
        subtitle={item.subtitle}
        focused={item.key === this.props.focusedKey}
        extraClassNames={this.props.extraListItemClasses}
        onMouseOver={this.onListItemMouseOver}
        onSelect={this.onListItemSelect}
      />
    )
  }

  render() {
    return (
      <div
        className={`utopia-list ${this.props.extraClasses.join(' ')}`}
        onKeyDown={this.onKeyDown}
      >
        {this.props.options.map(this.createListItem)}
      </div>
    )
  }
}

interface UtopiaListItemProps {
  itemKey: string
  name: string
  label: string
  subtitle: string
  focused: boolean
  extraClassNames: string[]
  onMouseOver: (key: string) => void
  onSelect: (key: string) => void
}

export class UtopiaListItem extends React.Component<UtopiaListItemProps> {
  onMouseOver = () => {
    this.props.onMouseOver(this.props.itemKey)
  }

  onMouseDown = () => {
    this.props.onSelect(this.props.itemKey)
  }

  render() {
    const focusedClass = this.props.focused ? 'utopia-focused' : ''
    return (
      <div
        key={this.props.itemKey}
        className={`utopia-list-item ${focusedClass} ${this.props.extraClassNames.join(' ')}`}
        onMouseOver={this.onMouseOver}
        onMouseDown={this.onMouseDown}
      >
        <span className='utopia-list-item-label'>{this.props.label}</span>{' '}
        <span className='utopia-list-item-subtitle'>{this.props.subtitle}</span>
      </div>
    )
  }
}
