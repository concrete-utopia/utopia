import React from 'react'
import * as RadixPopover from '@radix-ui/react-popover'
import { colorTheme } from '../../../uuiui'
import { styled } from '@stitches/react'

const handleClick = (event: React.MouseEvent | React.KeyboardEvent) => {
  event.preventDefault()
  event.stopPropagation()
}

interface KeyboardContext {
  length: number | null
  setLength: (numElements: number) => void
  active: number | null
  setActiveOnClick: (callback: () => void) => void
}

const PopoverKeyboardContext = React.createContext<KeyboardContext>({
  length: null,
  setLength: () => {},
  active: null,
  setActiveOnClick: (callback: () => void) => {},
});

interface PopoverListContentProps {
  items: Array<{
    label: string;
    onClick: () => void
  }>
}

const Button = styled(RadixPopover.Close, {
  '&:hover': {
    background: colorTheme.primary50.value,
  }
});

export const PopoverListContent = ({items}: PopoverListContentProps) => {
  const {setLength, active, setActiveOnClick} = React.useContext(PopoverKeyboardContext);

  React.useEffect(() => {
    setLength(items.length)
  }, [items.length, setLength])

  React.useEffect(() => {
    if (active === null) return;
    setActiveOnClick(() => {
      items[active].onClick()
    })
  }, [active, items, setActiveOnClick])

  return (
    <ul style={{padding: 0}}>
      {items.map(({ label, onClick }, index) => (
        <li key={`popover-item-${label}`} style={{listStyle: 'none'}}>
          <Button
            css={{
              // eslint-disable-next-line @typescript-eslint/strict-boolean-expressions
              background: active === index ? colorTheme.primary.value : 'none',
              border: 'none',
              borderRadius: 4,
              padding: 8,
              outline: 'none',
              color: 'white',
              cursor: 'pointer',
              width: '100%',
              textAlign: 'left',
            }}
            onClick={onClick}
          >
            {label}
          </Button>
        </li>
      ))}
    </ul>
  )
};

interface PopoverProps {
  activator: React.ReactNode
  align?: 'start' | 'center' | 'end'
  children: React.ReactNode
}

export const Popover = ({align = 'start', activator, children}: PopoverProps) => {
  const [selectedIndex, setSelectedIndex] = React.useState(0)
  const [numElements, setNumElements] = React.useState(0)
  const [activeOnClick, setActiveOnClick] = React.useState<(() => void) | null>(null)

  const handleKeyPress = React.useCallback((e: React.KeyboardEvent) => {
    // TODO: handle closing

    handleClick(e)

    if (e.key === 'ArrowDown') {
      setSelectedIndex((previousIndex) => Math.min(previousIndex + 1, numElements - 1))
    } else if (e.key === 'ArrowUp') {
      setSelectedIndex((previousIndex) => Math.max(previousIndex - 1, 0))
    } else if (e.key === 'Enter') {
      activeOnClick?.()
    }
  }, [activeOnClick, numElements])

  return (
    <PopoverKeyboardContext.Provider value={{
      length: numElements,
      setLength: setNumElements,
      active: selectedIndex,
      setActiveOnClick: (callback) => {
        setActiveOnClick(() => callback)
      },
    }}>
      <RadixPopover.Root>
        <RadixPopover.Trigger style={{
          background: 'none',
          outline: 'none',
          border: 'none',
          padding: 0,
          cursor: 'pointer',
        }}>
          {activator}
        </RadixPopover.Trigger>
        <RadixPopover.Anchor />
        <RadixPopover.Portal>
          <RadixPopover.Content
            onContextMenu={handleClick}
            onClick={handleClick}
            onKeyDown={handleKeyPress}
            align={align}
            sideOffset={8}
            style={{
              background: colorTheme.bg0.value,
              width: 260,
              padding: '4px 16px',
              borderRadius: 4,
            }}
          >
            {children}
            <PopoverListContent items={[
              {label: 'First', onClick: () => console.log('first')},
              {label: 'Second', onClick: () => console.log('second')}
            ]} />
          </RadixPopover.Content>
        </RadixPopover.Portal>
      </RadixPopover.Root>
    </PopoverKeyboardContext.Provider>
  )
}