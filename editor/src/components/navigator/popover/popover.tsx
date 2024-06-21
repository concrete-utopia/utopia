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
  setActive: (value: number) => void
  setActiveOnClick: (callback: () => void) => void
}

const PopoverKeyboardContext = React.createContext<KeyboardContext>({
  length: null,
  setLength: () => {},
  active: null,
  setActive: () => {},
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
  const {setLength, active, setActive, setActiveOnClick} = React.useContext(PopoverKeyboardContext);

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
    <ul style={{margin: 0, padding: 8}}>
      {items.map(({ label, onClick }, index) => (
        <li key={`popover-item-${label}`} style={{listStyle: 'none'}}>
          <Button
            css={{
              // eslint-disable-next-line @typescript-eslint/strict-boolean-expressions
              background: active === index ? `${colorTheme.primary.value} !important`: 'none',
              border: 'none',
              borderRadius: 4,
              fontSize: 11,
              padding: 8,
              outline: 'none',
              color: 'white',
              cursor: 'pointer',
              width: '100%',
              textAlign: 'left',
            }}
            // eslint-disable-next-line react/jsx-no-bind
            onClick={() => {
              onClick()
              setActive(-1)
            }}
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
  children: React.ReactElement | React.ReactElement[]
}

export const Popover = ({align = 'start', activator, children}: PopoverProps) => {
  const [open, setOpen] = React.useState(false)

  const [selectedIndex, setSelectedIndex] = React.useState(-1)
  const [numElements, setNumElements] = React.useState(0)
  const [activeOnClick, setActiveOnClick] = React.useState<(() => void) | null>(null)
 
  const containsListComponent = React.useMemo(() => React.Children.toArray(children).some((child) => {
    return React.isValidElement(child) && (child.type as any)?.name === 'PopoverListContent';
  }), [children]);

  const handleKeyPress = React.useCallback((e: React.KeyboardEvent) => {
    handleClick(e)

    if (!containsListComponent) {
      return
    }

    if (e.key === 'ArrowDown') {
      setSelectedIndex((previousIndex) => Math.min(previousIndex + 1, numElements - 1))
    } else if (e.key === 'ArrowUp') {
      setSelectedIndex((previousIndex) => Math.max(previousIndex - 1, 0))
    } else if (e.key === 'Enter') {
      activeOnClick?.()
      setOpen(false)
    }
  }, [activeOnClick, containsListComponent, numElements])

  return (
    <PopoverKeyboardContext.Provider value={{
      length: numElements,
      setLength: setNumElements,
      active: selectedIndex,
      setActive: setSelectedIndex,
      setActiveOnClick: (callback) => {
        setActiveOnClick(() => callback)
      },
    }}>
      <RadixPopover.Root open={open} onOpenChange={setOpen}>
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
              borderRadius: 4,
            }}
          >
            {children}
          </RadixPopover.Content>
        </RadixPopover.Portal>
      </RadixPopover.Root>
    </PopoverKeyboardContext.Provider>
  )
}