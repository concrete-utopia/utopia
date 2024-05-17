/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React, { useCallback, useMemo } from 'react'
import debounce from 'lodash.debounce'
import { colorTheme, Icn, type IcnProps } from '../../../uuiui'
import { dark } from '../../../uuiui/styles/theme/dark'
import type { JSXElementChild } from '../../../core/shared/element-template'
import type { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { type ComponentElementToInsert } from '../../custom-code/code-file'
import type {
  InsertMenuItem,
  InsertMenuItemGroup,
  InsertMenuItemValue,
} from '../../canvas/ui/floating-insert-menu'
import { UIGridRow } from '../../../components/inspector/widgets/ui-grid-row'
import { FlexRow, type Icon } from 'utopia-api'
import { insertableComponent } from '../../shared/project-components'
import type { StylePropOption, InsertableComponent } from '../../shared/project-components'
import type { Size } from '../../../core/shared/math-utils'
import { dataPasteHandler } from '../../../utils/paste-handler'
import { sortBy } from '../../../core/shared/array-utils'
import { iconPropsForIcon } from './component-picker-context-menu'

const FILTER_CATEGORIES: Array<string> = ['Everything']

interface Category {
  label: string
  items: Array<InsertMenuItem>
}

export interface ComponentPickerProps {
  allComponents: Array<InsertMenuItemGroup>
  onItemClick: (elementToInsert: InsertableComponent) => React.UIEventHandler
  closePicker: () => void
}

export interface ElementToInsert {
  name: string
  elementToInsert: (uid: string) => JSXElementChild
  additionalImports: Imports
}

export function elementToInsertToInsertableComponent(
  elementToInsert: ElementToInsert,
  uid: string,
  stylePropOptions: Array<StylePropOption>,
  defaultSize: Size | null,
  insertionCeiling: ElementPath | null,
  icon: Icon | null,
): InsertableComponent {
  const element = elementToInsert.elementToInsert(uid)
  return insertableComponent(
    elementToInsert.additionalImports,
    () => element as ComponentElementToInsert,
    elementToInsert.name,
    stylePropOptions,
    defaultSize,
    insertionCeiling,
    icon,
  )
}

export function componentPickerTestIdForProp(prop: string): string {
  return `component-picker-${prop}`
}

export const componentPickerCloseButtonTestId = `component-picker-close-button`
export const componentPickerFilterInputTestId = `component-picker-filter-input`

export function componentPickerOptionTestId(componentName: string, variant?: string): string {
  const variantSuffix = variant == null ? '' : `-${variant}`
  return `component-picker-option-${componentName}${variantSuffix}`
}

export const ComponentPicker = React.memo((props: ComponentPickerProps) => {
  const { onItemClick, closePicker } = props
  const [selectedComponentKey, setSelectedComponentKey] = React.useState<string | null>(null)
  const [filter, setFilter] = React.useState<string>('')
  const menuRef = React.useRef<HTMLDivElement | null>(null)

  const flatComponentsToShowUnsorted = useMemo(() => {
    return props.allComponents
      .flatMap((c) => c.options)
      .filter((v) => v.label.toLocaleLowerCase().includes(filter.toLocaleLowerCase().trim()))
  }, [props.allComponents, filter])

  const flatComponentsToShow = useMemo(
    () =>
      sortBy(flatComponentsToShowUnsorted, (a, b) =>
        a.label.toLocaleLowerCase().trim().localeCompare(b.label.toLocaleLowerCase().trim()),
      ),
    [flatComponentsToShowUnsorted],
  )

  const highlightedComponentKey = useMemo(() => {
    const firstOptionKey =
      flatComponentsToShow.length > 0 ? flatComponentsToShow[0].value.key : null
    if (selectedComponentKey == null) {
      return firstOptionKey
    }
    // check if selectedComponentKey is still in the list
    const found = flatComponentsToShow.some((c) => c.value.key === selectedComponentKey)
    return found ? selectedComponentKey : firstOptionKey
  }, [flatComponentsToShow, selectedComponentKey])

  const onItemHover = useCallback(
    (elementToInsert: InsertMenuItemValue) => {
      return () => {
        setSelectedComponentKey(elementToInsert.key)
      }
    },
    [setSelectedComponentKey],
  )

  const selectIndex = useCallback(
    (index: number) => {
      const newKey = flatComponentsToShow[index].value.key
      setSelectedComponentKey(newKey)
      const selectedComponent = menuRef.current?.querySelector(`[data-key="${newKey}"]`)
      if (selectedComponent != null) {
        // scroll into view
        selectedComponent.scrollIntoView({ block: 'nearest', behavior: 'smooth' })
      }
    },
    [flatComponentsToShow],
  )

  const onKeyDown = useCallback(
    (e: React.KeyboardEvent<HTMLInputElement>) => {
      if (e.key === 'ArrowDown') {
        const currentIndex = flatComponentsToShow.findIndex(
          (c) => c.value.key === highlightedComponentKey,
        )
        if (currentIndex >= 0 && currentIndex < flatComponentsToShow.length - 1) {
          selectIndex(currentIndex + 1)
        }
      } else if (e.key === 'ArrowUp') {
        const currentIndex = flatComponentsToShow.findIndex(
          (c) => c.value.key === highlightedComponentKey,
        )
        if (currentIndex > 0) {
          selectIndex(currentIndex - 1)
        }
      } else if (e.key === 'Enter') {
        const selectedComponent = flatComponentsToShow.find(
          (c) => c.value.key === highlightedComponentKey,
        )
        if (selectedComponent != null) {
          onItemClick(selectedComponent.value)(e)
        }
      } else if (e.key === 'Escape') {
        closePicker()
      } else {
        // we don't want to prevent default for other keys
        return
      }
      e.preventDefault()
      e.stopPropagation()
    },
    [flatComponentsToShow, highlightedComponentKey, onItemClick, selectIndex, closePicker],
  )

  const categorizedComponents = [
    {
      label: 'Everything',
      items: flatComponentsToShow,
    },
  ]

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        gap: 0,
        width: '100%',
        height: '100%',
        borderRadius: 10,
      }}
      onKeyDown={onKeyDown}
      ref={menuRef}
    >
      <ComponentPickerTopSection
        components={categorizedComponents}
        onFilterChange={setFilter}
        onKeyDown={onKeyDown}
      />
      <ComponentPickerComponentSection
        components={categorizedComponents}
        onItemClick={props.onItemClick}
        onItemHover={onItemHover}
        currentlySelectedKey={highlightedComponentKey}
      />
    </div>
  )
})

interface ComponentPickerTopSectionProps {
  components: Array<Category>
  onFilterChange: (filter: string) => void
  onKeyDown: (e: React.KeyboardEvent<HTMLInputElement>) => void
}

const ComponentPickerTopSection = React.memo((props: ComponentPickerTopSectionProps) => {
  const { components, onFilterChange, onKeyDown } = props

  return (
    <div
      style={{
        padding: '0px 8px 8px 8px',
        display: 'flex',
        flexDirection: 'column',
      }}
      tabIndex={0}
    >
      {components.length > 1 && <FilterButtons components={components} />}
      <FilterBar onFilterChange={onFilterChange} onKeyDown={onKeyDown} />
    </div>
  )
})

interface FilterBarProps {
  onFilterChange: (filter: string) => void
  onKeyDown?: (e: React.KeyboardEvent<HTMLInputElement>) => void
}

const FilterBar = React.memo((props: FilterBarProps) => {
  const { onFilterChange, onKeyDown } = props

  const [filter, setFilterState] = React.useState<string>('')
  const setFilter = React.useCallback(
    (s: string) => {
      setFilterState(s)
      onFilterChange(s)
    },
    [onFilterChange],
  )

  const handleFilterKeydown = React.useCallback(
    (e: React.KeyboardEvent<HTMLInputElement>) => {
      if (e.key === 'Enter') {
        if (onKeyDown != null) {
          onKeyDown(e)
        }
      } else if (e.key === 'Escape' && filter !== '') {
        // clear filter only if there is text,
        // otherwise it will close the picker
        setFilter('')
      } else if (onKeyDown != null) {
        onKeyDown(e)
      }
      e.stopPropagation()
    },
    [setFilter, onKeyDown, filter],
  )
  const handleFilterChange = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      setFilter(e.target.value)
    },
    [setFilter],
  )

  return (
    <input
      css={{
        height: 28,
        paddingLeft: 8,
        paddingRight: 8,
        background: 'transparent',
        // border: `1px solid ${dark.fg3.value}`, --> doesn't work because uses the css var
        border: `1px solid #888`,
        color: `#888`,
        borderRadius: 4,
        width: '100%',
        '&:focus': {
          color: '#ccc',
          borderColor: '#ccc',
        },
      }}
      placeholder='Filter...'
      autoComplete='off'
      autoFocus={true}
      spellCheck={false}
      onKeyDown={handleFilterKeydown}
      onChange={handleFilterChange}
      value={filter}
      data-testId={componentPickerFilterInputTestId}
      {...dataPasteHandler(true)}
    />
  )
})

interface FilterButtonsProps {
  components: Array<Category>
}

const FilterButtons = React.memo((props: FilterButtonsProps) => {
  const { components } = props

  const [focusedIndex, setFocusedIndex] = React.useState(0)

  const setActiveIndexAll = React.useCallback(() => setFocusedIndex(0), [setFocusedIndex])

  return (
    <div
      tabIndex={0}
      aria-describedby='Component categories'
      css={{
        display: 'flex',
        height: 30,
      }}
      onKeyDown={(event) => {
        if (event.key === 'ArrowRight') {
          setFocusedIndex((prev) => Math.min(prev + 1, FILTER_CATEGORIES.length))
        } else if (event.key === 'ArrowLeft') {
          setFocusedIndex((prev) => Math.max(prev - 1, 0))
        } else if (event.key === 'Enter') {
          document
            .getElementById(FILTER_CATEGORIES[Math.max(focusedIndex - 1, 0)])
            ?.scrollIntoView({ block: 'start', behavior: 'smooth' })
        } else {
          return
        }
        event.stopPropagation()
        event.preventDefault()
      }}
    >
      <div css={{ display: 'inline-block', marginRight: 8 }}>
        <FilterButton
          highlighted={focusedIndex === 0}
          index={-1}
          label='All'
          setActiveFocus={setActiveIndexAll}
        />
      </div>
      <ul
        css={{
          margin: '0 0 8px 0',
          padding: 0,
          overflowX: 'scroll',
          whiteSpace: 'nowrap',
        }}
      >
        {components.map(({ label }, index) => (
          <li key={label} css={{ display: 'inline-block' }}>
            <FilterButton
              highlighted={focusedIndex === index + 1}
              index={index}
              label={label}
              setActiveFocus={setFocusedIndex}
            />
          </li>
        ))}
      </ul>
    </div>
  )
})

interface FilterButtonProps {
  highlighted: boolean
  index: number
  label: string
  setActiveFocus: (index: number) => void
}

const FilterButton = React.memo((props: FilterButtonProps) => {
  const { highlighted, index, label, setActiveFocus } = props

  const ref = React.useRef<HTMLButtonElement>(null)

  React.useEffect(() => {
    if (highlighted && ref.current !== null) {
      ref.current.scrollIntoView({
        block: 'start',
        behavior: 'instant',
      })
    }
  }, [highlighted])

  return (
    <button
      tabIndex={-1}
      id={`button-${label}`}
      aria-selected={highlighted}
      css={{
        backgroundColor: highlighted ? colorTheme.primary.value : 'transparent',
        border: 'none',
        color: highlighted ? 'white' : '#ddd',
        cursor: 'pointer',
        fontSize: 12,
        padding: '4px 8px',
        borderRadius: 4,
        outlineOffset: -1,
        '&:hover': {
          color: 'white',
        },
        '&:focus': {
          backgroundColor: highlighted ? colorTheme.primary.value : 'transparent',
          color: highlighted ? 'white' : undefined,
        },
      }}
      onClick={() => {
        setActiveFocus(index + 1)
        const element = document.getElementById(`${label}`)
        if (element !== null) {
          document.getElementById('filter-container')?.scrollTo({
            top: element.offsetTop - 75,
            behavior: 'smooth',
          })
        } else {
          document.getElementById('filter-container')?.scrollTo({ top: 0, behavior: 'smooth' })
        }
      }}
      ref={ref}
    >
      {label}
    </button>
  )
})

interface ComponentPickerComponentSectionProps {
  components: Array<Category>
  onItemClick: (elementToInsert: InsertableComponent) => React.MouseEventHandler
  onItemHover: (elementToInsert: InsertMenuItemValue) => React.MouseEventHandler
  currentlySelectedKey: string | null
}

const ComponentPickerComponentSection = React.memo(
  (props: ComponentPickerComponentSectionProps) => {
    const { components, onItemClick, onItemHover, currentlySelectedKey } = props
    const [isScrolling, setIsScrolling] = React.useState<boolean>(false)
    const debouncedSetIsScrolling = React.useRef(debounce(() => setIsScrolling(false), 100))
    const onScroll = React.useCallback(() => {
      setIsScrolling(true)
      debouncedSetIsScrolling.current()
    }, [])
    return (
      <div
        data-role='component-scroll'
        id='filter-container'
        style={{
          maxHeight: 250,
          overflowY: 'scroll',
          scrollbarWidth: 'auto',
          scrollbarColor: 'gray transparent',
        }}
        onScroll={onScroll}
      >
        {components.flatMap((category) =>
          category.items.map((component, index) => {
            const isSelected = component.value.key === currentlySelectedKey
            return (
              <FlexRow
                css={{}}
                key={component.value.key}
                id={index === 0 ? category.label : undefined}
                style={{
                  alignItems: 'center',
                  cursor: 'pointer',
                  marginLeft: 8,
                  marginRight: 8,
                  borderRadius: 4,
                  // indentation!
                  paddingLeft: 8,
                  pointerEvents: isScrolling ? 'none' : 'auto',
                  color: isSelected ? 'white' : '#EEE',
                  backgroundColor: isSelected ? colorTheme.primary.value : undefined,
                }}
                onClick={onItemClick(component.value)}
                onMouseOver={onItemHover(component.value)}
                data-key={component.value.key}
                data-testid={component.value.key}
              >
                <FlexRow css={{ gap: 10, height: 28, alignItems: 'center' }}>
                  <Icn
                    {...iconPropsForIcon(component.value.icon ?? 'component')}
                    width={12}
                    height={12}
                  />
                  <span>{component.label}</span>
                </FlexRow>
              </FlexRow>
            )
          }),
        )}
      </div>
    )
  },
)
