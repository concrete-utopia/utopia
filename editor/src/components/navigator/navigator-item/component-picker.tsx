/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React, { useCallback, useMemo } from 'react'
import { color, colorTheme, Icn, StringInput, type IcnProps } from '../../../uuiui'
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

const FILTER_CATEGORIES = [
  'Layout',
  'HTML',
  'Shopify',
  'Advanced',
  'Forms',
  'Fragment',
  'Miscellaneous',
]

export interface ComponentPickerProps {
  allComponents: Array<InsertMenuItemGroup>
  onItemClick: (elementToInsert: InsertableComponent) => React.UIEventHandler
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
  const { onItemClick } = props
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

  const onKeyDown = useCallback(
    (e: React.KeyboardEvent<HTMLInputElement>) => {
      if (e.key === 'ArrowDown') {
        const currentIndex = flatComponentsToShow.findIndex(
          (c) => c.value.key === highlightedComponentKey,
        )
        if (currentIndex >= 0 && currentIndex < flatComponentsToShow.length - 1) {
          const newKey = flatComponentsToShow[currentIndex + 1].value.key
          setSelectedComponentKey(newKey)
          const selectedComponent = menuRef.current?.querySelector(`[data-key="${newKey}"]`)
          if (selectedComponent != null) {
            selectedComponent.scrollIntoView({ block: 'nearest', behavior: 'smooth' })
          }
        }
      } else if (e.key === 'ArrowUp') {
        const currentIndex = flatComponentsToShow.findIndex(
          (c) => c.value.key === highlightedComponentKey,
        )
        if (currentIndex > 0) {
          const newKey = flatComponentsToShow[currentIndex - 1].value.key
          setSelectedComponentKey(newKey)
          const selectedComponent = menuRef.current?.querySelector(`[data-key="${newKey}"]`)
          if (selectedComponent != null) {
            selectedComponent.scrollIntoView({ block: 'nearest', behavior: 'smooth' })
          }
        }
      } else if (e.key === 'Enter') {
        const selectedComponent = flatComponentsToShow.find(
          (c) => c.value.key === highlightedComponentKey,
        )
        if (selectedComponent != null) {
          onItemClick(selectedComponent.value)(e)
        }
      } else {
        // we don't want to prevent default for other keys
        return
      }
      e.preventDefault()
      e.stopPropagation()
    },
    [flatComponentsToShow, highlightedComponentKey, onItemClick],
  )

  const fakeCategorizedComponents = [
    {
      label: FILTER_CATEGORIES[0],
      items: flatComponentsToShow.slice(0, 1),
    },
    {
      label: FILTER_CATEGORIES[1],
      items: flatComponentsToShow.slice(2, 5),
    },
    {
      label: FILTER_CATEGORIES[2],
      items: flatComponentsToShow.slice(6, 7),
    },
    {
      label: FILTER_CATEGORIES[3],
      items: flatComponentsToShow.slice(7, 8),
    },
    {
      label: FILTER_CATEGORIES[4],
      items: flatComponentsToShow.slice(8, 9),
    },
    {
      label: FILTER_CATEGORIES[5],
      items: flatComponentsToShow.slice(9, 10),
    },
    {
      label: FILTER_CATEGORIES[6],
      items: flatComponentsToShow.slice(10),
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
        padding: 0,
        color: dark.fg1.value,
        background: colorTheme.bg0.value,
      }}
      onKeyDown={onKeyDown}
      ref={menuRef}
    >
      <ComponentPickerTopSection
        components={fakeCategorizedComponents}
        onFilterChange={setFilter}
        onKeyDown={onKeyDown}
      />
      <ComponentPickerComponentSection
        components={fakeCategorizedComponents}
        onItemClick={props.onItemClick}
        onItemHover={onItemHover}
        currentlySelectedKey={highlightedComponentKey}
      />
    </div>
  )
})

interface ComponentPickerTopSectionProps {
  components: Array<{
    label: string
    items: Array<InsertMenuItem>
  }>
  onFilterChange: (filter: string) => void
  onKeyDown: (e: React.KeyboardEvent<HTMLInputElement>) => void
}

const ComponentPickerTopSection = React.memo((props: ComponentPickerTopSectionProps) => {
  const { components, onFilterChange, onKeyDown } = props

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
      }}
      tabIndex={0}
    >
      <FilterButtons components={components} />
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
      } else if (e.key === 'Escape') {
        setFilter('')
      } else if (onKeyDown != null) {
        onKeyDown(e)
      }
      e.stopPropagation()
    },
    [setFilter, onKeyDown],
  )
  const handleFilterChange = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      setFilter(e.target.value)
    },
    [setFilter],
  )

  return (
    <div style={{ margin: '4px 8px' }}>
      <StringInput
        placeholder='Filter...'
        testId={componentPickerFilterInputTestId}
        value={filter}
        onKeyDown={handleFilterKeydown}
        onChange={handleFilterChange}
        pasteHandler={true}
        css={{
          color: colorTheme.fg1.value,
          boxShadow: `inset 0px 0px 0px 1px ${colorTheme.dynamicBlue.value}`,
          borderRadius: 2,
        }}
        autoFocus={true}
      />
    </div>
  )
})

interface FilterButtonsProps {
  components: Array<{
    label: string
    items: Array<InsertMenuItem>
  }>
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
      <div
        css={{
          overflowX: 'scroll',
          whiteSpace: 'nowrap',
          display: 'flex',
          flexDirection: 'row',
          gap: 6,
          padding: '10px 8px',
        }}
      >
        <FilterButton
          highlighted={focusedIndex === 0}
          index={-1}
          label='All'
          setActiveFocus={setActiveIndexAll}
        />
        {components.map(({ label }, index) => (
          <FilterButton
            key={label}
            highlighted={focusedIndex === index + 1}
            index={index}
            label={label}
            setActiveFocus={setFocusedIndex}
          />
        ))}
      </div>
    </div>
  )
})

interface FilterButtonProps {
  highlighted: boolean
  index: number
  label: string
  setActiveFocus: (index: number) => void
  onClick?: (e: React.MouseEvent) => void
}

export const FilterButton = React.memo((props: FilterButtonProps) => {
  const { highlighted, index, label, setActiveFocus } = props

  const ref = React.useRef<HTMLButtonElement>(null)

  React.useEffect(() => {
    if (highlighted && ref.current) {
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
        backgroundColor: highlighted ? colorTheme.dynamicBlue.value : 'transparent',
        border: `1px solid ${colorTheme.dynamicBlue.value}`,
        color: highlighted ? colorTheme.bg0.value : colorTheme.dynamicBlue.value,
        cursor: 'pointer',
        fontSize: 11,
        padding: '4px 8px',
        borderRadius: 10,
        '&:hover': {
          opacity: highlighted ? 1 : 0.8,
        },
      }}
      onClick={() => {
        setActiveFocus(index + 1)
        const element = document.getElementById(`${label}`)
        if (element) {
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
  components: Array<{
    label: string
    items: Array<InsertMenuItem>
  }>
  onItemClick: (elementToInsert: InsertableComponent) => React.MouseEventHandler
  onItemHover: (elementToInsert: InsertMenuItemValue) => React.MouseEventHandler
  currentlySelectedKey: string | null
}

const ComponentPickerComponentSection = React.memo(
  (props: ComponentPickerComponentSectionProps) => {
    const { components, onItemClick, onItemHover, currentlySelectedKey } = props
    return (
      <div
        style={{ maxHeight: 250, overflowY: 'scroll', scrollbarWidth: 'auto', paddingTop: 4 }}
        id='filter-container'
      >
        {components.reduce((acc, category) => {
          const categoryMarkdown = category.items.map((component, index) => {
            const isSelected = component.value.key === currentlySelectedKey
            return (
              <FlexRow
                key={component.value.key}
                id={index === 0 ? category.label : undefined}
                css={{
                  cursor: 'pointer',
                  margin: '0 8px',
                  color: isSelected ? colorTheme.primary.value : colorTheme.fg1.value,
                  '&:hover': {
                    opacity: 0.8,
                  },
                }}
                onClick={onItemClick(component.value)}
                onMouseOver={onItemHover(component.value)}
                data-key={component.value.key}
              >
                <UIGridRow
                  variant='|--32px--|<--------auto-------->'
                  padded={false}
                  style={{ minHeight: 28 }}
                  css={{ height: 28 }}
                >
                  <div css={{ display: 'flex', alignItems: 'center', gap: 8 }}>
                    <Icn
                      category='semantic'
                      type='classicarrow-right'
                      width={16}
                      height={16}
                      style={{ opacity: isSelected ? 1 : 0 }}
                      color='dynamic'
                    />
                    <Icn
                      {...iconPropsForIcon(component.value.icon ?? 'regular', isSelected)}
                      width={12}
                      height={12}
                      color={isSelected ? 'dynamic' : 'main'}
                    />
                  </div>
                  <span
                    style={{
                      paddingLeft: 8,
                      color: isSelected ? colorTheme.dynamicBlue.value : undefined,
                    }}
                  >
                    {component.label}
                  </span>
                </UIGridRow>
              </FlexRow>
            )
          })

          // return (
          //   <ComponentPickerOption
          //     key={`${comp.label}-label`}
          //     component={comp}
          //     onItemClick={onItemClick}
          //     onItemHover={onItemHover}
          //     currentlySelectedKey={currentlySelectedKey}
          //   />
          // )

          return [...acc, ...categoryMarkdown]
        }, [] as React.ReactNode[])}
      </div>
    )
  },
)

const ICON_TYPE_MAP: Record<Icon, string> = {
  column: 'flex-column',
  row: 'flex-row',
  regular: 'component',
}

function iconPropsForIcon(icon: Icon, isSelected: boolean = false): IcnProps {
  return {
    category: 'navigator-element',
    color: isSelected ? 'dynamic' : 'white',
    type: ICON_TYPE_MAP[icon],
  }
}

interface ComponentPickerOptionProps {
  component: InsertMenuItem
  id?: string
  onItemClick: (elementToInsert: InsertableComponent) => React.MouseEventHandler
  onItemHover: (elementToInsert: InsertMenuItemValue) => React.MouseEventHandler
  currentlySelectedKey: string | null
}

const ComponentPickerOption = React.memo((props: ComponentPickerOptionProps) => {
  const { component, id, onItemClick, onItemHover, currentlySelectedKey } = props

  const isSelected = component.value.key === currentlySelectedKey

  return (
    <FlexRow
      css={{}}
      style={{
        cursor: 'pointer',
        marginLeft: 8,
        marginRight: 8,
        borderRadius: 4,
        // indentation!
        paddingLeft: 8,
        color: isSelected ? colorTheme.primary.value : '#EEE',
      }}
      onClick={onItemClick(component.value)}
      onMouseOver={onItemHover(component.value)}
      data-key={component.value.key}
    >
      <UIGridRow
        variant='|--32px--|<--------auto-------->'
        padded={false}
        // required to overwrite minHeight on the bloody thing
        style={{ minHeight: 29 }}
        css={{
          height: 27,
        }}
      >
        <div css={{ display: 'flex', alignItems: 'center', gap: 8 }}>
          <Icn
            category='semantic'
            type='classicarrow-right'
            width={16}
            height={16}
            style={{ opacity: isSelected ? 1 : 0 }}
            color='dynamic'
          />
          <Icn
            {...iconPropsForIcon(component.value.icon ?? 'regular', isSelected)}
            width={12}
            height={12}
          />
        </div>
        <span
          style={{
            paddingLeft: 8,
            color: isSelected ? colorTheme.dynamicBlue.value : undefined,
          }}
        >
          {component.label}
        </span>
      </UIGridRow>
    </FlexRow>
  )
})
