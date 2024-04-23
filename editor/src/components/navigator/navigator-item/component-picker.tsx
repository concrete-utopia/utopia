/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { useColorTheme } from '../../../uuiui'
import { capitalize } from '../../../core/shared/string-utils'
import { type PreferredChildComponentDescriptor } from '../../custom-code/internal-property-controls'
import { jsxElementWithoutUID, type JSXElementChild } from '../../../core/shared/element-template'
import { type Imports } from '../../../core/shared/project-file-types'
import { elementFromInsertMenuItem } from '../../editor/insert-callbacks'
import { componentInfo, type ComponentInfo } from '../../custom-code/code-file'
import { when } from '../../../utils/react-conditionals'
import { defaultImportsForComponentModule } from '../../../core/property-controls/property-controls-local'
import type { InsertMenuItem, InsertMenuItemGroup } from '../../canvas/ui/floating-insert-menu'
import { assertNever } from '../../../core/shared/utils'

export interface ComponentPickerProps {
  insertionTargetName: string
  preferredComponents: Array<PreferredChildComponentDescriptor>
  allComponents: Array<InsertMenuItemGroup>
  onItemClick: (elementToInsert: ElementToInsert) => React.MouseEventHandler
  onClickCloseButton?: React.MouseEventHandler
}

export interface ElementToInsert {
  elementToInsert: (uid: string) => JSXElementChild
  additionalImports: Imports
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
  const colorTheme = useColorTheme()
  const [selectedTab, setSelectedTab] = React.useState<'preferred' | 'all'>('preferred')
  const [filter, setFilter] = React.useState<string>('')

  const preferredComponentsToShow =
    filter.trim() === ''
      ? props.preferredComponents
      : props.preferredComponents.filter((v) =>
          v.name.toLocaleLowerCase().includes(filter.toLocaleLowerCase().trim()),
        )

  const allComponentsToShow =
    filter.trim() === ''
      ? props.allComponents
      : props.allComponents.filter((v) =>
          v.label.toLocaleLowerCase().includes(filter.toLocaleLowerCase().trim()),
        )

  const componentsToShow:
    | { type: 'component-descriptor'; value: Array<PreferredChildComponentDescriptor> }
    | { type: 'insert-menu-item'; value: Array<InsertMenuItemGroup> } =
    selectedTab === 'preferred'
      ? { type: 'component-descriptor', value: preferredComponentsToShow }
      : { type: 'insert-menu-item', value: allComponentsToShow }

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        gap: 0,
        width: '100%',
        height: '100%',
        padding: 0,
        backgroundColor: colorTheme.white.value,
        borderRadius: 10,
      }}
      data-testId={componentPickerTestIdForProp(props.insertionTargetName)}
    >
      <ComponentPickerTopSection
        targetProp={props.insertionTargetName}
        onFilterChange={setFilter}
        onSelectTab={setSelectedTab}
        onClickCloseButton={props.onClickCloseButton}
      />
      <div
        style={{
          width: '100%',
          borderWidth: '1px 0 0 0',
          borderStyle: 'solid',
          borderColor: colorTheme.subduedBorder.value,
        }}
      />
      <ComponentPickerComponentSection
        components={componentsToShow}
        onItemClick={props.onItemClick}
      />
    </div>
  )
})

interface ComponentPickerTopSectionProps {
  targetProp: string
  onFilterChange: (filter: string) => void
  onSelectTab: (tab: 'preferred' | 'all') => void
  onClickCloseButton?: React.MouseEventHandler
}

const ComponentPickerTopSection = React.memo((props: ComponentPickerTopSectionProps) => {
  const { targetProp, onFilterChange, onSelectTab, onClickCloseButton } = props
  const [selectedTab, setSelectedTabState] = React.useState<'preferred' | 'all'>('preferred')
  const setSelectedTab = React.useCallback(
    (tab: 'preferred' | 'all') => {
      setSelectedTabState(tab)
      onSelectTab(tab)
    },
    [onSelectTab],
  )
  const switchToPreferredTab = React.useCallback(
    () => setSelectedTab('preferred'),
    [setSelectedTab],
  )
  const switchToAllTab = React.useCallback(() => setSelectedTab('all'), [setSelectedTab])

  return (
    <div
      style={{
        padding: '16px 16px',
        display: 'flex',
        flexDirection: 'column',
        width: '100%',
        alignItems: 'flex-start',
        justifyContent: 'flex-start',
        gap: 10,
        height: 'max-content',
      }}
    >
      <div
        style={{
          width: '100%',
          display: 'flex',
          flexDirection: 'row',
          gap: 5,
          fontFamily: 'Utopian-Inter',
          fontWeight: 700,
          fontSize: '11px',
        }}
      >
        <div>Insert into</div>
        <PickerPropLabel targetProp={targetProp} />
        <div style={{ flexGrow: 100 }} />
        <PickerTabButton
          title={'Preferred'}
          isSelected={selectedTab === 'preferred'}
          onClick={switchToPreferredTab}
        />
        <PickerTabButton
          title={'All Components'}
          isSelected={selectedTab === 'all'}
          onClick={switchToAllTab}
        />
        <div style={{ flexGrow: 1 }} />
        {when(onClickCloseButton != null, () => (
          <div
            style={{ fontWeight: 600, cursor: 'pointer' }}
            onClick={onClickCloseButton}
            data-testId={componentPickerCloseButtonTestId}
          >
            X
          </div>
        ))}
      </div>
      <FilterBar onFilterChange={onFilterChange} />
    </div>
  )
})

interface PickerTabButtonProps {
  title: string
  isSelected: boolean
  onClick: React.MouseEventHandler
}

const PickerTabButton = React.memo((props: PickerTabButtonProps) => {
  const colorTheme = useColorTheme()
  const { title, isSelected, onClick } = props
  return (
    <div
      style={{
        fontWeight: 600,
        color: isSelected ? colorTheme.black.value : colorTheme.subduedForeground.value,
        cursor: 'pointer',
      }}
      onClick={onClick}
    >
      {title}
    </div>
  )
})

interface PickerPropLabelProps {
  targetProp: string
}

const PickerPropLabel = React.memo((props: PickerPropLabelProps) => {
  const { targetProp } = props
  const colorTheme = useColorTheme()

  return (
    <div
      style={{
        border: '1px solid rgb(0, 0, 0, 1)',
        borderRadius: 3,
        height: 21,
        contain: 'layout',
      }}
    >
      <div
        style={{
          border: '1px solid rgb(0, 0, 0, 1)',
          height: 21,
          borderRadius: 3,
          padding: 3,
          margin: -1, // Honestly I give up
          position: 'relative',
          left: 3,
          top: -2,
          lineHeight: 'normal',
          backgroundColor: colorTheme.white.value,
        }}
        data-testId={`${componentPickerTestIdForProp(targetProp)}-prop-field`}
      >
        {capitalize(targetProp)}
      </div>
    </div>
  )
})

interface FilterBarProps {
  onFilterChange: (filter: string) => void
}

const FilterBar = React.memo((props: FilterBarProps) => {
  const colorTheme = useColorTheme()
  const { onFilterChange } = props

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
        ;(e.target as HTMLInputElement).blur() // Not sure why I need the cast here
      } else if (e.key === 'Escape') {
        setFilter('')
        ;(e.target as HTMLInputElement).blur()
      }
      e.stopPropagation()
    },
    [setFilter],
  )
  const handleFilterChange = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      setFilter(e.target.value)
    },
    [setFilter],
  )

  return (
    <div
      style={{
        padding: '10px 6px',
        display: 'flex',
        flexDirection: 'row',
        width: '100%',
        height: 27,
        alignItems: 'center',
        justifyContent: 'flex-start',
        gap: 8,
        border: '1px solid #989999',
        borderColor: colorTheme.subduedBorder.value,
        borderRadius: 6,
      }}
    >
      <div
        style={{
          fontFamily: 'Utopian-Inter',
          fontStyle: 'normal',
          fontWeight: 500,
          fontSize: '11px',
          color: colorTheme.subduedForeground.value,
        }}
      >
        🔍
      </div>
      <input
        style={{
          fontFamily: 'Utopian-Inter',
          fontStyle: 'normal',
          fontWeight: 500,
          fontSize: '11px',
          width: '100%',
          border: 'none',
        }}
        placeholder='Filter...'
        autoComplete='off'
        spellCheck={false}
        onKeyDown={handleFilterKeydown}
        onChange={handleFilterChange}
        value={filter}
        data-testId={componentPickerFilterInputTestId}
      />
    </div>
  )
})

interface ComponentPickerComponentSectionProps {
  components:
    | { type: 'component-descriptor'; value: Array<PreferredChildComponentDescriptor> }
    | { type: 'insert-menu-item'; value: Array<InsertMenuItemGroup> }
  onItemClick: (elementToInsert: ElementToInsert) => React.MouseEventHandler
}

const ComponentPickerComponentSection = React.memo(
  (props: ComponentPickerComponentSectionProps) => {
    const { components, onItemClick } = props

    return (
      <div
        style={{
          padding: 16,
          display: 'flex',
          flexDirection: 'column',
          width: '100%',
          height: 'max-content',
          overflowY: 'auto',
          gap: 10,
        }}
      >
        {components.type === 'component-descriptor'
          ? components.value.map((comp) => {
              return (
                <ComponentPickerOption
                  key={`${comp.name}-label`}
                  component={{ type: 'component-descriptor', value: comp }}
                  onItemClick={onItemClick}
                />
              )
            })
          : components.value.map((comp) => {
              return (
                <ComponentPickerOption
                  key={`${comp.label}-label`}
                  component={{ type: 'insert-menu-item', value: comp }}
                  onItemClick={onItemClick}
                />
              )
            })}
      </div>
    )
  },
)

interface ComponentPickerOptionProps {
  component:
    | { type: 'component-descriptor'; value: PreferredChildComponentDescriptor }
    | { type: 'insert-menu-item'; value: InsertMenuItemGroup }
  onItemClick: (elementToInsert: ElementToInsert) => React.MouseEventHandler
}

function variantsForComponent(
  component:
    | { type: 'component-descriptor'; value: PreferredChildComponentDescriptor }
    | { type: 'insert-menu-item'; value: InsertMenuItemGroup },
): ComponentInfo[] {
  switch (component.type) {
    case 'component-descriptor':
      return [
        componentInfo(
          '(empty)',
          () => jsxElementWithoutUID(component.value.name, [], []),
          defaultImportsForComponentModule(component.value.name, component.value.moduleName),
        ),
        ...(component.value.variants ?? []),
      ]
    case 'insert-menu-item':
      return component.value.options.map((v) =>
        componentInfo(v.label, v.value.element, v.value.importsToAdd),
      )
    default:
      assertNever(component)
  }
}

const ComponentPickerOption = React.memo((props: ComponentPickerOptionProps) => {
  const colorTheme = useColorTheme()
  const { component, onItemClick } = props

  const variants = variantsForComponent(component)

  const name =
    component.type === 'component-descriptor' ? component.value.name : component.value.label

  return (
    <div
      style={{
        backgroundColor: colorTheme.bg2.value,
        borderRadius: 5,
        display: 'flex',
        flexDirection: 'column',
        width: '100%',
        height: 'max-content',
        gap: 5,
        padding: 10,
        fontFamily: 'Utopian-Inter',
        fontWeight: 500,
        fontSize: '11px',
      }}
      data-testId={componentPickerOptionTestId(name)}
    >
      <div style={{ fontWeight: 700 }}>{name}</div>
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          width: '100%',
          height: 'max-content',
          alignItems: 'center',
          justifyContent: 'flex-start',
          flexWrap: 'wrap',
          gap: 9,
        }}
      >
        {variants?.map((v) => (
          <ComponentPickerVariant
            key={`${name}-${v.insertMenuLabel}`}
            componentName={name}
            variant={v}
            onItemClick={onItemClick}
          />
        ))}
      </div>
    </div>
  )
})

interface ComponentPickerVariantProps {
  componentName: string
  variant: ComponentInfo
  onItemClick: (elementToInsert: ElementToInsert) => React.MouseEventHandler
}

const ComponentPickerVariant = React.memo((props: ComponentPickerVariantProps) => {
  const colorTheme = useColorTheme()
  const { onItemClick, variant, componentName } = props

  return (
    <div
      onClick={onItemClick({
        elementToInsert: (uid) => elementFromInsertMenuItem(variant.elementToInsert(), uid),
        additionalImports: variant.importsToAdd,
      })}
      css={{
        backgroundColor: colorTheme.bg5.value,
        paddingTop: 5,
        paddingRight: 5,
        paddingBottom: 5,
        paddingLeft: 5,
        borderTopLeftRadius: 3,
        borderTopRightRadius: 3,
        borderBottomRightRadius: 3,
        borderBottomLeftRadius: 3,
        color:
          variant.insertMenuLabel === '(empty)'
            ? colorTheme.subduedForeground.value
            : colorTheme.black.value,
        '&:hover': {
          backgroundColor: colorTheme.dynamicBlue10.value,
        },
        cursor: 'pointer',
      }}
      data-testId={componentPickerOptionTestId(componentName, variant.insertMenuLabel)}
    >
      {variant.insertMenuLabel}
    </div>
  )
})
