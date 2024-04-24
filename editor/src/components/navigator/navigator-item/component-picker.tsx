/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { Icons, UtopiaTheme, useColorTheme } from '../../../uuiui'
import { dark } from '../../../uuiui/styles/theme/dark'
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
import { UIGridRow } from '../../../components/inspector/widgets/ui-grid-row'
import { FlexRow } from 'utopia-api'

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
        color: dark.fg3.value,
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
      {/* fake!! fraud!! */}
      <FlexRow
        css={{
          marginLeft: 8,
          marginRight: 8,
          borderRadius: 4,
          // indentation!
          paddingLeft: 8,
          color: '#EEE',
          '&:hover': {
            background: '#007aff',
            color: 'white',
          },
        }}
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
          <Icons.ComponentInstance
            color='white'
            style={{ transformOrigin: 'center center', transform: 'scale(.8)' }}
          />
          <label>Column</label>
        </UIGridRow>
      </FlexRow>
      <FlexRow
        css={{
          marginLeft: 8,
          marginRight: 8,
          borderRadius: 4,
          // indentation!
          paddingLeft: 17,
          color: '#EEE',
          '&:hover': {
            background: '#007aff',
            color: 'white',
          },
        }}
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
          {/* TODO BRANCH swap out for the new 12x12 navigator icons, instead of transform scale */}
          <Icons.ComponentInstance
            color='white'
            style={{ transformOrigin: 'center center', transform: 'scale(.8)' }}
          />
          <label>Column</label>
        </UIGridRow>
      </FlexRow>
      <FlexRow
        css={{
          marginLeft: 8,
          marginRight: 8,
          borderRadius: 4,
          // indentation!
          paddingLeft: 17,
          color: '#EEE',
          '&:hover': {
            background: '#007aff',
            color: 'white',
          },
        }}
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
          <Icons.ComponentInstance
            color='white'
            style={{ transformOrigin: 'center center', transform: 'scale(.8)' }}
          />
          <label>Column</label>
        </UIGridRow>
      </FlexRow>

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
        padding: '8px 8px',
        display: 'flex',
        flexDirection: 'column',
      }}
    >
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
    <input
      css={{
        height: 25,
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
    />
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
    // TODO BRANCH
    // this should return a fragment, consisting of the main element, and any variants of it with an indented label
    // basically what we're doing in the page selector!
    // if there are any variants, put an 8px margin underneath the last one
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
        // TODO BRANCH this needs to go since (empty) should just be the component name
        // once we've stratified this
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
