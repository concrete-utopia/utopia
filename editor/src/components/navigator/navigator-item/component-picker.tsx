/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { useColorTheme } from '../../../uuiui'
import { capitalize } from '../../../core/shared/string-utils'
import { type PreferredChildComponentDescriptor } from '../../custom-code/internal-property-controls'
import { type JSXElementChild } from '../../../core/shared/element-template'
import { type Imports } from '../../../core/shared/project-file-types'
import { elementFromInsertMenuItem } from '../../editor/insert-callbacks'
import { type ComponentInfo } from '../../custom-code/code-file'

export interface ComponentPickerProps {
  insertionTargetName: string
  preferredComponents: PreferredChildComponentDescriptor[]
  allComponents: PreferredChildComponentDescriptor[]
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

  const unfilteredComponentsToShow =
    selectedTab === 'preferred' ? props.preferredComponents : props.allComponents
  const componentsToShow =
    filter.trim() === ''
      ? unfilteredComponentsToShow
      : unfilteredComponentsToShow.filter((v) =>
          v.name.toLocaleLowerCase().includes(filter.toLocaleLowerCase().trim()),
        )

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
        {onClickCloseButton != null ? (
          <div
            style={{ fontWeight: 600 }}
            onClick={onClickCloseButton}
            data-testId={componentPickerCloseButtonTestId}
          >
            X
          </div>
        ) : null}
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
  components: PreferredChildComponentDescriptor[]
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
          gap: 10,
        }}
      >
        {components.map((componentDescriptor) => {
          return (
            <ComponentPickerOption
              key={`${componentDescriptor.name}-label`}
              componentDescriptor={componentDescriptor}
              onItemClick={onItemClick}
            />
          )
        })}
      </div>
    )
  },
)

interface ComponentPickerOptionProps {
  componentDescriptor: PreferredChildComponentDescriptor
  onItemClick: (elementToInsert: ElementToInsert) => React.MouseEventHandler
}

const ComponentPickerOption = React.memo((props: ComponentPickerOptionProps) => {
  const colorTheme = useColorTheme()
  const { componentDescriptor, onItemClick } = props

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
      data-testId={componentPickerOptionTestId(componentDescriptor.name)}
    >
      <div style={{ fontWeight: 700 }}>{componentDescriptor.name}</div>
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
        {componentDescriptor.variants?.map((v) => (
          <ComponentPickerVariant
            key={`${componentDescriptor.name}-${v.insertMenuLabel}`}
            componentName={componentDescriptor.name}
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
      }}
      data-testId={componentPickerOptionTestId(componentName, variant.insertMenuLabel)}
    >
      {variant.insertMenuLabel}
    </div>
  )
})
