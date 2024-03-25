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
  const switchToPreferredTab = React.useCallback(() => setSelectedTab('preferred'), [])
  const switchToAllTab = React.useCallback(() => setSelectedTab('all'), [])
  const [filter, setFilter] = React.useState<string>('')
  const handleFilterKeydown = React.useCallback((e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Enter') {
      ;(e.target as HTMLInputElement).blur()
    } else if (e.key === 'Escape') {
      setFilter('')
      ;(e.target as HTMLInputElement).blur()
    }
    e.stopPropagation()
  }, [])
  const handleFilterChange = React.useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    setFilter(e.target.value)
  }, [])

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
              data-testId={`${componentPickerTestIdForProp(props.insertionTargetName)}-prop-field`}
            >
              {capitalize(props.insertionTargetName)}
            </div>
          </div>
          <div style={{ flexGrow: 100 }} />
          <div
            style={{
              fontWeight: 600,
              color:
                selectedTab === 'preferred'
                  ? colorTheme.black.value
                  : colorTheme.subduedForeground.value,
            }}
            onMouseDown={switchToPreferredTab}
          >
            Preferred
          </div>
          <div
            style={{
              fontWeight: 600,
              color:
                selectedTab === 'all' ? colorTheme.black.value : colorTheme.subduedForeground.value,
            }}
            onMouseDown={switchToAllTab}
          >
            All Components
          </div>
          <div style={{ flexGrow: 1 }} />
          {props.onClickCloseButton != null ? (
            <div
              style={{ fontWeight: 600 }}
              onClick={props.onClickCloseButton}
              data-testId={componentPickerCloseButtonTestId}
            >
              X
            </div>
          ) : null}
        </div>
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
      </div>
      <div
        style={{
          width: '100%',
          borderWidth: '1px 0 0 0',
          borderStyle: 'solid',
          borderColor: colorTheme.subduedBorder.value,
        }}
      />
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
        {componentsToShow.map((option, idx) => {
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
              key={`${idx}-label`}
              data-testId={componentPickerOptionTestId(option.name)}
            >
              <div style={{ fontWeight: 700 }}>{option.name}</div>
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
                {option.variants?.map((v, i) => (
                  <div
                    key={`${idx}-${v.insertMenuLabel}`}
                    onClick={props.onItemClick({
                      elementToInsert: (uid) => elementFromInsertMenuItem(v.elementToInsert(), uid),
                      additionalImports: v.importsToAdd,
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
                        v.insertMenuLabel === '(empty)'
                          ? colorTheme.subduedForeground.value
                          : colorTheme.black.value,
                      '&:hover': {
                        backgroundColor: colorTheme.dynamicBlue10.value,
                      },
                    }}
                    data-testId={componentPickerOptionTestId(option.name, v.insertMenuLabel)}
                  >
                    {v.insertMenuLabel}
                  </div>
                ))}
              </div>
            </div>
          )
        })}
      </div>
    </div>
  )
})
