/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import type CreatableSelect from 'react-select/creatable'
import Utils from '../../utils/utils'
import type { SelectOption } from '../inspector/controls/select-control'
import { SelectControl } from '../inspector/controls/select-control'
import { top1000NPMPackagesOptions } from './top1000NPMPackagesOptions'
import { getControlStyles } from '../inspector/common/control-styles'
import { Tooltip, StringInput, OnClickOutsideHOC, UtopiaTheme, useColorTheme } from '../../uuiui'

interface DependencySearchSelectProps {
  addDependency: (packageName: string | null, packageVersion: string | null) => void
}

export const DependencySearchSelect = React.forwardRef(
  (
    { addDependency }: DependencySearchSelectProps,
    dependencyVersionInputRef: React.Ref<HTMLInputElement>,
  ) => {
    const dependencyNameSelectRef = React.useRef<CreatableSelect<SelectOption>>(null)
    const [stateEditedPackageName, setStateEditedPackageName] = React.useState<string>('')

    const onSubmitValue = React.useCallback(
      (newValue: string) => {
        setStateEditedPackageName(newValue)
        addDependency(newValue, null)
        setStateEditedPackageName('')
      },
      [setStateEditedPackageName, addDependency],
    )

    return (
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          alignItems: 'center',
          height: UtopiaTheme.layout.rowHeight.smaller,
          gap: 8,
          flexGrow: 1,
        }}
        key='addeditedPackage'
      >
        <SelectControl
          value={Utils.defaultIfNull<string>('', stateEditedPackageName)}
          options={top1000NPMPackagesOptions}
          id='edit-package-name'
          key='edit-package-name'
          testId='edit-package-name'
          onSubmitValue={onSubmitValue}
          controlStatus='simple'
          controlStyles={getControlStyles('simple')}
          style={{
            flexGrow: 1,
          }}
          DEPRECATED_controlOptions={{
            placeholder: 'Search for npm modules',
            focusOnMount: true,
            creatable: true,
            selectCreatableRef: dependencyNameSelectRef,
            onKeyDown: (e) => {
              const select = dependencyNameSelectRef.current
              if (e.key === 'Enter' && select != null) {
                if ((select.state as any).menuIsOpen as boolean) {
                  e.stopPropagation()
                } else {
                  addDependency(stateEditedPackageName, null)
                }
              }
            },
          }}
        />
      </div>
    )
  },
)

interface DependencyListItemEditingProps {
  addDependency: (packageName: string | null, packageVersion: string | null) => void
  handleAbandonEdit: () => void
  editedPackageName: string | null
  editedPackageVersion: string | null
}

export const DependencyListItemEditing = React.forwardRef(
  (
    {
      addDependency,
      handleAbandonEdit,
      editedPackageName,
      editedPackageVersion,
    }: DependencyListItemEditingProps,
    dependencyVersionInputRef: React.Ref<HTMLInputElement>,
  ) => {
    const colorTheme = useColorTheme()
    const [stateEditedPackageName, setStateEditedPackageName] = React.useState<string>(
      editedPackageName ?? '',
    )
    const [stateEditedPackageVersion, setStateEditedPackageVersion] = React.useState<string>(
      editedPackageVersion ?? '',
    )

    const onDependencyVersionChange = React.useCallback(
      (e: React.ChangeEvent<HTMLInputElement>) => {
        setStateEditedPackageVersion(e.target.value)
      },
      [],
    )

    const onKeyDown = React.useCallback(
      (e: React.KeyboardEvent) => {
        if (e.key === 'Enter') {
          e.preventDefault()
          addDependency(stateEditedPackageName, stateEditedPackageVersion)
          setStateEditedPackageName('')
        } else if (e.key === 'Escape') {
          e.preventDefault()
          handleAbandonEdit()
        }
      },
      [addDependency, handleAbandonEdit, stateEditedPackageName, stateEditedPackageVersion],
    )

    return (
      <OnClickOutsideHOC onClickOutside={handleAbandonEdit}>
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            height: UtopiaTheme.layout.rowHeight.smaller,
            backgroundColor: colorTheme.secondaryBackground.value,
            borderRadius: 2,
            paddingLeft: 8,
            paddingRight: 8,
            gap: 8,
          }}
          key='editedPackageRow'
        >
          <div style={{ flexGrow: 1, display: 'flex', alignItems: 'center' }}>
            {Utils.defaultIfNull<string>('', stateEditedPackageName)}
          </div>

          <Tooltip
            title={
              <span>
                Version Number (e.g. <span style={{ fontFamily: 'Monaco, monospace' }}>1.0.0</span>)
              </span>
            }
          >
            <StringInput
              focusOnMount={true}
              value={Utils.defaultIfNull<string>('', stateEditedPackageVersion)}
              id='add-package-version'
              testId=''
              key='add-package-version'
              onChange={onDependencyVersionChange}
              placeholder='0.0.0'
              ref={dependencyVersionInputRef}
              style={{
                flexGrow: 0,
                flexShrink: 0,
                flexBasis: 48,
              }}
              onKeyDown={onKeyDown}
            />
          </Tooltip>
        </div>
      </OnClickOutsideHOC>
    )
  },
)
