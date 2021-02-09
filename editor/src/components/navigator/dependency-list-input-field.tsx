/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import CreatableSelect from 'react-select/creatable'
import Utils from '../../utils/utils'
import {
  BasicSelectControl,
  SelectOption,
  SelectControl,
} from '../inspector/controls/select-control'
import { top1000NPMPackagesOptions } from './top1000NPMPackagesOptions'
import { getControlStyles } from '../inspector/common/control-status'
import { Tooltip, StringInput, FunctionIcons } from '../../uuiui'

interface DependencyListInputFieldProps {
  showVersionField: boolean
  addDependency: (packageName: string | null, packageVersion: string | null) => void
  closeField: () => void
  editedPackageName: string | null
  editedPackageVersion: string | null
  openVersionInput: boolean
}

export const DependencyListInputField = React.forwardRef(
  (
    {
      showVersionField,
      addDependency,
      closeField,
      editedPackageName,
      editedPackageVersion,
      openVersionInput,
    }: DependencyListInputFieldProps,
    dependencyVersionInputRef: React.Ref<HTMLInputElement>,
  ) => {
    const dependencyNameSelectRef = React.useRef<CreatableSelect<SelectOption>>(null)

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
        } else if (e.key === 'Escape') {
          e.preventDefault()
          closeField()
        }
      },
      [addDependency, closeField, stateEditedPackageName, stateEditedPackageVersion],
    )

    const onConfirmClick = React.useCallback(
      () => addDependency(stateEditedPackageName, stateEditedPackageVersion),
      [addDependency, stateEditedPackageName, stateEditedPackageVersion],
    )

    const onSubmitValue = React.useCallback(
      (newValue) => {
        setStateEditedPackageName(newValue)
        addDependency(newValue, null)
      },
      [setStateEditedPackageName, addDependency],
    )

    return (
      <div
        style={{
          padding: '0 8px',
          display: 'flex',
          flexDirection: 'row',
          alignItems: 'center',
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
            marginRight: 8,
          }}
          DEPRECATED_controlOptions={{
            focusOnMount: !openVersionInput,
            creatable: true,
            selectCreatableRef: dependencyNameSelectRef,
            onKeyDown: (e) => {
              const select = dependencyNameSelectRef.current
              if (e.key === 'Enter' && select != null) {
                if ((select.state as any).menuIsOpen) {
                  e.stopPropagation()
                } else {
                  addDependency(stateEditedPackageName, stateEditedPackageVersion)
                }
              }
            },
          }}
        />
        {showVersionField ? (
          <Tooltip
            title={
              <span>
                Version Number (e.g. <span style={{ fontFamily: 'Monaco, monospace' }}>1.0.0</span>)
              </span>
            }
          >
            <StringInput
              focusOnMount={openVersionInput}
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
                flexBasis: 64,
                marginRight: 8,
                fontFamily: 'Monaco, monospace',
                fontStyle: 'normal',
                fontSize: 9,
              }}
              onKeyDown={onKeyDown}
            />
          </Tooltip>
        ) : null}
        <FunctionIcons.Confirm
          style={{
            flexGrow: 0,
            flexShrink: 0,
            marginRight: 8,
          }}
          onClick={onConfirmClick}
        />
        <FunctionIcons.Close
          style={{
            flexGrow: 0,
            flexShrink: 0,
          }}
          onClick={closeField}
        />
      </div>
    )
  },
)
