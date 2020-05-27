/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { PackageDetails } from './dependency-list'
import { DependencyListInputField } from './dependency-list-input-field'
import { DependencyListItem } from './dependency-list-item'

interface DependencyListItemsProps {
  packages: Array<PackageDetails>
  openVersionInput: boolean
  editingLocked: boolean
  newlyLoadedItems: Array<string>
  dependencyBeingEdited: string | null
  openDependencyEditField: (dependencyName: string, openVersionInput?: boolean) => void
  updateDependencyToLatestVersion: (dependencyName: string) => void
  removeDependency: (key: string) => void
  addDependency: (packageName: string | null, packageVersion: string | null) => void
  closeField: () => void
  showInsertField: boolean
}

export const DependencyListItems: React.FunctionComponent<DependencyListItemsProps> = ({
  packages,
  openVersionInput,
  editingLocked,
  newlyLoadedItems,
  dependencyBeingEdited,
  openDependencyEditField,
  updateDependencyToLatestVersion,
  removeDependency,
  addDependency,
  closeField,
  showInsertField,
}) => {
  return (
    <React.Fragment>
      {[
        ...packages.map((packageDetails) => {
          const isNewlyLoaded = newlyLoadedItems.indexOf(packageDetails.name) >= 0
          return dependencyBeingEdited === packageDetails.name ? (
            <DependencyListInputField
              key={packageDetails.name}
              showVersionField={true}
              addDependency={addDependency}
              closeField={closeField}
              editedPackageName={packageDetails.name}
              editedPackageVersion={packageDetails.version}
              openVersionInput={openVersionInput}
            />
          ) : (
            <DependencyListItem
              key={packageDetails.name}
              isNewlyLoaded={isNewlyLoaded}
              packageDetails={packageDetails}
              openDependencyEditField={openDependencyEditField}
              editingLocked={editingLocked}
              updateDependencyToLatestVersion={updateDependencyToLatestVersion}
              removeDependency={removeDependency}
            />
          )
        }),
        !showInsertField ? null : (
          <DependencyListInputField
            showVersionField={false}
            addDependency={addDependency}
            closeField={closeField}
            editedPackageName={null}
            editedPackageVersion={null}
            openVersionInput={false}
          />
        ),
      ]}
    </React.Fragment>
  )
}
