/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import { FlexRow, UtopiaTheme } from '../../uuiui'
import { PackageDetails } from './dependency-list'
import { DependencyListItemEditing, DependencySearchSelect } from './dependency-list-input-field'
import { DependencyListItem } from './dependency-list-item'

interface DependencyListItemsProps {
  packages: Array<PackageDetails>
  editingLocked: boolean
  newlyLoadedItems: Array<string>
  dependencyBeingEdited: string | null
  openDependencyEditField: (dependencyName: string, openVersionInput?: boolean) => void
  updateDependencyToLatestVersion: (dependencyName: string) => void
  removeDependency: (key: string) => void
  addDependency: (packageName: string | null, packageVersion: string | null) => void
  handleAbandonEdit: () => void
}

export const DependencyListItems: React.FunctionComponent<DependencyListItemsProps> = ({
  packages,
  editingLocked,
  newlyLoadedItems,
  dependencyBeingEdited,
  openDependencyEditField,
  updateDependencyToLatestVersion,
  removeDependency,
  addDependency,
  handleAbandonEdit,
}) => {
  return (
    <React.Fragment>
      <FlexRow
        style={{ height: UtopiaTheme.layout.rowHeight.smaller, paddingLeft: 8, paddingRight: 8 }}
      >
        <DependencySearchSelect addDependency={addDependency} />
      </FlexRow>
      {[
        ...packages.map((packageDetails) => {
          const isNewlyLoaded = newlyLoadedItems.indexOf(packageDetails.name) >= 0
          return dependencyBeingEdited === packageDetails.name ? (
            <DependencyListItemEditing
              key={packageDetails.name}
              addDependency={addDependency}
              handleAbandonEdit={handleAbandonEdit}
              editedPackageName={packageDetails.name}
              editedPackageVersion={packageDetails.version}
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
      ]}
    </React.Fragment>
  )
}
