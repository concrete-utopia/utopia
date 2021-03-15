import { isUtopiaJSXComponent, jsxElement, JSXElement } from '../../core/shared/element-template'
import {
  isResolvedNpmDependency,
  PackageStatus,
  PackageStatusMap,
  PossiblyUnversionedNpmDependency,
} from '../../core/shared/npm-dependency-types'
import {
  Imports,
  isParsedTextFile,
  isParseSuccess,
  isTextFile,
  ProjectFile,
} from '../../core/shared/project-file-types'
import { getThirdPartyComponents } from '../../core/third-party/third-party-components'
import { addImport, emptyImports } from '../../core/workers/common/project-file-utils'
import { SelectOption } from '../../uuiui-deps'
import { ProjectContentTreeRoot, walkContentsTree } from '../assets'
import { PropertyControlsInfo } from '../custom-code/code-file'
import { getExportedComponentImports } from '../editor/export-utils'

export interface InsertableComponent {
  importsToAdd: Imports
  element: JSXElement
  name: string
}

export function insertableComponent(
  importsToAdd: Imports,
  element: JSXElement,
  name: string,
): InsertableComponent {
  return {
    importsToAdd: importsToAdd,
    element: element,
    name: name,
  }
}

export interface InsertableComponentGroupHTML {
  type: 'HTML_GROUP'
}

export function insertableComponentGroupHTML(): InsertableComponentGroupHTML {
  return {
    type: 'HTML_GROUP',
  }
}

export interface InsertableComponentGroupProjectComponent {
  type: 'PROJECT_COMPONENT_GROUP'
  path: string
}

export function insertableComponentGroupProjectComponent(
  path: string,
): InsertableComponentGroupProjectComponent {
  return {
    type: 'PROJECT_COMPONENT_GROUP',
    path: path,
  }
}

export interface InsertableComponentGroupProjectDependency {
  type: 'PROJECT_DEPENDENCY_GROUP'
  dependencyName: string
  dependencyStatus: PackageStatus
}

export function insertableComponentGroupProjectDependency(
  dependencyName: string,
  dependencyStatus: PackageStatus,
): InsertableComponentGroupProjectDependency {
  return {
    type: 'PROJECT_DEPENDENCY_GROUP',
    dependencyName: dependencyName,
    dependencyStatus: dependencyStatus,
  }
}

export type InsertableComponentGroupType =
  | InsertableComponentGroupHTML
  | InsertableComponentGroupProjectComponent
  | InsertableComponentGroupProjectDependency

export interface InsertableComponentGroup {
  source: InsertableComponentGroupType
  insertableComponents: Array<InsertableComponent>
}

export function insertableComponentGroup(
  source: InsertableComponentGroupType,
  insertableComponents: Array<InsertableComponent>,
): InsertableComponentGroup {
  return {
    source: source,
    insertableComponents: insertableComponents,
  }
}

export function getInsertableGroupLabel(insertableType: InsertableComponentGroupType): string {
  switch (insertableType.type) {
    case 'HTML_GROUP':
      return 'HTML Elements'
    case 'PROJECT_DEPENDENCY_GROUP':
      return insertableType.dependencyName
    case 'PROJECT_COMPONENT_GROUP':
      return insertableType.path
    default:
      const _exhaustiveCheck: never = insertableType
      throw new Error(`Unhandled insertable type ${JSON.stringify(insertableType)}`)
  }
}

export function getInsertableGroupPackageStatus(
  insertableType: InsertableComponentGroupType,
): PackageStatus {
  switch (insertableType.type) {
    case 'HTML_GROUP':
      return 'loaded'
    case 'PROJECT_DEPENDENCY_GROUP':
      return insertableType.dependencyStatus
    case 'PROJECT_COMPONENT_GROUP':
      return 'loaded'
    default:
      const _exhaustiveCheck: never = insertableType
      throw new Error(`Unhandled insertable type ${JSON.stringify(insertableType)}`)
  }
}

export function getDependencyStatus(
  packageStatus: PackageStatusMap,
  propertyControlsInfo: PropertyControlsInfo,
  dependencyName: string,
  defaultStatus: PackageStatus,
): PackageStatus {
  const regularStatus = packageStatus[dependencyName]?.status
  switch (regularStatus) {
    case null:
      return defaultStatus
    case 'loaded':
      if (dependencyName in propertyControlsInfo) {
        return 'loaded'
      } else {
        return 'loading'
      }
    default:
      return regularStatus
  }
}

const basicHTMLEntities = ['div', 'span', 'button', 'input']
const emptyImportsValue = emptyImports()

export function getComponentGroups(
  packageStatus: PackageStatusMap,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
  dependencies: Array<PossiblyUnversionedNpmDependency>,
  originatingPath: string,
): Array<InsertableComponentGroup> {
  let result: Array<InsertableComponentGroup> = []
  // Add entries for the exported components of files within the project.
  walkContentsTree(projectContents, (fullPath: string, file: ProjectFile) => {
    if (isTextFile(file)) {
      const possibleExportedComponents = getExportedComponentImports(
        originatingPath,
        fullPath,
        file.fileContents.parsed,
      )
      if (possibleExportedComponents != null) {
        const insertableComponents = possibleExportedComponents.map((exportedComponent) => {
          return insertableComponent(
            exportedComponent.importsToAdd,
            jsxElement(exportedComponent.listingName, [], []),
            exportedComponent.listingName,
          )
        })
        result.push(
          insertableComponentGroup(
            insertableComponentGroupProjectComponent(fullPath),
            insertableComponents,
          ),
        )
      }
    }
  })

  // Add entries for basic HTML entities.
  result.push(
    insertableComponentGroup(
      insertableComponentGroupHTML(),
      basicHTMLEntities.map((basicHTMLEntity) => {
        return insertableComponent(
          addImport('react', null, [], 'React', emptyImportsValue),
          jsxElement(basicHTMLEntity, [], []),
          basicHTMLEntity,
        )
      }),
    ),
  )

  // Add entries for dependencies of the project.
  for (const dependency of dependencies) {
    if (isResolvedNpmDependency(dependency)) {
      const componentDescriptor = getThirdPartyComponents(dependency.name, dependency.version)
      if (componentDescriptor != null) {
        const dependencyStatus = getDependencyStatus(
          packageStatus,
          propertyControlsInfo,
          dependency.name,
          'loaded',
        )
        const components = dependencyStatus === 'loaded' ? componentDescriptor.components : []
        const insertableComponents = components.map((component) =>
          insertableComponent(component.importsToAdd, component.element, component.name),
        )
        result.push(
          insertableComponentGroup(
            insertableComponentGroupProjectDependency(dependency.name, dependencyStatus),
            insertableComponents,
          ),
        )
      }
    }
  }

  return result
}

export function getComponentGroupsAsSelectOptions(
  packageStatus: PackageStatusMap,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
  dependencies: Array<PossiblyUnversionedNpmDependency>,
  originatingPath: string,
): Array<SelectOption> {
  const insertableGroups = getComponentGroups(
    packageStatus,
    propertyControlsInfo,
    projectContents,
    dependencies,
    originatingPath,
  )
  let result: Array<SelectOption> = []
  for (const insertableGroup of insertableGroups) {
    // If there's nothing in the group, don't include it otherwise we end up with a broken
    // looking entry in the drop down.
    if (insertableGroup.insertableComponents.length > 0) {
      const componentOptions: Array<SelectOption> = insertableGroup.insertableComponents.map(
        (component) => {
          return {
            label: component.name,
            value: component,
          }
        },
      )
      result.push({
        label: getInsertableGroupLabel(insertableGroup.source),
        value: null,
        options: componentOptions,
      })
    }
  }
  return result
}
