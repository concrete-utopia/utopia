import { PropertyControls } from 'utopia-api'
import {
  defaultPropertiesForComponent,
  defaultPropertiesForComponentInFile,
  parsedPropertyControlsForComponentInFile,
} from '../../core/property-controls/property-controls-utils'
import { flatMapEither, foldEither, right } from '../../core/shared/either'
import {
  JSXAttributes,
  jsxAttributesEntry,
  jsxAttributeValue,
  jsxElementWithoutUID,
  JSXElementWithoutUID,
} from '../../core/shared/element-template'
import { dropFileExtension } from '../../core/shared/file-utils'
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
import { emptyComments } from '../../core/workers/parser-printer/parser-printer-comments'
import { SelectOption } from '../../uuiui-deps'
import { ProjectContentTreeRoot, walkContentsTree } from '../assets'
import { PropertyControlsInfo } from '../custom-code/code-file'
import { getExportedComponentImports } from '../editor/export-utils'

export type StylePropOption = 'do-not-add' | 'add-size'

export interface InsertableComponent {
  importsToAdd: Imports
  element: JSXElementWithoutUID
  name: string
  stylePropOptions: Array<StylePropOption>
}

export function insertableComponent(
  importsToAdd: Imports,
  element: JSXElementWithoutUID,
  name: string,
  stylePropOptions: Array<StylePropOption>,
): InsertableComponent {
  return {
    importsToAdd: importsToAdd,
    element: element,
    name: name,
    stylePropOptions: stylePropOptions,
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

const doNotAddStyleProp: Array<StylePropOption> = ['do-not-add']
const addSizeAndNotStyleProp: Array<StylePropOption> = ['do-not-add', 'add-size']

export function stylePropOptionsForPropertyControls(
  propertyControls: PropertyControls,
): Array<StylePropOption> {
  if ('style' in propertyControls) {
    return addSizeAndNotStyleProp
  } else {
    return doNotAddStyleProp
  }
}

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
          const pathWithoutExtension = dropFileExtension(fullPath)
          const { defaultProps, parsedControls } = defaultPropertiesForComponentInFile(
            exportedComponent.listingName,
            pathWithoutExtension,
            propertyControlsInfo,
          )

          // Drill down into the parsed controls to see if this has an appropriate style object entry.
          const stylePropOptions: Array<StylePropOption> = foldEither(
            () => {
              return doNotAddStyleProp
            },
            (propertyControls) => {
              if ('style' in propertyControls) {
                return foldEither(
                  () => {
                    return doNotAddStyleProp
                  },
                  (controlDescription) => {
                    switch (controlDescription.type) {
                      case 'styleobject':
                        return addSizeAndNotStyleProp
                      default:
                        return doNotAddStyleProp
                    }
                  },
                  propertyControls['style'],
                )
              } else {
                return doNotAddStyleProp
              }
            },
            parsedControls,
          )
          let attributes: JSXAttributes = []
          for (const key of Object.keys(defaultProps)) {
            attributes.push(
              jsxAttributesEntry(
                key,
                jsxAttributeValue(defaultProps[key], emptyComments),
                emptyComments,
              ),
            )
          }
          return insertableComponent(
            exportedComponent.importsToAdd,
            jsxElementWithoutUID(exportedComponent.listingName, attributes, []),
            exportedComponent.listingName,
            stylePropOptions,
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
          jsxElementWithoutUID(basicHTMLEntity, [], []),
          basicHTMLEntity,
          addSizeAndNotStyleProp,
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
        const insertableComponents = components.map((component) => {
          let stylePropOptions: Array<StylePropOption> = doNotAddStyleProp
          // Drill down to see if this dependency component has a style object entry
          // against style.
          if (component.propertyControls != null) {
            if ('style' in component.propertyControls) {
              const styleControls = component.propertyControls['style']
              if (styleControls?.type === 'styleobject') {
                stylePropOptions = addSizeAndNotStyleProp
              }
            }
          }
          return insertableComponent(
            component.importsToAdd,
            component.element,
            component.name,
            stylePropOptions,
          )
        })
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
