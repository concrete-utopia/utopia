import { ImportType, PropertyControls } from 'utopia-api'
import { URL_HASH } from '../../common/env-vars'
import { parsePropertyControls } from '../../core/property-controls/property-controls-parser'
import {
  defaultPropertiesForComponentInFile,
  getDefaultPropsFromParsedControls,
  hasStyleControls,
} from '../../core/property-controls/property-controls-utils'
import { mapArrayToDictionary } from '../../core/shared/array-utils'
import {
  eitherToMaybe,
  flatMapEither,
  foldEither,
  forEachRight,
  right,
} from '../../core/shared/either'
import {
  emptyComments,
  isIntrinsicElementFromString,
  JSXAttributes,
  jsxAttributesEntry,
  jsxAttributeValue,
  jsxElementName,
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
import { mapToArray, mapValues } from '../../core/shared/object-utils'
import {
  importDetailsFromImportOption,
  Imports,
  isParsedTextFile,
  isParseSuccess,
  isTextFile,
  ProjectFile,
} from '../../core/shared/project-file-types'
import { getDefaultPropsAsAttributes } from '../../core/third-party/shared'
import { addImport, emptyImports } from '../../core/workers/common/project-file-utils'
import { SelectOption } from '../../uuiui-deps'
import { ProjectContentTreeRoot, walkContentsTree } from '../assets'
import {
  PropertyControlsInfo,
  ComponentDescriptor,
  ComponentDescriptorsForFile,
} from '../custom-code/code-file'
import { getExportedComponentImports } from '../editor/export-utils'

export type StylePropOption = 'do-not-add' | 'add-size'
export type WrapContentOption = 'wrap-content' | 'do-now-wrap-content'

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

const emptyImportsValue = emptyImports()

const doNotAddStyleProp: Array<StylePropOption> = ['do-not-add']
const addSizeAndNotStyleProp: Array<StylePropOption> = ['do-not-add', 'add-size']

const stockHTMLPropertyControls: PropertyControls = {
  style: {
    control: 'style-controls',
  },
}

function makeHTMLDescriptor(
  tag: string,
  extraPropertyControls: PropertyControls,
): ComponentDescriptor {
  const propertyControls: PropertyControls = {
    ...stockHTMLPropertyControls,
    ...extraPropertyControls,
  }
  const defaultValues = getDefaultPropsAsAttributes(propertyControls)
  return {
    propertyControls: parsePropertyControls(propertyControls),
    componentInfo: {
      importsToAdd: {
        react: {
          importedAs: 'React',
          importedFromWithin: [],
          importedWithName: null,
        },
      },
      elementToInsert: jsxElementWithoutUID(tag, defaultValues, []),
    },
  }
}

const basicHTMLElementsDescriptors = {
  div: makeHTMLDescriptor('div', {}),
  span: makeHTMLDescriptor('span', {}),
  h1: makeHTMLDescriptor('h1', {}),
  h2: makeHTMLDescriptor('h2', {}),
  p: makeHTMLDescriptor('p', {}),
  button: makeHTMLDescriptor('button', {}),
  input: makeHTMLDescriptor('input', {}),
  video: makeHTMLDescriptor('video', {
    controls: {
      control: 'checkbox',
      defaultValue: true,
    },
    autoPlay: {
      control: 'checkbox',
      defaultValue: true,
    },
    loop: {
      control: 'checkbox',
      defaultValue: true,
    },
    src: {
      control: 'string-input',
      defaultValue: 'https://dl8.webmfiles.org/big-buck-bunny_trailer.webm',
    },
    style: {
      control: 'style-controls',
      defaultValue: {
        width: '250px',
        height: '120px',
      },
    },
  }),
  img: makeHTMLDescriptor('img', {
    src: {
      control: 'string-input',
      defaultValue: `/editor/icons/favicons/favicon-128.png?hash=${URL_HASH}"`,
    },
    style: {
      control: 'style-controls',
      defaultValue: {
        width: '64px',
        height: '64px',
      },
    },
  }),
}

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
                    switch (controlDescription.control) {
                      case 'style-controls':
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

  function addDependencyDescriptor(
    moduleName: string | null,
    groupType: InsertableComponentGroupType,
    components: ComponentDescriptorsForFile,
  ): void {
    const insertableComponents = Object.keys(components).map((componentName) => {
      const component = components[componentName]
      let stylePropOptions: Array<StylePropOption> = doNotAddStyleProp
      const propertyControls = component.propertyControls
      // Drill down to see if this dependency component has a style object entry
      // against style.
      if (hasStyleControls(propertyControls)) {
        stylePropOptions = addSizeAndNotStyleProp
      }

      const probablyIntrinsicElement =
        moduleName == null || isIntrinsicElementFromString(componentName)

      // TODO fallback imports!
      const fallbackImports: Array<ImportType> = probablyIntrinsicElement
        ? []
        : [
            {
              type: null,
              source: moduleName!, // if we updgrade TS we can remove this ! from here
              name: componentName,
            },
          ]

      return insertableComponent(
        component.componentInfo.importsToAdd,
        component.componentInfo.elementToInsert,
        componentName,
        stylePropOptions,
      )
    })
    result.push(insertableComponentGroup(groupType, insertableComponents))
  }

  // Add HTML entries.
  addDependencyDescriptor(null, insertableComponentGroupHTML(), basicHTMLElementsDescriptors)

  // Add entries for dependencies of the project.
  for (const dependency of dependencies) {
    if (isResolvedNpmDependency(dependency)) {
      const dependencyStatus = getDependencyStatus(
        packageStatus,
        propertyControlsInfo,
        dependency.name,
        'loaded',
      )
      const propertyControlsForDependency = propertyControlsInfo[dependency.name]
      if (propertyControlsForDependency != null) {
        addDependencyDescriptor(
          dependency.name,
          insertableComponentGroupProjectDependency(dependency.name, dependencyStatus),
          propertyControlsForDependency,
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
