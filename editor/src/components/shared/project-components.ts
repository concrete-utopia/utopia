import { PropertyControls } from 'utopia-api'
import { URL_HASH } from '../../common/env-vars'
import { defaultPropertiesForComponentInFile } from '../../core/property-controls/property-controls-utils'
import { flatMapEither, foldEither, right } from '../../core/shared/either'
import {
  emptyComments,
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
import {
  ComponentDescriptor,
  componentDescriptor,
  DependencyDescriptor,
} from '../../core/third-party/third-party-types'
import { addImport, emptyImports } from '../../core/workers/common/project-file-utils'
import { SelectOption } from '../../uuiui-deps'
import { ProjectContentTreeRoot, walkContentsTree } from '../assets'
import { PropertyControlsInfo } from '../custom-code/code-file'
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
    control: 'styleobject',
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
  let defaultProps: JSXAttributes = []
  function addDefaultProps(targetPropertyControls: PropertyControls): void {
    for (const propKey of Object.keys(targetPropertyControls)) {
      const prop = targetPropertyControls[propKey]
      if (prop.control === 'folder') {
        addDefaultProps(prop.controls)
      } else {
        if (prop?.defaultValue != null) {
          defaultProps.push(
            jsxAttributesEntry(
              propKey,
              jsxAttributeValue(prop.defaultValue, emptyComments),
              emptyComments,
            ),
          )
        }
      }
    }
  }
  addDefaultProps(propertyControls)
  return componentDescriptor(
    addImport('', 'react', null, [], 'React', emptyImportsValue),
    jsxElementWithoutUID(tag, defaultProps, []),
    tag,
    propertyControls,
  )
}

const basicHTMLElementsDescriptor: DependencyDescriptor = {
  name: 'HTML Elements',
  components: [
    makeHTMLDescriptor('div', {}),
    makeHTMLDescriptor('span', {}),
    makeHTMLDescriptor('h1', {}),
    makeHTMLDescriptor('h2', {}),
    makeHTMLDescriptor('p', {}),
    makeHTMLDescriptor('button', {}),
    makeHTMLDescriptor('input', {}),
    makeHTMLDescriptor('video', {
      controls: {
        control: 'boolean',
        defaultValue: true,
      },
      autoPlay: {
        control: 'boolean',
        defaultValue: true,
      },
      loop: {
        control: 'boolean',
        defaultValue: true,
      },
      src: {
        control: 'string',
        defaultValue: 'https://dl8.webmfiles.org/big-buck-bunny_trailer.webm',
      },
      style: {
        control: 'styleobject',
        defaultValue: {
          width: '250px',
          height: '120px',
        },
      },
    }),
    makeHTMLDescriptor('img', {
      src: {
        control: 'string',
        defaultValue: `/editor/icons/favicons/favicon-128.png?hash=${URL_HASH}"`,
      },
      style: {
        control: 'styleobject',
        defaultValue: {
          width: '64px',
          height: '64px',
        },
      },
    }),
  ],
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

  function addDependencyDescriptor(
    groupType: InsertableComponentGroupType,
    components: Array<ComponentDescriptor>,
  ): void {
    const insertableComponents = components.map((component) => {
      let stylePropOptions: Array<StylePropOption> = doNotAddStyleProp
      // Drill down to see if this dependency component has a style object entry
      // against style.
      if (component.propertyControls != null) {
        if ('style' in component.propertyControls) {
          const styleControls = component.propertyControls['style']
          if (styleControls?.control === 'styleobject') {
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
    result.push(insertableComponentGroup(groupType, insertableComponents))
  }

  // Add HTML entries.
  addDependencyDescriptor(insertableComponentGroupHTML(), basicHTMLElementsDescriptor.components)

  // Add entries for dependencies of the project.
  for (const dependency of dependencies) {
    if (isResolvedNpmDependency(dependency)) {
      const possibleComponentDescriptor = getThirdPartyComponents(
        dependency.name,
        dependency.version,
      )
      if (possibleComponentDescriptor != null) {
        const dependencyStatus = getDependencyStatus(
          packageStatus,
          propertyControlsInfo,
          dependency.name,
          'loaded',
        )
        const components =
          dependencyStatus === 'loaded' ? possibleComponentDescriptor.components : []
        addDependencyDescriptor(
          insertableComponentGroupProjectDependency(dependency.name, dependencyStatus),
          components,
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
