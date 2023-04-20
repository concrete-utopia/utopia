import { PropertyControls } from 'utopia-api/core'
import { URL_HASH } from '../../common/env-vars'
import {
  hasStyleControls,
  propertyControlsForComponentInFile,
} from '../../core/property-controls/property-controls-utils'
import {
  emptyComments,
  jsExpressionValue,
  JSXAttributes,
  jsxAttributesFromMap,
  jsxConditionalExpressionWithoutUID,
  jsxElementWithoutUID,
  jsxFragmentWithoutUID,
  jsxTextBlock,
  simpleAttribute,
} from '../../core/shared/element-template'
import { dropFileExtension } from '../../core/shared/file-utils'
import { size, Size } from '../../core/shared/math-utils'
import {
  isResolvedNpmDependency,
  PackageStatus,
  PackageStatusMap,
  PossiblyUnversionedNpmDependency,
} from '../../core/shared/npm-dependency-types'
import { Imports, isTextFile, ProjectFile } from '../../core/shared/project-file-types'
import { fastForEach } from '../../core/shared/utils'
import { addImport, emptyImports } from '../../core/workers/common/project-file-utils'
import { SelectOption } from '../../uuiui-deps'
import { ProjectContentTreeRoot, walkContentsTree } from '../assets'
import {
  PropertyControlsInfo,
  ComponentDescriptor,
  ComponentDescriptorsForFile,
  ComponentElementToInsert,
  clearComponentElementToInsertUniqueIDs,
} from '../custom-code/code-file'
import { defaultViewElementStyle } from '../editor/defaults'
import { getExportedComponentImports } from '../editor/export-utils'

export type StylePropOption = 'do-not-add' | 'add-size'
export type WrapContentOption = 'wrap-content' | 'do-now-wrap-content'

export interface InsertableComponent {
  importsToAdd: Imports
  element: ComponentElementToInsert
  name: string
  stylePropOptions: Array<StylePropOption>
  defaultSize: Size | null
}

export function insertableComponent(
  importsToAdd: Imports,
  element: ComponentElementToInsert,
  name: string,
  stylePropOptions: Array<StylePropOption>,
  defaultSize: Size | null,
): InsertableComponent {
  return {
    importsToAdd: importsToAdd,
    element: element,
    name: name,
    stylePropOptions: stylePropOptions,
    defaultSize: defaultSize,
  }
}

export function clearInsertableComponentUniqueIDs(
  insertableComponentToFix: InsertableComponent,
): InsertableComponent {
  const updatedElement =
    typeof insertableComponentToFix.element === 'string'
      ? insertableComponentToFix.element
      : clearComponentElementToInsertUniqueIDs(insertableComponentToFix.element)
  return {
    ...insertableComponentToFix,
    element: updatedElement,
  }
}

export interface InsertableComponentGroupSamples {
  type: 'SAMPLES_GROUP'
}

export interface InsertableComponentGroupHTML {
  type: 'HTML_GROUP'
}

export function insertableComponentGroupHTML(): InsertableComponentGroupHTML {
  return {
    type: 'HTML_GROUP',
  }
}

export interface InsertableComponentGroupConditionals {
  type: 'CONDITIONALS_GROUP'
}

export function insertableComponentGroupConditionals(): InsertableComponentGroupConditionals {
  return {
    type: 'CONDITIONALS_GROUP',
  }
}

export interface InsertableComponentGroupFragment {
  type: 'FRAGMENT_GROUP'
}

export function insertableComponentGroupFragment(): InsertableComponentGroupFragment {
  return {
    type: 'FRAGMENT_GROUP',
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
  | InsertableComponentGroupConditionals
  | InsertableComponentGroupFragment
  | InsertableComponentGroupSamples

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

export function clearInsertableComponentGroupUniqueIDs(
  insertableGroup: InsertableComponentGroup,
): InsertableComponentGroup {
  return {
    source: insertableGroup.source,
    insertableComponents: insertableGroup.insertableComponents.map(
      clearInsertableComponentUniqueIDs,
    ),
  }
}

export function getInsertableGroupLabel(insertableType: InsertableComponentGroupType): string {
  switch (insertableType.type) {
    case 'SAMPLES_GROUP':
      return 'Sample elements'
    case 'HTML_GROUP':
      return 'HTML Elements'
    case 'PROJECT_DEPENDENCY_GROUP':
      return insertableType.dependencyName
    case 'PROJECT_COMPONENT_GROUP':
      return insertableType.path
    case 'CONDITIONALS_GROUP':
      return 'Conditionals'
    case 'FRAGMENT_GROUP':
      return 'Fragment'
    default:
      const _exhaustiveCheck: never = insertableType
      throw new Error(`Unhandled insertable type ${JSON.stringify(insertableType)}`)
  }
}

export function getInsertableGroupPackageStatus(
  insertableType: InsertableComponentGroupType,
): PackageStatus {
  switch (insertableType.type) {
    case 'SAMPLES_GROUP':
    case 'HTML_GROUP':
    case 'PROJECT_COMPONENT_GROUP':
    case 'CONDITIONALS_GROUP':
    case 'FRAGMENT_GROUP':
      return 'loaded'
    case 'PROJECT_DEPENDENCY_GROUP':
      return insertableType.dependencyStatus
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
      for (const infoKey of Object.keys(propertyControlsInfo)) {
        if (infoKey.startsWith(dependencyName)) {
          return 'loaded'
        }
      }
      return 'loading'
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
  attributes?: JSXAttributes,
): ComponentDescriptor {
  const propertyControls: PropertyControls = {
    ...stockHTMLPropertyControls,
    ...extraPropertyControls,
  }
  return {
    properties: propertyControls,
    variants: [
      {
        insertMenuLabel: tag,
        importsToAdd: {
          react: {
            importedAs: 'React',
            importedFromWithin: [],
            importedWithName: null,
          },
        },
        elementToInsert: jsxElementWithoutUID(tag, attributes ?? [], []),
      },
    ],
  }
}

export const defaultImageAttributes: JSXAttributes = [
  simpleAttribute('style', {
    width: '64px',
    height: '64px',
    position: 'absolute',
  }),
  simpleAttribute('src', `/editor/icons/favicons/favicon-128.png?hash=${URL_HASH}`),
]

const basicHTMLElementsDescriptors = {
  div: makeHTMLDescriptor(
    'div',
    {},
    jsxAttributesFromMap({
      style: defaultViewElementStyle(),
    }),
  ),
  span: makeHTMLDescriptor('span', {}),
  h1: makeHTMLDescriptor('h1', {}),
  h2: makeHTMLDescriptor('h2', {}),
  p: makeHTMLDescriptor('p', {}),
  button: makeHTMLDescriptor('button', {}),
  input: makeHTMLDescriptor('input', {}),
  video: makeHTMLDescriptor(
    'video',
    {
      controls: {
        control: 'checkbox',
      },
      autoPlay: {
        control: 'checkbox',
      },
      loop: {
        control: 'checkbox',
      },
      src: {
        control: 'string-input',
      },
      style: {
        control: 'style-controls',
      },
    },
    [
      simpleAttribute('style', {
        width: '250px',
        height: '120px',
        position: 'absolute',
      }),
      simpleAttribute('controls', true),
      simpleAttribute('autoPlay', true),
      simpleAttribute('loop', true),
      simpleAttribute('src', 'https://dl8.webmfiles.org/big-buck-bunny_trailer.webm'),
    ],
  ),
  img: makeHTMLDescriptor(
    'img',
    {
      src: {
        control: 'string-input',
      },
      style: {
        control: 'style-controls',
      },
    },
    defaultImageAttributes,
  ),
}

const conditionalElementsDescriptors: ComponentDescriptorsForFile = {
  conditional: {
    properties: {},
    variants: [
      {
        insertMenuLabel: 'Conditional',
        elementToInsert: jsxConditionalExpressionWithoutUID(
          jsExpressionValue(true, emptyComments),
          'true',
          jsExpressionValue(null, emptyComments),
          jsExpressionValue(null, emptyComments),
          emptyComments,
        ),
        importsToAdd: {},
      },
    ],
  },
}

const fragmentElementsDescriptors: ComponentDescriptorsForFile = {
  fragment: {
    properties: {},
    variants: [
      {
        insertMenuLabel: 'Fragment',
        elementToInsert: jsxFragmentWithoutUID([], true),
        importsToAdd: {
          react: {
            importedAs: 'React',
            importedFromWithin: [],
            importedWithName: null,
          },
        },
      },
    ],
  },
}

const samplesDescriptors: ComponentDescriptorsForFile = {
  sampleText: {
    properties: {},
    variants: [
      {
        insertMenuLabel: 'Sample text',
        elementToInsert: jsxElementWithoutUID('span', [], [jsxTextBlock('Sample text')]),
        importsToAdd: {},
      },
    ],
  },
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

export function getNonEmptyComponentGroups(
  packageStatus: PackageStatusMap,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
  dependencies: Array<PossiblyUnversionedNpmDependency>,
  originatingPath: string,
): Array<InsertableComponentGroup> {
  const groups = getComponentGroups(
    packageStatus,
    propertyControlsInfo,
    projectContents,
    dependencies,
    originatingPath,
  )
  return groups.filter((group) => {
    return group.insertableComponents.length > 0
  })
}

const SceneDefaultWidth = 325
const SceneDefaultHeight = 350

// Scene components from utopia-api are special: they should appear as the first insertable component, and
// they should have a custom default size
export function moveSceneToTheBeginningAndSetDefaultSize(
  groups: Array<InsertableComponentGroup>,
): Array<InsertableComponentGroup> {
  const utopiaApiGroupIdx = groups.findIndex(
    (group) => getInsertableGroupLabel(group.source) === 'utopia-api',
  )
  if (utopiaApiGroupIdx > -1) {
    const utopiaApiGroup = groups[utopiaApiGroupIdx]
    const sceneIdx = utopiaApiGroup.insertableComponents.findIndex((comp) => comp.name === 'Scene')
    if (sceneIdx > -1) {
      const scene = utopiaApiGroup.insertableComponents[sceneIdx]
      const utopiaApiGroupWithoutScene = insertableComponentGroup(utopiaApiGroup.source, [
        ...utopiaApiGroup.insertableComponents.slice(0, sceneIdx),
        ...utopiaApiGroup.insertableComponents.slice(sceneIdx + 1),
      ])
      const groupsWithoutUtopiaApi = [
        ...groups.slice(0, utopiaApiGroupIdx),
        ...groups.slice(utopiaApiGroupIdx + 1),
      ]
      const newSceneGroup = insertableComponentGroup(
        insertableComponentGroupProjectComponent('Storyboard'),
        [
          insertableComponent(
            scene.importsToAdd,
            scene.element,
            scene.name,
            scene.stylePropOptions,
            size(SceneDefaultWidth, SceneDefaultHeight),
          ),
        ],
      )
      return [newSceneGroup, ...groupsWithoutUtopiaApi, utopiaApiGroupWithoutScene]
    }
  }
  return groups
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
        let insertableComponents: Array<InsertableComponent> = []
        fastForEach(possibleExportedComponents, (exportedComponent) => {
          const pathWithoutExtension = dropFileExtension(fullPath)
          const propertyControls = propertyControlsForComponentInFile(
            exportedComponent.listingName,
            pathWithoutExtension,
            propertyControlsInfo,
          )

          // Drill down into the property controls to see if this has an appropriate style object entry.
          const stylePropOptions = hasStyleControls(propertyControls)
            ? addSizeAndNotStyleProp
            : doNotAddStyleProp

          const propertyControlsForDependency =
            propertyControlsInfo[fullPath] ?? propertyControlsInfo[pathWithoutExtension]
          if (
            propertyControlsForDependency != null &&
            propertyControlsForDependency[exportedComponent.listingName] != null
          ) {
            const descriptor = propertyControlsForDependency[exportedComponent.listingName]
            fastForEach(descriptor.variants, (insertOption) => {
              insertableComponents.push(
                insertableComponent(
                  insertOption.importsToAdd,
                  insertOption.elementToInsert,
                  insertOption.insertMenuLabel,
                  stylePropOptions,
                  null,
                ),
              )
            })
          } else {
            insertableComponents.push(
              insertableComponent(
                exportedComponent.importsToAdd,
                jsxElementWithoutUID(exportedComponent.listingName, [], []),
                exportedComponent.listingName,
                stylePropOptions,
                null,
              ),
            )
          }
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
    let insertableComponents: Array<InsertableComponent> = []
    fastForEach(Object.keys(components), (componentName) => {
      const component = components[componentName]
      const propertyControls = component.properties
      // Drill down into the property controls to see if this has an appropriate style object entry.
      const stylePropOptions = hasStyleControls(propertyControls)
        ? addSizeAndNotStyleProp
        : doNotAddStyleProp

      fastForEach(component.variants, (insertOption) => {
        insertableComponents.push(
          insertableComponent(
            insertOption.importsToAdd,
            insertOption.elementToInsert,
            insertOption.insertMenuLabel,
            stylePropOptions,
            null,
          ),
        )
      })
    })
    result.push(insertableComponentGroup(groupType, insertableComponents))
  }

  // Add HTML entries.
  addDependencyDescriptor(null, insertableComponentGroupHTML(), basicHTMLElementsDescriptors)

  // Add conditionals group.
  addDependencyDescriptor(
    null,
    insertableComponentGroupConditionals(),
    conditionalElementsDescriptors,
  )

  // Add fragment group.
  addDependencyDescriptor(null, insertableComponentGroupFragment(), fragmentElementsDescriptors)

  // Add samples group
  addDependencyDescriptor(null, { type: 'SAMPLES_GROUP' }, samplesDescriptors)

  // Add entries for dependencies of the project.
  for (const dependency of dependencies) {
    if (isResolvedNpmDependency(dependency)) {
      const dependencyStatus = getDependencyStatus(
        packageStatus,
        propertyControlsInfo,
        dependency.name,
        'loaded',
      )
      for (const infoKey of Object.keys(propertyControlsInfo)) {
        if (infoKey.startsWith(dependency.name)) {
          const propertyControlsForDependency = propertyControlsInfo[infoKey]
          addDependencyDescriptor(
            dependency.name,
            insertableComponentGroupProjectDependency(dependency.name, dependencyStatus),
            propertyControlsForDependency,
          )
        }
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
