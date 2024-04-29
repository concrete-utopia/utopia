import type { PropertyControls } from '../custom-code/internal-property-controls'
import { URL_HASH } from '../../common/env-vars'
import {
  hasStyleControls,
  propertyControlsForComponentInFile,
} from '../../core/property-controls/property-controls-utils'
import type {
  JSXAttributes,
  JSXElementChild,
  JSXMapExpressionWithoutUID,
  UtopiaJSXComponent,
} from '../../core/shared/element-template'
import {
  emptyComments,
  jsExpressionValue,
  jsxAttributesFromMap,
  jsxConditionalExpressionWithoutUID,
  jsxElement,
  jsxElementWithoutUID,
  jsxFragmentWithoutUID,
  jsxTextBlock,
  simpleAttribute,
} from '../../core/shared/element-template'
import { dropFileExtension } from '../../core/shared/file-utils'
import type { Size } from '../../core/shared/math-utils'
import { size } from '../../core/shared/math-utils'
import type {
  PackageStatus,
  PackageStatusMap,
  PossiblyUnversionedNpmDependency,
} from '../../core/shared/npm-dependency-types'
import { isResolvedNpmDependency } from '../../core/shared/npm-dependency-types'
import type { ElementPath, Imports, ProjectFile } from '../../core/shared/project-file-types'
import { importAlias, isTextFile } from '../../core/shared/project-file-types'
import { assertNever, fastForEach } from '../../core/shared/utils'
import type { SelectOption } from '../../uuiui-deps'
import type { ProjectContentTreeRoot } from '../assets'
import { walkContentsTree } from '../assets'
import type {
  PropertyControlsInfo,
  ComponentDescriptor,
  ComponentDescriptorsForFile,
  ComponentElementToInsert,
  ComponentInfo,
} from '../custom-code/code-file'
import {
  ComponentDescriptorDefaults,
  clearComponentElementToInsertUniqueIDs,
  defaultComponentDescriptor,
} from '../custom-code/code-file'
import { defaultElementStyle } from '../editor/defaults'
import { getExportedComponentImportsFromParseSuccess } from '../editor/export-utils'
import {
  groupJSXElement,
  groupJSXElementImportsToAdd,
} from '../canvas/canvas-strategies/strategies/group-helpers'
import type { InsertMenuMode } from '../canvas/ui/floating-insert-menu-helpers'
import { insertMenuModes } from '../canvas/ui/floating-insert-menu-helpers'
import { elementUsesProperty } from '../../core/model/element-template-utils'
import { intrinsicHTMLElementNamesThatSupportChildren } from '../../core/shared/dom-utils'
import { getTopLevelElementByExportsDetail } from '../../core/model/project-file-utils'
import { type Icon } from 'utopia-api'

export type StylePropOption = 'do-not-add' | 'add-size'

export interface InsertableComponent {
  importsToAdd: Imports
  element: () => ComponentElementToInsert
  name: string
  stylePropOptions: Array<StylePropOption>
  defaultSize: Size | null
  insertionCeiling: ElementPath | null
  icon: Icon | null
}

export function insertableComponent(
  importsToAdd: Imports,
  element: () => ComponentElementToInsert,
  name: string,
  stylePropOptions: Array<StylePropOption>,
  defaultSize: Size | null,
  insertionCeiling: ElementPath | null,
  icon: Icon | null,
): InsertableComponent {
  const component = {
    importsToAdd: importsToAdd,
    element: element,
    name: name,
    stylePropOptions: stylePropOptions,
    defaultSize: defaultSize,
    insertionCeiling: insertionCeiling,
    icon: icon,
  }
  return component
}

export function clearInsertableComponentUniqueIDs(
  insertableComponentToFix: InsertableComponent,
): InsertableComponent {
  const updatedElement =
    typeof insertableComponentToFix.element() === 'string'
      ? insertableComponentToFix.element()
      : clearComponentElementToInsertUniqueIDs(insertableComponentToFix.element())
  return {
    ...insertableComponentToFix,
    element: () => updatedElement,
  }
}

export interface InsertableComponentGroupSamples {
  type: 'SAMPLES_GROUP'
}

export interface InsertableComponentGroupHTML {
  type: 'HTML_GROUP'
}

export interface InsertableComponentGroupDiv {
  type: 'HTML_DIV'
}

export function insertableComponentGroupDiv(): InsertableComponentGroupDiv {
  return { type: 'HTML_DIV' }
}

export function insertableComponentGroupHTML(): InsertableComponentGroupHTML {
  return {
    type: 'HTML_GROUP',
  }
}

export interface InsertableComponentGroupConditionals {
  type: 'CONDITIONALS_GROUP'
}

export interface InsertableComponentGroupMap {
  type: 'MAP_GROUP'
}

export function insertableComponentGroupConditionals(): InsertableComponentGroupConditionals {
  return {
    type: 'CONDITIONALS_GROUP',
  }
}

export function insertableComponentGroupGroups(): InsertableComponentGroupGroups {
  return {
    type: 'GROUPS_GROUP',
  }
}

export interface InsertableComponentGroupGroups {
  type: 'GROUPS_GROUP'
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
  | InsertableComponentGroupDiv
  | InsertableComponentGroupProjectComponent
  | InsertableComponentGroupProjectDependency
  | InsertableComponentGroupConditionals
  | InsertableComponentGroupFragment
  | InsertableComponentGroupSamples
  | InsertableComponentGroupGroups
  | InsertableComponentGroupMap

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
    case 'GROUPS_GROUP':
      return 'Group'
    case 'HTML_DIV':
      return 'Div'
    case 'MAP_GROUP':
      return 'List'
    default:
      assertNever(insertableType)
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
    case 'GROUPS_GROUP':
    case 'HTML_DIV':
    case 'MAP_GROUP':
      return 'loaded'
    case 'PROJECT_DEPENDENCY_GROUP':
      return insertableType.dependencyStatus
    default:
      assertNever(insertableType)
  }
}

export interface InsertableVariable extends InsertableComponent {
  depth: number
  variableType: InsertableVariableType
  originalName: string
}

export type InsertableVariableType =
  | 'string'
  | 'number'
  | 'bigint'
  | 'boolean'
  | 'symbol'
  | 'undefined'
  | 'object'
  | 'function'
  | 'array'
  | 'image'

export function insertableVariable(
  importsToAdd: Imports,
  element: () => ComponentElementToInsert,
  name: string,
  stylePropOptions: Array<StylePropOption>,
  defaultSize: Size | null,
  variableType: InsertableVariableType,
  depth: number,
  originalName: string,
  insertionCeiling: ElementPath | null,
): InsertableVariable {
  return {
    ...insertableComponent(
      importsToAdd,
      element,
      name,
      stylePropOptions,
      defaultSize,
      insertionCeiling,
      null,
    ),
    variableType: variableType,
    depth: depth,
    originalName: originalName,
  }
}

export function isInsertableVariable(
  insertable: InsertableComponent,
): insertable is InsertableVariable {
  return 'variableType' in insertable
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
  attributes?: () => JSXAttributes,
): ComponentDescriptor {
  const supportsChildren = intrinsicHTMLElementNamesThatSupportChildren.includes(tag)
  const propertyControls: PropertyControls = {
    ...stockHTMLPropertyControls,
    ...extraPropertyControls,
  }
  return {
    properties: propertyControls,
    supportsChildren: supportsChildren,
    preferredChildComponents: [],
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
        elementToInsert: () => jsxElementWithoutUID(tag, attributes?.() ?? [], []),
      },
    ],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
  }
}

export const defaultImageAttributes = (): JSXAttributes => [
  simpleAttribute('style', {
    width: '64px',
    height: '64px',
    position: 'absolute',
  }),
  simpleAttribute('src', `/editor/utopia-logo-white-fill.png?hash=${URL_HASH}`),
]

const basicHTMLElementsDescriptors = {
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
    () => [
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

const divComponentGroup = {
  div: makeHTMLDescriptor('div', {}, () =>
    jsxAttributesFromMap({
      style: defaultElementStyle(),
    }),
  ),
}

const conditionalElementsDescriptors: ComponentDescriptorsForFile = {
  conditional: {
    properties: {},
    supportsChildren: true,
    preferredChildComponents: [],
    variants: [
      {
        insertMenuLabel: 'Conditional',
        elementToInsert: () =>
          jsxConditionalExpressionWithoutUID(
            jsExpressionValue(true, emptyComments),
            'true',
            jsExpressionValue(null, emptyComments),
            jsExpressionValue(null, emptyComments),
            emptyComments,
          ),
        importsToAdd: {},
      },
    ],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
  },
}

const groupElementsDescriptors: ComponentDescriptorsForFile = {
  group: {
    properties: {},
    supportsChildren: true,
    preferredChildComponents: [],
    variants: [
      {
        insertMenuLabel: 'Group',
        elementToInsert: () => groupJSXElement([]),
        importsToAdd: groupJSXElementImportsToAdd(),
      },
    ],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
  },
}

export const fragmentComponentInfo: ComponentInfo = {
  insertMenuLabel: 'Fragment',
  elementToInsert: () => jsxFragmentWithoutUID([], true),
  importsToAdd: {
    react: {
      importedAs: 'React',
      importedFromWithin: [],
      importedWithName: null,
    },
  },
}

export const mapComponentInfo: ComponentInfo = {
  insertMenuLabel: 'List',
  elementToInsert: (): JSXMapExpressionWithoutUID => ({
    type: 'JSX_MAP_EXPRESSION',
    javascriptWithUIDs: '[1, 2, 3].map(() => <\nPlaceholder data-uid="placeholder-id" />);',
    originalJavascript: '[1, 2, 3].map(() => (\n            <Placeholder />\n          ))',
    transpiledJavascript:
      'return [1, 2, 3].map(() => utopiaCanvasJSXLookup("placeholder-id", {\n  callerThis: this\n}));',
    definedElsewhere: ['React', 'Placeholder', 'utopiaCanvasJSXLookup'],
    sourceMap: null,
    elementsWithin: {
      'placeholder-id': jsxElement('Placeholder', 'placeholder-id', jsxAttributesFromMap({}), []),
    },
    comments: emptyComments,
    valuesInScopeFromParameters: [],
  }),
  importsToAdd: {
    react: {
      importedAs: 'React',
      importedFromWithin: [],
      importedWithName: null,
    },
    'utopia-api': {
      importedAs: null,
      importedFromWithin: [importAlias('Placeholder')],
      importedWithName: null,
    },
  },
}

export const fragmentElementsDescriptors: ComponentDescriptorsForFile = {
  fragment: {
    properties: {},
    supportsChildren: true,
    variants: [fragmentComponentInfo],
    preferredChildComponents: [],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
  },
}

export const mapElementDescriptors: ComponentDescriptorsForFile = {
  map: {
    properties: {},
    supportsChildren: false,
    variants: [mapComponentInfo],
    preferredChildComponents: [],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
  },
}

const samplesDescriptors: ComponentDescriptorsForFile = {
  sampleText: {
    properties: {},
    supportsChildren: false,
    preferredChildComponents: [],
    variants: [
      {
        insertMenuLabel: 'Sample text',
        elementToInsert: () => jsxElementWithoutUID('span', [], [jsxTextBlock('Sample text')]),
        importsToAdd: {},
      },
    ],
    source: defaultComponentDescriptor(),
    ...ComponentDescriptorDefaults,
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
  insertMenuMode: InsertMenuMode,
  packageStatus: PackageStatusMap,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
  dependencies: Array<PossiblyUnversionedNpmDependency>,
  originatingPath: string,
): Array<InsertableComponentGroup> {
  const groups = getComponentGroups(
    insertMenuMode,
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
            null,
            null,
          ),
        ],
      )
      return [newSceneGroup, ...groupsWithoutUtopiaApi, utopiaApiGroupWithoutScene]
    }
  }
  return groups
}

function isDescriptorEligibleForMode(
  insertMenuMode: InsertMenuMode,
  component: ComponentDescriptor,
): boolean {
  return insertMenuMode === 'wrap' ? component.supportsChildren : true
}

function isUtopiaJSXComponentEligibleForMode(
  insertMenuMode: InsertMenuMode,
  element: UtopiaJSXComponent | null,
): boolean {
  if (insertMenuMode === 'wrap') {
    return (
      element != null &&
      element.param != null &&
      elementUsesProperty(element.rootElement, element.param, 'children')
    )
  }
  return true
}

export function getComponentGroups(
  insertMenuMode: InsertMenuMode,
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
      if (file.fileContents.parsed.type !== 'PARSE_SUCCESS') {
        return
      }
      const parsed = file.fileContents.parsed
      const possibleExportedComponents = getExportedComponentImportsFromParseSuccess(
        originatingPath,
        fullPath,
        file.fileContents.parsed,
      )
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
          if (isDescriptorEligibleForMode(insertMenuMode, descriptor)) {
            fastForEach(descriptor.variants, (insertOption) => {
              insertableComponents.push(
                insertableComponent(
                  insertOption.importsToAdd,
                  insertOption.elementToInsert,
                  insertOption.insertMenuLabel,
                  stylePropOptions,
                  null,
                  null,
                  descriptor.icon,
                ),
              )
            })
          }
        } else {
          const element = getTopLevelElementByExportsDetail(parsed, exportedComponent.listingName)
          if (element == null || isUtopiaJSXComponentEligibleForMode(insertMenuMode, element)) {
            insertableComponents.push(
              insertableComponent(
                exportedComponent.importsToAdd,
                () => jsxElementWithoutUID(exportedComponent.listingName, [], []),
                exportedComponent.listingName,
                stylePropOptions,
                null,
                null,
                null,
              ),
            )
          }
        }
      })
      result.push(
        insertableComponentGroup(
          insertableComponentGroupProjectComponent(fullPath),
          insertableComponents,
        ),
      )
    }
  })

  function addDependencyDescriptor(
    groupType: InsertableComponentGroupType,
    components: ComponentDescriptorsForFile,
    defaultSize?: Size,
  ): void {
    let insertableComponents: Array<InsertableComponent> = []
    fastForEach(Object.keys(components), (componentName) => {
      const component = components[componentName]
      const propertyControls = component.properties
      // Drill down into the property controls to see if this has an appropriate style object entry.
      const stylePropOptions = hasStyleControls(propertyControls)
        ? addSizeAndNotStyleProp
        : doNotAddStyleProp
      if (isDescriptorEligibleForMode(insertMenuMode, component)) {
        fastForEach(component.variants, (insertOption) => {
          insertableComponents.push(
            insertableComponent(
              insertOption.importsToAdd,
              insertOption.elementToInsert,
              insertOption.insertMenuLabel,
              stylePropOptions,
              defaultSize ?? null,
              null,
              component.icon,
            ),
          )
        })
      }
    })
    result.push(insertableComponentGroup(groupType, insertableComponents))
  }

  addDependencyDescriptor(insertableComponentGroupDiv(), divComponentGroup, {
    width: 100,
    height: 100,
  })

  // Add HTML entries.
  addDependencyDescriptor(insertableComponentGroupHTML(), basicHTMLElementsDescriptors)

  // Add conditionals group.
  addDependencyDescriptor(insertableComponentGroupConditionals(), conditionalElementsDescriptors)

  // Add fragment group.
  addDependencyDescriptor(insertableComponentGroupFragment(), fragmentElementsDescriptors)

  // Add map group.
  addDependencyDescriptor({ type: 'MAP_GROUP' }, mapElementDescriptors)

  // Add samples group.
  addDependencyDescriptor({ type: 'SAMPLES_GROUP' }, samplesDescriptors)

  // Add groups group.
  addDependencyDescriptor(insertableComponentGroupGroups(), groupElementsDescriptors) // TODO instead of this, use createWrapInGroupActions!

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
            insertableComponentGroupProjectDependency(dependency.name, dependencyStatus),
            propertyControlsForDependency,
          )
        }
      }
    }
  }

  return result.filter((group) =>
    insertMenuModesForInsertableComponentGroupType(group.source).includes(insertMenuMode),
  )
}

export function getComponentGroupsAsSelectOptions(
  insertMenuMode: InsertMenuMode,
  packageStatus: PackageStatusMap,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
  dependencies: Array<PossiblyUnversionedNpmDependency>,
  originatingPath: string,
): Array<SelectOption> {
  const insertableGroups = getComponentGroups(
    insertMenuMode,
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

export function insertMenuModesForInsertableComponentGroupType(
  groupType: InsertableComponentGroupType,
): InsertMenuMode[] {
  switch (groupType.type) {
    case 'CONDITIONALS_GROUP':
    case 'FRAGMENT_GROUP':
    case 'HTML_GROUP':
    case 'PROJECT_COMPONENT_GROUP':
    case 'PROJECT_DEPENDENCY_GROUP':
    case 'SAMPLES_GROUP':
    case 'HTML_DIV':
    case 'MAP_GROUP':
      return insertMenuModes.all
    case 'GROUPS_GROUP':
      return insertMenuModes.onlyWrap
    default:
      assertNever(groupType)
  }
}
