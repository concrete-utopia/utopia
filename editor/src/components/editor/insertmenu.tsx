/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import {
  colorTheme,
  FlexRow,
  Icn,
  UtopiaStyles,
  UtopiaTheme,
  InspectorSubsectionHeader,
} from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import {
  JSXElementName,
  jsxElementName,
  jsxElementNameEquals,
  isUtopiaJSXComponent,
  jsxAttributeValue,
  jsxElement,
  JSXAttributes,
} from '../../core/shared/element-template'
import { generateUID } from '../../core/shared/uid-utils'
import {
  TemplatePath,
  isCodeFile,
  isUIJSFile,
  importDetails,
  importAlias,
  Imports,
  importsEquals,
} from '../../core/shared/project-file-types'
import Utils from '../../utils/utils'
import {
  defaultAnimatedDivElement,
  defaultEllipseElement,
  defaultRectangleElement,
  defaultTextElement,
  defaultDivElement,
} from './defaults'
import { FontSettings } from '../inspector/common/css-utils'
import { existingUIDs } from '../navigator/left-pane'
import { EditorAction, EditorDispatch } from './action-types'
import { enableInsertModeForJSXElement, enableInsertModeForScene } from './actions/actions'
import {
  ElementInsertionSubject,
  insertionSubjectIsScene,
  Mode,
  insertionSubjectIsJSXElement,
} from './editor-modes'
import { insertImage } from './image-insert'
import { getOpenFilename, getOpenUIJSFile } from './store/editor-state'
import { useEditorState } from './store/store-hook'
import { last } from '../../core/shared/array-utils'
import { defaultIfNull } from '../../core/shared/optional-utils'
import { forEachRight } from '../../core/shared/either'
import { dropFileExtension } from '../../core/shared/file-utils'
import { objectMap } from '../../core/shared/object-utils'
import {
  defaultPropertiesForComponentInFile,
  findMissingDefaultsAndGetWarning,
} from '../../core/property-controls/property-controls-utils'
import { WarningIcon } from '../../uuiui/warning-icon'
import { usePossiblyResolvedPackageDependencies } from './npm-dependency/npm-dependency'
import {
  PossiblyUnversionedNpmDependency,
  isResolvedNpmDependency,
  PackageStatusMap,
  PackageStatus,
} from '../../core/shared/npm-dependency-types'
import { getThirdPartyComponents } from '../../core/third-party/third-party-components'
import { isBuiltInDependency } from '../../core/es-modules/package-manager/built-in-dependencies'
import { NpmDependencyVersionAndStatusIndicator } from '../navigator/dependecy-version-status-indicator'
import { PropertyControlsInfo } from '../custom-code/code-file'

interface CurrentFileComponent {
  componentName: string
  defaultProps: { [prop: string]: unknown }
  detectedProps: Array<string>
}

interface InsertMenuProps {
  lastFontSettings: FontSettings | null
  editorDispatch: EditorDispatch
  selectedViews: Array<TemplatePath>
  mode: Mode
  existingUIDs: Array<string>
  currentlyOpenFilename: string | null
  currentFileComponents: Array<CurrentFileComponent>
  dependencies: Array<PossiblyUnversionedNpmDependency>
  packageStatus: PackageStatusMap
  propertyControlsInfo: PropertyControlsInfo
}

export const InsertMenu = betterReactMemo('InsertMenu', () => {
  const props = useEditorState((store) => {
    const openFileFullPath = getOpenFilename(store.editor)
    let currentlyOpenFilename: string | null = null
    if (openFileFullPath != null) {
      const splitFilename = openFileFullPath.split('/')
      currentlyOpenFilename = defaultIfNull<string | null>(null, last(splitFilename))
    }

    let currentFileComponents: Array<CurrentFileComponent> = []
    const openUIJSFile = getOpenUIJSFile(store.editor)
    if (openUIJSFile != null && openFileFullPath != null) {
      forEachRight(openUIJSFile.fileContents, (fileContents) => {
        Utils.fastForEach(fileContents.topLevelElements, (topLevelElement) => {
          if (isUtopiaJSXComponent(topLevelElement)) {
            const componentName = topLevelElement.name
            const defaultProps = defaultPropertiesForComponentInFile(
              componentName,
              dropFileExtension(openFileFullPath),
              store.editor.propertyControlsInfo,
            )
            const detectedProps = topLevelElement.propsUsed
            currentFileComponents.push({
              componentName: componentName,
              defaultProps: defaultProps,
              detectedProps: detectedProps,
            })
          }
        })
      })
    }

    return {
      lastFontSettings: store.editor.lastUsedFont,
      editorDispatch: store.dispatch,
      selectedViews: store.editor.selectedViews,
      mode: store.editor.mode,
      existingUIDs: existingUIDs(openUIJSFile),
      currentlyOpenFilename: currentlyOpenFilename,
      currentFileComponents: currentFileComponents,
      packageStatus: store.editor.nodeModules.packageStatus,
      propertyControlsInfo: store.editor.propertyControlsInfo,
    }
  }, 'InsertMenu')

  const dependencies = usePossiblyResolvedPackageDependencies()

  const propsWithDependencies: InsertMenuProps = {
    ...props,
    dependencies: dependencies,
  }

  return <InsertMenuInner {...propsWithDependencies} />
})

export interface ComponentBeingInserted {
  importsToAdd: Imports
  elementName: JSXElementName
}

export function componentBeingInserted(
  importsToAdd: Imports,
  elementName: JSXElementName,
): ComponentBeingInserted {
  return {
    importsToAdd: importsToAdd,
    elementName: elementName,
  }
}

export function componentBeingInsertedEquals(
  first: ComponentBeingInserted | null,
  second: ComponentBeingInserted | null,
): boolean {
  if (first == null) {
    return second == null
  } else {
    if (second == null) {
      return false
    } else {
      return (
        importsEquals(first.importsToAdd, second.importsToAdd) &&
        jsxElementNameEquals(first.elementName, second.elementName)
      )
    }
  }
}

const divComponentBeingInserted = componentBeingInserted({}, jsxElementName('div', []))

const imageComponentBeingInserted = componentBeingInserted({}, jsxElementName('img', []))

const textComponentBeingInserted = componentBeingInserted(
  { 'utopia-api': importDetails(null, [importAlias('Text')], null) },
  jsxElementName('Text', []),
)

const ellipseComponentBeingInserted = componentBeingInserted(
  { 'utopia-api': importDetails(null, [importAlias('Ellipse')], null) },
  jsxElementName('Ellipse', []),
)

const rectangleComponentBeingInserted = componentBeingInserted(
  { 'utopia-api': importDetails(null, [importAlias('Rectangle')], null) },
  jsxElementName('Rectangle', []),
)

const animatedDivComponentBeingInserted = componentBeingInserted(
  { 'react-spring': importDetails(null, [importAlias('animated')], null) },
  jsxElementName('animated', ['div']),
)

class InsertMenuInner extends React.Component<InsertMenuProps> {
  shouldComponentUpdate(nextProps: InsertMenuProps) {
    const shouldUpdate =
      this.props.lastFontSettings !== nextProps.lastFontSettings ||
      this.props.editorDispatch !== nextProps.editorDispatch ||
      this.props.selectedViews !== nextProps.selectedViews ||
      this.props.mode !== nextProps.mode ||
      this.props.dependencies !== nextProps.dependencies ||
      this.props.packageStatus !== nextProps.packageStatus ||
      this.props.propertyControlsInfo !== nextProps.propertyControlsInfo

    return shouldUpdate
  }

  dispatchOne(action: EditorAction): void {
    this.props.editorDispatch([action], 'everyone')
  }

  sceneInsertMode = () => {
    this.props.editorDispatch([enableInsertModeForScene('scene')], 'everyone')
  }

  divInsertMode = () => {
    const newUID = generateUID(this.props.existingUIDs)
    this.props.editorDispatch(
      [enableInsertModeForJSXElement(defaultDivElement(newUID), newUID, {}, null)],
      'everyone',
    )
  }

  imageInsert = () => {
    insertImage(this.props.editorDispatch)
  }

  textInsertMode = () => {
    const newUID = generateUID(this.props.existingUIDs)
    this.props.editorDispatch(
      [
        enableInsertModeForJSXElement(
          defaultTextElement(newUID),
          newUID,
          { 'utopia-api': importDetails(null, [importAlias('Text')], null) },
          null,
        ),
      ],
      'everyone',
    )
  }

  animatedDivInsertMode = () => {
    const newUID = generateUID(this.props.existingUIDs)
    this.props.editorDispatch(
      [
        enableInsertModeForJSXElement(
          defaultAnimatedDivElement(newUID),
          newUID,
          { 'react-spring': importDetails(null, [importAlias('animated')], null) },
          null,
        ),
      ],
      'everyone',
    )
  }

  ellipseInsertMode = () => {
    const newUID = generateUID(this.props.existingUIDs)
    this.props.editorDispatch(
      [
        enableInsertModeForJSXElement(
          defaultEllipseElement(newUID),
          newUID,
          { 'utopia-api': importDetails(null, [importAlias('Ellipse')], null) },
          null,
        ),
      ],
      'everyone',
    )
  }

  rectangleInsertMode = () => {
    const newUID = generateUID(this.props.existingUIDs)
    this.props.editorDispatch(
      [
        enableInsertModeForJSXElement(
          defaultRectangleElement(newUID),
          newUID,
          { 'utopia-api': importDetails(null, [importAlias('Rectangle')], null) },
          null,
        ),
      ],
      'everyone',
    )
  }

  getDependencyStatus(dependencyName: string, defaultStatus: PackageStatus): PackageStatus {
    const regularStatus = this.props.packageStatus[dependencyName]?.status
    switch (regularStatus) {
      case null:
        return defaultStatus
      case 'loaded':
        if (dependencyName in this.props.propertyControlsInfo) {
          return 'loaded'
        } else {
          return 'loading'
        }
      default:
        return regularStatus
    }
  }

  render() {
    let sceneSelected: boolean = false
    let currentlyBeingInserted: ComponentBeingInserted | null = null
    if (this.props.mode.type === 'insert') {
      if (insertionSubjectIsScene(this.props.mode.subject)) {
        sceneSelected = true
      } else if (insertionSubjectIsJSXElement(this.props.mode.subject)) {
        const insertionSubject: ElementInsertionSubject = this.props.mode.subject
        currentlyBeingInserted = componentBeingInserted(
          insertionSubject.importsToAdd,
          insertionSubject.element.name,
        )
      }
    }

    return (
      <React.Fragment>
        <InsertGroup label='Storyboard' dependencyStatus='loaded' dependencyVersion={null}>
          <InsertItem
            type='scene'
            label='Scene'
            selected={sceneSelected}
            onMouseDown={this.sceneInsertMode}
          />
        </InsertGroup>
        <InsertGroup label='Utopia Components' dependencyStatus='loaded' dependencyVersion={null}>
          <InsertItem
            type='div'
            label='div'
            selected={componentBeingInsertedEquals(
              currentlyBeingInserted,
              divComponentBeingInserted,
            )}
            onMouseDown={this.divInsertMode}
          />
          <InsertItem
            type='image'
            label='Image'
            selected={componentBeingInsertedEquals(
              currentlyBeingInserted,
              imageComponentBeingInserted,
            )}
            onMouseDown={this.imageInsert}
          />
          <InsertItem
            type='text'
            label='Text'
            selected={componentBeingInsertedEquals(
              currentlyBeingInserted,
              textComponentBeingInserted,
            )}
            onMouseDown={this.textInsertMode}
          />
          <InsertItem
            type='ellipse'
            label='Ellipse'
            selected={componentBeingInsertedEquals(
              currentlyBeingInserted,
              ellipseComponentBeingInserted,
            )}
            onMouseDown={this.ellipseInsertMode}
          />
          <InsertItem
            type='rectangle'
            label='Rectangle'
            selected={componentBeingInsertedEquals(
              currentlyBeingInserted,
              rectangleComponentBeingInserted,
            )}
            onMouseDown={this.rectangleInsertMode}
          />
          <InsertItem
            type='div'
            label='Animated Div'
            selected={componentBeingInsertedEquals(
              currentlyBeingInserted,
              animatedDivComponentBeingInserted,
            )}
            onMouseDown={this.animatedDivInsertMode}
          />
        </InsertGroup>
        {this.props.currentlyOpenFilename == null ? null : (
          <InsertGroup
            label='Current File'
            subLabel={this.props.currentlyOpenFilename}
            dependencyStatus='loaded'
            dependencyVersion={null}
          >
            {this.props.currentFileComponents.map((currentFileComponent) => {
              const { componentName, defaultProps, detectedProps } = currentFileComponent
              const warningMessage = findMissingDefaultsAndGetWarning(detectedProps, defaultProps)
              const insertItemOnMouseDown = () => {
                const newUID = generateUID(this.props.existingUIDs)
                let props: JSXAttributes = objectMap(jsxAttributeValue, defaultProps)
                props['data-uid'] = jsxAttributeValue(newUID)
                const newElement = jsxElement(jsxElementName(componentName, []), props, [])
                this.props.editorDispatch(
                  [enableInsertModeForJSXElement(newElement, newUID, {}, null)],
                  'everyone',
                )
              }

              return (
                <InsertItem
                  key={`insert-item-${currentFileComponent.componentName}`}
                  type={'component'}
                  label={currentFileComponent.componentName}
                  selected={componentBeingInsertedEquals(
                    currentlyBeingInserted,
                    componentBeingInserted(
                      {},
                      jsxElementName(currentFileComponent.componentName, []),
                    ),
                  )}
                  onMouseDown={insertItemOnMouseDown}
                  warningMessage={warningMessage}
                />
              )
            })}
          </InsertGroup>
        )}
        {this.props.dependencies.map((dependency, dependencyIndex) => {
          if (isResolvedNpmDependency(dependency)) {
            const componentDescriptor = getThirdPartyComponents(dependency.name, dependency.version)
            if (componentDescriptor == null) {
              return null
            } else {
              const dependencyStatus = this.getDependencyStatus(dependency.name, 'loaded')
              const components = dependencyStatus === 'loaded' ? componentDescriptor.components : []
              return (
                <InsertGroup
                  label={componentDescriptor.name}
                  key={dependency.name}
                  dependencyVersion={dependency.version}
                  dependencyStatus={dependencyStatus}
                >
                  {components.map((component, componentIndex) => {
                    const insertItemOnMouseDown = () => {
                      const newUID = generateUID(this.props.existingUIDs)
                      const newElement = {
                        ...component.element,
                        props: {
                          ...component.element.props,
                          ['data-uid']: jsxAttributeValue(newUID),
                        },
                      }
                      this.props.editorDispatch(
                        [
                          enableInsertModeForJSXElement(
                            newElement,
                            newUID,
                            component.importsToAdd,
                            null,
                          ),
                        ],
                        'everyone',
                      )
                    }
                    return (
                      <InsertItem
                        key={`insert-item-third-party-${dependencyIndex}-${componentIndex}`}
                        type={'component'}
                        label={component.name}
                        selected={componentBeingInsertedEquals(
                          currentlyBeingInserted,
                          componentBeingInserted(component.importsToAdd, component.element.name),
                        )}
                        onMouseDown={insertItemOnMouseDown}
                      />
                    )
                  })}
                </InsertGroup>
              )
            }
          } else {
            if (isBuiltInDependency(dependency.name)) {
              return null
            } else {
              return (
                <InsertGroup
                  label={dependency.name}
                  dependencyStatus={this.getDependencyStatus(dependency.name, 'loading')}
                  dependencyVersion={null}
                />
              )
            }
          }
        })}
      </React.Fragment>
    )
  }
}

interface InsertGroupProps {
  label: string
  subLabel?: string
  dependencyStatus: PackageStatus
  dependencyVersion: string | null
}

export const InsertGroup: React.FunctionComponent<InsertGroupProps> = betterReactMemo(
  'InsertGroup',
  (props) => {
    return (
      <div style={{ paddingBottom: 12 }}>
        <FlexRow style={{ height: UtopiaTheme.layout.rowHeight.medium }}>
          <InspectorSubsectionHeader>
            <div style={{ color: colorTheme.emphasizedForeground.value, fontWeight: 500 }}>
              {props.label}
            </div>
            {props.subLabel == null ? null : (
              <div style={{ color: colorTheme.tertiaryForeground.value, paddingLeft: 10 }}>
                {props.subLabel}
              </div>
            )}
          </InspectorSubsectionHeader>
          <div style={{ flexGrow: 1, textAlign: 'right' }}>
            <NpmDependencyVersionAndStatusIndicator
              status={props.dependencyStatus}
              version={props.dependencyVersion}
            />
          </div>
        </FlexRow>
        <div style={{ padding: 8 }}>{props.children}</div>
      </div>
    )
  },
)

interface InsertItemProps {
  label: string
  selected: boolean
  type: string
  onMouseDown?: (event: React.MouseEvent<HTMLDivElement>) => void
  category?: string
  disabled?: boolean
  warningMessage?: string
}

export const InsertItem: React.StatelessComponent<InsertItemProps> = (props) => {
  const regularIcon = (
    <Icn
      category={props.category ? props.category : 'element'}
      type={props.type}
      color={props.selected ? 'white' : 'darkgray'}
      width={18}
      height={18}
    />
  )
  const resultingIcon =
    props.warningMessage == null ? regularIcon : <WarningIcon tooltipText={props.warningMessage} />

  return (
    <FlexRow
      css={{
        height: UtopiaTheme.layout.rowHeight.medium,
        background: props.selected ? UtopiaStyles.backgrounds.blue : 'initial',
        color: props.selected ? colorTheme.white.value : 'initial',
        opacity: props.disabled ? 0.3 : 1,
        '&:hover': {
          border: `1px solid ${colorTheme.primary.value}`,
        },
      }}
      onMouseDown={props.disabled ? Utils.NO_OP : props.onMouseDown}
    >
      {resultingIcon}
      <span className='pl8 '>{props.label}</span>
    </FlexRow>
  )
}
