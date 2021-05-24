/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import {
  JSXElementName,
  jsxElementName,
  jsxElementNameEquals,
  jsxAttributeValue,
  setJSXAttributesAttribute,
  jsxElement,
} from '../../core/shared/element-template'
import { generateUID } from '../../core/shared/uid-utils'
import {
  ElementPath,
  isTextFile,
  importDetails,
  importAlias,
  Imports,
  importsEquals,
  forEachParseSuccess,
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
import { EditorAction, EditorDispatch } from './action-types'
import { enableInsertModeForJSXElement, enableInsertModeForScene } from './actions/action-creators'
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
import { emptyComments } from '../../core/workers/parser-printer/parser-printer-comments'
import {
  FlexRow,
  UtopiaTheme,
  InspectorSubsectionHeader,
  colorTheme,
  Icn,
  UtopiaStyles,
  UIRow,
} from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'
import {
  getComponentGroups,
  getDependencyStatus,
  getInsertableGroupLabel,
  getInsertableGroupPackageStatus,
} from '../shared/project-components'
import { ProjectContentTreeRoot } from '../assets'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'

interface InsertMenuProps {
  lastFontSettings: FontSettings | null
  editorDispatch: EditorDispatch
  selectedViews: Array<ElementPath>
  mode: Mode
  currentlyOpenFilename: string | null
  dependencies: Array<PossiblyUnversionedNpmDependency>
  packageStatus: PackageStatusMap
  propertyControlsInfo: PropertyControlsInfo
  projectContents: ProjectContentTreeRoot
}

export const InsertMenu = betterReactMemo('InsertMenu', () => {
  const props = useEditorState((store) => {
    const openFileFullPath = getOpenFilename(store.editor)

    return {
      lastFontSettings: store.editor.lastUsedFont,
      editorDispatch: store.dispatch,
      selectedViews: store.editor.selectedViews,
      mode: store.editor.mode,
      currentlyOpenFilename: openFileFullPath,
      packageStatus: store.editor.nodeModules.packageStatus,
      propertyControlsInfo: store.editor.propertyControlsInfo,
      projectContents: store.editor.projectContents,
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

  getNewUID = () => generateUidWithExistingComponents(this.props.projectContents)

  divInsertMode = () => {
    const newUID = this.getNewUID()
    this.props.editorDispatch(
      [enableInsertModeForJSXElement(defaultDivElement(newUID), newUID, {}, null)],
      'everyone',
    )
  }

  imageInsert = () => {
    insertImage(this.props.editorDispatch)
  }

  textInsertMode = () => {
    const newUID = this.getNewUID()
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
    const newUID = this.getNewUID()
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
    const newUID = this.getNewUID()
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
    const newUID = this.getNewUID()
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

    const insertableGroups =
      this.props.currentlyOpenFilename == null
        ? []
        : getComponentGroups(
            this.props.packageStatus,
            this.props.propertyControlsInfo,
            this.props.projectContents,
            this.props.dependencies,
            this.props.currentlyOpenFilename,
          )

    return [
      // FIXME: Once scenes are refactored, this should be removed.
      <InsertGroup
        label='Storyboard'
        key='insert-group-storyboard'
        dependencyStatus='loaded'
        dependencyVersion={null}
      >
        <InsertItem
          type='scene'
          label='Scene'
          selected={sceneSelected}
          onMouseDown={this.sceneInsertMode}
        />
      </InsertGroup>,
      ...insertableGroups.map((insertableGroup, groupIndex) => {
        return (
          <InsertGroup
            label={getInsertableGroupLabel(insertableGroup.source)}
            key={`insert-group-${groupIndex}`}
            dependencyVersion={null}
            dependencyStatus={getInsertableGroupPackageStatus(insertableGroup.source)}
          >
            {insertableGroup.insertableComponents.map((component, componentIndex) => {
              const insertItemOnMouseDown = () => {
                const newUID = this.getNewUID()
                const newElement = jsxElement(
                  component.element.name,
                  newUID,
                  setJSXAttributesAttribute(
                    component.element.props,
                    'data-uid',
                    jsxAttributeValue(newUID, emptyComments),
                  ),
                  component.element.children,
                )
                this.props.editorDispatch(
                  [enableInsertModeForJSXElement(newElement, newUID, component.importsToAdd, null)],
                  'everyone',
                )
              }
              return (
                <InsertItem
                  key={`insert-item-third-party-${groupIndex}-${componentIndex}`}
                  type={'component'}
                  label={component.name}
                  selected={componentBeingInsertedEquals(
                    currentlyBeingInserted,
                    componentBeingInserted(component.importsToAdd, component.element.name),
                  )}
                  // eslint-disable-next-line react/jsx-no-bind
                  onMouseDown={insertItemOnMouseDown}
                />
              )
            })}
          </InsertGroup>
        )
      }),
    ]
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
        <UIRow rowHeight={'normal'}>
          <InspectorSubsectionHeader>
            <div style={{ color: colorTheme.emphasizedForeground.value, fontWeight: 500 }}>
              {props.label}
            </div>
            {props.subLabel == null ? null : (
              <div style={{ color: colorTheme.secondaryForeground.value, paddingLeft: 10 }}>
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
        </UIRow>
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
    <UIRow
      rowHeight={'normal'}
      css={{
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
    </UIRow>
  )
}
