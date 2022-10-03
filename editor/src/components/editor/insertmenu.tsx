/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import {
  JSXElementName,
  jsxElementName,
  jsxElementNameEquals,
  jsxAttributeValue,
  setJSXAttributesAttribute,
  jsxElement,
  emptyComments,
  JSXAttributes,
  JSXAttributesPart,
  isJSXAttributesEntry,
  getJSXAttribute,
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
import { forEachRight, isLeft } from '../../core/shared/either'
import { dropFileExtension } from '../../core/shared/file-utils'
import { objectMap } from '../../core/shared/object-utils'
import { WarningIcon } from '../../uuiui/warning-icon'
import { usePossiblyResolvedPackageDependencies } from './npm-dependency/npm-dependency'
import {
  PossiblyUnversionedNpmDependency,
  isResolvedNpmDependency,
  PackageStatusMap,
  PackageStatus,
} from '../../core/shared/npm-dependency-types'
import { NpmDependencyVersionAndStatusIndicator } from '../navigator/dependecy-version-status-indicator'
import { PropertyControlsInfo } from '../custom-code/code-file'
import {
  FlexRow,
  UtopiaTheme,
  InspectorSubsectionHeader,
  useColorTheme,
  Icn,
  UtopiaStyles,
  UIRow,
} from '../../uuiui'
import {
  getDependencyStatus,
  getInsertableGroupLabel,
  getInsertableGroupPackageStatus,
  getNonEmptyComponentGroups,
} from '../shared/project-components'
import { ProjectContentTreeRoot } from '../assets'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { UTOPIA_UID_KEY } from '../../core/model/utopia-constants'
import CanvasActions from '../canvas/canvas-actions'
import {
  boundingArea,
  createInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import { CanvasMousePositionRaw } from '../../utils/global-positions'
import { emptyModifiers, Modifier } from '../../utils/modifiers'
import * as EP from '../../core/shared/element-path'
import * as PP from '../../core/shared/property-path'
import { setJSXValueInAttributeAtPath } from '../../core/shared/jsx-attributes'

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

export const InsertMenu = React.memo(() => {
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
  props: JSXAttributes
}

export function componentBeingInserted(
  importsToAdd: Imports,
  elementName: JSXElementName,
  props: JSXAttributes,
): ComponentBeingInserted {
  return {
    importsToAdd: importsToAdd,
    elementName: elementName,
    props: props,
  }
}

function isUidProp(prop: JSXAttributesPart): boolean {
  return isJSXAttributesEntry(prop) && prop.key === UTOPIA_UID_KEY
}

const isNonUidProp = (prop: JSXAttributesPart) => !isUidProp(prop)

function nonUidPropsEqual(l: JSXAttributes, r: JSXAttributes): boolean {
  const nonUidL = l.filter(isNonUidProp)
  const nonUidR = r.filter(isNonUidProp)
  return (
    nonUidL.length === nonUidR.length && nonUidL.every((v, i) => isUidProp(v) || nonUidR[i] === v)
  )
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
        jsxElementNameEquals(first.elementName, second.elementName) &&
        nonUidPropsEqual(first.props, second.props)
      )
    }
  }
}

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
          insertionSubject.element.props,
        )
      }
    }

    const insertableGroups =
      this.props.currentlyOpenFilename == null
        ? []
        : getNonEmptyComponentGroups(
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

                const updatedPropsWithPosition = addPositionAbsoluteToProps(component.element.props)

                const newElement = jsxElement(
                  component.element.name,
                  newUID,
                  setJSXAttributesAttribute(
                    updatedPropsWithPosition,
                    'data-uid',
                    jsxAttributeValue(newUID, emptyComments),
                  ),
                  component.element.children,
                )
                this.props.editorDispatch(
                  [
                    enableInsertModeForJSXElement(newElement, newUID, component.importsToAdd, null),
                    CanvasActions.createInteractionSession(
                      createInteractionViaMouse(
                        CanvasMousePositionRaw!,
                        emptyModifiers,
                        boundingArea(),
                      ),
                    ),
                  ],
                  'everyone',
                )
              }
              const insertItemOnMouseUp = () => {
                this.props.editorDispatch(
                  [CanvasActions.clearInteractionSession(false)],
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
                    componentBeingInserted(
                      component.importsToAdd,
                      component.element.name,
                      component.element.props,
                    ),
                  )}
                  // eslint-disable-next-line react/jsx-no-bind
                  onMouseDown={insertItemOnMouseDown}
                  // eslint-disable-next-line react/jsx-no-bind
                  onMouseUp={insertItemOnMouseUp}
                />
              )
            })}
          </InsertGroup>
        )
      }),
    ]
  }
}

function addPositionAbsoluteToProps(props: JSXAttributes) {
  const styleAttributes = getJSXAttribute(props, 'style') ?? jsxAttributeValue({}, emptyComments)

  const updatedStyleAttrs = setJSXValueInAttributeAtPath(
    styleAttributes,
    PP.fromString('position'),
    jsxAttributeValue('absolute', emptyComments),
  )

  if (isLeft(updatedStyleAttrs)) {
    throw new Error(`Problem setting position absolute on an element we just created.`)
  }

  return setJSXAttributesAttribute(props, 'style', updatedStyleAttrs.value)
}

interface InsertGroupProps {
  label: string
  subLabel?: string
  dependencyStatus: PackageStatus
  dependencyVersion: string | null
}

export const InsertGroup: React.FunctionComponent<React.PropsWithChildren<InsertGroupProps>> =
  React.memo((props) => {
    const colorTheme = useColorTheme()
    return (
      <div style={{ paddingBottom: 12 }}>
        <UIRow rowHeight={'normal'}>
          <InspectorSubsectionHeader>
            <div style={{ color: colorTheme.emphasizedForeground.value, fontWeight: 500 }}>
              {props.label}
            </div>
            {props.subLabel == null ? null : (
              <div style={{ color: colorTheme.subduedForeground.value, paddingLeft: 10 }}>
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
  })

interface InsertItemProps {
  label: string
  selected: boolean
  type: string
  onMouseDown?: (event: React.MouseEvent<HTMLDivElement>) => void
  onMouseUp?: (event: React.MouseEvent<HTMLDivElement>) => void
  category?: string
  disabled?: boolean
  warningMessage?: string
}

export const InsertItem: React.FunctionComponent<React.PropsWithChildren<InsertItemProps>> = (
  props,
) => {
  const colorTheme = useColorTheme()
  const regularIcon = (
    <Icn
      category={props.category ?? 'element'}
      type={props.type}
      color={props.selected ? 'primary' : 'main'}
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
        gap: 8,
        '&:hover': {
          border: `1px solid ${colorTheme.primary.value}`,
        },
      }}
      onMouseDown={props.disabled ? Utils.NO_OP : props.onMouseDown}
      onMouseUp={props.disabled ? Utils.NO_OP : props.onMouseUp}
      data-testid={`insert-item-${props.label}`}
    >
      {resultingIcon}
      <span>{props.label}</span>
    </UIRow>
  )
}
