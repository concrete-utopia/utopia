/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import {
  JSXElementName,
  jsxElementNameEquals,
  jsExpressionValue,
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
  importDetails,
  importAlias,
  Imports,
  importsEquals,
} from '../../core/shared/project-file-types'
import Utils from '../../utils/utils'
import { FontSettings } from '../inspector/common/css-utils'
import { EditorAction, EditorDispatch } from './action-types'
import { enableInsertModeForJSXElement } from './actions/action-creators'
import { InsertionSubject, Mode } from './editor-modes'
import { getOpenFilename } from './store/editor-state'
import { Substores, useEditorState } from './store/store-hook'
import { WarningIcon } from '../../uuiui/warning-icon'
import { usePossiblyResolvedPackageDependencies } from './npm-dependency/npm-dependency'
import {
  PossiblyUnversionedNpmDependency,
  PackageStatusMap,
  PackageStatus,
} from '../../core/shared/npm-dependency-types'
import { NpmDependencyVersionAndStatusIndicator } from '../navigator/dependecy-version-status-indicator'
import { PropertyControlsInfo } from '../custom-code/code-file'
import { InspectorSubsectionHeader, useColorTheme, Icn, UtopiaStyles, UIRow } from '../../uuiui'
import {
  getInsertableGroupLabel,
  getInsertableGroupPackageStatus,
  getNonEmptyComponentGroups,
  InsertableComponent,
  moveSceneToTheBeginningAndSetDefaultSize,
} from '../shared/project-components'
import { ProjectContentTreeRoot } from '../assets'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { UTOPIA_UID_KEY } from '../../core/model/utopia-constants'
import CanvasActions from '../canvas/canvas-actions'
import {
  boundingArea,
  createHoverInteractionViaMouse,
  createInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import { Modifier } from '../../utils/modifiers'
import * as PP from '../../core/shared/property-path'
import { setJSXValueInAttributeAtPath } from '../../core/shared/jsx-attributes'
import { windowToCanvasCoordinates } from '../canvas/dom-lookup'
import { CanvasVector, point, windowPoint } from '../../core/shared/math-utils'
import { isLeft } from '../../core/shared/either'
import { useDispatch } from './store/dispatch-context'
import { assertNever } from '../../core/shared/utils'
import { defaultDivElement } from './defaults'

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
  canvasScale: number
  canvasOffset: CanvasVector
}

export const InsertMenu = React.memo(() => {
  const dispatch = useDispatch()
  const restOfEditorProps = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        lastFontSettings: store.editor.lastUsedFont,
        mode: store.editor.mode,
        packageStatus: store.editor.nodeModules.packageStatus,
        propertyControlsInfo: store.editor.propertyControlsInfo,
      }
    },
    'InsertMenu restOfEditorProps',
  )

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'InsertMenu selectedViews',
  )

  const canvasProps = useEditorState(
    Substores.canvas,
    (store) => {
      return {
        currentlyOpenFilename: store.editor.canvas.openFile?.filename ?? null,
        canvasScale: store.editor.canvas.scale,
      }
    },
    'InsertMenu canvasProps',
  )

  const roundedCanvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'InsertMenu roundedCanvasOffset',
  )

  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'InsertMenu projectContents',
  )

  const dependencies = usePossiblyResolvedPackageDependencies()

  const propsWithDependencies: InsertMenuProps = {
    ...restOfEditorProps,
    ...canvasProps,
    selectedViews: selectedViews,
    canvasOffset: roundedCanvasOffset,
    projectContents: projectContents,
    editorDispatch: dispatch,
    dependencies: dependencies,
  }

  return <InsertMenuInner {...propsWithDependencies} />
})

type ElementBeingInserted =
  | {
      type: 'component'
      importsToAdd: Imports
      elementName: JSXElementName
      props: JSXAttributes
    }
  | {
      type: 'fragment'
    }
  | {
      type: 'conditional'
    }

function componentBeingInserted(
  importsToAdd: Imports,
  elementName: JSXElementName,
  props: JSXAttributes,
): ElementBeingInserted {
  return {
    type: 'component',
    importsToAdd: importsToAdd,
    elementName: elementName,
    props: props,
  }
}

function elementBeingInserted(insertableComponent: InsertableComponent): ElementBeingInserted {
  switch (insertableComponent.element.type) {
    case 'JSX_CONDITIONAL_EXPRESSION':
      return { type: 'conditional' }
    case 'JSX_FRAGMENT':
      return { type: 'fragment' }
    case 'JSX_ELEMENT':
      return componentBeingInserted(
        insertableComponent.importsToAdd,
        insertableComponent.element.name,
        insertableComponent.element.props,
      )
    default:
      assertNever(insertableComponent.element)
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

export function elementBeingInsertedEquals(
  first: ElementBeingInserted | null,
  second: ElementBeingInserted | null,
): boolean {
  if (first == null) {
    return second == null
  }

  if (second == null) {
    return false
  }

  if (first.type === 'component' && second.type === 'component') {
    return (
      importsEquals(first.importsToAdd, second.importsToAdd) &&
      jsxElementNameEquals(first.elementName, second.elementName) &&
      nonUidPropsEqual(first.props, second.props)
    )
  }

  return first.type === second.type
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

  getNewUID = () => generateUidWithExistingComponents(this.props.projectContents)

  render() {
    let currentlyBeingInserted: ElementBeingInserted | null = null

    if (this.props.mode.type === 'insert' && this.props.mode.subjects.length === 1) {
      const insertionSubject: InsertionSubject = this.props.mode.subjects[0]
      currentlyBeingInserted = componentBeingInserted(
        insertionSubject.importsToAdd,
        insertionSubject.element.name,
        insertionSubject.element.props,
      )
    }

    const insertableGroups =
      this.props.currentlyOpenFilename == null
        ? []
        : moveSceneToTheBeginningAndSetDefaultSize(
            getNonEmptyComponentGroups(
              this.props.packageStatus,
              this.props.propertyControlsInfo,
              this.props.projectContents,
              this.props.dependencies,
              this.props.currentlyOpenFilename,
            ),
          )

    return [
      ...insertableGroups.map((insertableGroup, groupIndex) => {
        return (
          <InsertGroup
            label={getInsertableGroupLabel(insertableGroup.source)}
            key={`insert-group-${groupIndex}`}
            dependencyVersion={null}
            dependencyStatus={getInsertableGroupPackageStatus(insertableGroup.source)}
          >
            {insertableGroup.insertableComponents.map((component, componentIndex) => {
              const insertItemOnMouseDown = (event: React.MouseEvent) => {
                const newUID = this.getNewUID()

                const mousePoint = windowToCanvasCoordinates(
                  this.props.canvasScale,
                  this.props.canvasOffset,
                  windowPoint(point(event.clientX, event.clientY)),
                ).canvasPositionRounded

                const createInteractionSessionCommand = CanvasActions.createInteractionSession(
                  createInteractionViaMouse(
                    mousePoint,
                    Modifier.modifiersForEvent(event),
                    boundingArea(),
                    'zero-drag-permitted',
                  ),
                )

                switch (component.element.type) {
                  case 'JSX_ELEMENT': {
                    const newElement = jsxElement(
                      component.element.name,
                      newUID,
                      setJSXAttributesAttribute(
                        addPositionAbsoluteToProps(component.element.props),
                        'data-uid',
                        jsExpressionValue(newUID, emptyComments),
                      ),
                      component.element.children,
                    )

                    return this.props.editorDispatch(
                      [
                        enableInsertModeForJSXElement(
                          newElement,
                          newUID,
                          component.importsToAdd,
                          component.defaultSize,
                        ),
                        createInteractionSessionCommand,
                      ],
                      'everyone',
                    )
                  }
                  case 'JSX_CONDITIONAL_EXPRESSION': {
                    return this.props.editorDispatch(
                      [
                        enableInsertModeForJSXElement(
                          defaultDivElement(newUID),
                          newUID,
                          component.importsToAdd,
                          component.defaultSize,
                          { wrapInContainer: 'conditional' },
                        ),
                        createInteractionSessionCommand,
                      ],
                      'everyone',
                    )
                  }
                  case 'JSX_FRAGMENT':
                    return this.props.editorDispatch(
                      [
                        enableInsertModeForJSXElement(
                          defaultDivElement(newUID),
                          newUID,
                          component.importsToAdd,
                          component.defaultSize,
                          { wrapInContainer: 'fragment' },
                        ),
                        createInteractionSessionCommand,
                      ],
                      'everyone',
                    )
                  default:
                    assertNever(component.element)
                }
              }

              const insertItemOnMouseUp = (event: React.MouseEvent) => {
                const mousePoint = windowToCanvasCoordinates(
                  this.props.canvasScale,
                  this.props.canvasOffset,
                  windowPoint(point(event.clientX, event.clientY)),
                ).canvasPositionRounded

                this.props.editorDispatch(
                  [CanvasActions.clearInteractionSession(false)],
                  'everyone',
                )
                this.props.editorDispatch(
                  [
                    CanvasActions.createInteractionSession(
                      createHoverInteractionViaMouse(
                        mousePoint,
                        Modifier.modifiersForEvent(event),
                        boundingArea(),
                        'zero-drag-permitted',
                      ),
                    ),
                  ],
                  'everyone',
                )
              }
              return (
                <InsertItem
                  key={`insert-item-third-party-${groupIndex}-${componentIndex}`}
                  type={'component'}
                  label={component.name}
                  selected={elementBeingInsertedEquals(
                    currentlyBeingInserted,
                    elementBeingInserted(component),
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
  const styleAttributes = getJSXAttribute(props, 'style') ?? jsExpressionValue({}, emptyComments)

  const updatedStyleAttrs = setJSXValueInAttributeAtPath(
    styleAttributes,
    PP.fromString('position'),
    jsExpressionValue('absolute', emptyComments),
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
        <div style={{ padding: 8, color: colorTheme.subduedForeground.value }}>
          {props.children}
        </div>
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
        background: props.selected ? UtopiaStyles.backgrounds.blue : undefined,
        color: props.selected ? colorTheme.white.value : undefined,
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
