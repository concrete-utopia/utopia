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
import { TemplatePath, isCodeFile, isUIJSFile } from '../../core/shared/project-file-types'
import Utils from '../../utils/utils'
import {
  defaultAnimatedDivElement,
  defaultEllipseElement,
  defaultRectangleElement,
  defaultTextElement,
  defaultViewElement,
} from './defaults'
import { FontSettings } from '../inspector/new-inspector/css-utils'
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
import { dropExtension } from '../../core/shared/string-utils'
import { objectMap } from '../../core/shared/object-utils'
import {
  defaultPropertiesForComponentInFile,
  findMissingDefaultsAndGetWarning,
} from '../../core/property-controls/property-controls-utils'
import { WarningIcon } from '../../uuiui/warning-icon'

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
}

export const InsertMenu = betterReactMemo('InsertMenu', () => {
  const props: InsertMenuProps = useEditorState((store) => {
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
              dropExtension(openFileFullPath),
              store.editor.codeResultCache,
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
    }
  })
  return <InsertMenuInner {...props} />
})

export interface ComponentBeingInserted {
  importedFrom: string | null
  elementName: JSXElementName
}

export function componentBeingInserted(
  importedFrom: string | null,
  elementName: JSXElementName,
): ComponentBeingInserted {
  return {
    importedFrom: importedFrom,
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
        first.importedFrom === second.importedFrom &&
        jsxElementNameEquals(first.elementName, second.elementName)
      )
    }
  }
}

const viewComponentBeingInserted = componentBeingInserted('utopia-api', jsxElementName('View', []))

const imageComponentBeingInserted = componentBeingInserted(null, jsxElementName('img', []))

const textComponentBeingInserted = componentBeingInserted('utopia-api', jsxElementName('Text', []))

const ellipseComponentBeingInserted = componentBeingInserted(
  'utopia-api',
  jsxElementName('Ellipse', []),
)

const rectangleComponentBeingInserted = componentBeingInserted(
  'utopia-api',
  jsxElementName('Rectangle', []),
)

const animatedDivComponentBeingInserted = componentBeingInserted(
  'react-spring',
  jsxElementName('animated', ['div']),
)

class InsertMenuInner extends React.Component<InsertMenuProps, {}> {
  shouldComponentUpdate(nextProps: InsertMenuProps) {
    const shouldUpdate =
      this.props.lastFontSettings !== nextProps.lastFontSettings ||
      this.props.editorDispatch !== nextProps.editorDispatch ||
      this.props.selectedViews !== nextProps.selectedViews ||
      this.props.mode !== nextProps.mode

    return shouldUpdate
  }

  dispatchOne(action: EditorAction): void {
    this.props.editorDispatch([action], 'everyone')
  }

  sceneInsertMode = () => {
    this.props.editorDispatch([enableInsertModeForScene('scene')], 'everyone')
  }

  viewInsertMode = () => {
    const newUID = generateUID(this.props.existingUIDs)
    this.props.editorDispatch(
      [enableInsertModeForJSXElement(defaultViewElement(newUID), newUID, 'utopia-api', null)],
      'everyone',
    )
  }

  imageInsert = () => {
    insertImage(this.props.editorDispatch)
  }

  textInsertMode = () => {
    const newUID = generateUID(this.props.existingUIDs)
    this.props.editorDispatch(
      [enableInsertModeForJSXElement(defaultTextElement(newUID), newUID, 'utopia-api', null)],
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
          'react-spring',
          null,
        ),
      ],
      'everyone',
    )
  }

  ellipseInsertMode = () => {
    const newUID = generateUID(this.props.existingUIDs)
    this.props.editorDispatch(
      [enableInsertModeForJSXElement(defaultEllipseElement(newUID), newUID, 'utopia-api', null)],
      'everyone',
    )
  }

  rectangleInsertMode = () => {
    const newUID = generateUID(this.props.existingUIDs)
    this.props.editorDispatch(
      [enableInsertModeForJSXElement(defaultRectangleElement(newUID), newUID, 'utopia-api', null)],
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
          insertionSubject.importFromPath,
          insertionSubject.element.name,
        )
      }
    }

    return (
      <React.Fragment>
        <InsertGroup label='Storyboard'>
          <InsertItem
            type='scene'
            label='Scene'
            selected={sceneSelected}
            onMouseDown={this.sceneInsertMode}
          />
        </InsertGroup>
        <InsertGroup label='Utopia Components'>
          <InsertItem
            type='view'
            label='View'
            selected={componentBeingInsertedEquals(
              currentlyBeingInserted,
              viewComponentBeingInserted,
            )}
            onMouseDown={this.viewInsertMode}
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
          <InsertGroup label='Current File' subLabel={this.props.currentlyOpenFilename}>
            {this.props.currentFileComponents.map((currentFileComponent) => {
              const { componentName, defaultProps, detectedProps } = currentFileComponent
              const warningMessage = findMissingDefaultsAndGetWarning(detectedProps, defaultProps)
              const insertItemOnMouseDown = () => {
                const newUID = generateUID(this.props.existingUIDs)
                let props: JSXAttributes = objectMap(jsxAttributeValue, defaultProps)
                props['data-uid'] = jsxAttributeValue(newUID)
                const newElement = jsxElement(jsxElementName(componentName, []), props, [], null)
                this.props.editorDispatch(
                  [enableInsertModeForJSXElement(newElement, newUID, null, null)],
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
                      null,
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
      </React.Fragment>
    )
  }
}

interface InsertGroupProps {
  label: string
  subLabel?: string
}

export const InsertGroup: React.StatelessComponent<InsertGroupProps> = (props) => {
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
      </FlexRow>
      <div style={{ padding: 8 }}>{props.children}</div>
    </div>
  )
}

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
