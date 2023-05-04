/** @jsxRuntime classic */
/** @jsx jsx */
import { CSSObject, jsx } from '@emotion/react'
import React from 'react'
import WindowedSelect, {
  InputActionMeta,
  OptionProps,
  StylesConfig,
  createFilter,
} from 'react-windowed-select'
import { RightMenuTab } from '../../components/editor/store/editor-state'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { UTOPIA_UID_KEY } from '../../core/model/utopia-constants'
import { isLeft } from '../../core/shared/either'
import {
  JSXAttributes,
  JSXAttributesPart,
  JSXElementName,
  emptyComments,
  getJSXAttribute,
  isJSXAttributesEntry,
  jsExpressionValue,
  jsxElement,
  jsxElementNameEquals,
  setJSXAttributesAttribute,
} from '../../core/shared/element-template'
import { setJSXValueInAttributeAtPath } from '../../core/shared/jsx-attributes'
import { CanvasVector, point, windowPoint } from '../../core/shared/math-utils'
import {
  PackageStatus,
  PackageStatusMap,
  PossiblyUnversionedNpmDependency,
} from '../../core/shared/npm-dependency-types'
import { ElementPath, Imports, importsEquals } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { assertNever } from '../../core/shared/utils'
import { Modifier } from '../../utils/modifiers'
import Utils from '../../utils/utils'
import { Icn, InspectorSubsectionHeader, UIRow, useColorTheme } from '../../uuiui'
import { getControlStyles } from '../../uuiui-deps'
import { InspectorInputEmotionStyle } from '../../uuiui/inputs/base-input'
import { WarningIcon } from '../../uuiui/warning-icon'
import { ProjectContentTreeRoot } from '../assets'
import CanvasActions from '../canvas/canvas-actions'
import {
  boundingArea,
  createHoverInteractionViaMouse,
  createInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../canvas/dom-lookup'
import { PropertyControlsInfo } from '../custom-code/code-file'
import { FontSettings } from '../inspector/common/css-utils'
import { NpmDependencyVersionAndStatusIndicator } from '../navigator/dependecy-version-status-indicator'
import {
  InsertableComponent,
  getInsertableGroupLabel,
  getNonEmptyComponentGroups,
  moveSceneToTheBeginningAndSetDefaultSize,
} from '../shared/project-components'
import { enableInsertModeForJSXElement, setRightMenuTab } from './actions/action-creators'
import { defaultDivElement } from './defaults'
import { Mode } from './editor-modes'
import { usePossiblyResolvedPackageDependencies } from './npm-dependency/npm-dependency'
import { useDispatch } from './store/dispatch-context'
import { Substores, useEditorState } from './store/store-hook'

interface InsertMenuProps {
  lastFontSettings: FontSettings | null
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

const CustomOption = React.memo((props: OptionProps<ComponentItem, false>) => {
  const dispatch = useDispatch()
  const component: InsertableComponent = props.data.value
  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    '',
  )
  const { canvasScale, canvasOffset } = useEditorState(
    Substores.canvasOffset,
    (store) => ({
      canvasScale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
    }),
    '',
  )
  const getNewUID = React.useCallback(
    () => generateUidWithExistingComponents(projectContents),
    [projectContents],
  )

  const insertItemOnMouseDown = (event: React.MouseEvent) => {
    const newUID = getNewUID()

    const mousePoint = windowToCanvasCoordinates(
      canvasScale,
      canvasOffset,
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

        return dispatch(
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
        return dispatch(
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
        return dispatch(
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
      canvasScale,
      canvasOffset,
      windowPoint(point(event.clientX, event.clientY)),
    ).canvasPositionRounded

    dispatch([CanvasActions.clearInteractionSession(false)], 'everyone')
    dispatch(
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
    <div ref={props.innerRef} {...props.innerProps}>
      <InsertItem
        key={`insert-item-third-party-${props.innerProps.id}`}
        type={'component'}
        label={component.name}
        selected={false}
        // eslint-disable-next-line react/jsx-no-bind
        onMouseDown={insertItemOnMouseDown}
        // eslint-disable-next-line react/jsx-no-bind
        onMouseUp={insertItemOnMouseUp}
      />
    </div>
  )
})

type GroupItem = {
  label: string
  options: ComponentItem[]
}

type ComponentItem = {
  label: string
  source: string
  value: InsertableComponent
}

function useStyles(): StylesConfig<GroupItem, false> {
  const colorTheme = useColorTheme()
  return React.useMemo(
    () => ({
      container: (styles): CSSObject => ({
        height: '100%',
        display: 'flex',
        flexDirection: 'column',
        paddingLeft: 8,
        paddingRight: 8,
      }),
      control: (styles): CSSObject => ({
        background: 'transparent',
        outline: 'none',
        ':focus-within': {
          outline: 'none',
          border: 'none',
        },
        paddingBottom: 20,
        paddingTop: 10,
      }),
      valueContainer: (styles): CSSObject => ({
        display: 'flex',
        position: 'relative',
        flexGrow: 1,
        flexShrink: 0,
        alignItems: 'center',
        gap: 4,
      }),
      indicatorsContainer: (styles): CSSObject => ({
        display: 'none',
      }),
      menu: (styles): CSSObject => {
        return {
          flex: 1,
          maxHeight: '100%',
          display: 'flex',
          flexDirection: 'column',
        }
      },
      menuList: (styles): CSSObject => {
        return {
          maxHeight: '100%',
          overflow: 'scroll',
        }
      },
      input: (styles): CSSObject => {
        return {
          ...(InspectorInputEmotionStyle({
            hasLabel: false,
            controlStyles: getControlStyles('simple'),
          }) as CSSObject),
          paddingLeft: 4,
          backgroundColor: colorTheme.bg4.value,
          flexGrow: 1,
          display: 'flex',
          alignItems: 'center',
          cursor: 'text',
          border: `1px solid transparent`,
        }
      },
      placeholder: (styles): CSSObject => {
        return { ...styles, marginLeft: 5 }
      },
      group: (): CSSObject => {
        return {
          paddingBottom: 20,
        }
      },
      groupHeading: (styles): CSSObject => {
        return {
          fontWeight: 700,
          paddingBottom: 5,
        }
      },
    }),
    [colorTheme],
  )
}

const InsertMenuInner = React.memo((props: InsertMenuProps) => {
  const dispatch = useDispatch()
  const [filter, setFilter] = React.useState('')

  function onFilterChange(newValue: string, actionMeta: InputActionMeta) {
    if (actionMeta.action !== 'input-blur' && actionMeta.action !== 'menu-close') {
      setFilter(newValue.toLowerCase())
    }
  }

  const onFilterEscape = React.useCallback(
    (e: React.KeyboardEvent) => {
      if (e.key === 'Escape') {
        dispatch([setRightMenuTab(RightMenuTab.Inspector)])
      }
    },
    [dispatch],
  )

  const insertableGroups = React.useMemo(() => {
    if (props.currentlyOpenFilename == null) {
      return []
    }
    return moveSceneToTheBeginningAndSetDefaultSize(
      getNonEmptyComponentGroups(
        props.packageStatus,
        props.propertyControlsInfo,
        props.projectContents,
        props.dependencies,
        props.currentlyOpenFilename,
      ),
    )
  }, [
    props.packageStatus,
    props.propertyControlsInfo,
    props.projectContents,
    props.dependencies,
    props.currentlyOpenFilename,
  ])

  const listOptions = React.useMemo((): GroupItem[] => {
    return insertableGroups.map((g) => {
      const groupLabel = getInsertableGroupLabel(g.source)
      return {
        label: groupLabel,
        options: g.insertableComponents.map((c): ComponentItem => {
          return {
            label: c.name,
            source: groupLabel,
            value: c,
          }
        }),
      }
    })
  }, [insertableGroups])

  const styles = useStyles()

  return (
    <WindowedSelect
      autoFocus
      inputValue={filter}
      onInputChange={onFilterChange}
      isMulti={false}
      controlShouldRenderValue={false}
      hideSelectedOptions={false}
      menuIsOpen
      placeholder='Filter…'
      tabSelectsValue={false}
      options={listOptions}
      onKeyDown={onFilterEscape}
      components={{
        Option: CustomOption,
      }}
      styles={styles}
      filterOption={createFilter({
        ignoreAccents: true,
        stringify: (c) => c.data.source + c.data.label,
        ignoreCase: true,
        trim: true,
        matchFrom: 'any',
      })}
    />
  )
})

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
        background: props.selected ? colorTheme.primary.value : undefined,
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
