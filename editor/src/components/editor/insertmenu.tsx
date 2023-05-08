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
import { isLeft } from '../../core/shared/either'
import {
  JSXAttributes,
  JSXElementName,
  emptyComments,
  getJSXAttribute,
  jsExpressionValue,
  jsxElement,
  jsxElementNameEquals,
  setJSXAttributesAttribute,
} from '../../core/shared/element-template'
import { setJSXValueInAttributeAtPath } from '../../core/shared/jsx-attributes'
import { CanvasVector, canvasPoint, point, windowPoint } from '../../core/shared/math-utils'
import {
  PackageStatus,
  PackageStatusMap,
  PossiblyUnversionedNpmDependency,
} from '../../core/shared/npm-dependency-types'
import { ElementPath, Imports, importsEquals } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { assertNever } from '../../core/shared/utils'
import { Modifier, emptyModifiers } from '../../utils/modifiers'
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
import { CanvasAction } from '../canvas/canvas-types'
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
import { EditorDispatch } from './action-types'
import { enableInsertModeForJSXElement, setRightMenuTab } from './actions/action-creators'
import { defaultDivElement } from './defaults'
import { InsertionSubject, Mode } from './editor-modes'
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
      jsxElementNameEquals(first.elementName, second.elementName)
    )
  }

  return first.type === second.type
}

const makeInteraction = (
  component: InsertableComponent,
  newUID: string,
  createInteractionSessionCommand: CanvasAction,
  dispatch: EditorDispatch,
) => {
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

const Option = React.memo((props: OptionProps<ComponentOptionItem, false>) => {
  const dispatch = useDispatch()
  const component: InsertableComponent = props.data.value
  const { isFocused } = props

  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'CustomOption project contents',
  )

  const { canvasScale, canvasOffset } = useEditorState(
    Substores.canvasOffset,
    (store) => ({
      canvasScale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
    }),
    'CustomOption canvas offsets',
  )

  const getNewUID = React.useCallback(
    () => generateUidWithExistingComponents(projectContents),
    [projectContents],
  )

  const mode = React.useMemo(() => props.selectProps.mode, [props.selectProps])

  const currentlyBeingInserted = React.useMemo(() => {
    if (mode == null || mode.type !== 'insert' || mode.subjects.length !== 1) {
      return null
    }

    const insertionSubject: InsertionSubject = mode.subjects[0]

    return componentBeingInserted(
      insertionSubject.importsToAdd,
      insertionSubject.element.name,
      insertionSubject.element.props,
    )
  }, [mode])

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
    makeInteraction(component, newUID, createInteractionSessionCommand, dispatch)
  }

  const insertItemOnMouseUp = (_event: React.MouseEvent) => {
    dispatch([CanvasActions.clearInteractionSession(false)], 'everyone')
  }

  const isSelected = React.useMemo(() => {
    const beingInserted = elementBeingInsertedEquals(
      currentlyBeingInserted,
      elementBeingInserted(component),
    )
    return beingInserted || (isFocused && currentlyBeingInserted == null)
  }, [component, currentlyBeingInserted, isFocused])

  return (
    <div ref={props.innerRef} {...props.innerProps}>
      <InsertItem
        key={`insert-item-third-party-${props.innerProps.id}`}
        type={'component'}
        label={component.name}
        selected={isSelected}
        // eslint-disable-next-line react/jsx-no-bind
        onMouseDown={insertItemOnMouseDown}
        // eslint-disable-next-line react/jsx-no-bind
        onMouseUp={insertItemOnMouseUp}
      />
    </div>
  )
})

type GroupOptionItem = {
  label: string
  options: ComponentOptionItem[]
}

type ComponentOptionItem = {
  label: string
  source: string
  value: InsertableComponent
}

function useSelectStyles(hasResults: boolean): StylesConfig<GroupOptionItem, false> {
  const colorTheme = useColorTheme()
  return React.useMemo(
    () => ({
      container: (styles): CSSObject => ({
        height: '100%',
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
        padding: 8,
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
          marginTop: 8,
          paddingLeft: 8,
          paddingRight: 8,
          height: '100%',
        }
      },
      menuList: (styles): CSSObject => {
        return {
          height: '100%',
          overflow: 'scroll',
          paddingBottom: 100,
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
          border: `1px solid ${hasResults ? 'transparent' : colorTheme.error.value}`,
        }
      },
      placeholder: (styles): CSSObject => {
        return { ...styles, position: 'absolute', marginLeft: 5 }
      },
      group: (): CSSObject => {
        return {
          paddingBottom: 20,
        }
      },
      groupHeading: (styles): CSSObject => {
        return {
          fontWeight: 700,
          paddingBottom: 16,
        }
      },
    }),
    [colorTheme, hasResults],
  )
}

const InsertMenuInner = React.memo((props: InsertMenuProps) => {
  const dispatch = useDispatch()
  const [filter, setFilter] = React.useState('')

  function onFilterChange(newValue: string, actionMeta: InputActionMeta) {
    if (actionMeta.action !== 'input-blur' && actionMeta.action !== 'menu-close') {
      setFilter(newValue.trim())
    }
  }

  const onKeyDown = React.useCallback(
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

  const options = React.useMemo((): GroupOptionItem[] => {
    return insertableGroups.map((g) => {
      const groupLabel = getInsertableGroupLabel(g.source)
      return {
        label: groupLabel,
        options: g.insertableComponents.map((c): ComponentOptionItem => {
          return {
            label: c.name,
            source: groupLabel,
            value: c,
          }
        }),
      }
    })
  }, [insertableGroups])

  const onChange = React.useCallback(
    (e: ComponentOptionItem) => {
      onFilterChange(e.label, { action: 'input-change' })

      makeInteraction(
        e.value,
        generateUidWithExistingComponents(props.projectContents),
        CanvasActions.createInteractionSession(
          createInteractionViaMouse(
            canvasPoint({ x: 0, y: 0 }),
            emptyModifiers,
            boundingArea(),
            'zero-drag-permitted',
          ),
        ),
        dispatch,
      )

      dispatch([CanvasActions.clearInteractionSession(false)], 'everyone')

      dispatch(
        [
          CanvasActions.createInteractionSession(
            createHoverInteractionViaMouse(
              canvasPoint({ x: 0, y: 0 }),
              emptyModifiers,
              boundingArea(),
              'zero-drag-permitted',
            ),
          ),
        ],
        'everyone',
      )
    },
    [dispatch, props.projectContents],
  )

  const filterOption = createFilter({
    ignoreAccents: true,
    stringify: (c) => c.data.source + c.data.label,
    ignoreCase: true,
    trim: true,
    matchFrom: 'any',
  })

  function alwaysTrue() {
    return true
  }

  const { value, hasResults } = React.useMemo(() => {
    const filtered = options
      .flatMap((g) => g.options)
      .filter((o) => filterOption({ data: o } as any, filter))
    return { hasResults: filtered.length > 0, value: filter.length > 0 ? filtered[0] : undefined }
  }, [options, filter, filterOption])

  const styles = useSelectStyles(hasResults)

  return (
    <WindowedSelect
      autoFocus
      key={`insert-menu-select-${filter}`} // required to programmatically set the filter
      value={value}
      inputValue={filter}
      onInputChange={onFilterChange}
      isMulti={false}
      controlShouldRenderValue={false}
      hideSelectedOptions={false}
      menuIsOpen
      placeholder='Filter…'
      tabSelectsValue={false}
      options={options}
      onKeyDown={onKeyDown}
      mode={props.mode}
      components={{ Option: Option }}
      onChange={onChange}
      styles={styles}
      filterOption={!hasResults ? alwaysTrue : filterOption}
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
      color={props.selected ? 'on-highlight-main' : 'main'}
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
        borderRadius: 2,
        padding: 4,
        background: props.selected ? colorTheme.primary.value : undefined,
        color: props.selected ? colorTheme.white.value : undefined,
        opacity: props.disabled ? 0.3 : 1,
        gap: 8,
        border: '1px solid transparent',
        '&:hover': {
          borderColor: `${colorTheme.primary.value}`,
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
