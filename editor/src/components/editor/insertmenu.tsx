/** @jsxRuntime classic */
/** @jsx jsx */
import type { CSSObject } from '@emotion/react'
import { jsx } from '@emotion/react'
import React, { useState } from 'react'
import type {
  InputActionMeta,
  InputProps,
  MenuListComponentProps,
  OptionProps,
  StylesConfig,
} from 'react-windowed-select'
import WindowedSelect, { components, createFilter } from 'react-windowed-select'
import { RightMenuTab } from '../../components/editor/store/editor-state'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { isLeft } from '../../core/shared/either'
import type { JSXAttributes, JSXElementName } from '../../core/shared/element-template'
import {
  emptyComments,
  getJSXAttribute,
  jsExpressionValue,
  jsxElement,
  jsxElementNameEquals,
  setJSXAttributesAttribute,
} from '../../core/shared/element-template'
import { setJSXValueInAttributeAtPath } from '../../core/shared/jsx-attribute-utils'
import type { CanvasVector } from '../../core/shared/math-utils'
import { canvasPoint, point, windowPoint } from '../../core/shared/math-utils'
import type {
  PackageStatus,
  PackageStatusMap,
  PossiblyUnversionedNpmDependency,
} from '../../core/shared/npm-dependency-types'
import type { ElementPath, Imports } from '../../core/shared/project-file-types'
import { importsEquals } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { NO_OP, assertNever } from '../../core/shared/utils'
import { Modifier, emptyModifiers } from '../../utils/modifiers'
import { Icn, InspectorSubsectionHeader, UIRow, UtopiaTheme, useColorTheme } from '../../uuiui'
import { getControlStyles } from '../../uuiui-deps'
import { InspectorInputEmotionStyle } from '../../uuiui/inputs/base-input'
import type { ProjectContentTreeRoot } from '../assets'
import CanvasActions from '../canvas/canvas-actions'
import {
  boundingArea,
  createHoverInteractionViaMouse,
  createInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import type { CanvasAction } from '../canvas/canvas-types'
import { windowToCanvasCoordinates } from '../canvas/dom-lookup'
import type { PropertyControlsInfo } from '../custom-code/code-file'
import type { FontSettings } from '../inspector/common/css-utils'
import { NpmDependencyVersionAndStatusIndicator } from '../navigator/dependecy-version-status-indicator'
import type { InsertableComponent } from '../shared/project-components'
import {
  getInsertableGroupLabel,
  getNonEmptyComponentGroups,
  moveSceneToTheBeginningAndSetDefaultSize,
} from '../shared/project-components'
import type { EditorDispatch } from './action-types'
import { enableInsertModeForJSXElement, setRightMenuTab } from './actions/action-creators'
import { defaultDivElement } from './defaults'
import type { InsertionSubject, Mode } from './editor-modes'
import { usePossiblyResolvedPackageDependencies } from './npm-dependency/npm-dependency'
import { useDispatch } from './store/dispatch-context'
import { Substores, useEditorState } from './store/store-hook'

export const InsertMenuId = 'insert-menu-inspector-tab'

export const InsertMenuFilterTestId = 'insert-menu-filter'

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
  | {
      type: 'map'
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
  const element = insertableComponent.element()
  switch (element.type) {
    case 'JSX_CONDITIONAL_EXPRESSION':
      return { type: 'conditional' }
    case 'JSX_FRAGMENT':
      return { type: 'fragment' }
    case 'JSX_MAP_EXPRESSION':
      return { type: 'map' }
    case 'JSX_ELEMENT':
      return componentBeingInserted(insertableComponent.importsToAdd, element.name, element.props)
    default:
      assertNever(element)
  }
}

export function elementBeingInsertedEquals(
  first: ElementBeingInserted,
  second: ElementBeingInserted,
): boolean {
  if (first.type === 'component' && second.type === 'component') {
    return (
      importsEquals(first.importsToAdd, second.importsToAdd) &&
      jsxElementNameEquals(first.elementName, second.elementName)
    )
  }

  return first.type === second.type
}

const enableInsertMode = (
  component: InsertableComponent,
  newUID: string,
  createInteractionSessionCommand: CanvasAction,
  dispatch: EditorDispatch,
) => {
  const element = component.element()
  switch (element.type) {
    case 'JSX_ELEMENT': {
      const newElement = jsxElement(
        element.name,
        newUID,
        setJSXAttributesAttribute(
          addPositionAbsoluteToProps(element.props),
          'data-uid',
          jsExpressionValue(newUID, emptyComments),
        ),
        element.children,
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
    case 'JSX_MAP_EXPRESSION':
      return NO_OP() // // we don't support draw to insert for maps
    default:
      assertNever(element)
  }
}

const Input = (props: InputProps) => {
  return <components.Input {...props} data-testid={InsertMenuFilterTestId} />
}

const Option = React.memo((props: OptionProps<ComponentOptionItem, false>) => {
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()
  const component: InsertableComponent = props.data.value
  const { isFocused } = props
  const isActive: boolean = (props.selectProps as any).isActive

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

  const mode = React.useMemo(() => (props.selectProps as any).mode as Mode, [props.selectProps])

  const currentlyBeingInserted = React.useMemo(() => {
    if (mode === null || mode.type !== 'insert' || mode.subjects.length !== 1) {
      return null
    }

    const insertionSubject: InsertionSubject = mode.subjects[0]

    return componentBeingInserted(
      insertionSubject.importsToAdd,
      insertionSubject.element.name,
      insertionSubject.element.props,
    )
  }, [mode])

  const insertItemOnMouseDown = React.useCallback(
    (event: React.MouseEvent) => {
      const mousePoint = windowToCanvasCoordinates(
        canvasScale,
        canvasOffset,
        windowPoint(point(event.clientX, event.clientY)),
      ).canvasPositionRounded
      enableInsertMode(
        component,
        generateUidWithExistingComponents(projectContents),
        CanvasActions.createInteractionSession(
          createInteractionViaMouse(
            mousePoint,
            Modifier.modifiersForEvent(event),
            boundingArea(),
            'zero-drag-permitted',
          ),
        ),
        dispatch,
      )
    },
    [dispatch, canvasOffset, canvasScale, projectContents, component],
  )

  const insertItemOnMouseUp = React.useCallback(
    (_event: React.MouseEvent) => {
      dispatch([CanvasActions.clearInteractionSession(false)], 'everyone')
    },
    [dispatch],
  )

  const isComponentCurrentlyInserted =
    currentlyBeingInserted != null &&
    elementBeingInsertedEquals(currentlyBeingInserted, elementBeingInserted(component))

  const isSelected = React.useMemo(() => {
    return (
      isComponentCurrentlyInserted || (isActive && isFocused && currentlyBeingInserted !== null)
    )
  }, [isComponentCurrentlyInserted, isActive, isFocused, currentlyBeingInserted])

  const [isHovered, setIsHovered] = useState(false)
  const setIsHoveredTrue = React.useCallback(() => {
    setIsHovered(true)
  }, [])

  const setIsHoveredFalse = React.useCallback(() => {
    setIsHovered(false)
  }, [])

  return (
    <div ref={props.innerRef} {...props.innerProps} style={{}}>
      <UIRow
        rowHeight={'smaller'}
        css={{
          borderRadius: 2,
          padding: 4,
          color: isHovered
            ? colorTheme.dynamicBlue.value
            : isSelected
            ? colorTheme.bg1.value
            : colorTheme.fg1.value,
          background: isComponentCurrentlyInserted ? colorTheme.dynamicBlue.value : undefined,
          gap: 4,
          border: '1px solid transparent',
        }}
        onMouseDown={insertItemOnMouseDown}
        onMouseUp={insertItemOnMouseUp}
        onMouseEnter={setIsHoveredTrue}
        onMouseLeave={setIsHoveredFalse}
        data-testid={`insert-item-${props.label}`}
      >
        <Icn
          category='element'
          type='component'
          color={isHovered ? 'dynamic' : isSelected ? 'on-light-main' : 'main'}
          width={18}
          height={18}
          style={{ transform: 'scale(0.8)' }}
        />
        <span>{props.label}</span>
      </UIRow>
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
          display: 'flex',
          flexDirection: 'column',
          gap: 2,
          paddingLeft: 8,
          paddingRight: 8,
        }
      },
      input: (styles): CSSObject => {
        return {
          ...(InspectorInputEmotionStyle({
            hasLabel: false,
            controlStyles: getControlStyles('simple'),
          }) as CSSObject),
          paddingLeft: 4,
          backgroundColor: colorTheme.bg2.value,
          flexGrow: 1,
          display: 'flex',
          alignItems: 'center',
          cursor: 'text',
          border: `1px solid ${hasResults ? 'transparent' : colorTheme.error.value}`,
        }
      },
      placeholder: (styles): CSSObject => {
        return {
          ...styles,
          position: 'absolute',
          marginLeft: 5,
        }
      },
      group: (): CSSObject => {
        return {
          display: 'flex',
          flexDirection: 'column',
          gap: 2,
        }
      },
      groupHeading: (styles): CSSObject => {
        return {
          display: 'flex',
          alignItems: 'center',
          height: UtopiaTheme.layout.rowHeight.smaller,
          fontWeight: 700,
        }
      },
    }),
    [colorTheme, hasResults],
  )
}

const MenuList = React.memo((menuListProps: MenuListComponentProps<ComponentOptionItem, false>) => {
  const focusedOption: ComponentOptionItem | null = (menuListProps as any).focusedOption
  const { selectProps } = menuListProps

  React.useEffect(() => {
    selectProps.onFocusedOptionChange(focusedOption)
  }, [focusedOption, selectProps])

  return <components.MenuList {...menuListProps} />
})

const InsertMenuInner = React.memo((props: InsertMenuProps) => {
  const dispatch = useDispatch()
  const [filter, setFilter] = React.useState('')

  const insertableGroups = React.useMemo(() => {
    if (props.currentlyOpenFilename == null) {
      return []
    }
    return moveSceneToTheBeginningAndSetDefaultSize(
      getNonEmptyComponentGroups(
        'insert',
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

  const filterOption = createFilter({
    ignoreAccents: true,
    stringify: (c) => c.data.source + c.data.label,
    ignoreCase: true,
    trim: true,
    matchFrom: 'any',
  })

  const { hasResults } = React.useMemo(() => {
    const filteredOptions = options
      .flatMap((g) => g.options)
      .filter((o) => filterOption({ data: o } as any, filter))
    return {
      hasResults: filteredOptions.length > 0,
    }
  }, [options, filter, filterOption])

  function onFilterChange(newValue: string, actionMeta: InputActionMeta) {
    if (actionMeta.action !== 'input-blur' && actionMeta.action !== 'menu-close') {
      setFilter(newValue.trim())
    }
  }

  const [focusedOption, setFocusedOption] = React.useState<ComponentOptionItem | null>(null)

  const onKeyDown = React.useCallback(
    (e: React.KeyboardEvent) => {
      setIsActive(true)
      if (e.key === 'Escape') {
        dispatch([setRightMenuTab(RightMenuTab.Inspector)])
      }
      if (e.key === 'Enter' && focusedOption != null) {
        enableInsertMode(
          focusedOption.value,
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
      }
    },
    [dispatch, props.projectContents, focusedOption],
  )

  const onChange = React.useCallback(
    (e: ComponentOptionItem) => {
      onFilterChange(e.label, { action: 'input-change' })

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
    [dispatch],
  )

  function alwaysTrue() {
    return true
  }

  const styles = useSelectStyles(hasResults)

  const [isActive, setIsActive] = React.useState(true)
  function onMouseLeave() {
    setIsActive(false)
  }
  function onMouseEnter() {
    setIsActive(true)
  }

  return (
    <div
      id={InsertMenuId}
      style={{ height: '100%' }}
      onMouseLeave={onMouseLeave}
      onMouseEnter={onMouseEnter}
    >
      <WindowedSelect
        autoFocus
        key={'insert-menu-select'}
        value={focusedOption}
        inputValue={filter}
        // eslint-disable-next-line react/jsx-no-bind
        onInputChange={onFilterChange}
        isMulti={false}
        controlShouldRenderValue={false}
        hideSelectedOptions={false}
        menuIsOpen
        placeholder='Select…'
        tabSelectsValue={false}
        options={options}
        onKeyDown={onKeyDown}
        mode={props.mode}
        components={{
          Option: Option,
          Input: Input,
          MenuList: MenuList,
        }}
        isActive={isActive}
        onFocusedOptionChange={setFocusedOption}
        onChange={onChange}
        styles={styles}
        filterOption={!hasResults ? alwaysTrue : filterOption}
      />
    </div>
  )
})
InsertMenuInner.displayName = 'InsertMenuInner'

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
