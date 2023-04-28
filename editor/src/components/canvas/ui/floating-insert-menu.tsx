/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { CSSObject } from '@emotion/serialize'
import WindowedSelect, {
  ActionMeta,
  InputActionMeta,
  OptionProps,
  StylesConfig,
  ValueType,
} from 'react-windowed-select'

import { getControlStyles } from '../../../uuiui-deps'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'

import {
  FlexColumn,
  FlexRow,
  OnClickOutsideHOC,
  useColorTheme,
  UtopiaStyles,
  UtopiaTheme,
} from '../../../uuiui'
import { usePossiblyResolvedPackageDependencies } from '../../editor/npm-dependency/npm-dependency'
import {
  getInsertableGroupLabel,
  getNonEmptyComponentGroups,
  insertableComponent,
  InsertableComponent,
  InsertableComponentGroup,
  InsertableComponentGroupType,
} from '../../shared/project-components'
import {
  closeFloatingInsertMenu,
  insertInsertable,
  updateJSXElementName,
  wrapInElement,
} from '../../editor/actions/action-creators'
import {
  elementOnlyHasSingleTextChild,
  generateUidWithExistingComponents,
} from '../../../core/model/element-template-utils'
import {
  emptyComments,
  jsExpressionValue,
  JSXConditionalExpressionWithoutUID,
  jsxElement,
  JSXElementName,
  JSXFragmentWithoutUID,
  jsxTextBlock,
  setJSXAttributesAttribute,
} from '../../../core/shared/element-template'
import {
  getElementsToTarget,
  useHandleCloseOnESCOrEnter,
} from '../../inspector/common/inspector-utils'
import { EditorAction } from '../../editor/action-types'
import { InspectorInputEmotionStyle } from '../../../uuiui/inputs/base-input'
import { when } from '../../../utils/react-conditionals'
import { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { safeIndex } from '../../../core/shared/array-utils'
import { LayoutSystem } from 'utopia-api/core'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { optionalMap } from '../../../core/shared/optional-utils'
import { useDispatch } from '../../editor/store/dispatch-context'
import { assertNever } from '../../../core/shared/utils'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import { emptyElementPath } from '../../../core/shared/element-path'

export const FloatingMenuTestId = 'floating-menu-test-id'

type InsertMenuItemValue = InsertableComponent & {
  source: InsertableComponentGroupType | null
  key: string
}

type InsertMenuItem = {
  label: string
  source: string | null
  value: InsertMenuItemValue
}

type InsertMenuItemGroup = {
  label: string
  options: Array<InsertMenuItem>
}

type InsertableComponentFlatList = Array<InsertMenuItemGroup>

function convertInsertableComponentsToFlatList(
  insertableComponents: InsertableComponentGroup[],
): InsertableComponentFlatList {
  return insertableComponents.flatMap((componentGroup) => {
    return {
      label: getInsertableGroupLabel(componentGroup.source),
      options: componentGroup.insertableComponents.map(
        (componentToBeInserted, index): InsertMenuItem => {
          const source = index === 0 ? componentGroup.source : null
          return {
            label: componentToBeInserted.name,
            source: optionalMap(getInsertableGroupLabel, source),
            value: {
              ...componentToBeInserted,
              key: `${getInsertableGroupLabel(componentGroup.source)}-${
                componentToBeInserted.name
              }`,
              source: source,
            },
          }
        },
      ),
    }
  })
}

function useGetInsertableComponents(): InsertableComponentFlatList {
  const dependencies = usePossiblyResolvedPackageDependencies()

  const { packageStatus, propertyControlsInfo } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        packageStatus: store.editor.nodeModules.packageStatus,
        propertyControlsInfo: store.editor.propertyControlsInfo,
      }
    },
    'useGetInsertableComponents',
  )

  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'useGetInsertableComponents projectContents',
  )

  const fullPath = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.openFile?.filename ?? null,
    'useGetInsertableComponents fullPath',
  )

  const insertableComponents = React.useMemo(() => {
    if (fullPath == null) {
      return []
    } else {
      return convertInsertableComponentsToFlatList(
        getNonEmptyComponentGroups(
          packageStatus,
          propertyControlsInfo,
          projectContents,
          dependencies,
          fullPath,
        ),
      )
    }
  }, [packageStatus, propertyControlsInfo, projectContents, dependencies, fullPath])

  return insertableComponents
}

function useComponentSelectorStyles(): StylesConfig<InsertMenuItem, false> {
  const colorTheme = useColorTheme()
  // componentSelectorStyles will only be recreated if the theme changes, otherwise we re-use the same object
  return React.useMemo(
    () => ({
      container: (styles): CSSObject => ({
        // the outermost element. It contains the popup menu,  so don't set a height on it!
        // shouldn't contain any sizing
        // ...styles,
        flexGrow: 1,
        display: 'flex',
        flexDirection: 'column',
      }),
      control: (styles): CSSObject => ({
        // need to remove styles here, since that implicitly sets a height of 38
        // ...styles,
        display: 'flex',
        background: 'transparent',
        height: UtopiaTheme.layout.rowHeight.normal,
        outline: 'none',
        paddingLeft: 8,
        paddingRight: 8,
        ':focus-within': {
          outline: 'none',
          border: 'none',
        },
      }),
      valueContainer: (styles): CSSObject => ({
        // the container for added options (tags) and input
        // sibling to indicatorsContainer
        // default styles mess with layout, so ignore them
        // ...styles,
        display: 'flex',
        position: 'relative',
        flexGrow: 1,
        flexShrink: 0,
        overflowX: 'scroll',
        alignItems: 'center',
        gap: 4,
        paddingLeft: 0,
        paddingRight: 0,
        paddingTop: 0,
        paddingBottom: 0,
      }),
      indicatorsContainer: (styles): CSSObject => ({
        // ...styles,
        display: 'none',
      }),

      multiValue: (styles, { data }): CSSObject => {
        return {
          // ...styles,
          cursor: 'pointer',
          display: 'flex',
          alignItems: 'center',
        }
      },
      multiValueLabel: (styles, { data }): CSSObject => ({
        // ...styles,
        fontSize: 10,
        padding: '2px 4px',
      }),
      multiValueRemove: (styles, { data }): CSSObject => ({
        // ...styles,
        width: 11,
        display: 'flex',
        paddingTop: 2,
        opacity: 0.4,
        color: styles.color,
        ':hover': {
          opacity: 1,
          backgroundColor: styles.color,
        },
      }),
      menu: (styles): CSSObject => {
        // the outer shell
        return {
          // ...styles,
          boxShadow: 'none',
          borderRadius: 0,
          background: 'transparent',
          overflowY: 'scroll',
          flex: 1,
        }
      },
      menuList: (styles): CSSObject => {
        // the list wrapper
        return {
          // ...styles,
          position: 'relative',
          maxHeight: 210,
          paddingLeft: 8,
          paddingRight: 8,
          overflowY: 'auto',
          display: 'flex',
          flexDirection: 'column',
          gap: 6,
        }
      },
      input: (styles): CSSObject => {
        return {
          // ...styles,
          ...(InspectorInputEmotionStyle({
            hasLabel: false,
            controlStyles: getControlStyles('simple'),
          }) as CSSObject),
          paddingLeft: 4,
          backgroundColor: colorTheme.bg4.value,
          // color: 'black',
          // fontSize: 11,
          flexGrow: 1,
          // letterSpacing: 0.3,
          // background: 'transparent',
          display: 'flex',
          alignItems: 'center',
        }
      },
      placeholder: (styles): CSSObject => {
        return { ...styles, marginLeft: 4 }
      },
      option: (styles, { data, isDisabled, isFocused, isSelected }): CSSObject => {
        // a single entry in the options list

        return {
          // ...styles,
          height: UtopiaTheme.layout.rowHeight.smaller,
          display: 'flex',
          alignItems: 'center',
          paddingLeft: 4,
          paddingRight: 4,
          cursor: isDisabled ? 'not-allowed' : 'default',
          color: isFocused ? colorTheme.bg0.value : colorTheme.fg0.value,
          backgroundColor: isFocused ? colorTheme.primary.value : 'transparent',
          borderRadius: UtopiaTheme.inputBorderRadius,
        }
      },
      group: (): CSSObject => {
        return {
          // ...styles,
          // paddingTop: 6,
        }
      },
      groupHeading: (styles): CSSObject => {
        return {
          // ...styles,
          display: 'none',
        }
      },
    }),
    [colorTheme],
  )
}

interface CheckboxRowProps {
  id: string
  checked: boolean
  onChange: (value: boolean) => void
}

const CustomOption = (props: OptionProps<InsertMenuItem, false>) => {
  const { innerRef, innerProps, isDisabled, isFocused, label, data } = props
  const colorTheme = useColorTheme()
  return (
    <div
      ref={innerRef}
      {...innerProps}
      style={{
        boxSizing: 'border-box',
        height: UtopiaTheme.layout.rowHeight.smaller,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'space-between',
        paddingLeft: 4,
        paddingRight: 4,
        cursor: isDisabled ? 'not-allowed' : 'default',
        color: isFocused ? colorTheme.bg0.value : colorTheme.fg0.value,
        backgroundColor: isFocused ? colorTheme.primary.value : 'transparent',
        borderRadius: UtopiaTheme.inputBorderRadius,
      }}
    >
      <div style={{ paddingRight: 2 }}>{label}</div>
      <div
        style={{
          color: colorTheme.fg7.value,
          height: UtopiaTheme.layout.rowHeight.smaller,
          right: 12,
          display: 'flex',
          alignItems: 'center',
          pointerEvents: 'none',
          overflow: 'hidden',
        }}
      >
        <div style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
          {data.source ?? ''}
        </div>
        <span style={{ minWidth: 0 }}></span>
      </div>
    </div>
  )
}

const CheckboxRow = React.memo<React.PropsWithChildren<CheckboxRowProps>>(
  ({ id, checked, onChange, children }) => {
    const colorTheme = useColorTheme()

    const handleChange = React.useCallback(
      (event: React.ChangeEvent<HTMLInputElement>) => {
        onChange(event.target.checked)
      },
      [onChange],
    )

    return (
      <FlexRow css={{ height: 25, gap: 8, flex: 1 }}>
        <input
          type='checkbox'
          checked={checked}
          onChange={handleChange}
          css={{
            '&:focus': {
              outline: 'auto',
              outlineColor: colorTheme.primary.value,
              outlineOffset: 0,
            },
          }}
          id={id}
        />
        <label htmlFor={id} tabIndex={1}>
          {children}
        </label>
      </FlexRow>
    )
  },
)

function getMenuTitle(insertMenuMode: 'closed' | 'insert' | 'convert' | 'wrap'): string {
  switch (insertMenuMode) {
    case 'closed':
      return ''
    case 'convert':
      return 'Convert to'
    case 'insert':
      return 'Add Element'
    case 'wrap':
      return 'Wrap in'
  }
}

export var FloatingMenu = React.memo(() => {
  const colorTheme = useColorTheme()

  // This is a ref so that changing the highlighted element does not trigger a re-render loop
  // This is FINE because we only use the value in callbacks
  const activelySelectedInsertOptionRef = React.useRef<InsertMenuItem | null>(null)

  const ariaLiveMessages = React.useMemo(
    () => ({
      onFocus: ({ focused }: { focused: InsertMenuItem }) => {
        activelySelectedInsertOptionRef.current = focused
      },
    }),
    [],
  )

  const [filterInputValue, setFilterInputValue] = React.useState('')
  const onInputValueChange = React.useCallback((newValue: string, actionMeta: InputActionMeta) => {
    // when the user "tabs out" to the checkboxes, prevent react-select from clearing the input text
    if (actionMeta.action !== 'input-blur' && actionMeta.action !== 'menu-close') {
      setFilterInputValue(newValue)
      activelySelectedInsertOptionRef.current = null
    }
  }, [])

  const floatingMenuState = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.floatingInsertMenu,
    'FloatingMenu floatingMenuState',
  )

  const showInsertionControls = floatingMenuState.insertMenuMode === 'insert'

  const menuTitle: string = getMenuTitle(floatingMenuState.insertMenuMode)

  const componentSelectorStyles = useComponentSelectorStyles()
  const dispatch = useDispatch()

  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const selectedViewsref = useRefEditorState((store) => store.editor.selectedViews)
  const insertableComponents = useGetInsertableComponents()
  const shouldWrapContentsByDefault = useRefEditorState((store) => {
    // We only care about this when the menu is first opened
    const firstSelectedView = store.editor.selectedViews[0]
    if (firstSelectedView != null) {
      const selectedJSXElement = MetadataUtils.getJSXElementFromMetadata(
        store.editor.jsxMetadata,
        firstSelectedView,
      )
      return selectedJSXElement != null && elementOnlyHasSingleTextChild(selectedJSXElement)
    }

    return false
  })

  const [addContentForInsertion, setAddContentForInsertion] = React.useState(false)

  const [fixedSizeForInsertion, setFixedSizeForInsertion] = React.useState(false)

  const onChangeConditionalOrFragment = React.useCallback(
    (element: JSXConditionalExpressionWithoutUID | JSXFragmentWithoutUID): Array<EditorAction> => {
      let actionsToDispatch: Array<EditorAction> = []
      const selectedViews = selectedViewsref.current
      switch (floatingMenuState.insertMenuMode) {
        case 'wrap':
          actionsToDispatch = [
            wrapInElement(selectedViews, {
              element: {
                ...element,
                uid: generateUidWithExistingComponents(projectContentsRef.current),
              },
              importsToAdd: emptyImports(),
            }),
          ]
          break
        case 'insert':
          const targetParent = safeIndex(selectedViews, 0) ?? emptyElementPath

          const importsToAdd: Imports =
            element.type === 'JSX_FRAGMENT'
              ? {
                  react: {
                    importedAs: 'React',
                    importedFromWithin: [],
                    importedWithName: null,
                  },
                }
              : {}

          actionsToDispatch = [
            insertInsertable(
              targetParent,
              insertableComponent(importsToAdd, element, '', [], null),
              fixedSizeForInsertion ? 'add-size' : 'do-not-add',
              floatingMenuState.indexPosition,
            ),
          ]
          break
        case 'convert':
        case 'closed':
          break
        default:
          assertNever(floatingMenuState)
      }
      return actionsToDispatch
    },
    [selectedViewsref, floatingMenuState, projectContentsRef, fixedSizeForInsertion],
  )

  const onChangeElement = React.useCallback(
    (pickedInsertableComponent: InsertMenuItemValue): Array<EditorAction> => {
      if (pickedInsertableComponent.element.type !== 'JSX_ELEMENT') {
        return []
      }
      const selectedViews = selectedViewsref.current
      let actionsToDispatch: Array<EditorAction> = []
      switch (floatingMenuState.insertMenuMode) {
        case 'wrap':
          const newUID = generateUidWithExistingComponents(projectContentsRef.current)

          const newElement = jsxElement(
            pickedInsertableComponent.element.name,
            newUID,
            setJSXAttributesAttribute(
              pickedInsertableComponent.element.props,
              'data-uid',
              jsExpressionValue(newUID, emptyComments),
            ),
            pickedInsertableComponent.element.children,
          )

          actionsToDispatch = [
            wrapInElement(selectedViews, {
              element: newElement,
              importsToAdd: pickedInsertableComponent.importsToAdd,
            }),
          ]
          break
        case 'insert':
          let elementToInsert = pickedInsertableComponent
          if (addContentForInsertion && pickedInsertableComponent.element.children.length === 0) {
            elementToInsert = {
              ...pickedInsertableComponent,
              element: {
                ...pickedInsertableComponent.element,
                children: [jsxTextBlock('Utopia')],
              },
            }
          }

          const targetParent: ElementPath | null =
            floatingMenuState.parentPath ?? safeIndex(selectedViews, 0) ?? null
          if (targetParent != null) {
            // TODO multiselect?
            actionsToDispatch = [
              insertInsertable(
                targetParent,
                elementToInsert,
                fixedSizeForInsertion ? 'add-size' : 'do-not-add',
                floatingMenuState.indexPosition,
              ),
            ]
          }
          break
        case 'convert':
          const { element, importsToAdd } = pickedInsertableComponent
          // this is taken from render-as.tsx
          const targetsForUpdates = getElementsToTarget(selectedViews)
          actionsToDispatch = targetsForUpdates.flatMap((path) => {
            return updateJSXElementName(path, element.name, importsToAdd)
          })
          break
        case 'closed':
          break
        default:
          assertNever(floatingMenuState)
      }

      return actionsToDispatch
    },
    [
      floatingMenuState,
      projectContentsRef,
      selectedViewsref,
      fixedSizeForInsertion,
      addContentForInsertion,
    ],
  )

  const onChange = React.useCallback(
    (value: ValueType<InsertMenuItem, false>) => {
      if (value != null && !Array.isArray(value)) {
        const pickedInsertableComponent = (value as InsertMenuItem).value

        function getActionsToDispatch() {
          switch (pickedInsertableComponent.element.type) {
            case 'JSX_CONDITIONAL_EXPRESSION':
            case 'JSX_FRAGMENT':
              return onChangeConditionalOrFragment(pickedInsertableComponent.element)
            case 'JSX_ELEMENT':
              return onChangeElement(pickedInsertableComponent)
            default:
              assertNever(pickedInsertableComponent.element)
          }
        }

        const actionsToDispatch = getActionsToDispatch()

        dispatch([...actionsToDispatch, closeFloatingInsertMenu()])
      }
    },
    [onChangeConditionalOrFragment, onChangeElement, dispatch],
  )

  useHandleCloseOnESCOrEnter(
    React.useCallback(
      (key: 'Escape' | 'Enter') => {
        if (key === 'Escape') {
          dispatch([closeFloatingInsertMenu()])
        } else {
          onChange(activelySelectedInsertOptionRef.current)
        }
      },
      [dispatch, onChange],
    ),
  )

  return (
    <div
      style={{
        position: 'relative',
        fontSize: 11,
      }}
      data-testid={FloatingMenuTestId}
    >
      <FlexColumn
        style={{
          ...UtopiaStyles.popup,
          width: 280,
          height: 280,
          overflow: 'hidden',
        }}
      >
        <div
          style={{
            color: colorTheme.primary.value,
            display: 'flex',
            paddingLeft: 9,
            paddingRight: 8,
            height: UtopiaTheme.layout.rowHeight.normal,
            alignItems: 'center',
          }}
        >
          <b>{menuTitle}</b>
        </div>

        <WindowedSelect
          ariaLiveMessages={ariaLiveMessages}
          inputValue={filterInputValue}
          onInputChange={onInputValueChange}
          autoFocus
          isMulti={false}
          controlShouldRenderValue={false}
          hideSelectedOptions={false}
          menuIsOpen
          onChange={onChange}
          options={insertableComponents}
          placeholder='Search...'
          styles={componentSelectorStyles}
          tabSelectsValue={false}
          components={{ Option: CustomOption }}
        />
        {showInsertionControls ? (
          // <FlexColumn
          //   style={{
          //     position: 'absolute',
          //     bottom: 0,
          //     width: '100%',
          //     background: colorTheme.bg0.value,
          //     borderRadius: 6,
          //   }}
          // >
          <FlexRow
            css={{
              height: UtopiaTheme.layout.rowHeight.smaller,
              paddingLeft: 8,
              paddingRight: 8,
              position: 'absolute',
              bottom: 0,
              width: '100%',
              background: colorTheme.bg0.value,
              borderRadius: 6,
            }}
          >
            <CheckboxRow
              id='add-content-label'
              checked={addContentForInsertion}
              onChange={setAddContentForInsertion}
            >
              Add content
            </CheckboxRow>
            <CheckboxRow
              id='fixed-dimensions-label'
              checked={fixedSizeForInsertion}
              onChange={setFixedSizeForInsertion}
            >
              Fixed dimensions
            </CheckboxRow>
          </FlexRow>
        ) : // </FlexColumn>
        null}
      </FlexColumn>
    </div>
  )
})

interface FloatingInsertMenuProps {}

export const FloatingInsertMenu = React.memo((props: FloatingInsertMenuProps) => {
  const dispatch = useDispatch()
  const isVisible = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.floatingInsertMenu.insertMenuMode !== 'closed',
    'FloatingInsertMenu insertMenuOpen',
  )
  const onClickOutside = React.useCallback(() => {
    dispatch([closeFloatingInsertMenu()])
  }, [dispatch])

  return isVisible ? (
    <OnClickOutsideHOC onClickOutside={onClickOutside}>
      <div
        style={{
          pointerEvents: 'initial',
          position: 'absolute',
          left: '50%',
          top: '50%',
          transform: 'translateX(-50%) translateY(-50%)',
          zIndex: 30,
          // ^ above navigator
        }}
      >
        <FloatingMenu />
      </div>
    </OnClickOutsideHOC>
  ) : null
})
