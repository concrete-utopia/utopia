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
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'

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
  InsertableComponent,
  InsertableComponentGroup,
  InsertableComponentGroupType,
} from '../../shared/project-components'
import {
  closeFloatingInsertMenu,
  insertInsertable,
  updateJSXElementName,
  wrapInView,
  wrapInElement,
  enableInsertModeForJSXElement,
  switchEditorMode,
} from '../../editor/actions/action-creators'
import {
  elementOnlyHasSingleTextChild,
  generateUidWithExistingComponents,
} from '../../../core/model/element-template-utils'
import {
  emptyComments,
  jsxAttributeValue,
  jsxElement,
  JSXElementName,
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
import { ElementPath } from '../../../core/shared/project-file-types'
import { safeIndex } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { LayoutSystem } from 'utopia-api/core'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { optionalMap } from '../../../core/shared/optional-utils'
import {
  boundingArea,
  createInteractionViaKeyboard,
  createInteractionViaUI,
  keyboardCatcherControl,
} from '../canvas-strategies/interaction-state'
import { EditorModes, insertionParent, insertionSubject } from '../../editor/editor-modes'
import CanvasActions from '../canvas-actions'
import { EditorStorePatched } from '../../editor/store/editor-state'
import { setJSXValueAtPath } from '../../../core/shared/jsx-attributes'
import { UTOPIA_UID_KEY } from '../../../core/model/utopia-constants'
import { forceRight } from '../../../core/shared/either'

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
        (insertableComponent, index): InsertMenuItem => {
          const source = index === 0 ? componentGroup.source : null
          return {
            label: insertableComponent.name,
            source: optionalMap(getInsertableGroupLabel, source),
            value: {
              ...insertableComponent,
              key: `${getInsertableGroupLabel(componentGroup.source)}-${insertableComponent.name}`,
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

  const { packageStatus, propertyControlsInfo, projectContents, fullPath } = useEditorState(
    (store) => {
      return {
        packageStatus: store.editor.nodeModules.packageStatus,
        propertyControlsInfo: store.editor.propertyControlsInfo,
        projectContents: store.editor.projectContents,
        fullPath: store.editor.canvas.openFile?.filename ?? null,
      }
    },
    'RenderAsRow',
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

function getIsFlexBasedOnName_KILLME_EXPERIMENTAL(name: JSXElementName): boolean {
  return (
    name.propertyPath.propertyElements.length === 0 &&
    (name.baseVariable === 'FlexRow' || name.baseVariable === 'FlexCol')
  )
}

function getIsFlexDirectionBasedOnName_KILLME_SERIOUSLY_EXPERIMENTAL(
  name: JSXElementName,
): 'horizontal' | 'vertical' | null {
  if (name.propertyPath.propertyElements.length === 0) {
    if (name.baseVariable === 'FlexRow') {
      return 'horizontal'
    } else if (name.baseVariable === 'FlexCol') {
      return 'vertical'
    }
  }
  return null
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
          maxHeight: 150,
          padding: 4,
          paddingLeft: 8,
          paddingRight: 8,
          overflowY: 'scroll',
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
          color: isFocused ? colorTheme.inverted.fg0.value : colorTheme.fg0.value,
          backgroundColor: isFocused ? colorTheme.primary.value : 'transparent',
          borderRadius: UtopiaTheme.inputBorderRadius,
        }
      },
      group: (): CSSObject => {
        return {
          // ...styles,
          paddingTop: 6,
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
        color: isFocused ? colorTheme.inverted.fg0.value : colorTheme.fg0.value,
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
  const editorStoreRef = useRefEditorState((store) => store)

  const scheduleClearUiInteraction = useClearUIInteraction(editorStoreRef)

  const floatingMenuState = useEditorState(
    (store) => store.editor.floatingInsertMenu,
    'FloatingMenu floatingMenuState',
  )

  const showInsertionControls = floatingMenuState.insertMenuMode === 'insert'
  const showWrapControls = floatingMenuState.insertMenuMode === 'wrap'

  const menuTitle: string = getMenuTitle(floatingMenuState.insertMenuMode)

  const componentSelectorStyles = useComponentSelectorStyles()
  const dispatch = useEditorState((store) => store.dispatch, 'FloatingMenu dispatch')

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
  const [wrapContentForInsertion, setWrapContentForInsertion] = React.useState(
    shouldWrapContentsByDefault.current,
  )
  const [fixedSizeForInsertion, setFixedSizeForInsertion] = React.useState(false)
  const [preserveVisualPositionForWrap, setPreserveVisualPositionForWrap] = React.useState(false)

  const onChange = React.useCallback(
    (value: ValueType<InsertMenuItem, false>) => {
      if (value != null && !Array.isArray(value)) {
        const pickedInsertableComponent = (value as InsertMenuItem).value
        const selectedViews = selectedViewsref.current

        let actionsToDispatch: Array<EditorAction> = []
        switch (floatingMenuState.insertMenuMode) {
          case 'wrap': {
            const newUID = generateUidWithExistingComponents(projectContentsRef.current)
            const newElement = jsxElement(
              pickedInsertableComponent.element.name,
              newUID,
              setJSXAttributesAttribute(
                pickedInsertableComponent.element.props,
                'data-uid',
                jsxAttributeValue(newUID, emptyComments),
              ),
              pickedInsertableComponent.element.children,
            )

            const isFlexLayoutSystemMaybe_KILLME = getIsFlexBasedOnName_KILLME_EXPERIMENTAL(
              newElement.name,
            )
            const flexDirection_KILLME =
              getIsFlexDirectionBasedOnName_KILLME_SERIOUSLY_EXPERIMENTAL(newElement.name)

            actionsToDispatch = [
              preserveVisualPositionForWrap
                ? wrapInView(
                    selectedViews,
                    {
                      element: newElement,
                      importsToAdd: pickedInsertableComponent.importsToAdd,
                    },
                    isFlexLayoutSystemMaybe_KILLME ? 'flex' : LayoutSystem.PinSystem,
                    flexDirection_KILLME,
                  )
                : wrapInElement(selectedViews, {
                    element: newElement,
                    importsToAdd: pickedInsertableComponent.importsToAdd,
                  }),
            ]
            break
          }
          case 'insert': {
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
              if (wrapContentForInsertion) {
                actionsToDispatch = [
                  insertInsertable(
                    targetParent,
                    elementToInsert,
                    fixedSizeForInsertion ? 'add-size' : 'do-not-add',
                    wrapContentForInsertion ? 'wrap-content' : 'do-now-wrap-content',
                    floatingMenuState.indexPosition,
                  ),
                ]
              } else {
                const uid = generateUidWithExistingComponents(projectContentsRef.current)

                const propsWithUid = forceRight(
                  setJSXValueAtPath(
                    elementToInsert.element.props,
                    PP.create([UTOPIA_UID_KEY]),
                    jsxAttributeValue(uid, emptyComments),
                  ),
                  `Could not set data-uid on props of insertable element ${elementToInsert.element.name}`,
                )

                actionsToDispatch = [
                  enableInsertModeForJSXElement(
                    jsxElement(
                      elementToInsert.element.name,
                      uid,
                      propsWithUid,
                      elementToInsert.element.children,
                    ),
                    uid,
                    elementToInsert.importsToAdd,
                    elementToInsert.defaultSize,
                    insertionParent(targetParent, EP.dynamicPathToStaticPath(targetParent)),
                    floatingMenuState.indexPosition?.type === 'absolute'
                      ? floatingMenuState.indexPosition.index
                      : null,
                  ),
                  CanvasActions.createInteractionSession(createInteractionViaUI(boundingArea())),
                ]

                scheduleClearUiInteraction()
              }
            }
            break
          }
          case 'convert': {
            // this is taken from render-as.tsx
            const targetsForUpdates = getElementsToTarget(selectedViews)
            actionsToDispatch = targetsForUpdates.flatMap((path) => {
              return updateJSXElementName(
                path,
                pickedInsertableComponent.element.name,
                pickedInsertableComponent.importsToAdd,
              )
            })
            break
          }
          case 'closed':
            break
          default:
            const _exhaustiveCheck: never = floatingMenuState
            throw new Error(`Unhandled type ${JSON.stringify(floatingMenuState)}`)
        }
        dispatch([...actionsToDispatch, closeFloatingInsertMenu()])
      }
    },
    [
      dispatch,
      floatingMenuState,
      projectContentsRef,
      selectedViewsref,
      fixedSizeForInsertion,
      addContentForInsertion,
      wrapContentForInsertion,
      preserveVisualPositionForWrap,
      scheduleClearUiInteraction,
    ],
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
          <FlexColumn>
            <FlexRow
              css={{
                height: UtopiaTheme.layout.rowHeight.smaller,
                paddingLeft: 8,
                paddingRight: 8,
                borderTop: `1px solid ${colorTheme.border1.value}`,
              }}
            >
              <CheckboxRow
                id='wrap-parents-content-label'
                checked={wrapContentForInsertion}
                onChange={setWrapContentForInsertion}
              >
                Wrap content
              </CheckboxRow>
            </FlexRow>
            <FlexRow
              css={{
                height: UtopiaTheme.layout.rowHeight.smaller,
                paddingLeft: 8,
                paddingRight: 8,
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
          </FlexColumn>
        ) : null}
        {when(
          showWrapControls,
          <FlexRow
            css={{
              height: UtopiaTheme.layout.rowHeight.normal,
              paddingLeft: 8,
              paddingRight: 8,
              borderTop: `1px solid ${colorTheme.border1.value}`,
            }}
          >
            <CheckboxRow
              id='preserve-visual-position-checkbox'
              checked={preserveVisualPositionForWrap}
              onChange={setPreserveVisualPositionForWrap}
            >
              Try to preserve visual position
            </CheckboxRow>
          </FlexRow>,
        )}
      </FlexColumn>
    </div>
  )
})

interface FloatingInsertMenuProps {}

export const FloatingInsertMenu = React.memo((props: FloatingInsertMenuProps) => {
  const dispatch = useEditorState((store) => store.dispatch, 'FloatingInsertMenu dispatch')
  const isVisible = useEditorState(
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

export function useClearUIInteraction(editorStoreRef: { readonly current: EditorStorePatched }) {
  return React.useCallback(() => {
    const clearUiInteraction = () => {
      window.removeEventListener('mousedown', clearUiInteraction)

      if (editorStoreRef.current.editor.canvas.interactionSession?.interactionData.type === 'UI') {
        editorStoreRef.current.dispatch(
          [CanvasActions.clearInteractionSession(true), switchEditorMode(EditorModes.selectMode())],
          'everyone',
        )
      }
    }
    window.addEventListener('mousedown', clearUiInteraction, { once: true, capture: true })
  }, [editorStoreRef])
}
