import React from 'react'
import {
  useContextMenu,
  Menu,
  type ShowContextMenuParams,
  contextMenu,
  type TriggerEvent,
} from 'react-contexify'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  type ElementInstanceMetadata,
  getJSXElementNameAsString,
  jsxAttributesFromMap,
  jsxElement,
  jsxElementFromJSXElementWithoutUID,
  jsxElementNameFromString,
  getJSXElementNameLastPart,
  setJSXAttributesAttribute,
  jsExpressionValue,
  isIntrinsicHTMLElement,
} from '../../../core/shared/element-template'
import type { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import {
  deleteView,
  insertAsChildTarget,
  insertInsertable,
  insertJSXElement,
  replaceJSXElement,
  replaceMappedElement,
  setProp_UNSAFE,
  showToast,
  wrapInElement,
} from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import {
  ComponentPicker,
  elementToInsertToInsertableComponent,
  type ElementToInsert,
} from './component-picker'
import type { PreferredChildComponentDescriptor } from '../../custom-code/internal-property-controls'
import { fixUtopiaElement, generateConsistentUID } from '../../../core/shared/uid-utils'
import { getAllUniqueUids } from '../../../core/model/get-unique-ids'
import { elementFromInsertMenuItem } from '../../editor/insert-callbacks'
import { ContextMenuWrapper } from '../../context-menu-wrapper'
import { BodyMenuOpenClass, assertNever } from '../../../core/shared/utils'
import { type ContextMenuItem } from '../../context-menu-items'
import { FlexRow, Icn, type IcnProps } from '../../../uuiui'
import type {
  EditorAction,
  EditorDispatch,
  InsertAsChildTarget,
  ReplaceKeepChildrenAndStyleTarget,
  ReplaceTarget,
  WrapTarget,
} from '../../editor/action-types'
import { type ProjectContentTreeRoot } from '../../assets'
import {
  type PropertyControlsInfo,
  type ComponentInfo,
  componentElementToInsertHasChildren,
} from '../../custom-code/code-file'
import { type Icon } from 'utopia-api'
import { getRegisteredComponent } from '../../../core/property-controls/property-controls-utils'
import { defaultImportsForComponentModule } from '../../../core/property-controls/property-controls-local'
import { useGetInsertableComponents } from '../../canvas/ui/floating-insert-menu'
import { atom, useAtom, useSetAtom } from 'jotai'
import {
  childInsertionPath,
  conditionalClauseInsertionPath,
  replaceWithSingleElement,
} from '../../editor/store/insertion-path'
import { mapComponentInfo, type InsertableComponent } from '../../shared/project-components'
import type { ConditionalCase } from '../../../core/model/conditionals'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import { absolute } from '../../../utils/utils'
import { notice } from '../../common/notice'
import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'
import { emptyComments } from 'utopia-shared/src/types'
import { intrinsicHTMLElementNamesThatSupportChildren } from '../../../core/shared/dom-utils'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import { forceNotNull } from '../../../core/shared/optional-utils'

type RenderPropTarget = { type: 'render-prop'; prop: string }
type ConditionalTarget = { type: 'conditional'; conditionalCase: ConditionalCase }

export type InsertionTarget =
  | RenderPropTarget
  | ReplaceTarget
  | ReplaceKeepChildrenAndStyleTarget
  | InsertAsChildTarget
  | ConditionalTarget
  | WrapTarget

export function renderPropTarget(prop: string): RenderPropTarget {
  return {
    type: 'render-prop',
    prop: prop,
  }
}
export function conditionalTarget(conditionalCase: ConditionalCase): ConditionalTarget {
  return { type: 'conditional', conditionalCase: conditionalCase }
}

export function isReplaceTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is ReplaceTarget {
  return insertionTarget.type === 'replace-target'
}

export function isWrapTarget(insertionTarget: InsertionTarget): insertionTarget is WrapTarget {
  return insertionTarget.type === 'wrap-target'
}

export function isReplaceKeepChildrenAndStyleTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is ReplaceKeepChildrenAndStyleTarget {
  return insertionTarget.type === 'replace-target-keep-children-and-style'
}

export function isInsertAsChildTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is InsertAsChildTarget {
  return insertionTarget.type === 'insert-as-child'
}

export function isRenderPropTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is RenderPropTarget {
  return insertionTarget.type === 'render-prop'
}

export function isConditionalTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is ConditionalTarget {
  return insertionTarget.type === 'conditional'
}

interface ComponentPickerContextMenuAtomData {
  targets: ElementPath[]
  insertionTarget: InsertionTarget
}

const ComponentPickerContextMenuAtom = atom<ComponentPickerContextMenuAtomData>({
  targets: [EP.emptyElementPath],
  insertionTarget: insertAsChildTarget(),
})

function getIconForComponent(
  targetName: string,
  moduleName: string | null,
  propertyControlsInfo: PropertyControlsInfo,
): Icon {
  if (moduleName == null) {
    return 'component'
  }

  const registeredComponent = getRegisteredComponent(targetName, moduleName, propertyControlsInfo)

  return registeredComponent?.icon ?? 'component'
}

interface PreferredChildComponentDescriptorWithIcon extends PreferredChildComponentDescriptor {
  icon: Icon
}

export function preferredChildrenForTarget(
  targetElement: ElementInstanceMetadata | null,
  insertionTarget: InsertionTarget,
  propertyControlsInfo: PropertyControlsInfo,
  elementChildren: Array<ElementInstanceMetadata>,
): Array<PreferredChildComponentDescriptorWithIcon> {
  const targetJSXElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(targetElement)
  const elementImportInfo = targetElement?.importInfo
  if (elementImportInfo == null || targetJSXElement == null) {
    return []
  }

  const targetName = getJSXElementNameAsString(targetJSXElement.name)
  const registeredComponent = getRegisteredComponent(
    targetName,
    elementImportInfo.filePath,
    propertyControlsInfo,
  )

  // TODO: we don't deal with components registered with the same name in multiple files
  if (registeredComponent != null) {
    if (
      isInsertAsChildTarget(insertionTarget) ||
      isReplaceTarget(insertionTarget) ||
      isReplaceKeepChildrenAndStyleTarget(insertionTarget)
    ) {
      // If we want to keep the children of this element when it has some, don't include replacements that have children.
      const includeComponentsWithChildren =
        !isReplaceKeepChildrenAndStyleTarget(insertionTarget) || elementChildren.length === 0

      return registeredComponent.preferredChildComponents.map((childComponent) => {
        return {
          ...childComponent,
          variants: childComponent.variants.filter((variant) => {
            // Includes everything if components with children are permitted, otherwise only includes components without children.
            return (
              includeComponentsWithChildren ||
              !componentElementToInsertHasChildren(variant.elementToInsert())
            )
          }),
          icon: getIconForComponent(
            childComponent.name,
            childComponent.moduleName,
            propertyControlsInfo,
          ),
        }
      })
    } else if (isRenderPropTarget(insertionTarget)) {
      for (const [registeredPropName, registeredPropValue] of Object.entries(
        registeredComponent.properties,
      )) {
        if (
          registeredPropName === insertionTarget.prop &&
          registeredPropValue.control === 'jsx' &&
          registeredPropValue.preferredChildComponents != null
        ) {
          return registeredPropValue.preferredChildComponents.map((v) => ({
            ...v,
            icon: getIconForComponent(v.name, v.moduleName, propertyControlsInfo),
          }))
        }
      }
    }
  }

  return []
}

function augmentPreferredChildren(
  preferredChildren: PreferredChildComponentDescriptorWithIcon[],
  insertionTarget: InsertionTarget,
): PreferredChildComponentDescriptorWithIcon[] {
  if (insertionTarget.type === 'insert-as-child') {
    return [
      ...preferredChildren,
      {
        name: 'List',
        moduleName: null,
        variants: [mapComponentInfo],
        icon: 'code',
      },
    ]
  }

  return preferredChildren
}

function getTargetParentFromInsertionTarget(
  target: ElementPath,
  insertionTarget: InsertionTarget,
): ElementPath {
  return isReplaceTarget(insertionTarget) || isReplaceKeepChildrenAndStyleTarget(insertionTarget)
    ? EP.parentPath(target)
    : target
}

const usePreferredChildrenForTarget = (
  target: ElementPath,
  insertionTarget: InsertionTarget,
): Array<PreferredChildComponentDescriptorWithIcon> => {
  const targetParent = getTargetParentFromInsertionTarget(target, insertionTarget)

  const targetElement = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, targetParent),
    'usePreferredChildrenForTarget targetElement',
  )
  const targetChildren = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.getChildrenUnordered(store.editor.jsxMetadata, target),
    'usePreferredChildrenForTarget targetChildren',
  )

  const preferredChildren = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return preferredChildrenForTarget(
        targetElement,
        insertionTarget,
        store.editor.propertyControlsInfo,
        targetChildren,
      )
    },
    'usePreferredChildrenForSelectedElement propertyControlsInfo',
  )

  return augmentPreferredChildren(preferredChildren, insertionTarget)
}

export type ShowComponentPickerContextMenuCallback = (
  selectedViews: ElementPath[],
  insertionTarget: InsertionTarget,
  pickerType?: 'preferred' | 'full',
) => ShowComponentPickerContextMenu

export type ShowComponentPickerContextMenu = (
  event: TriggerEvent,
  params?: Pick<ShowContextMenuParams, 'props' | 'position'> | undefined,
) => void

const PreferredMenuId = 'component-picker-context-menu'
const FullMenuId = 'component-picker-context-menu-full'

export const useCreateCallbackToShowComponentPicker =
  (): ShowComponentPickerContextMenuCallback => {
    const { show: showPreferred } = useContextMenu({ id: PreferredMenuId })
    const { show: showFull } = useContextMenu({ id: FullMenuId })
    const setContextMenuProps = useSetAtom(ComponentPickerContextMenuAtom)
    const editorRef = useRefEditorState((store) => ({
      jsxMetadata: store.editor.jsxMetadata,
      propertyControlsInfo: store.editor.propertyControlsInfo,
    }))

    return React.useCallback(
      (
          selectedViews: ElementPath[],
          insertionTarget: InsertionTarget,
          overridePickerType?: 'preferred' | 'full',
        ) =>
        (
          event: TriggerEvent,
          params?: Pick<ShowContextMenuParams, 'props' | 'position'> | undefined,
        ) => {
          event.stopPropagation()
          event.preventDefault()

          let pickerType: 'preferred' | 'full'
          let targets = selectedViews

          if (overridePickerType != null) {
            pickerType = overridePickerType
          } else if (isWrapTarget(insertionTarget)) {
            pickerType = 'full'
          } else {
            // for insertion and replacement we still don't support multiple selection
            // so we pick the first one
            targets = selectedViews.slice(0, 1)
            const firstTarget = targets[0]
            const targetParent =
              isReplaceTarget(insertionTarget) ||
              isReplaceKeepChildrenAndStyleTarget(insertionTarget)
                ? EP.parentPath(firstTarget)
                : firstTarget
            const targetElement = MetadataUtils.findElementByElementPath(
              editorRef.current.jsxMetadata,
              targetParent,
            )
            const targetChildren = MetadataUtils.getChildrenUnordered(
              editorRef.current.jsxMetadata,
              targetParent,
            )
            const preferredChildren = preferredChildrenForTarget(
              targetElement,
              insertionTarget,
              editorRef.current.propertyControlsInfo,
              targetChildren,
            )

            pickerType = preferredChildren.length > 0 ? 'preferred' : 'full'
          }

          setContextMenuProps({ targets: selectedViews, insertionTarget: insertionTarget })
          const show = pickerType === 'preferred' ? showPreferred : showFull
          show({ ...params, event })
        },
      [editorRef, showPreferred, showFull, setContextMenuProps],
    )
  }

function defaultVariantItem(
  elementName: string,
  label: string | React.ReactNode,
  imports: Imports,
  submenuName: string | React.ReactNode | null,
  onItemClick: (preferredChildToInsert: ElementToInsert) => void,
): ContextMenuItem<unknown> {
  return {
    name: label,
    submenuName: submenuName,
    enabled: true,
    action: () =>
      onItemClick({
        name: elementName,
        elementToInsert: (uid: string) =>
          jsxElement(elementName, uid, jsxAttributesFromMap({}), []),
        additionalImports: imports,
      }),
  }
}

function singletonItem(
  label: string | React.ReactNode,
  variant: ComponentInfo,
  onItemClick: (preferredChildToInsert: ElementToInsert) => void,
): ContextMenuItem<unknown> {
  return {
    name: label,
    submenuName: null,
    enabled: true,
    action: () =>
      onItemClick({
        name: variant.insertMenuLabel,
        elementToInsert: (uid: string) => elementFromInsertMenuItem(variant.elementToInsert(), uid),
        additionalImports: variant.importsToAdd,
      }),
  }
}

function variantItem(
  variant: ComponentInfo,
  submenuName: string | React.ReactNode | null,
  onItemClick: (preferredChildToInsert: ElementToInsert) => void,
): ContextMenuItem<unknown> {
  return {
    name: variant.insertMenuLabel,
    submenuName: submenuName,
    enabled: true,
    action: () =>
      onItemClick({
        name: variant.insertMenuLabel,
        elementToInsert: (uid: string) => elementFromInsertMenuItem(variant.elementToInsert(), uid),
        additionalImports: variant.importsToAdd,
      }),
  }
}

const separatorItem: ContextMenuItem<unknown> = {
  name: <div key='separator' className='contexify_separator' />,
  enabled: false,
  isSeparator: true,
  action: () => null,
}

function moreItem(
  menuWrapperRef: React.RefObject<HTMLDivElement>,
  showComponentPickerContextMenu: ShowComponentPickerContextMenu,
): ContextMenuItem<unknown> {
  return {
    name: <FlexRow style={{ paddingLeft: 22 }}>More…</FlexRow>,
    enabled: true,
    action: (_data, _dispatch, _rightClickCoordinate, e) => {
      // FIXME Yeah this is horrific
      const currentMenu = (menuWrapperRef.current?.childNodes[1] as HTMLDivElement) ?? null
      const position =
        currentMenu == null
          ? undefined
          : {
              x: currentMenu.offsetLeft,
              y: currentMenu.offsetTop,
            }

      showComponentPickerContextMenu(e as React.MouseEvent<any>, {
        position: position,
      })
    },
  }
}

export function insertComponentPickerItem(
  toInsert: InsertableComponent,
  targets: ElementPath[],
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  dispatch: EditorDispatch,
  insertionTarget: InsertionTarget,
) {
  const uniqueIds = new Set(getAllUniqueUids(projectContents).uniqueIDs)
  const uid = generateConsistentUID('prop', uniqueIds)
  const elementWithoutUID = toInsert.element()
  // TODO: for most of the operations we still only support one target
  const firstTarget = targets[0]

  const actions = ((): Array<EditorAction> => {
    if (elementWithoutUID.type === 'JSX_ELEMENT') {
      const element = jsxElementFromJSXElementWithoutUID(elementWithoutUID, uid)
      const fixedElement = fixUtopiaElement(element, uniqueIds).value

      if (fixedElement.type !== 'JSX_ELEMENT') {
        throw new Error('JSXElementWithoutUid is not converted to JSXElement')
      }

      // if we are inserting into a render prop
      if (isRenderPropTarget(insertionTarget)) {
        return [
          setProp_UNSAFE(
            firstTarget,
            PP.create(insertionTarget.prop),
            fixedElement,
            toInsert.importsToAdd ?? undefined,
          ),
        ]
      }

      // Replacing a mapped element requires a different function
      if (
        isReplaceTarget(insertionTarget) &&
        MetadataUtils.isJSXMapExpression(EP.parentPath(firstTarget), metadata)
      ) {
        return [replaceMappedElement(fixedElement, firstTarget, toInsert.importsToAdd)]
      }

      if (isWrapTarget(insertionTarget)) {
        const newUID = generateUidWithExistingComponents(projectContents)

        const newElement = jsxElement(
          element.name,
          newUID,
          setJSXAttributesAttribute(
            element.props,
            'data-uid',
            jsExpressionValue(newUID, emptyComments),
          ),
          element.children,
        )
        return [
          wrapInElement(targets, {
            element: newElement,
            importsToAdd: toInsert.importsToAdd,
          }),
        ]
      }

      if (
        isReplaceTarget(insertionTarget) ||
        isReplaceKeepChildrenAndStyleTarget(insertionTarget)
      ) {
        return [
          replaceJSXElement(fixedElement, firstTarget, toInsert.importsToAdd, insertionTarget),
        ]
      }

      if (!isConditionalTarget(insertionTarget)) {
        return [insertJSXElement(fixedElement, firstTarget, toInsert.importsToAdd ?? undefined)]
      }
    }

    if (isInsertAsChildTarget(insertionTarget)) {
      return [insertInsertable(childInsertionPath(firstTarget), toInsert, 'do-not-add', null)]
    }

    if (isConditionalTarget(insertionTarget)) {
      return [
        insertInsertable(
          conditionalClauseInsertionPath(
            firstTarget,
            insertionTarget.conditionalCase,
            replaceWithSingleElement(),
          ),
          toInsert,
          'do-not-add',
          null,
        ),
      ]
    }

    if (isWrapTarget(insertionTarget)) {
      const elementToInsert = toInsert.element()
      if (
        elementToInsert.type === 'JSX_MAP_EXPRESSION' &&
        !targets.every((target) => MetadataUtils.isJSXElement(target, metadata))
      ) {
        return [
          showToast(
            notice(
              'We are working on support to insert Lists, Conditionals and Fragments into Lists',
              'INFO',
              false,
              'wrap-component-picker-item-nested-map',
            ),
          ),
        ]
      }
      return [
        wrapInElement(targets, {
          element: {
            ...elementToInsert,
            uid: generateUidWithExistingComponents(projectContents),
          },
          importsToAdd: emptyImports(),
        }),
      ]
    }

    if (isReplaceTarget(insertionTarget)) {
      if (
        MetadataUtils.isJSXMapExpression(EP.parentPath(firstTarget), metadata) &&
        elementWithoutUID.type !== 'JSX_ELEMENT'
      ) {
        return [
          showToast(
            notice(
              'We are working on support to insert Lists, Conditionals and Fragments into Lists',
              'INFO',
              false,
              'insert-component-picker-item-nested-map',
            ),
          ),
        ]
      }
      const index = MetadataUtils.getIndexInParent(metadata, pathTrees, firstTarget)
      return [
        deleteView(firstTarget),
        insertInsertable(
          childInsertionPath(EP.parentPath(firstTarget)),
          toInsert,
          'do-not-add',
          absolute(index),
        ),
      ]
    }

    return [
      showToast(
        notice(
          toastMessage(insertionTarget, toInsert),
          'INFO',
          false,
          'insert-component-picker-item-nested-map',
        ),
      ),
    ]
  })()

  dispatch(actions)
}

function toastMessage(insertionTarget: InsertionTarget, toInsert: InsertableComponent) {
  switch (insertionTarget.type) {
    case 'replace-target':
      return `Swapping to ${toInsert.name} isn't supported yet`
    case 'replace-target-keep-children-and-style':
      return `Replacing with ${toInsert.name} isn't supported yet`
    case 'insert-as-child':
      return `Inserting ${toInsert.name} as child isn't supported yet`
    case 'conditional':
      return `Inserting ${toInsert.name} into conditional ${insertionTarget.type} isn't supported yet`
    case 'render-prop':
      return `Inserting ${toInsert.name} into render prop ${insertionTarget.prop} isn't supported yet`
    case 'wrap-target':
      return `Wrapping with ${toInsert.name} isn't supported yet`
    default:
      assertNever(insertionTarget)
  }
}

function insertPreferredChild(
  preferredChildToInsert: ElementToInsert,
  targets: ElementPath[],
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  dispatch: EditorDispatch,
  insertionTarget: InsertionTarget,
) {
  const uniqueIds = new Set(getAllUniqueUids(projectContents).uniqueIDs)
  const uid = generateConsistentUID('prop', uniqueIds)
  const toInsert = elementToInsertToInsertableComponent(
    preferredChildToInsert,
    uid,
    ['do-not-add'],
    null,
    null,
    null,
  )

  insertComponentPickerItem(
    toInsert,
    targets,
    projectContents,
    metadata,
    pathTrees,
    dispatch,
    insertionTarget,
  )
}

interface ComponentPickerContextMenuProps {
  targets: ElementPath[]
  insertionTarget: InsertionTarget
}

export function iconPropsForIcon(icon: Icon, inverted: boolean = false): IcnProps {
  return {
    category: 'navigator-element',
    type: icon,
    color: inverted ? 'black' : 'white',
  }
}

export function labelTestIdForComponentIcon(
  componentName: string,
  moduleName: string,
  icon: Icon,
): string {
  return `variant-label-${componentName}-${moduleName}-${icon}`
}

function contextMenuItemsFromVariants(
  preferredChildComponentDescriptor: PreferredChildComponentDescriptorWithIcon,
  submenuLabel: React.ReactElement,
  defaultVariantImports: Imports,
  onItemClick: (_: ElementToInsert) => void,
): ContextMenuItem<unknown>[] {
  const allJSXElements = preferredChildComponentDescriptor.variants.every(
    (v) => v.elementToInsert().type === 'JSX_ELEMENT',
  )

  if (allJSXElements) {
    return [
      defaultVariantItem(
        preferredChildComponentDescriptor.name,
        '(empty)',
        defaultVariantImports,
        submenuLabel,
        onItemClick,
      ),
      ...preferredChildComponentDescriptor.variants.map((variant) => {
        return variantItem(variant, submenuLabel, onItemClick)
      }),
    ]
  }

  if (preferredChildComponentDescriptor.variants.length === 1) {
    return [singletonItem(submenuLabel, preferredChildComponentDescriptor.variants[0], onItemClick)]
  }

  return preferredChildComponentDescriptor.variants.map((variant) => {
    return variantItem(variant, submenuLabel, onItemClick)
  })
}

const ComponentPickerContextMenuSimple = React.memo<ComponentPickerContextMenuProps>(
  ({ targets, insertionTarget }) => {
    const showFullMenu = useCreateCallbackToShowComponentPicker()(targets, insertionTarget, 'full')

    // for insertion we currently only support one target
    const firstTarget = targets[0]
    const preferredChildren = usePreferredChildrenForTarget(firstTarget, insertionTarget)

    const dispatch = useDispatch()

    const projectContentsRef = useRefEditorState((state) => state.editor.projectContents)
    const metadataRef = useRefEditorState((state) => state.editor.jsxMetadata)
    const elementPathTreesRef = useRefEditorState((state) => state.editor.elementPathTree)

    const onItemClick = React.useCallback(
      (preferredChildToInsert: ElementToInsert) =>
        insertPreferredChild(
          preferredChildToInsert,
          targets,
          projectContentsRef.current,
          metadataRef.current,
          elementPathTreesRef.current,
          dispatch,
          insertionTarget,
        ),
      [targets, projectContentsRef, metadataRef, elementPathTreesRef, dispatch, insertionTarget],
    )
    const wrapperRef = React.useRef<HTMLDivElement>(null)

    const items: Array<ContextMenuItem<unknown>> = preferredChildren
      .flatMap<ContextMenuItem<unknown>>((data) => {
        const iconProps = iconPropsForIcon(data.icon)

        const submenuLabel = (
          <FlexRow
            style={{ gap: 10, width: 228 }}
            data-testId={labelTestIdForComponentIcon(data.name, data.moduleName ?? '', data.icon)}
          >
            <Icn {...iconProps} width={12} height={12} />
            {data.name}
          </FlexRow>
        )

        const defaultVariantImports = defaultImportsForComponentModule(data.name, data.moduleName)

        const jsxName = jsxElementNameFromString(data.name)
        const name = getJSXElementNameLastPart(jsxName)
        if (data.variants == null || data.variants.length === 0) {
          return [defaultVariantItem(name, submenuLabel, defaultVariantImports, null, onItemClick)]
        }

        return contextMenuItemsFromVariants(data, submenuLabel, defaultVariantImports, onItemClick)
      })
      .concat([separatorItem, moreItem(wrapperRef, showFullMenu)])

    return (
      <ContextMenuWrapper items={items} data={{}} id={PreferredMenuId} forwardRef={wrapperRef} />
    )
  },
)

const ComponentPickerContextMenuFull = React.memo<ComponentPickerContextMenuProps>(
  ({ targets, insertionTarget }) => {
    // for insertion we currently only support one target
    const firstTarget = targets[0]
    const targetChildren = useEditorState(
      Substores.metadata,
      (store) => MetadataUtils.getChildrenUnordered(store.editor.jsxMetadata, firstTarget),
      'usePreferredChildrenForTarget targetChildren',
    )

    const areAllJsxElements = useEditorState(
      Substores.metadata,
      (store) =>
        targets.every((target) => MetadataUtils.isJSXElement(target, store.editor.jsxMetadata)),
      'areAllJsxElements targetElement',
    )

    const allInsertableComponents = useGetInsertableComponents('insert').flatMap((group) => {
      return {
        label: group.label,
        options: group.options.filter((option) => {
          const element = option.value.element()
          if (
            isInsertAsChildTarget(insertionTarget) ||
            isConditionalTarget(insertionTarget) ||
            isReplaceTarget(insertionTarget)
          ) {
            return true
          }
          if (isReplaceKeepChildrenAndStyleTarget(insertionTarget)) {
            // If we want to keep the children of this element when it has some, don't include replacements that have children.
            return targetChildren.length === 0 || !componentElementToInsertHasChildren(element)
          }
          if (isWrapTarget(insertionTarget)) {
            if (element.type === 'JSX_ELEMENT' && isIntrinsicHTMLElement(element.name)) {
              // when it is an intrinsic html element, we check if it supports children from our list
              return intrinsicHTMLElementNamesThatSupportChildren.includes(
                element.name.baseVariable,
              )
            }
            if (element.type === 'JSX_MAP_EXPRESSION') {
              // we cannot currently wrap in List a conditional, fragment or map expression
              return areAllJsxElements
            }
            return true
          }
          // Right now we only support inserting JSX elements when we insert into a render prop or when replacing elements
          return element.type === 'JSX_ELEMENT'
        }),
      }
    })

    const dispatch = useDispatch()

    const projectContentsRef = useRefEditorState((state) => state.editor.projectContents)
    const metadataRef = useRefEditorState((state) => state.editor.jsxMetadata)
    const elementPathTreesRef = useRefEditorState((state) => state.editor.elementPathTree)

    const hideAllContextMenus = React.useCallback(() => {
      contextMenu.hideAll()
    }, [])

    const onItemClick = React.useCallback(
      (preferredChildToInsert: InsertableComponent) => (e: React.UIEvent) => {
        e.stopPropagation()
        e.preventDefault()

        insertComponentPickerItem(
          preferredChildToInsert,
          targets,
          projectContentsRef.current,
          metadataRef.current,
          elementPathTreesRef.current,
          dispatch,
          insertionTarget,
        )

        hideAllContextMenus()
      },
      [
        targets,
        projectContentsRef,
        metadataRef,
        elementPathTreesRef,
        dispatch,
        insertionTarget,
        hideAllContextMenus,
      ],
    )

    const squashEvents = React.useCallback((e: React.UIEvent<unknown>) => {
      e.stopPropagation()
    }, [])

    const onVisibilityChange = React.useCallback((isVisible: boolean) => {
      if (isVisible) {
        document.body.classList.add(BodyMenuOpenClass)
      } else {
        document.body.classList.remove(BodyMenuOpenClass)
      }
    }, [])

    return (
      <Menu
        id={FullMenuId}
        animation={false}
        style={{ width: 260 }}
        onClick={squashEvents}
        onVisibilityChange={onVisibilityChange}
      >
        <ComponentPicker
          allComponents={allInsertableComponents}
          onItemClick={onItemClick}
          closePicker={hideAllContextMenus}
          shownInToolbar={false}
        />
      </Menu>
    )
  },
)

export const ComponentPickerContextMenu = React.memo(() => {
  const [{ targets, insertionTarget }] = useAtom(ComponentPickerContextMenuAtom)

  return (
    <React.Fragment>
      <ComponentPickerContextMenuSimple targets={targets} insertionTarget={insertionTarget} />
      <ComponentPickerContextMenuFull targets={targets} insertionTarget={insertionTarget} />
    </React.Fragment>
  )
})
