import React from 'react'
import { getSimpleAttributeAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import { isRight, right } from '../../core/shared/either'
import type {
  JSExpression,
  JSXAttributes,
  JSXElement,
  ComputedStyle,
  StyleAttributeMetadata,
} from '../../core/shared/element-template'
import { isJSXElement, jsExpressionValue, emptyComments } from '../../core/shared/element-template'
import { getJSXAttributesAtPath } from '../../core/shared/jsx-attribute-utils'
import * as PP from '../../core/shared/property-path'
import * as EP from '../../core/shared/element-path'
import Utils from '../../utils/utils'
import { setFocus } from '../common/actions'
import type { EditorAction } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import { setProp_UNSAFE, transientActions, unsetProperty } from '../editor/actions/action-creators'

import type { ElementsToRerender } from '../editor/store/editor-state'
import {
  getJSXComponentsAndImportsForPathFromState,
  isOpenFileUiJs,
  getElementFromProjectContents,
} from '../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import {
  InspectorCallbackContext,
  InspectorPropsContext,
  stylePropPathMappingFn,
} from './common/property-path-hooks'
import { ComponentSection } from './sections/component-section/component-section'
import { EventHandlersSection } from './sections/event-handlers-section/event-handlers-section'
import type { CSSTarget, TargetSelectorLength } from './sections/header-section/target-selector'
import { cssTarget } from './sections/header-section/target-selector'
import { ImgSection } from './sections/image-section/image-section'
import { WarningSubsection } from './sections/layout-section/warning-subsection/warning-subsection'
import { ClassNameSubsection } from './sections/style-section/className-subsection/className-subsection'
import { StyleSection } from './sections/style-section/style-section'
import type { TargetSelectorSectionProps } from './sections/target-selector-section'
import { TargetSelectorSection } from './sections/target-selector-section'
import { usePropControlledRef_DANGEROUS } from './common/inspector-utils'
import {
  useKeepReferenceEqualityIfPossible,
  useKeepShallowReferenceEquality,
} from '../../utils/react-performance'
import { getElementsToTarget } from './common/inspector-utils'
import type { ElementPath, PropertyPath } from '../../core/shared/project-file-types'
import { unless, when } from '../../utils/react-conditionals'
import {
  isKeyboardAbsoluteStrategy,
  isKeyboardReorderStrategy,
  isStrategyActive,
} from '../canvas/canvas-strategies/canvas-strategies'
import type { StrategyState } from '../canvas/canvas-strategies/interaction-state'
import { LowPriorityStoreProvider } from '../editor/store/store-context-providers'
import { FlexSection } from './flex-section'
import { useDispatch } from '../editor/store/dispatch-context'
import { styleStringInArray } from '../../utils/common-constants'
import { ConditionalSection } from './sections/layout-section/conditional-section'
import { allSelectedElementsContractSelector } from './editor-contract-section'
import { FragmentSection } from './sections/layout-section/fragment-section'
import { RootElementIndicator } from './controls/root-element-indicator'
import { CodeElementSection } from './sections/code-element-section'
import {
  maybeInvalidGroupState,
  groupErrorToastAction,
} from '../canvas/canvas-strategies/strategies/group-helpers'
import { FlexCol } from 'utopia-api'
import { SettingsPanel } from './sections/settings-panel/inspector-settingspanel'
import { strictEvery } from '../../core/shared/array-utils'
import { SimplifiedLayoutSubsection } from './sections/layout-section/self-layout-subsection/simplified-layout-subsection'
import { ConstraintsSection } from './constraints-section'
import { usePermissions } from '../editor/store/permissions'
import { DisableControlsInSubtree } from '../../uuiui/utilities/disable-subtree'
import {
  getComponentDescriptorForTarget,
  getInspectorPreferencesForTargets,
} from '../../core/property-controls/property-controls-utils'
import { ListSection } from './sections/layout-section/list-section'
import { isIntrinsicElementMetadata } from '../../core/model/project-file-utils'
import { assertNever } from '../../core/shared/utils'
import { DataReferenceSection } from './sections/data-reference-section'
import {
  elementSupportsChildrenFromPropertyControls,
  replaceFirstChildAndDeleteSiblings,
} from '../editor/element-children'
import { InspectorSectionHeader } from './section-header'
import { ContainerSubsection } from './sections/style-section/container-subsection/container-subsection'
import { isTailwindEnabled } from '../../core/tailwind/tailwind-compilation'

export interface ElementPathElement {
  name?: string
  path: ElementPath
}

export interface InspectorPartProps<T> {
  input: T
  onSubmitValue: (output: T, paths: Array<PropertyPath>) => void
}
export interface InspectorProps extends TargetSelectorSectionProps {
  setSelectedTarget: React.Dispatch<React.SetStateAction<string[]>>
  selectedViews: Array<ElementPath>
}

function buildNonDefaultPositionPaths(propertyTarget: Array<string>): Array<PropertyPath> {
  return [
    stylePropPathMappingFn('right', propertyTarget),
    stylePropPathMappingFn('bottom', propertyTarget),
  ]
}

export function shouldUpdateLowPriorityUI(
  strategyState: StrategyState,
  elementsToRerender: ElementsToRerender,
): boolean {
  return (
    (!isStrategyActive(strategyState) && elementsToRerender === 'rerender-all-elements') ||
    isKeyboardAbsoluteStrategy(strategyState.currentStrategy) ||
    isKeyboardReorderStrategy(strategyState.currentStrategy)
  )
}

export const InspectorSectionsContainerTestID = 'inspector-sections-container'

export const Inspector = React.memo<InspectorProps>((props: InspectorProps) => {
  const { selectedViews, setSelectedTarget, targets } = props

  const isCodeElement = useEditorState(
    Substores.metadata,
    (store) =>
      store.editor.selectedViews.length > 0 &&
      store.editor.selectedViews.every(
        (path) =>
          MetadataUtils.isConditional(path, store.editor.jsxMetadata) ||
          MetadataUtils.isExpressionOtherJavascript(path, store.editor.jsxMetadata) ||
          MetadataUtils.isJSXMapExpression(path, store.editor.jsxMetadata),
      ),
    'Inspector isCodeElement',
  )

  const isDataReference = useEditorState(
    Substores.projectContentsAndMetadata,
    (store) =>
      store.editor.selectedViews.length > 0 &&
      store.editor.selectedViews.every((path) =>
        MetadataUtils.isElementDataReference(
          getElementFromProjectContents(path, store.editor.projectContents),
        ),
      ),
    'Inspector isDataReference',
  )

  const multiselectedContract = useEditorState(
    Substores.metadata,
    allSelectedElementsContractSelector,
    'Inspector multiselectedContract',
  )

  React.useEffect(() => {
    setSelectedTarget(targets[0].path)
  }, [selectedViews, targets, setSelectedTarget])

  const dispatch = useDispatch()
  const { focusedPanel, anyComponents } = useEditorState(
    Substores.fullStore,
    (store) => {
      const rootMetadata = store.editor.jsxMetadata
      let anyComponentsInner: boolean = false
      let hasNonDefaultPositionAttributesInner: boolean = false

      Utils.fastForEach(selectedViews, (view) => {
        const { components: rootComponents } = getJSXComponentsAndImportsForPathFromState(
          view,
          store.editor,
        )
        anyComponentsInner =
          anyComponentsInner || MetadataUtils.isComponentInstance(view, rootComponents)
        const possibleElement = MetadataUtils.findElementByElementPath(rootMetadata, view)
        const elementProps = store.editor.allElementProps[EP.toString(view)]
        if (possibleElement != null && elementProps != null) {
          // Slightly coarse in definition, but element metadata is in a weird little world of
          // its own compared to the props.
          if (isRight(possibleElement.element)) {
            const elem = possibleElement.element.value
            if (isJSXElement(elem)) {
              if (!hasNonDefaultPositionAttributesInner) {
                for (const nonDefaultPositionPath of buildNonDefaultPositionPaths(
                  styleStringInArray,
                )) {
                  const attributeAtPath = getJSXAttributesAtPath(elem.props, nonDefaultPositionPath)
                  if (attributeAtPath.attribute.type !== 'ATTRIBUTE_NOT_FOUND') {
                    hasNonDefaultPositionAttributesInner = true
                  }
                }
              }
            }
          }
        }
      })
      return {
        focusedPanel: store.editor.focusedPanel,
        anyComponents: anyComponentsInner,
        hasNonDefaultPositionAttributes: hasNonDefaultPositionAttributesInner,
      }
    },
    'Inspector',
  )

  const onFocus = React.useCallback(
    (event: React.FocusEvent<HTMLElement>) => {
      if (focusedPanel !== 'inspector') {
        queueMicrotask(() => {
          dispatch([setFocus('inspector')], 'inspector')
        })
      }
    },
    [dispatch, focusedPanel],
  )

  const rootElementIsSelected = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews.some(EP.isRootElementOfInstance),
    'RootElementIndicator aRootElementIsSelected',
  )

  const anyKnownElements = useEditorState(
    Substores.projectContentsAndMetadata,
    (store) => {
      return strictEvery(store.editor.selectedViews, (view) => {
        return (
          MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, view) != null ||
          MetadataUtils.isElementDataReference(
            getElementFromProjectContents(view, store.editor.projectContents),
          )
        )
      })
    },
    'Inspector anyKnownElements',
  )

  const canEdit = usePermissions().edit

  const inspectorPreferences = useEditorState(
    Substores.propertyControlsInfo,
    (store) =>
      getInspectorPreferencesForTargets(
        store.editor.selectedViews,
        store.editor.propertyControlsInfo,
        store.editor.projectContents,
      ),
    'Inspector inspectorPreferences',
  )

  const supportsChildren = useEditorState(
    Substores.metadataAndPropertyControlsInfo,
    (store) => {
      return store.editor.selectedViews.every((view) =>
        elementSupportsChildrenFromPropertyControls(
          store.editor.jsxMetadata,
          store.editor.propertyControlsInfo,
          view,
        ),
      )
    },
    'Inspector supportChildren',
  )

  const {
    value: styleSectionOpen,
    toggle: toggleStyleSection,
    set: setStyleSectionOpen,
  } = useBoolean(true)
  const { value: advancedSectionOpen, toggle: toggleAdvancedSection } = useBoolean(false)

  const shouldExpandStyleSection = useShouldExpandStyleSection()

  React.useEffect(() => {
    setStyleSectionOpen(shouldExpandStyleSection)
  }, [setStyleSectionOpen, shouldExpandStyleSection])

  const shouldHideInspectorSections = useShouldHideInspectorSections()

  const shouldShowStyleSectionContents = styleSectionOpen && !shouldHideInspectorSections
  const shouldShowAdvancedSectionContents = advancedSectionOpen && !shouldHideInspectorSections

  const shouldShowClassNameSubsection =
    isTailwindEnabled() && inspectorPreferences.includes('visual')
  const shouldShowTargetSelectorSection = canEdit && inspectorPreferences.includes('visual')
  const shouldShowFlexSection =
    multiselectedContract === 'frame' &&
    inspectorPreferences.includes('layout-system') &&
    supportsChildren

  const shouldShowSimplifiedLayoutSection = inspectorPreferences.includes('layout')

  const shouldShowContainerSection =
    selectedViews.length > 0 && inspectorPreferences.includes('layout')

  function renderInspectorContents() {
    return (
      <React.Fragment>
        <div
          style={{
            display: anyKnownElements ? 'none' : undefined,
          }}
        >
          <SettingsPanel />
        </div>
        <div
          style={{
            display: anyKnownElements ? undefined : 'none',
            height: '100%',
          }}
          data-testid={InspectorSectionsContainerTestID}
        >
          <DisableControlsInSubtree disable={!canEdit}>
            {when(isDataReference, <DataReferenceSection paths={selectedViews} />)}
            {when(
              isCodeElement,
              <>
                <CodeElementSection paths={selectedViews} />
                <ConditionalSection paths={selectedViews} />
                <ListSection paths={selectedViews} />
              </>,
            )}
            {unless(
              isCodeElement || isDataReference,
              <FlexCol
                data-testid='inspector-column'
                css={{
                  overflowY: 'scroll',
                  width: '100%',
                  height: '100%',
                  position: 'relative',
                  paddingBottom: 50,
                }}
              >
                {when(rootElementIsSelected, <RootElementIndicator />)}
                {anyComponents || multiselectedContract === 'fragment' ? (
                  <ComponentSection isScene={false} />
                ) : null}

                {unless(
                  shouldHideInspectorSections,
                  <InspectorSectionHeader
                    title='Styles'
                    toggle={toggleStyleSection}
                    open={styleSectionOpen}
                    uppercase={true}
                  />,
                )}
                {when(
                  shouldShowStyleSectionContents,
                  <>
                    {when(multiselectedContract === 'fragment', <FragmentSection />)}
                    {when(
                      multiselectedContract !== 'fragment' && shouldShowSimplifiedLayoutSection,
                      // Position and Sizing sections are shown if Frame or Group is selected
                      <>
                        <SimplifiedLayoutSubsection />
                        {when(shouldShowContainerSection, <ContainerSubsection />)}
                        <ConstraintsSection />
                      </>,
                    )}
                    {when(shouldShowFlexSection, <FlexSection />)}
                    {when(
                      multiselectedContract === 'frame' || multiselectedContract === 'wrapper-div',
                      // All the regular inspector sections are only visible if frames are selected
                      <>
                        <StyleSection />
                        <WarningSubsection />
                        <ImgSection />
                        <EventHandlersSection />
                      </>,
                    )}
                  </>,
                )}
                {unless(
                  shouldHideInspectorSections,
                  <InspectorSectionHeader
                    title='Advanced'
                    toggle={toggleAdvancedSection}
                    open={advancedSectionOpen}
                    uppercase={true}
                  />,
                )}
                {when(
                  shouldShowAdvancedSectionContents,
                  <>
                    {when(shouldShowClassNameSubsection, <ClassNameSubsection />)}
                    {when(
                      shouldShowTargetSelectorSection,
                      <TargetSelectorSection
                        targets={props.targets}
                        selectedTargetPath={props.selectedTargetPath}
                        onSelectTarget={props.onSelectTarget}
                        onStyleSelectorRename={props.onStyleSelectorRename}
                        onStyleSelectorDelete={props.onStyleSelectorDelete}
                        onStyleSelectorInsert={props.onStyleSelectorInsert}
                      />,
                    )}
                  </>,
                )}
              </FlexCol>,
            )}
          </DisableControlsInSubtree>
        </div>
      </React.Fragment>
    )
  }

  return (
    <div
      id='inspector'
      style={{
        width: '100%',
        position: 'relative',
        height: '100%',
      }}
      onFocus={onFocus}
    >
      {renderInspectorContents()}
    </div>
  )
})
Inspector.displayName = 'Inspector'

const DefaultStyleTargets: Array<CSSTarget> = [
  cssTarget(styleStringInArray, 0),
  cssTarget(['css'], 0),
]

export const InspectorEntryPoint: React.FunctionComponent<React.PropsWithChildren<unknown>> =
  React.memo(() => {
    return (
      <LowPriorityStoreProvider>
        <MultiselectInspector />
      </LowPriorityStoreProvider>
    )
  })

const MultiselectInspector: React.FunctionComponent<React.PropsWithChildren<unknown>> = React.memo(
  () => {
    const selectedViews = useEditorState(
      Substores.selectedViews,
      (store) => store.editor.selectedViews,
      'InspectorEntryPoint selectedViews',
    )

    return (
      <SingleInspectorEntryPoint
        key={'inspector-entry-selected-views'}
        selectedViews={selectedViews}
      />
    )
  },
)

function updateTargets(localJSXElement: JSXElement): Array<CSSTarget> {
  let localTargets: Array<CSSTarget> = []
  function recursivelyDiscoverStyleTargets(styleObject: any, localPath: Array<string>): void {
    if (typeof styleObject === 'object' && styleObject != null) {
      let selectorLength: TargetSelectorLength = 0
      const keys = Object.keys(styleObject)
      keys.forEach((key) => {
        if (typeof styleObject[key] === 'object') {
          recursivelyDiscoverStyleTargets((styleObject as any)[key], [...localPath, key])
        } else if (typeof selectorLength === 'number') {
          selectorLength = selectorLength + 1
        }
      })
      localTargets.push(cssTarget(localPath, selectorLength))
    }
  }
  let defaults = [...DefaultStyleTargets]
  defaults.reverse().forEach((defaultTarget) => {
    const styleObject = getSimpleAttributeAtPath(
      right(localJSXElement.props),
      PP.createFromArray(defaultTarget.path),
    )
    if (isRight(styleObject) && styleObject.value instanceof Object) {
      recursivelyDiscoverStyleTargets(styleObject.value, defaultTarget.path)
    } else {
      // todo count keys
      localTargets.push(defaultTarget)
    }
  })
  localTargets.reverse()
  return localTargets
}

export const SingleInspectorEntryPoint: React.FunctionComponent<
  React.PropsWithChildren<{
    selectedViews: Array<ElementPath>
  }>
> = React.memo((props) => {
  const { selectedViews } = props
  const dispatch = useDispatch()
  const { jsxMetadata, isUIJSFile } = useEditorState(
    Substores.fullStore,
    (store) => {
      return {
        jsxMetadata: store.editor.jsxMetadata,
        isUIJSFile: isOpenFileUiJs(store.editor),
      }
    },
    'SingleInspectorEntryPoint',
  )

  let targets: Array<CSSTarget> = [...DefaultStyleTargets]

  Utils.fastForEach(selectedViews, (path) => {
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
    if (elementMetadata != null) {
      if (isRight(elementMetadata.element) && isJSXElement(elementMetadata.element.value)) {
        const jsxElement = elementMetadata.element.value
        targets = updateTargets(jsxElement)
      }
    }
  })

  // FIXME TODO HACK until we have better memoization in the Canvas Spy, we sacrifice using R.equals once
  // in order to prevent a big rerender of the inspector

  const targetsReferentiallyStable = useKeepReferenceEqualityIfPossible(targets)

  const refElementsToTargetForUpdates = usePropControlledRef_DANGEROUS(
    getElementsToTarget(selectedViews),
  )

  // Memoized Callbacks
  const [selectedTarget, setSelectedTarget] = React.useState<Array<string>>(
    targetsReferentiallyStable[0].path,
  )

  const onSelectTarget = React.useCallback((targetPath: Array<string>) => {
    setSelectedTarget(targetPath)
  }, [])

  const onStyleSelectorRename = React.useCallback(
    (renameTarget: CSSTarget, label: string) => {
      const originalRenameTarget: CSSTarget = { ...renameTarget }
      let newPath = [...renameTarget.path]
      newPath[newPath.length - 1] = label
      const actions: Array<EditorAction> = refElementsToTargetForUpdates.current.map((elem) =>
        EditorActions.renamePropKey(elem, originalRenameTarget, newPath),
      )
      let newTargetPath = [...originalRenameTarget.path]
      newTargetPath[newTargetPath.length - 1] = label
      if (Utils.shallowEqual(originalRenameTarget, selectedTarget)) {
        setSelectedTarget(newTargetPath)
      }
      dispatch(actions, 'everyone')
    },
    [refElementsToTargetForUpdates, dispatch, selectedTarget],
  )

  const onStyleSelectorDelete = React.useCallback(
    (deleteTarget: CSSTarget) => {
      const path = PP.createFromArray(deleteTarget.path)
      const actions = Utils.flatMapArray(
        (elem) => [EditorActions.unsetProperty(elem, path)],
        refElementsToTargetForUpdates.current,
      )
      dispatch(actions, 'everyone')
    },
    [refElementsToTargetForUpdates, dispatch],
  )

  const onStyleSelectorInsert = React.useCallback(
    (parent: CSSTarget | undefined, label: string) => {
      const newPath = [...(parent?.path ?? []), label]
      const newPropertyPath = PP.createFromArray(newPath)
      const actions: Array<EditorAction> = refElementsToTargetForUpdates.current.map((elem) =>
        EditorActions.setProp_UNSAFE(elem, newPropertyPath, jsExpressionValue({}, emptyComments)),
      )
      dispatch(actions, 'everyone')
      setSelectedTarget(newPath)
    },
    [refElementsToTargetForUpdates, dispatch],
  )

  const inspector = isUIJSFile ? (
    <InspectorContextProvider selectedViews={selectedViews} targetPath={selectedTarget}>
      <Inspector
        selectedViews={selectedViews}
        targets={targetsReferentiallyStable}
        selectedTargetPath={selectedTarget}
        setSelectedTarget={setSelectedTarget}
        onSelectTarget={onSelectTarget}
        onStyleSelectorRename={onStyleSelectorRename}
        onStyleSelectorDelete={onStyleSelectorDelete}
        onStyleSelectorInsert={onStyleSelectorInsert}
      />
    </InspectorContextProvider>
  ) : null

  return inspector
})

export const InspectorContextProvider = React.memo<{
  selectedViews: Array<ElementPath>
  targetPath: Array<string>
  children: React.ReactNode
}>((props) => {
  const { selectedViews } = props
  const dispatch = useDispatch()
  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)

  const { jsxMetadata, allElementProps, projectContents } = useEditorState(
    Substores.projectContentsAndMetadata,
    (store) => {
      return {
        jsxMetadata: store.editor.jsxMetadata,
        allElementProps: store.editor.allElementProps,
        projectContents: store.editor.projectContents,
      }
    },
    'InspectorContextProvider',
  )

  let newEditedMultiSelectedProps: JSXAttributes[] = []
  let newSpiedProps: Array<{ [key: string]: any }> = []
  let newComputedStyles: Array<ComputedStyle> = []
  let newAttributeMetadatas: Array<StyleAttributeMetadata> = []

  Utils.fastForEach(selectedViews, (path) => {
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
    if (elementMetadata != null) {
      if (elementMetadata.computedStyle == null || elementMetadata.attributeMetadata == null) {
        /**
         * This early return will cause the inspector to render with empty fields.
         * Because the computedStyle is only used in some cases for some controls,
         * the empty inspector helps us catch an otherwise silent regression
         */
        return
      }
      const jsxElement = getElementFromProjectContents(path, projectContents)
      if (jsxElement == null) {
        /**
         * This early return will cause the inspector to render with empty fields.
         * With missing jsxElement manipulating style props is not possible.
         */
        return
      }

      const jsxAttributes = isJSXElement(jsxElement) ? jsxElement.props : []
      newEditedMultiSelectedProps.push(jsxAttributes)
      newSpiedProps.push(allElementProps[EP.toString(path)] ?? {})
      newComputedStyles.push(elementMetadata.computedStyle)
      newAttributeMetadatas.push(elementMetadata.attributeMetadata)
    }
  })

  const editedMultiSelectedProps = useKeepReferenceEqualityIfPossible(newEditedMultiSelectedProps)
  const spiedProps = useKeepReferenceEqualityIfPossible(newSpiedProps)
  const computedStyles = useKeepReferenceEqualityIfPossible(newComputedStyles)
  const attributeMetadatas = useKeepReferenceEqualityIfPossible(newAttributeMetadatas)

  const selectedViewsRef = usePropControlledRef_DANGEROUS(selectedViews)
  const refElementsToTargetForUpdates = usePropControlledRef_DANGEROUS(
    getElementsToTarget(selectedViews),
  )

  const jsxMetadataRef = useRefEditorState((store) => store.editor.jsxMetadata)

  const onSubmitValueForHooks = React.useCallback(
    (newValue: JSExpression, path: PropertyPath, transient: boolean) => {
      const actionsArray = [
        ...refElementsToTargetForUpdates.current.flatMap((elem): EditorAction[] => {
          // if the target is the children prop, replace the elements instead
          if (path.propertyElements[0] === 'children') {
            const element = MetadataUtils.findElementByElementPath(jsxMetadataRef.current, elem)
            const children =
              element != null && isRight(element.element) && isJSXElement(element.element.value)
                ? element.element.value.children
                : []
            return replaceFirstChildAndDeleteSiblings(elem, children, newValue)
          }
          return [setProp_UNSAFE(elem, path, newValue)]
        }),
      ]
      const actions: EditorAction[] = transient
        ? [transientActions(actionsArray, refElementsToTargetForUpdates.current)]
        : actionsArray
      dispatch(actions, 'everyone')
    },
    [dispatch, refElementsToTargetForUpdates, jsxMetadataRef],
  )

  const onUnsetValue = React.useCallback(
    (property: PropertyPath | Array<PropertyPath>, transient: boolean) => {
      let actionsArray: Array<EditorAction> = []

      const propTop = PP.create('style', 'top')
      const propBottom = PP.create('style', 'bottom')
      const propLeft = PP.create('style', 'left')
      const propRight = PP.create('style', 'right')

      const invalidGroupState = maybeInvalidGroupState(
        refElementsToTargetForUpdates.current,
        metadataRef.current,
        {
          onGroup: () => null,
          onGroupChild: () => {
            return Array.isArray(property) &&
              property.some(
                (p) =>
                  PP.pathsEqual(p, propTop) ||
                  PP.pathsEqual(p, propBottom) ||
                  PP.pathsEqual(p, propLeft) ||
                  PP.pathsEqual(p, propRight),
              )
              ? 'child-has-missing-pins'
              : null
          },
        },
      )
      if (invalidGroupState != null) {
        dispatch([groupErrorToastAction(invalidGroupState)])
        return
      }

      Utils.fastForEach(refElementsToTargetForUpdates.current, (elem) => {
        if (Array.isArray(property)) {
          Utils.fastForEach(property, (p) => {
            actionsArray.push(unsetProperty(elem, p))
          })
        } else {
          actionsArray.push(unsetProperty(elem, property))
        }
      })

      const actions: EditorAction[] = transient
        ? [transientActions(actionsArray, refElementsToTargetForUpdates.current)]
        : actionsArray
      dispatch(actions, 'everyone')
    },
    [dispatch, refElementsToTargetForUpdates, metadataRef],
  )

  const collectActionsToSubmitValue = React.useCallback(
    (path: PropertyPath, transient: boolean, newValuePrinter: () => any): Array<EditorAction> => {
      const actionsArray = [
        ...refElementsToTargetForUpdates.current.map((elem) => {
          return setProp_UNSAFE(elem, path, newValuePrinter())
        }),
      ]
      return transient
        ? [transientActions(actionsArray, refElementsToTargetForUpdates.current)]
        : actionsArray
    },
    [refElementsToTargetForUpdates],
  )

  const collectActionsToUnsetValue = React.useCallback(
    (property: PropertyPath | Array<PropertyPath>, transient: boolean): Array<EditorAction> => {
      let actionsArray: Array<EditorAction> = []
      Utils.fastForEach(refElementsToTargetForUpdates.current, (elem) => {
        if (Array.isArray(property)) {
          Utils.fastForEach(property, (p) => {
            actionsArray.push(unsetProperty(elem, p))
          })
        } else {
          actionsArray.push(unsetProperty(elem, property))
        }
      })

      return transient
        ? [transientActions(actionsArray, refElementsToTargetForUpdates.current)]
        : actionsArray
    },
    [refElementsToTargetForUpdates],
  )

  const callbackContextValueMemoized = useKeepShallowReferenceEquality({
    onSubmitValue: onSubmitValueForHooks,
    onUnsetValue: onUnsetValue,
    collectActionsToSubmitValue: collectActionsToSubmitValue,
    collectActionsToUnsetValue: collectActionsToUnsetValue,
    selectedViewsRef: selectedViewsRef,
  })

  return (
    <InspectorCallbackContext.Provider value={callbackContextValueMemoized}>
      <InspectorPropsContext.Provider
        value={{
          selectedViews: selectedViews,
          editedMultiSelectedProps: editedMultiSelectedProps,
          targetPath: props.targetPath,
          spiedProps: spiedProps,
          computedStyles: computedStyles,
          selectedAttributeMetadatas: attributeMetadatas,
        }}
      >
        {props.children}
      </InspectorPropsContext.Provider>
    </InspectorCallbackContext.Provider>
  )
})

function useBoolean(starting: boolean): {
  value: boolean
  set: (_: boolean) => void
  toggle: () => void
} {
  const [value, set] = React.useState(starting)
  const toggle = React.useCallback(() => set((v) => !v), [])
  return { value, set, toggle }
}

function useShouldExpandStyleSection(): boolean {
  const shouldExpandFromElementOrComponent = useEditorState(
    Substores.metadata,
    (store) =>
      store.editor.selectedViews.every((target) => {
        const instance = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, target)
        if (instance == null) {
          return true
        }
        return (
          MetadataUtils.isFragmentFromMetadata(instance) ||
          MetadataUtils.isGroupAgainstImports(instance) ||
          isIntrinsicElementMetadata(instance)
        )
      }),
    'useShouldExpandStyleSection shouldExpandFromElementOrComponent',
  )

  return useEditorState(
    Substores.propertyControlsInfo,
    (store) => {
      return store.editor.selectedViews.every((target) => {
        const { propertyControlsInfo, projectContents } = store.editor
        const descriptor = getComponentDescriptorForTarget(
          { propertyControlsInfo, projectContents },
          target,
        )

        if (descriptor == null || descriptor.inspector == null) {
          return shouldExpandFromElementOrComponent
        }

        if (descriptor.inspector.type === 'hidden') {
          return false
        }

        switch (descriptor.inspector.display) {
          case 'collapsed':
            return false
          case 'expanded':
            return true
          default:
            assertNever(descriptor.inspector.display)
        }
      })
    },
    'useShouldExpandStyleSection shouldExpandFromComponentDescription',
  )
}

function useShouldHideInspectorSections(): boolean {
  return useEditorState(
    Substores.propertyControlsInfo,
    (store) =>
      store.editor.selectedViews.some((target) => {
        const { propertyControlsInfo, projectContents } = store.editor
        const descriptor = getComponentDescriptorForTarget(
          { propertyControlsInfo, projectContents },
          target,
        )

        if (descriptor?.inspector == null) {
          return false
        }

        return descriptor.inspector.type === 'hidden'
      }),

    'Inspector inspectorPreferences',
  )
}
