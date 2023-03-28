import React from 'react'
import {
  findElementAtPath,
  getSimpleAttributeAtPath,
  MetadataUtils,
} from '../../core/model/element-metadata-utils'
import { isRight, right } from '../../core/shared/either'
import {
  isJSXElement,
  JSExpression,
  JSXAttributes,
  jsExpressionValue,
  JSXElement,
  ComputedStyle,
  StyleAttributeMetadata,
  emptyComments,
} from '../../core/shared/element-template'
import { getJSXAttributeAtPath } from '../../core/shared/jsx-attributes'
import * as PP from '../../core/shared/property-path'
import * as EP from '../../core/shared/element-path'
import Utils from '../../utils/utils'
import { isAspectRatioLockedNew } from '../aspect-ratio'
import { setFocus } from '../common/actions'
import { Alignment, Distribution, EditorAction } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import {
  alignSelectedViews,
  distributeSelectedViews,
  setAspectRatioLock,
  setProp_UNSAFE,
  transientActions,
  unsetProperty,
} from '../editor/actions/action-creators'

import {
  EditorStorePatched,
  ElementsToRerender,
  getJSXComponentsAndImportsForPathFromState,
  getOpenUtopiaJSXComponentsFromStateMultifile,
  isOpenFileUiJs,
} from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import {
  InspectorCallbackContext,
  InspectorPropsContext,
  stylePropPathMappingFn,
} from './common/property-path-hooks'
import { ComponentSection } from './sections/component-section/component-section'
import { EventHandlersSection } from './sections/event-handlers-section/event-handlers-section'
import {
  CSSTarget,
  cssTarget,
  TargetSelectorLength,
} from './sections/header-section/target-selector'
import { ImgSection } from './sections/image-section/image-section'
import { WarningSubsection } from './sections/layout-section/warning-subsection/warning-subsection'
import { SettingsPanel } from './sections/settings-panel/inspector-settingspanel'
import { ClassNameSubsection } from './sections/style-section/className-subsection/className-subsection'
import { StyleSection } from './sections/style-section/style-section'
import {
  TargetSelectorSection,
  TargetSelectorSectionProps,
} from './sections/target-selector-section'
import { usePropControlledRef_DANGEROUS } from './common/inspector-utils'
import {
  useKeepReferenceEqualityIfPossible,
  useKeepShallowReferenceEquality,
} from '../../utils/react-performance'
import { Icn, useColorTheme, UtopiaTheme, FlexRow, Button } from '../../uuiui'
import { getElementsToTarget } from './common/inspector-utils'
import { ElementPath, PropertyPath } from '../../core/shared/project-file-types'
import { when } from '../../utils/react-conditionals'
import { createSelector } from 'reselect'
import { isTwindEnabled } from '../../core/tailwind/tailwind'
import {
  isKeyboardAbsoluteStrategy,
  isStrategyActive,
} from '../canvas/canvas-strategies/canvas-strategies'
import type { StrategyState } from '../canvas/canvas-strategies/interaction-state'
import { LowPriorityStoreProvider } from '../editor/store/store-context-providers'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { FlexSection } from './flex-section'
import { useDispatch } from '../editor/store/dispatch-context'
import { styleStringInArray } from '../../utils/common-constants'
import { SizingSection } from './sizing-section'
import { PositionSection } from './sections/layout-section/position-section'
import { ConditionalSection } from './sections/layout-section/conditional-section'

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
  elementPath: Array<ElementPathElement>
}

interface AlignDistributeButtonProps {
  onMouseUp: () => void
  toolTip: string
  iconType: string
  disabled: boolean
}

const AlignDistributeButton = React.memo<AlignDistributeButtonProps>(
  (props: AlignDistributeButtonProps) => {
    return (
      <Button disabled={props.disabled} onMouseUp={props.onMouseUp}>
        <Icn
          tooltipText={props.toolTip}
          category='layout/commands'
          type={props.iconType}
          width={16}
          height={16}
        />
      </Button>
    )
  },
)
AlignDistributeButton.displayName = 'AlignDistributeButton'

const AlignmentButtons = React.memo((props: { numberOfTargets: number }) => {
  const dispatch = useDispatch()
  const alignSelected = React.useCallback(
    (alignment: Alignment) => {
      dispatch([alignSelectedViews(alignment)], 'everyone')
    },
    [dispatch],
  )

  const distributeSelected = React.useCallback(
    (distribution: Distribution) => {
      dispatch([distributeSelectedViews(distribution)], 'everyone')
    },
    [dispatch],
  )
  const disableAlign = props.numberOfTargets === 0
  const disableDistribute = props.numberOfTargets < 3
  const multipleTargets = props.numberOfTargets > 1

  const alignLeft = React.useCallback(() => alignSelected('left'), [alignSelected])
  const alignHCenter = React.useCallback(() => alignSelected('hcenter'), [alignSelected])
  const alignRight = React.useCallback(() => alignSelected('right'), [alignSelected])
  const alignTop = React.useCallback(() => alignSelected('top'), [alignSelected])
  const alignVCenter = React.useCallback(() => alignSelected('vcenter'), [alignSelected])
  const alignBottom = React.useCallback(() => alignSelected('bottom'), [alignSelected])
  const distributeHorizontal = React.useCallback(
    () => distributeSelected('horizontal'),
    [distributeSelected],
  )
  const distributeVertical = React.useCallback(
    () => distributeSelected('vertical'),
    [distributeSelected],
  )
  const colorTheme = useColorTheme()
  return (
    <FlexRow
      style={{
        justifyContent: 'space-around',
        height: UtopiaTheme.layout.rowHeight.normal,
        position: 'sticky',
        top: 0,
        zIndex: 1,
        background: colorTheme.inspectorBackground.value,
      }}
    >
      <AlignDistributeButton
        onMouseUp={alignLeft}
        toolTip={`Align to left of ${multipleTargets ? 'selection' : 'parent'}`}
        iconType='alignLeft'
        disabled={disableAlign}
      />
      <AlignDistributeButton
        onMouseUp={alignHCenter}
        toolTip={`Align to horizontal center of ${multipleTargets ? 'selection' : 'parent'}`}
        iconType='alignHorizontalCenter'
        disabled={disableAlign}
      />
      <AlignDistributeButton
        onMouseUp={alignRight}
        toolTip={`Align to right of ${multipleTargets ? 'selection' : 'parent'}`}
        iconType='alignRight'
        disabled={disableAlign}
      />
      <AlignDistributeButton
        onMouseUp={alignTop}
        toolTip={`Align to top of ${multipleTargets ? 'selection' : 'parent'}`}
        iconType='alignTop'
        disabled={disableAlign}
      />
      <AlignDistributeButton
        onMouseUp={alignVCenter}
        toolTip={`Align to vertical center of ${multipleTargets ? 'selection' : 'parent'}`}
        iconType='alignVerticalCenter'
        disabled={disableAlign}
      />
      <AlignDistributeButton
        onMouseUp={alignBottom}
        toolTip={`Align to bottom of ${multipleTargets ? 'selection' : 'parent'}`}
        iconType='alignBottom'
        disabled={disableAlign}
      />
      <AlignDistributeButton
        onMouseUp={distributeHorizontal}
        toolTip={`Distribute horizontally ↔`}
        iconType='distributeHorizontal'
        disabled={disableDistribute}
      />
      <AlignDistributeButton
        onMouseUp={distributeVertical}
        toolTip={`Distribute vertically ↕️`}
        iconType='distributeVertical'
        disabled={disableDistribute}
      />
    </FlexRow>
  )
})
AlignmentButtons.displayName = 'AlignmentButtons'

function buildNonDefaultPositionPaths(propertyTarget: Array<string>): Array<PropertyPath> {
  return [
    stylePropPathMappingFn('right', propertyTarget),
    stylePropPathMappingFn('bottom', propertyTarget),
  ]
}

export function shouldInspectorUpdate(
  strategyState: StrategyState,
  elementsToRerender: ElementsToRerender,
): boolean {
  return (
    (!isStrategyActive(strategyState) && elementsToRerender === 'rerender-all-elements') ||
    isKeyboardAbsoluteStrategy(strategyState.currentStrategy)
  )
}

export const Inspector = React.memo<InspectorProps>((props: InspectorProps) => {
  const colorTheme = useColorTheme()
  const { selectedViews, setSelectedTarget, targets } = props
  React.useEffect(() => {
    setSelectedTarget(targets[0].path)
  }, [selectedViews, targets, setSelectedTarget])

  const dispatch = useDispatch()
  const {
    focusedPanel,
    anyComponents,
    anyUnknownElements,
    hasNonDefaultPositionAttributes,
    aspectRatioLocked,
  } = useEditorState(
    Substores.fullStore,
    (store) => {
      const rootMetadata = store.editor.jsxMetadata
      let anyComponentsInner: boolean = false
      let anyUnknownElementsInner: boolean = false
      let hasNonDefaultPositionAttributesInner: boolean = false
      let aspectRatioLockedInner: boolean = false

      Utils.fastForEach(selectedViews, (view) => {
        const { components: rootComponents } = getJSXComponentsAndImportsForPathFromState(
          view,
          store.editor,
          store.derived,
        )
        anyComponentsInner =
          anyComponentsInner || MetadataUtils.isComponentInstance(view, rootComponents)
        const possibleElement = MetadataUtils.findElementByElementPath(rootMetadata, view)
        const elementProps = store.editor.allElementProps[EP.toString(view)]
        if (possibleElement != null && elementProps != null) {
          // Slightly coarse in definition, but element metadata is in a weird little world of
          // its own compared to the props.
          aspectRatioLockedInner =
            aspectRatioLockedInner || isAspectRatioLockedNew(possibleElement, elementProps)

          const metadataNotFound =
            MetadataUtils.findElementByElementPath(rootMetadata, view) == null
          if (metadataNotFound) {
            anyUnknownElementsInner = true
          }
          if (isRight(possibleElement.element)) {
            const elem = possibleElement.element.value
            if (isJSXElement(elem)) {
              if (!hasNonDefaultPositionAttributesInner) {
                for (const nonDefaultPositionPath of buildNonDefaultPositionPaths(
                  styleStringInArray,
                )) {
                  const attributeAtPath = getJSXAttributeAtPath(elem.props, nonDefaultPositionPath)
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
        anyUnknownElements: anyUnknownElementsInner,
        hasNonDefaultPositionAttributes: hasNonDefaultPositionAttributesInner,
        aspectRatioLocked: aspectRatioLockedInner,
      }
    },
    'Inspector',
  )

  const onFocus = React.useCallback(
    (event: React.FocusEvent<HTMLElement>) => {
      if (focusedPanel !== 'inspector') {
        dispatch([setFocus('inspector')], 'inspector')
      }
    },
    [dispatch, focusedPanel],
  )

  const toggleAspectRatioLock = React.useCallback(() => {
    const actions: EditorAction[] = selectedViews.map((path) =>
      setAspectRatioLock(path, !aspectRatioLocked),
    )
    dispatch(actions, 'everyone')
  }, [dispatch, selectedViews, aspectRatioLocked])

  const shouldShowInspector = React.useMemo(() => {
    return props.elementPath.length !== 0 && !anyUnknownElements
  }, [props.elementPath, anyUnknownElements])

  function renderInspectorContents() {
    return (
      <React.Fragment>
        <div
          style={{
            display: shouldShowInspector ? 'none' : undefined,
          }}
        >
          <SettingsPanel />
        </div>
        <div
          style={{
            display: shouldShowInspector ? undefined : 'none',
          }}
        >
          <AlignmentButtons numberOfTargets={selectedViews.length} />
          {when(isTwindEnabled(), <ClassNameSubsection />)}
          {anyComponents ? <ComponentSection isScene={false} /> : null}
          <ConditionalSection paths={selectedViews} />
          <TargetSelectorSection
            targets={props.targets}
            selectedTargetPath={props.selectedTargetPath}
            onSelectTarget={props.onSelectTarget}
            onStyleSelectorRename={props.onStyleSelectorRename}
            onStyleSelectorDelete={props.onStyleSelectorDelete}
            onStyleSelectorInsert={props.onStyleSelectorInsert}
          />
          <PositionSection
            hasNonDefaultPositionAttributes={hasNonDefaultPositionAttributes}
            aspectRatioLocked={aspectRatioLocked}
            toggleAspectRatioLock={toggleAspectRatioLock}
          />
          <SizingSection />
          <FlexSection />
          <StyleSection />
          <WarningSubsection />
          <ImgSection />
          <EventHandlersSection />
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
        color: colorTheme.neutralForeground.value,
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
  const { jsxMetadata, isUIJSFile, allElementProps } = useEditorState(
    Substores.fullStore,
    (store) => {
      return {
        jsxMetadata: store.editor.jsxMetadata,
        isUIJSFile: isOpenFileUiJs(store.editor),
        allElementProps: store.editor.allElementProps,
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

  const elementPath = useKeepReferenceEqualityIfPossible(
    React.useMemo(() => {
      if (selectedViews.length === 0) {
        return []
      }

      let elements: Array<ElementPathElement> = []
      Utils.fastForEach(EP.allPathsForLastPart(selectedViews[0]), (path) => {
        const component = MetadataUtils.findElementByElementPath(jsxMetadata, path)
        if (component != null) {
          elements.push({
            name: MetadataUtils.getElementLabel(allElementProps, path, jsxMetadata),
            path: path,
          })
        }
      })
      return elements
    }, [selectedViews, jsxMetadata, allElementProps]),
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
        elementPath={elementPath}
        onSelectTarget={onSelectTarget}
        onStyleSelectorRename={onStyleSelectorRename}
        onStyleSelectorDelete={onStyleSelectorDelete}
        onStyleSelectorInsert={onStyleSelectorInsert}
      />
    </InspectorContextProvider>
  ) : null

  return inspector
})

const rootComponentsSelector = createSelector(
  (store: EditorStorePatched) => store.editor.projectContents,
  (store: EditorStorePatched) => store.editor.codeResultCache.curriedResolveFn,
  (store: EditorStorePatched) => store.editor.canvas.openFile?.filename ?? null,
  (projectContents, curriedResolveFn, openFilePath) => {
    const resolveFn = curriedResolveFn(projectContents)
    return getOpenUtopiaJSXComponentsFromStateMultifile(projectContents, resolveFn, openFilePath)
  },
)

export const InspectorContextProvider = React.memo<{
  selectedViews: Array<ElementPath>
  targetPath: Array<string>
  children: React.ReactNode
}>((props) => {
  const { selectedViews } = props
  const dispatch = useDispatch()
  const { jsxMetadata, allElementProps } = useEditorState(
    Substores.metadata,
    (store) => {
      return {
        jsxMetadata: store.editor.jsxMetadata,
        allElementProps: store.editor.allElementProps,
      }
    },
    'InspectorContextProvider',
  )

  const rootComponents = useKeepReferenceEqualityIfPossible(
    useEditorState(
      Substores.fullStore,
      rootComponentsSelector,
      'InspectorContextProvider rootComponents',
    ),
  )

  let newEditedMultiSelectedProps: JSXAttributes[] = []
  let newSpiedProps: Array<{ [key: string]: any }> = []
  let newComputedStyles: Array<ComputedStyle> = []
  let newAttributeMetadatas: Array<StyleAttributeMetadata> = []

  Utils.fastForEach(selectedViews, (path) => {
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
    if (elementMetadata != null) {
      if (elementMetadata.computedStyle == null || elementMetadata.attributeMetadatada == null) {
        /**
         * This early return will cause the inspector to render with empty fields.
         * Because the computedStyle is only used in some cases for some controls,
         * the empty inspector helps us catch an otherwise silent regression
         */
        return
      }
      const jsxElement = findElementAtPath(path, rootComponents)
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
      newAttributeMetadatas.push(elementMetadata.attributeMetadatada)
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

  const onSubmitValueForHooks = React.useCallback(
    (newValue: JSExpression, path: PropertyPath, transient: boolean) => {
      const actionsArray = [
        ...refElementsToTargetForUpdates.current.map((elem) => {
          return setProp_UNSAFE(elem, path, newValue)
        }),
      ]
      const actions: EditorAction[] = transient
        ? [transientActions(actionsArray, refElementsToTargetForUpdates.current)]
        : actionsArray
      dispatch(actions, 'everyone')
    },
    [dispatch, refElementsToTargetForUpdates],
  )

  const onUnsetValue = React.useCallback(
    (property: PropertyPath | Array<PropertyPath>, transient: boolean) => {
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

      const actions: EditorAction[] = transient
        ? [transientActions(actionsArray, refElementsToTargetForUpdates.current)]
        : actionsArray
      dispatch(actions, 'everyone')
    },
    [dispatch, refElementsToTargetForUpdates],
  )

  const collectActionsToSubmitValue = React.useCallback(
    (newValue: JSExpression, path: PropertyPath, transient: boolean): Array<EditorAction> => {
      const actionsArray = [
        ...refElementsToTargetForUpdates.current.map((elem) => {
          return setProp_UNSAFE(elem, path, newValue)
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
