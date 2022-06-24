import React from 'react'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import {
  DetectedLayoutSystem,
  emptyComments,
  jsxAttributeValue,
} from '../../../../../core/shared/element-template'
import { shallowEqual } from '../../../../../core/shared/equality-utils'
import { fastForEach } from '../../../../../core/shared/utils'
import {
  FunctionIcons,
  Icn,
  Icons,
  InspectorSectionIcons,
  InspectorSubsectionHeader,
  SquareButton,
  Tooltip,
  useColorTheme,
} from '../../../../../uuiui'
import { usePropControlledState } from '../../../../../uuiui-deps'
import { InlineIndicator, InlineLink } from '../../../../../uuiui/inline-button'
import { useEditorState, useRefEditorState } from '../../../../editor/store/store-hook'
import { ExpandableIndicator } from '../../../../navigator/navigator-item/expandable-indicator'
import { CSSPosition } from '../../../common/css-utils'
import * as EP from '../../../../../core/shared/element-path'
import {
  FlexElementSubsectionExperiment,
  useInitialAdvancedSectionState,
  useInitialCrossSectionState,
  useInitialFixedSectionState,
  useInitialSizeSectionState,
} from '../flex-element-subsection/flex-element-subsection'
import { GiganticSizePinsSubsection } from './gigantic-size-pins-subsection'
import { runEscapeHatch, selectComponents } from '../../../../editor/actions/action-creators'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { unless, when } from '../../../../../utils/react-conditionals'
import {
  InspectorCallbackContext,
  InspectorPropsContext,
  stylePropPathMappingFn,
} from '../../../common/property-path-hooks'
import { StyleLayoutProp } from '../../../../../core/layout/layout-helpers-new'
import { usePropControlledStateV2 } from '../../../common/inspector-utils'
import { useContextSelector } from 'use-context-selector'
import { PropertyPath } from '../../../../../core/shared/project-file-types'

export type SelfLayoutTab = 'absolute' | 'flex' | 'flow' | 'sticky'

function useActiveLayoutTab(
  position: CSSPosition | null,
  parentLayoutSystem: DetectedLayoutSystem,
) {
  let value: SelfLayoutTab
  if (position === 'absolute' || position === 'sticky') {
    value = position
  } else if (parentLayoutSystem === 'flex') {
    value = 'flex'
  } else if (parentLayoutSystem === 'grid') {
    // TODO GRID
    // value = 'grid'
    value = 'flow'
  } else {
    value = 'flow'
  }
  return usePropControlledState(value)
}

interface SelfLayoutSubsectionProps {
  position: CSSPosition | null
  parentLayoutSystem: DetectedLayoutSystem
  parentFlexDirection: string | null
  aspectRatioLocked: boolean
  toggleAspectRatioLock: () => void
}

const useLayoutSectionInitialToggleState = (
  activeTab: SelfLayoutTab,
  parentFlexDirection: string | null,
): boolean => {
  const initialCrossSectionState = useInitialCrossSectionState(parentFlexDirection)
  const initialFixedSectionState = useInitialFixedSectionState(parentFlexDirection)
  const initialAdvancedSectionState = useInitialAdvancedSectionState()
  const initialSizeSectionState = useInitialSizeSectionState()
  if (activeTab != 'flex') {
    return true
  } else {
    return (
      initialCrossSectionState ||
      initialFixedSectionState ||
      initialAdvancedSectionState ||
      initialSizeSectionState
    )
  }
}

export const LayoutSubsection = React.memo((props: SelfLayoutSubsectionProps) => {
  const [activeTab, setActiveTab] = useActiveLayoutTab(props.position, props.parentLayoutSystem)

  const initialLayoutSectionOpen = useLayoutSectionInitialToggleState(
    activeTab,
    props.parentFlexDirection,
  )

  const [selfLayoutSectionOpen, setSelfLayoutSectionOpen] =
    usePropControlledStateV2(initialLayoutSectionOpen)

  const toggleSection = React.useCallback(
    () => setSelfLayoutSectionOpen(!selfLayoutSectionOpen),
    [selfLayoutSectionOpen, setSelfLayoutSectionOpen],
  )
  return (
    <>
      <LayoutSectionHeader
        layoutType={activeTab}
        toggleSection={toggleSection}
        selfLayoutSectionOpen={selfLayoutSectionOpen}
      />
      {when(selfLayoutSectionOpen, <LayoutSubsectionContent {...props} />)}
    </>
  )
})

export const LayoutSubsectionContent = React.memo((props: SelfLayoutSubsectionProps) => {
  const [activeTab, setActiveTab] = useActiveLayoutTab(props.position, props.parentLayoutSystem)

  const selectedViews = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.selectedViews
  })
  return (
    <>
      {when(activeTab === 'flex', <FlexInfoBox />)}
      {unless(
        activeTab === 'flex',
        <GiganticSizePinsSubsection
          key={selectedViews.map(EP.toString).join(',')}
          layoutType={activeTab}
          parentFlexDirection={props.parentFlexDirection}
          aspectRatioLocked={props.aspectRatioLocked}
          toggleAspectRatioLock={props.toggleAspectRatioLock}
        />,
      )}
      {when(
        activeTab === 'flex',
        <FlexElementSubsectionExperiment parentFlexDirection={props.parentFlexDirection} />,
      )}
    </>
  )
})

interface LayoutSectionHeaderProps {
  layoutType: SelfLayoutTab | 'grid'
  selfLayoutSectionOpen: boolean
  toggleSection: () => void
}

const selfLayoutProperties: Array<StyleLayoutProp> = [
  'alignSelf',
  'bottom',
  'flex',
  'flexBasis',
  'flexGrow',
  'flexShrink',
  'left',
  'marginBottom',
  'marginLeft',
  'marginRight',
  'marginTop',
  'maxHeight',
  'maxWidth',
  'minHeight',
  'minWidth',
  'paddingBottom',
  'paddingLeft',
  'paddingRight',
  'paddingTop',
  'padding',
  'position',
  'right',
  'top',
  'left',
  'top',
  'right',
  'bottom',
]

function selfLayoutConfigPropertyPaths(propertyTarget: ReadonlyArray<string>): Array<PropertyPath> {
  return selfLayoutProperties.map((name) => {
    return stylePropPathMappingFn(name, propertyTarget)
  })
}

function useDeleteAllSelfLayoutConfig() {
  const propertyTarget = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })
  const { onUnsetValue } = React.useContext(InspectorCallbackContext)
  return React.useCallback(() => {
    onUnsetValue(selfLayoutConfigPropertyPaths(propertyTarget), false)
  }, [onUnsetValue, propertyTarget])
}

const LayoutSectionHeader = React.memo((props: LayoutSectionHeaderProps) => {
  const colorTheme = useColorTheme()
  const { layoutType, selfLayoutSectionOpen, toggleSection } = props
  const onDeleteAllConfig = useDeleteAllSelfLayoutConfig()

  const dispatch = useRefEditorState((store) => store.dispatch)
  const selectedViews = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.selectedViews
  })
  const onAbsoluteButtonClick = React.useCallback(() => {
    dispatch.current([runEscapeHatch(selectedViews)], 'everyone')
  }, [dispatch, selectedViews])

  return (
    <InspectorSubsectionHeader>
      <div style={{ flexGrow: 1, display: 'flex', gap: 8 }}>
        <InspectorSectionIcons.Layout />
        <span
          style={{
            textTransform: 'uppercase',
            fontWeight: 600,
            paddingRight: 8,
            color: colorTheme.primary.value,
            fontSize: 10,
          }}
        >
          {layoutType}
        </span>
        <ParentIndicatorAndLink />
        <ChildrenOrContentIndicator />
      </div>
      {when(
        selfLayoutSectionOpen && layoutType !== 'absolute',
        <Tooltip title='Use Absolute Positioning' placement='bottom'>
          <SquareButton highlight onClick={onAbsoluteButtonClick}>
            <div style={{ color: colorTheme.brandNeonPink.value }}>*</div>
          </SquareButton>
        </Tooltip>,
      )}
      {when(
        selfLayoutSectionOpen,
        <SquareButton highlight onClick={onDeleteAllConfig}>
          <FunctionIcons.Delete />
        </SquareButton>,
      )}
      <SquareButton highlight onClick={toggleSection}>
        <ExpandableIndicator
          testId='layout-system-expand'
          visible
          collapsed={!selfLayoutSectionOpen}
          selected={false}
        />
      </SquareButton>
    </InspectorSubsectionHeader>
  )
})

interface ParentIndicatorAndLinkProps {
  style?: React.CSSProperties
}
const ParentIndicatorAndLink = (props: ParentIndicatorAndLinkProps) => {
  const parentPath = useEditorState((store) => {
    if (store.editor.selectedViews.length !== 1) {
      return null
    }
    const target = store.editor.selectedViews[0]
    const parent = EP.parentPath(target)
    return EP.isStoryboardPath(parent) ? null : parent
  }, 'ParentIndicatorAndLink parentPath')

  const dispatch = useEditorState((store) => store.dispatch, 'ParentIndicatorAndLink dispatch')

  const handleClick = React.useCallback(() => {
    if (parentPath != null) {
      dispatch([selectComponents([parentPath], false)], 'everyone')
    }
  }, [parentPath, dispatch])

  if (parentPath == null) {
    return null
  }

  return (
    <InlineLink
      style={{
        fontSize: 10,
        fontWeight: 400,
        paddingLeft: 0,
        paddingRight: 0,
        textTransform: 'capitalize',
        ...props.style,
      }}
      onClick={handleClick}
    >
      parent
    </InlineLink>
  )
}

function useElementHasChildrenOrContent() {
  return useEditorState((store) => {
    if (store.editor.selectedViews.length !== 1) {
      return {
        hasChildren: false,
        hasContent: false,
      }
    }
    const element = MetadataUtils.findElementByElementPath(
      store.editor.jsxMetadata,
      store.editor.selectedViews[0],
    )
    const textContent = element != null ? MetadataUtils.getTextContentOfElement(element) : null
    return {
      hasChildren: (element?.specialSizeMeasurements.renderedChildrenCount ?? 0) > 0,
      hasContent: textContent != null && textContent.length > 0,
    }
  }, 'ChildrenLink children')
}

const ChildrenOrContentIndicator = () => {
  const theme = useColorTheme()
  const { hasChildren, hasContent } = useElementHasChildrenOrContent()

  return (
    <InlineIndicator
      shouldIndicate={hasChildren || hasContent}
      style={{
        fontSize: 10,
        paddingLeft: 0,
        paddingRight: 0,
        fontWeight: 400,
        textDecoration: hasChildren || hasContent ? undefined : 'line-through',
      }}
    >
      {hasChildren ? 'Children' : 'Content'}
    </InlineIndicator>
  )
}

const FlexInfoBox = React.memo(() => {
  const { hasChildren, hasContent } = useElementHasChildrenOrContent()

  return (
    <UIGridRow padded tall={false} variant={'|--32px--|<--------auto-------->'}>
      <span
        style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        <Icn category='layout/systems' type='flexbox' color={'main'} width={16} height={16} />
      </span>
      <p>
        This element is positioned and sized by its{' '}
        <ParentIndicatorAndLink style={{ textTransform: 'lowercase', fontSize: 11 }} />
        {hasChildren || hasContent ? (
          <span>
            {' '}
            and <ChildrenLinkInInfobox />
          </span>
        ) : null}
        .
      </p>
    </UIGridRow>
  )
})

const ChildrenLinkInInfobox = () => {
  const { hasChildren, hasContent } = useElementHasChildrenOrContent()

  if (hasChildren) {
    return (
      <InlineLink
        style={{
          paddingLeft: 0,
          paddingRight: 0,
          fontWeight: 400,
        }}
      >
        children
      </InlineLink>
    )
  } else if (hasContent) {
    return <span>content</span>
  } else return null
}
