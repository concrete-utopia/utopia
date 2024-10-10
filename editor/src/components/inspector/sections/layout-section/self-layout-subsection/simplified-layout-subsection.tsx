import React from 'react'
import { when } from '../../../../../utils/react-conditionals'
import { FlexColumn, FlexRow, Tooltip, UtopiaTheme } from '../../../../../uuiui'
import { Link } from '../../../../../uuiui/link'
import { useConvertWrapperToFrame } from '../../../../canvas/canvas-strategies/strategies/group-conversion-helpers'
import { Substores, useEditorState } from '../../../../editor/store/store-hook'
import {
  EditorContractDropdown,
  allSelectedElementsContractSelector,
} from '../../../editor-contract-section'
import { FixedHugDropdown } from '../../../fill-hug-fixed-control'
import { ResizeToFitControl } from '../../../resize-to-fit-control'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { RadiusRow } from '../../style-section/container-subsection/radius-row'
import { ClipContentControl } from './clip-content-control'
import { FrameUpdatingLayoutSection } from './frame-updating-layout-section'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { getInspectorPreferencesForTargets } from '../../../../../core/property-controls/property-controls-utils'
import { AlignmentButtons } from '../../../alignment-buttons'
import { GridPlacementSubsection } from '../../style-section/container-subsection/grid-cell-subsection'

export const SimplifiedLayoutSubsection = React.memo(() => {
  const selectedElementContract = useEditorState(
    Substores.metadata,
    allSelectedElementsContractSelector,
    'EditorContractDropdown selectedElementContract',
  )

  const showLayoutSection =
    selectedElementContract === 'frame' || selectedElementContract === 'group'

  const showWrapperSectionWarning = selectedElementContract === 'wrapper-div'

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
    'SimplifiedLayoutSubsection isCodeElement',
  )

  const inspectorPreferences = useEditorState(
    Substores.propertyControlsInfo,
    (store) =>
      getInspectorPreferencesForTargets(
        store.editor.selectedViews,
        store.editor.propertyControlsInfo,
        store.editor.projectContents,
      ),
    'SimplifiedLayoutSubsection inspectorPreferences',
  )

  const shouldShowAlignmentButtons = !isCodeElement && inspectorPreferences.includes('layout')

  const shouldShowGridCellSection = useEditorState(
    Substores.metadata,
    (store) =>
      store.editor.selectedViews.length === 1 &&
      MetadataUtils.isGridCell(store.editor.jsxMetadata, store.editor.selectedViews[0]),
    'Inspector shouldShowGridCellSection',
  )

  return (
    <FlexColumn>
      <FlexRow
        style={{
          flexGrow: 1,
          gap: 8,
          height: UtopiaTheme.layout.rowHeight.normal,
          padding: `0 ${UtopiaTheme.layout.inspectorXPadding}px 0 4px`,
          fontWeight: 600,
        }}
      >
        <EditorContractDropdown />
        <ResizeToFitControl />
      </FlexRow>
      {when(showWrapperSectionWarning, <WrapperElementDisclosureBox />)}
      <FlexColumn style={{ paddingLeft: 8, paddingRight: 8 }}>
        {when(
          showLayoutSection,
          <>
            {when(shouldShowAlignmentButtons, <AlignmentButtons />)}
            <FrameUpdatingLayoutSection />
            <UIGridRow padded={false} variant='<--1fr--><--1fr-->|22px|'>
              <FixedHugDropdown dimension='width' />
              <FixedHugDropdown dimension='height' />
            </UIGridRow>
            <UIGridRow padded={false} variant='<-------------1fr------------->'>
              <RadiusRow />
            </UIGridRow>
            <UIGridRow padded={false} variant='<-------------1fr------------->'>
              <ClipContentControl />
            </UIGridRow>
          </>,
        )}
      </FlexColumn>
      {when(shouldShowGridCellSection, <GridPlacementSubsection />)}
    </FlexColumn>
  )
})
SimplifiedLayoutSubsection.displayName = 'SimplifiedLayoutSubsection'

const WrapperElementDisclosureBox = React.memo(() => {
  const convertToFrame = useConvertWrapperToFrame()
  return (
    <FlexColumn style={{ gap: 8, paddingLeft: 8, paddingRight: 8 }}>
      <span
        style={{
          whiteSpace: 'initial',
          ['textWrap' as any]: 'balance', // this is an experimental Chrome feature
        }}
      >
        Element collapsed because it only has absolute children.
        <Tooltip title={'Convert element to Frame'} placement='left'>
          <Link onClick={convertToFrame}>Make it a Frame</Link>
        </Tooltip>
        to style and position it.
      </span>
    </FlexColumn>
  )
})
