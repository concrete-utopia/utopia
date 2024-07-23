/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { createSelector } from 'reselect'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { allElemsEqual, mapDropNulls } from '../../core/shared/array-utils'
import * as EP from '../../core/shared/element-path'
import { assertNever } from '../../core/shared/utils'
import { when } from '../../utils/react-conditionals'
import {
  Button,
  FlexColumn,
  FlexRow,
  InspectorSectionHeader,
  PopupList,
  Subdued,
  Tooltip,
  colorTheme,
} from '../../uuiui'
import type { ControlStyles } from '../../uuiui-deps'
import { getControlStyles } from '../../uuiui-deps'
import type { EditorContract } from '../canvas/canvas-strategies/strategies/contracts/contract-helpers'
import { getEditorContractForElement } from '../canvas/canvas-strategies/strategies/contracts/contract-helpers'
import { getElementFragmentLikeType } from '../canvas/canvas-strategies/strategies/fragment-like-helpers'
import {
  getCommandsForConversionToDesiredType,
  getInstanceForFragmentToFrameConversion,
  getInstanceForFragmentToGroupConversion,
  getInstanceForFrameToFragmentConversion,
  getInstanceForFrameToGroupConversion,
  getInstanceForGroupToFrameConversion,
  isConversionForbidden,
} from '../canvas/canvas-strategies/strategies/group-conversion-helpers'
import type { GroupProblem } from '../canvas/canvas-strategies/strategies/group-helpers'
import {
  getFixGroupProblemsCommands,
  getGroupChildState,
  getGroupState,
  invalidGroupStateToString,
  isInvalidGroupState,
  treatElementAsGroupLike,
} from '../canvas/canvas-strategies/strategies/group-helpers'
import { notice } from '../common/notice'
import { addToast, applyCommandsAction } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import type { MetadataSubstate } from '../editor/store/store-hook-substore-types'
import type { SelectOption } from './controls/select-control'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import { WarningIcon } from '../../uuiui/warning-icon'

const simpleControlStyles = getControlStyles('simple')
const disabledControlStyles: ControlStyles = {
  ...getControlStyles('simple'),
  mainColor: colorTheme.fg5.value,
}

const selectedElementContractSelector = createSelector(
  metadataSelector,
  (store: MetadataSubstate) => store.editor.domReconstructedMetadata,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  selectedViewsSelector,
  (
    metadata,
    domReconstructedMetadata,
    allElementProps,
    pathTrees,
    selectedViews,
  ): EditorContract | null => {
    if (selectedViews.length !== 1) {
      return null // TODO make it work for mixed selection
    }
    return getEditorContractForElement(
      metadata,
      domReconstructedMetadata,
      allElementProps,
      pathTrees,
      selectedViews[0],
    )
  },
)

export const allSelectedElementsContractSelector = createSelector(
  metadataSelector,
  (store: MetadataSubstate) => store.editor.domReconstructedMetadata,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  selectedViewsSelector,
  (
    metadata,
    domReconstructedMetadata,
    allElementProps,
    pathTrees,
    selectedViews,
  ): EditorContract | 'mixed-multiselection' => {
    const contracts = selectedViews.map((selectedView) =>
      getEditorContractForElement(
        metadata,
        domReconstructedMetadata,
        allElementProps,
        pathTrees,
        selectedView,
      ),
    )
    if (allElemsEqual(contracts)) {
      return contracts[0]
    } else {
      return 'mixed-multiselection'
    }
  },
)

export function groupSectionOption(wrapperType: EditorContract): SelectOption<EditorContract> {
  switch (wrapperType) {
    case 'frame':
      return { value: 'frame', label: 'Frame' }
    case 'wrapper-div':
      return {
        value: 'wrapper-div',
        label: (
          <span style={{ display: 'inline-flex', flexDirection: 'row', alignItems: 'center' }}>
            Wrapper <WarningIcon style={{ display: 'inline', marginLeft: 4 }} color='subdued' />
          </span>
        ),
      }
    case 'fragment':
      return { value: 'fragment', label: 'Fragment' }
    case 'group':
      return { value: 'group', label: 'Group' }
    default:
      assertNever(wrapperType)
  }
}

const FragmentOption = groupSectionOption('fragment')
const FrameOption = groupSectionOption('frame')
const WrapperDivOption = groupSectionOption('wrapper-div')
const GroupOption = groupSectionOption('group')

export const EditorFixProblemsButtonTestId = 'editor-fix-problems-button'

export const EditorContractDropdown = React.memo(() => {
  const dispatch = useDispatch()

  const metadataRef = useRefEditorState(metadataSelector)
  const domReconstructedMetadataRef = useRefEditorState(
    (store) => store.editor.domReconstructedMetadata,
  )
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)

  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const selectedElementContract = useEditorState(
    Substores.metadata,
    selectedElementContractSelector,
    'EditorContractDropdown selectedElementContract',
  )

  const selectedViews = useEditorState(
    Substores.selectedViews,
    selectedViewsSelector,
    'EditorContractDropdown selectedViews',
  )

  const onChange = React.useCallback(
    ({ value }: SelectOption) => {
      const currentType: EditorContract | null = selectedElementContract

      if (currentType == null) {
        // for now, in case of multiselect etc, do nothing
        return
      }

      // If any of the selected views are root elements of components,
      // bounce the entire operation as changing those currently is a problem.
      if (selectedViews.some((selectedView) => EP.isRootElementOfInstance(selectedView))) {
        dispatch(
          [
            addToast(
              notice(
                `Cannot change root elements of components.`,
                'WARNING',
                false,
                'change-root-element-of-component',
              ),
            ),
          ],
          'everyone',
        )
        return
      }

      const desiredType = value as EditorContract

      const commands = getCommandsForConversionToDesiredType(
        metadataRef.current,
        domReconstructedMetadataRef.current,
        elementPathTreeRef.current,
        allElementPropsRef.current,
        selectedViews,
        currentType,
        desiredType,
      )

      if (commands.length > 0) {
        dispatch([applyCommandsAction(commands)])
      }
    },
    [
      selectedElementContract,
      selectedViews,
      metadataRef,
      domReconstructedMetadataRef,
      elementPathTreeRef,
      allElementPropsRef,
      dispatch,
    ],
  )

  const currentValue = React.useMemo(() => {
    if (selectedElementContract === 'fragment') {
      return FragmentOption
    } else if (selectedElementContract === 'group') {
      return GroupOption
    } else if (selectedElementContract === 'frame' || selectedElementContract == null) {
      return FrameOption
    } else if (selectedElementContract === 'wrapper-div') {
      return WrapperDivOption
    }
    assertNever(selectedElementContract)
  }, [selectedElementContract])

  const options = React.useMemo((): Array<SelectOption> => {
    function maybeDisable(option: SelectOption, disabled: boolean, tooltip?: string) {
      return {
        ...option,
        disabled: disabled,
        tooltip: tooltip,
      }
    }
    function maybeReasonForConversionForbidden(result: unknown): string | undefined {
      return isConversionForbidden(result) ? result.reason : undefined
    }

    let disabledOptions: {
      group?: string
      frame?: string
      fragment?: string
    } = {}

    if (selectedViews.length > 1) {
      disabledOptions.group = 'Cannot change for multiselect'
      disabledOptions.frame = 'Cannot change for multiselect'
      disabledOptions.fragment = 'Cannot change for multiselect'
    } else if (selectedViews.length === 1) {
      const view = selectedViews[0]
      switch (currentValue.value) {
        case 'frame':
        case 'wrapper-div':
          disabledOptions.group = maybeReasonForConversionForbidden(
            getInstanceForFrameToGroupConversion(
              metadataRef.current,
              domReconstructedMetadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              view,
            ),
          )
          disabledOptions.fragment = maybeReasonForConversionForbidden(
            getInstanceForFrameToFragmentConversion(
              metadataRef.current,
              domReconstructedMetadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              view,
            ),
          )
          break
        case 'fragment':
          disabledOptions.frame = maybeReasonForConversionForbidden(
            getInstanceForFragmentToFrameConversion(
              metadataRef.current,
              domReconstructedMetadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              view,
              'do-not-convert-if-it-has-static-children',
            ),
          )
          disabledOptions.group = maybeReasonForConversionForbidden(
            getInstanceForFragmentToGroupConversion(
              metadataRef.current,
              domReconstructedMetadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              view,
            ),
          )
          break
        case 'group':
          disabledOptions.frame = maybeReasonForConversionForbidden(
            getInstanceForGroupToFrameConversion(
              metadataRef.current,
              domReconstructedMetadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              view,
            ),
          )
          break
        default:
          assertNever(currentValue.value)
      }
    }

    return [
      maybeDisable(FragmentOption, disabledOptions.fragment != null, disabledOptions.fragment),
      maybeDisable(FrameOption, disabledOptions.frame != null, disabledOptions.frame),
      maybeDisable(GroupOption, disabledOptions.group != null, disabledOptions.group),
    ]
  }, [
    selectedViews,
    currentValue.value,
    metadataRef,
    domReconstructedMetadataRef,
    elementPathTreeRef,
    allElementPropsRef,
  ])

  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)

  const groupProblems = useEditorState(
    Substores.metadata,
    (store): GroupProblem[] =>
      mapDropNulls((path) => {
        if (treatElementAsGroupLike(store.editor.jsxMetadata, path)) {
          const state = getGroupState(
            path,
            store.editor.jsxMetadata,
            store.editor.domReconstructedMetadata,
            store.editor.elementPathTree,
            store.editor.allElementProps,
            projectContentsRef.current,
          )
          if (isInvalidGroupState(state)) {
            return { target: 'group', path: path, state: state }
          }
        } else if (treatElementAsGroupLike(store.editor.jsxMetadata, EP.parentPath(path))) {
          const metadata = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
          const state = getGroupChildState(projectContentsRef.current, metadata)
          if (isInvalidGroupState(state)) {
            return { target: 'group-child', path: path, state: state }
          }
        }
        return null
      }, selectedViews),
    'EditorContractDropdown groupProblems',
  )

  const fixGroupProblems = React.useCallback(() => {
    const commands = getFixGroupProblemsCommands(metadataRef.current, groupProblems)
    if (commands.length > 0) {
      dispatch([applyCommandsAction(commands)])
    }
  }, [groupProblems, dispatch, metadataRef])

  return (
    <FlexRow data-testid={EditorContractSelectorTestID} style={{ flex: 1, gap: 6 }}>
      <PopupList
        id={'editor-contract-popup-list'}
        value={currentValue}
        options={options}
        onSubmitValue={onChange}
        controlStyles={simpleControlStyles}
        containerMode={'noBorder'}
        style={{ borderRadius: 2, width: 'min-width', paddingRight: 6 }}
      />
      {when(
        groupProblems.length > 0,
        <Tooltip
          title={groupProblems
            .map((problem) => `Fix: ${invalidGroupStateToString(problem.state).toLowerCase()}`)
            .join(', ')}
          placement='left'
        >
          <Button
            highlight
            spotlight
            data-testid={EditorFixProblemsButtonTestId}
            style={{
              backgroundColor: colorTheme.errorForeground20.value,
              color: colorTheme.errorForeground.value,
              padding: '0 6px',
              borderRadius: 2,
            }}
            onClick={fixGroupProblems}
          >
            Fix problems
          </Button>
        </Tooltip>,
      )}
    </FlexRow>
  )
})
EditorContractDropdown.displayName = 'EditorContractDropdown'

export const EditorContractSelectorTestID = 'editor-contract-dropdown'

export const GroupSection = React.memo(() => {
  return (
    <FlexColumn>
      <InspectorSectionHeader
        css={{
          transition: 'color .1s ease-in-out',
          color: colorTheme.fg1.value,
          '--buttonContentOpacity': 0.3,
          '&:hover': {
            color: colorTheme.fg1.value,
            '--buttonContentOpacity': 1,
          },
        }}
      >
        <FlexRow
          style={{
            flexGrow: 1,
            alignSelf: 'stretch',
            gap: 8,
          }}
        >
          <span style={{ textTransform: 'capitalize', fontSize: '11px' }}>Group</span>
        </FlexRow>
      </InspectorSectionHeader>
      <FlexRow style={{ padding: 4 }}>
        <EditorContractDropdown />
      </FlexRow>
    </FlexColumn>
  )
})
