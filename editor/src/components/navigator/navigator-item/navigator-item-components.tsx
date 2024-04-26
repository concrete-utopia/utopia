import React from 'react'
import { type ElementPath } from '../../../core/shared/project-file-types'
import type { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import type { IcnProps } from '../../../uuiui'
import { useColorTheme, Button, FlexRow, Icn } from '../../../uuiui'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import { when } from '../../../utils/react-conditionals'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { NavigatorEntry } from '../../editor/store/editor-state'
import {
  getMetadata,
  isConditionalClauseNavigatorEntry,
  isRegularNavigatorEntry,
  isRenderPropValueNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../../editor/store/editor-state'
import type { SelectionLocked } from '../../canvas/canvas-types'
import { getJSXElementNameAsString } from '../../../core/shared/element-template'
import { getRegisteredComponent } from '../../../core/property-controls/property-controls-utils'

export const NavigatorHintCircleDiameter = 8

const outletAwareBackgroundColor = (
  colorTheme: ReturnType<typeof useColorTheme>,
  isOutlet: boolean,
) => (isOutlet ? colorTheme.aqua.value : colorTheme.navigatorResizeHintBorder.value)

interface NavigatorHintProps {
  testId: string
  shouldBeShown: boolean
  shouldAcceptMouseEvents: boolean
  margin: number
  isOutletOrDescendantOfOutlet: boolean
}

export const NavigatorHintTop = React.forwardRef<HTMLDivElement, NavigatorHintProps>(
  (props, ref) => {
    const colorTheme = useColorTheme()
    return (
      <div
        data-testid={props.testId}
        ref={ref}
        style={{
          position: 'relative',
          opacity: props.shouldBeShown ? 1 : 0,
          zIndex: 1,
          pointerEvents: props.shouldAcceptMouseEvents ? 'inherit' : 'none',
        }}
      >
        <div
          style={{
            position: 'absolute',
            top: -8,
            width: '100%',
            height: 16,
          }}
        >
          <div
            style={{
              marginLeft: props.margin,
              height: '100%',
              display: 'flex',
              flexDirection: 'row',
              alignItems: 'center',
              justifyContent: 'flex-start',
            }}
          >
            <div
              style={{
                backgroundColor: outletAwareBackgroundColor(
                  colorTheme,
                  props.isOutletOrDescendantOfOutlet,
                ),
                height: 2,
                flexGrow: 1,
              }}
            />
            <div
              style={{
                position: 'absolute',
                backgroundColor: colorTheme.bg0.value,
                width: NavigatorHintCircleDiameter,
                height: NavigatorHintCircleDiameter,
                contain: 'layout',
                border: `2px solid ${outletAwareBackgroundColor(
                  colorTheme,
                  props.isOutletOrDescendantOfOutlet,
                )}`,
                borderRadius: '50%',
              }}
            />
          </div>
        </div>
      </div>
    )
  },
)

export const NavigatorHintBottom = React.forwardRef<HTMLDivElement, NavigatorHintProps>(
  (props, ref) => {
    const colorTheme = useColorTheme()
    return (
      <div
        data-testid={props.testId}
        ref={ref}
        style={{
          opacity: props.shouldBeShown ? 1 : 0,
          position: 'relative',
          zIndex: 1,
          pointerEvents: props.shouldAcceptMouseEvents ? 'inherit' : 'none',
        }}
      >
        <div
          style={{
            position: 'absolute',
            bottom: -8,
            width: '100%',
            height: 16,
          }}
        >
          <div
            style={{
              marginLeft: props.margin,
              height: '100%',
              display: 'flex',
              flexDirection: 'row',
              alignItems: 'center',
              justifyContent: 'flex-start',
            }}
          >
            <div
              style={{
                backgroundColor: outletAwareBackgroundColor(
                  colorTheme,
                  props.isOutletOrDescendantOfOutlet,
                ),
                height: 2,
                flexGrow: 1,
              }}
            />
            <div
              style={{
                position: 'absolute',
                backgroundColor: colorTheme.bg0.value,
                width: NavigatorHintCircleDiameter,
                height: NavigatorHintCircleDiameter,
                contain: 'layout',
                border: `2px solid ${outletAwareBackgroundColor(
                  colorTheme,
                  props.isOutletOrDescendantOfOutlet,
                )}`,
                borderRadius: '50%',
              }}
            />
          </div>
        </div>
      </div>
    )
  },
)

interface VisiblityIndicatorProps {
  shouldShow: boolean
  visibilityEnabled: boolean
  selected: boolean
  iconColor: IcnProps['color']
  onClick: () => void
}

export const VisibilityIndicator: React.FunctionComponent<
  React.PropsWithChildren<VisiblityIndicatorProps>
> = React.memo((props) => {
  const color = props.iconColor

  return (
    <Button
      onClick={props.onClick}
      style={{
        height: 12,
        width: 12,
        opacity: props.shouldShow ? 1 : 0,
      }}
    >
      {props.visibilityEnabled ? (
        <Icn category='semantic' type='eyeopen' color={color} width={12} height={12} />
      ) : (
        <Icn category='semantic' type='eye-strikethrough' color={color} width={12} height={12} />
      )}
    </Button>
  )
})

const useSupportsChildren = (target: ElementPath): 'all' | 'all-with-preferred' | 'none' => {
  const targetElement = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, target),
    'useSupportsChildren targetElement',
  )

  return useEditorState(
    Substores.restOfEditor,
    (store) => {
      const targetJSXElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(targetElement)
      if (targetJSXElement == null) {
        return 'none'
      }

      if (MetadataUtils.intrinsicElementThatSupportsChildren(targetJSXElement)) {
        return 'all'
      }

      const elementImportInfo = targetElement?.importInfo
      if (elementImportInfo == null) {
        return 'none'
      }

      const targetName = getJSXElementNameAsString(targetJSXElement.name)
      const registeredComponent = getRegisteredComponent(
        targetName,
        elementImportInfo.filePath,
        store.editor.propertyControlsInfo,
      )

      if ((registeredComponent?.preferredChildComponents?.length ?? 0) > 0) {
        return 'all-with-preferred'
      } else if (registeredComponent?.supportsChildren ?? false) {
        return 'all'
      } else {
        return 'none'
      }
    },
    'useSupportsChildren supportsChildren',
  )
}

interface AddChildButtonProps {
  target: ElementPath
  iconColor: IcnProps['color']
  showPreferredPicker: React.MouseEventHandler<HTMLDivElement>
  showFullPicker: React.MouseEventHandler<HTMLDivElement>
}

export function addChildButtonTestId(target: ElementPath): string {
  return `add-child-button-${EP.toString(target)}`
}

const AddChildButton = React.memo((props: AddChildButtonProps) => {
  const color = props.iconColor
  const supportsChildren = useSupportsChildren(props.target)

  return (
    <Button
      onClick={
        supportsChildren === 'all-with-preferred' ? props.showPreferredPicker : props.showFullPicker
      }
      style={{
        height: 12,
        width: 12,
      }}
      data-testid={addChildButtonTestId(props.target)}
    >
      <Icn
        category='semantic'
        type='plus-in-white-translucent-circle'
        color={color}
        width={12}
        height={12}
      />
    </Button>
  )
})

const ReplaceElementButton = React.memo((props: AddChildButtonProps) => {
  const showPreferred = useSupportsChildren(EP.parentPath(props.target)) === 'all-with-preferred'

  return (
    <Button
      onClick={showPreferred ? props.showPreferredPicker : props.showFullPicker}
      style={{
        height: 12,
        width: 12,
      }}
    >
      <Icn
        category='tools'
        type='convert-action'
        color={'main'} // FIXME Add missing colours
        width={18}
        height={18}
        style={{ transform: 'scale(0.67)' }}
      />
    </Button>
  )
})

interface SelectionLockedIndicatorProps {
  shouldShow: boolean
  value: SelectionLocked
  selected: boolean
  iconColor: IcnProps['color']
  isDescendantOfLocked: boolean
  onClick: (value: SelectionLocked) => void
}

export const SelectionLockedIndicator: React.FunctionComponent<
  React.PropsWithChildren<SelectionLockedIndicatorProps>
> = React.memo((props) => {
  const { shouldShow, value, selected, iconColor, isDescendantOfLocked, onClick } = props
  const color = iconColor

  const handleClick = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      event.stopPropagation()
      switch (value) {
        case 'selectable':
          onClick('locked-hierarchy')
          break
        case 'locked-hierarchy':
          onClick('locked')
          break
        case 'locked':
        default:
          onClick('selectable')
          break
      }
    },
    [onClick, value],
  )
  return (
    <Button
      onClick={handleClick}
      onMouseDown={stopPropagation}
      style={{
        height: 12,
        width: 12,
        display: shouldShow ? 'block' : 'none',
      }}
    >
      {when(
        value === 'locked',
        <Icn category='semantic' type='lockclosed' color={color} width={12} height={12} />,
      )}
      {when(
        value === 'locked-hierarchy',
        <Icn category='semantic' type='lockcloseddot' color={color} width={12} height={12} />,
      )}
      {when(
        value === 'selectable' && !isDescendantOfLocked,
        <Icn category='semantic' type='lockopen' color={color} width={12} height={12} />,
      )}
      {when(
        value === 'selectable' && isDescendantOfLocked,
        <Icn category='semantic' type='dot' color={color} width={12} height={12} />,
      )}
    </Button>
  )
})

interface OriginalComponentNameLabelProps {
  selected: boolean
  instanceOriginalComponentName: string | null
}

export const OriginalComponentNameLabel: React.FunctionComponent<
  React.PropsWithChildren<OriginalComponentNameLabelProps>
> = React.memo((props) => {
  const colorTheme = useColorTheme()
  return (
    <div
      style={{
        fontStyle: 'normal',
        paddingRight: 4,
        paddingLeft: 4,
        fontSize: 10,
        color: props.selected ? colorTheme.white.value : colorTheme.navigatorComponentName.value,
        display: props.instanceOriginalComponentName == null ? 'none' : undefined,
      }}
    >
      {props.instanceOriginalComponentName}
    </div>
  )
})

interface NavigatorItemActionSheetProps {
  selected: boolean
  highlighted: boolean
  navigatorEntry: NavigatorEntry
  isVisibleOnCanvas: boolean // TODO FIXME bad name, also, use state
  instanceOriginalComponentName: string | null
  isSlot: boolean
  iconColor: IcnProps['color']
  background?: string | any
  dispatch: EditorDispatch
  showInsertChildPickerPreferred: React.MouseEventHandler<HTMLDivElement>
  showInsertChildPickerFull: React.MouseEventHandler<HTMLDivElement>
  showReplaceElementPickerPreferred: React.MouseEventHandler<HTMLDivElement>
  showReplaceElementPickerFull: React.MouseEventHandler<HTMLDivElement>
}

export const NavigatorItemActionSheet: React.FunctionComponent<
  React.PropsWithChildren<NavigatorItemActionSheetProps>
> = React.memo((props) => {
  const { navigatorEntry, dispatch } = props

  const toggleHidden = React.useCallback(() => {
    if (isRegularNavigatorEntry(navigatorEntry)) {
      dispatch([EditorActions.toggleHidden([navigatorEntry.elementPath])], 'everyone')
    }
  }, [dispatch, navigatorEntry])

  const toggleSelectable = React.useCallback(
    (newValue: SelectionLocked) => {
      if (isRegularNavigatorEntry(navigatorEntry)) {
        dispatch(
          [EditorActions.toggleSelectionLock([navigatorEntry.elementPath], newValue)],
          'everyone',
        )
      }
    },
    [dispatch, navigatorEntry],
  )
  const isLockedElement = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return store.editor.lockedElements.simpleLock.some((path) =>
        EP.pathsEqual(navigatorEntry.elementPath, path),
      )
    },
    'NavigatorItemActionSheet isLockedElement',
  )
  const isLockedHierarchy = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return store.editor.lockedElements.hierarchyLock.some((path) =>
        EP.pathsEqual(navigatorEntry.elementPath, path),
      )
    },
    'NavigatorItemActionSheet isLockedHierarchy',
  )

  const jsxMetadataRef = useRefEditorState((store) => getMetadata(store.editor))
  const isSceneElement = React.useMemo(() => {
    return (
      isRegularNavigatorEntry(navigatorEntry) &&
      MetadataUtils.isProbablyScene(jsxMetadataRef.current, navigatorEntry.elementPath)
    )
  }, [navigatorEntry, jsxMetadataRef])

  const isDescendantOfLocked = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return MetadataUtils.isDescendantOfHierarchyLockedElement(
        navigatorEntry.elementPath,
        store.editor.lockedElements,
      )
    },
    'NavigatorItemActionSheet descendant of locked',
  )

  const isConditionalClauseTitle = isConditionalClauseNavigatorEntry(props.navigatorEntry)

  return (
    <FlexRow
      style={{
        padding: '4px 5px 4px 4px',
        borderRadius: '0 5px 5px 0',
        position: 'fixed',
        gap: 4,
        right: 0,
        background:
          props.highlighted ||
          props.selected ||
          !props.isVisibleOnCanvas ||
          isLockedElement ||
          isLockedHierarchy ||
          isDescendantOfLocked
            ? props.background
            : 'transparent',
      }}
    >
      <OriginalComponentNameLabel
        selected={props.selected}
        instanceOriginalComponentName={props.instanceOriginalComponentName}
      />
      {(navigatorEntry.type === 'REGULAR' || navigatorEntry.type === 'RENDER_PROP_VALUE') &&
      (props.highlighted || props.selected) ? (
        <>
          <AddChildButton
            target={navigatorEntry.elementPath}
            iconColor={props.iconColor}
            showPreferredPicker={props.showInsertChildPickerPreferred}
            showFullPicker={props.showInsertChildPickerFull}
          />
          <ReplaceElementButton
            target={navigatorEntry.elementPath}
            iconColor={props.iconColor}
            showPreferredPicker={props.showReplaceElementPickerPreferred}
            showFullPicker={props.showReplaceElementPickerFull}
          />
        </>
      ) : null}
      <SelectionLockedIndicator
        key={`selection-locked-indicator-${varSafeNavigatorEntryToKey(navigatorEntry)}`}
        shouldShow={
          !isSceneElement &&
          (props.highlighted ||
            props.selected ||
            isLockedElement ||
            isLockedHierarchy ||
            isDescendantOfLocked) &&
          !props.isSlot &&
          !isConditionalClauseTitle
        }
        value={isLockedElement ? 'locked' : isLockedHierarchy ? 'locked-hierarchy' : 'selectable'}
        isDescendantOfLocked={isDescendantOfLocked}
        selected={props.selected}
        iconColor={props.iconColor}
        onClick={toggleSelectable}
      />
      <VisibilityIndicator
        key={`visibility-indicator-${varSafeNavigatorEntryToKey(navigatorEntry)}`}
        shouldShow={
          !props.isSlot && (props.highlighted || props.selected || !props.isVisibleOnCanvas)
        }
        visibilityEnabled={props.isVisibleOnCanvas}
        selected={props.selected}
        iconColor={props.iconColor}
        onClick={toggleHidden}
      />
    </FlexRow>
  )
})
