import * as React from 'react'
import {
  colorTheme,
  FlexColumn,
  FlexRow,
  FunctionIcons,
  H1,
  Icons,
  OnClickOutsideHOC,
  SectionActionSheet,
  StringInput,
  UtopiaStyles,
  UtopiaTheme,
  SquareButton,
} from 'uuiui'
import Utils from '../../../../utils/utils'
import { ContextMenuWrapper, betterReactMemo } from 'uuiui-deps'
import { useEditorState } from '../../../editor/store/store-hook'
import { ExpandableIndicator } from '../../../navigator/navigator-item/expandable-indicator'

export type TargetSelectorLength = number | 'mixed'

export interface CSSTarget {
  path: Array<string>
  selectorLength: TargetSelectorLength
}

export function cssTarget(path: Array<string>, selectorLength: TargetSelectorLength) {
  return {
    path,
    selectorLength,
  }
}

interface TargetSelectorPanelProps {
  targets: Array<CSSTarget>
  selectedTargetPath: Array<string>
  onSelect: (targetPath: Array<string>) => void
  style?: React.CSSProperties
  onStyleSelectorRename: (renameTarget: CSSTarget, label: string) => void
  onStyleSelectorDelete: (deleteTarget: CSSTarget) => void
  onStyleSelectorInsert: (parent: CSSTarget, label: string) => void
}

export const TargetSelectorPanel = betterReactMemo(
  'TargetSelectorPanel',
  (props: TargetSelectorPanelProps) => {
    const {
      targets,
      onSelect,
      selectedTargetPath: selectedTarget,
      onStyleSelectorRename,
      onStyleSelectorDelete,
      onStyleSelectorInsert,
    } = props
    const [addingIndex, setAddingIndex] = React.useState<number | null>(null)
    const [addingIndentLevel, setAddingIndentLevel] = React.useState<number | null>(null)
    const exitAdding = React.useCallback(() => {
      setAddingIndex(null)
      setAddingIndentLevel(null)
    }, [])

    const onRenameByIndex = React.useCallback(
      (index: number, label: string) => {
        onStyleSelectorRename(targets[index], label)
      },
      [targets, onStyleSelectorRename],
    )

    const [isOpen, setIsOpen] = React.useState<boolean>(false)

    const onDeleteByIndex = React.useCallback(
      (index: number) => onStyleSelectorDelete(targets[index]),
      [targets, onStyleSelectorDelete],
    )
    const onSelectByIndex = React.useCallback((index: number) => onSelect(targets[index].path), [
      targets,
      onSelect,
    ])
    const onInsertByIndex = React.useCallback(
      (index: number, label: string) => onStyleSelectorInsert(targets[index], label),
      [targets, onStyleSelectorInsert],
    )
    const targetIndex = getCSSTargetIndex(selectedTarget, targets)

    const slicedTargetsAdding = React.useMemo(() => targets.slice(0, addingIndex ?? undefined), [
      addingIndex,
      targets,
    ])

    const slicedTargets = React.useMemo(
      () => (addingIndex != null ? targets.slice(addingIndex, targets.length) : targets),
      [targets, addingIndex],
    )

    return (
      <FlexColumn
        style={{
          position: 'relative',
          paddingTop: '8px',
          userSelect: 'none',
          WebkitUserSelect: 'none',
        }}
      >
        <TargetListHeader
          isOpen={isOpen}
          setIsOpen={setIsOpen}
          setAddingIndex={setAddingIndex}
          selectedTargetPath={selectedTarget}
          isAdding={addingIndex != null}
        />
        {/* outer flexColumn takes a fixed height (or can grow to fill space, this way addable row can
      be at the top without being included in scrollable list */}
        {isOpen ? (
          <FlexColumn
            className='label-fixedHeightList'
            style={{ height: 100, overflowY: 'scroll' }}
          >
            <FlexColumn
              className='label-scrollableList'
              style={{
                borderTop: `1px solid ${colorTheme.secondaryBorder.value}`,
                borderBottom: `1px solid ${colorTheme.secondaryBorder.value}`,
                paddingTop: 5,
                paddingBottom: 8,
                backgroundColor: colorTheme.emphasizedBackground.value,
                flexGrow: 1,
                overflowY: 'scroll',
              }}
            >
              {addingIndex != null ? (
                <React.Fragment>
                  <TargetList
                    targets={slicedTargetsAdding}
                    selectionOffset={0}
                    selectedItemIndex={targetIndex}
                    setAddingIndex={setAddingIndex}
                    setAddingIndentLevel={setAddingIndentLevel}
                    onSelect={onSelectByIndex}
                    onRenameByIndex={onRenameByIndex}
                    onDeleteByIndex={onDeleteByIndex}
                  />
                  <AddingRow
                    onInsert={onInsertByIndex}
                    addingIndex={addingIndex}
                    addingIndentLevel={addingIndentLevel}
                    finishAdding={exitAdding}
                  />
                </React.Fragment>
              ) : null}
              <TargetList
                targets={slicedTargets}
                selectionOffset={addingIndex != null ? addingIndex : 0}
                selectedItemIndex={targetIndex}
                setAddingIndex={setAddingIndex}
                setAddingIndentLevel={setAddingIndentLevel}
                onSelect={onSelectByIndex}
                onRenameByIndex={onRenameByIndex}
                onDeleteByIndex={onDeleteByIndex}
              />
            </FlexColumn>
          </FlexColumn>
        ) : null}
      </FlexColumn>
    )
  },
)

interface TargetListProps {
  targets: Array<CSSTarget>
  selectionOffset: number
  selectedItemIndex: number
  setAddingIndex: React.Dispatch<React.SetStateAction<number | null>>
  setAddingIndentLevel: React.Dispatch<React.SetStateAction<number | null>>
  onSelect: (index: number) => void
  onRenameByIndex: (index: number, label: string) => void
  onDeleteByIndex: (index: number) => void
}

const TargetList = betterReactMemo('TargetList', (props: TargetListProps) => {
  const {
    targets,
    selectionOffset,
    selectedItemIndex,
    setAddingIndex,
    setAddingIndentLevel,
    onSelect,
    onRenameByIndex,
    onDeleteByIndex,
  } = props

  return (
    <React.Fragment>
      {targets.map((target: CSSTarget, itemIndex: number) => (
        <TargetListItem
          id={`target-list-item-${itemIndex}`}
          key={`target-list-item-${itemIndex}`}
          itemIndex={itemIndex}
          target={target}
          selectionOffset={selectionOffset}
          selectedItemIndex={selectedItemIndex}
          onSelect={onSelect}
          setAddingIndex={setAddingIndex}
          setAddingIndentLevel={setAddingIndentLevel}
          onRenameByIndex={onRenameByIndex}
          onDeleteByIndex={onDeleteByIndex}
        />
      ))}
    </React.Fragment>
  )
})

interface TargetListItemProps {
  itemIndex: number
  target: CSSTarget
  selectionOffset: number
  selectedItemIndex: number
  onSelect: (index: number) => void
  setAddingIndex: React.Dispatch<React.SetStateAction<number | null>>
  setAddingIndentLevel: React.Dispatch<React.SetStateAction<number | null>>
  id: string
  onRenameByIndex: (index: number, label: string) => void
  onDeleteByIndex: (index: number) => void
}
TargetList.displayName = 'TargetList'

const TargetListItem = betterReactMemo('TargetListItem', (props: TargetListItemProps) => {
  const {
    itemIndex,
    target,
    selectionOffset,
    selectedItemIndex,
    onSelect,
    setAddingIndex,
    setAddingIndentLevel,
    id,
    onRenameByIndex,
    onDeleteByIndex,
  } = props
  const fixedItemIndex = itemIndex + selectionOffset
  const itemLabel = getCSSTargetLabel(target)

  const [itemBeingRenamedId, setItemBeingRenamedId] = React.useState<number | null>(null)
  const [renameValue, setRenameValue] = React.useState<string | null>(null)

  const { dispatch } = useEditorState(
    (store) => ({
      dispatch: store.dispatch,
    }),
    'TargetListItem',
  )

  const startRename = React.useCallback(() => {
    setItemBeingRenamedId(fixedItemIndex)
    setRenameValue(itemLabel)
  }, [fixedItemIndex, itemLabel, setItemBeingRenamedId, setRenameValue])

  const clearRenameState = React.useCallback(() => {
    setItemBeingRenamedId(null)
    setRenameValue(null)
  }, [setItemBeingRenamedId, setRenameValue])

  const onRenameKeydown = React.useCallback(
    (event: React.KeyboardEvent<HTMLInputElement>) => {
      if (event.key === 'Enter' && itemBeingRenamedId != null && renameValue != null) {
        onRenameByIndex(itemBeingRenamedId, renameValue)
        clearRenameState()
      } else if (event.key === 'Escape') {
        clearRenameState()
      }
    },
    [clearRenameState, itemBeingRenamedId, renameValue, onRenameByIndex],
  )

  const onRenameChange = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      setRenameValue(event.target.value)
    },
    [setRenameValue],
  )

  const isSelected = fixedItemIndex === selectedItemIndex

  const onSelectByMouseCallback = React.useCallback(() => {
    onSelect(fixedItemIndex)
  }, [fixedItemIndex, onSelect])
  const onSelectByEnterCallback = React.useCallback(
    (e) => {
      if (e.buttons === 1) {
        onSelect(fixedItemIndex)
      }
    },
    [fixedItemIndex, onSelect],
  )
  const setAddingStateCallback = React.useCallback(() => {
    setAddingIndex(fixedItemIndex + 1)
    setAddingIndentLevel(target.path.length)
  }, [fixedItemIndex, setAddingIndex, setAddingIndentLevel, target.path.length])

  const deleteCurrentItem = React.useCallback(() => {
    onDeleteByIndex(itemIndex)
  }, [onDeleteByIndex, itemIndex])

  return (
    <ContextMenuWrapper
      id={`${id}-contextMenu`}
      items={[
        {
          name: 'Add nested selector',
          enabled: true,
          action: () => {
            setAddingStateCallback()
          },
        },
        {
          name: 'Rename',
          enabled: true,
          action: () => {
            startRename()
          },
        },
        {
          name: 'Delete',
          enabled: true,
          action: deleteCurrentItem,
        },
      ]}
      data={null}
      dispatch={dispatch}
    >
      <FlexRow
        tabIndex={0}
        style={{
          flexShrink: 0,
          position: 'relative',
          marginLeft: 5,
          marginRight: 5,
          marginTop: 1,
          marginBottom: 1,
          paddingLeft: 8 + 10 * (target.path.length - 1),
          paddingRight: 12,
          borderRadius: UtopiaTheme.inputBorderRadius,
          height: UtopiaTheme.layout.rowHeight.medium - 2,
          fontWeight: isSelected ? 600 : 400,
          fontStyle: target.selectorLength > 0 ? undefined : 'italic',
          backgroundImage: isSelected ? UtopiaStyles.backgrounds.blue : undefined,
          color: isSelected
            ? UtopiaTheme.color.white.value
            : colorTheme.neutralForeground.o(target.selectorLength > 0 ? 100 : 40).value,
        }}
        onDoubleClick={startRename}
        onMouseDown={onSelectByMouseCallback}
        onMouseEnter={onSelectByEnterCallback}
      >
        {fixedItemIndex === itemBeingRenamedId ? (
          <StringInput
            className='w100pct'
            onKeyDown={onRenameKeydown}
            onChange={onRenameChange}
            onBlurCapture={clearRenameState}
            autoFocus
            placeholder={itemLabel}
            defaultValue={renameValue !== null ? renameValue : undefined}
          />
        ) : (
          <React.Fragment>
            <div style={{ flexGrow: 1 }}>{itemLabel}</div>
            <div>{target.selectorLength === 0 ? null : target.selectorLength}</div>
          </React.Fragment>
        )}
      </FlexRow>
    </ContextMenuWrapper>
  )
})

interface TargetListHeaderProps {
  isOpen: boolean
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>
  setAddingIndex: React.Dispatch<React.SetStateAction<number | null>>
  selectedTargetPath: Array<string>
  isAdding: boolean
}

const TargetListHeader = betterReactMemo('TargetListHeader', (props: TargetListHeaderProps) => {
  const { isOpen, setIsOpen, setAddingIndex, selectedTargetPath: selectedItem, isAdding } = props

  const startAdding = React.useCallback(() => {
    setIsOpen(true)
    setAddingIndex(0)
  }, [setIsOpen, setAddingIndex])

  const togglePathPanel = React.useCallback(() => setIsOpen((value) => !value), [setIsOpen])

  const titleStyle = selectedItem[0] === 'style' ? undefined : { color: colorTheme.primary.value }

  return (
    <FlexRow
      style={{
        paddingLeft: 8,
        paddingRight: 8,
        height: UtopiaTheme.layout.rowHeight.large,
        backgroundColor: colorTheme.slightlyEmphasizedBackground.value,
        cursor: 'pointer',
      }}
      css={{
        '&:hover': {
          backgroundColor: colorTheme.emphasizedBackground.value,
        },
      }}
    >
      <H1 style={{ flexGrow: 1, display: 'inline', overflow: 'hidden', ...titleStyle }}>
        {selectedItem}
      </H1>
      <SectionActionSheet className='actionsheet'>
        <SquareButton highlight disabled={isAdding} onClick={startAdding}>
          <FunctionIcons.Add />
        </SquareButton>
        <SquareButton highlight onClick={togglePathPanel}>
          <ExpandableIndicator visible collapsed={!isOpen} selected={false} />
        </SquareButton>
      </SectionActionSheet>
    </FlexRow>
  )
})

interface AddingRowProps {
  onInsert: (index: number, label: string) => void
  addingIndex: number
  addingIndentLevel: number | null
  finishAdding: () => void
}

const AddingRow = betterReactMemo('AddingRow', (props: AddingRowProps) => {
  const { addingIndex, finishAdding, addingIndentLevel, onInsert } = props
  const [value, setValue] = React.useState<string>('')

  const addItem = React.useCallback(() => {
    if (addingIndex != null) {
      onInsert(addingIndex - 1, value)
    }
    finishAdding()
  }, [addingIndex, finishAdding, value, onInsert])

  const onAddKeydown = React.useCallback(
    (e: React.KeyboardEvent<HTMLInputElement>) => {
      if (e.key === 'Enter') {
        addItem()
      } else if (e.key === 'Escape') {
        finishAdding()
      }
    },
    [addItem, finishAdding],
  )

  const onAddChange = React.useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    setValue(e.target.value)
  }, [])

  return (
    <FlexRow
      style={{
        height: UtopiaTheme.layout.rowHeight.medium,
        minHeight: UtopiaTheme.layout.rowHeight.medium,
        marginRight: 8,
        marginBottom: 4,
        marginLeft: 8 + 10 * (addingIndentLevel ?? 0),
      }}
    >
      <OnClickOutsideHOC onClickOutside={finishAdding}>
        <StringInput
          style={{ flexGrow: 1 }}
          autoFocus
          onKeyDown={onAddKeydown}
          onChange={onAddChange}
          value={value}
        />
      </OnClickOutsideHOC>
    </FlexRow>
  )
})

function getCSSTargetLabel(target: CSSTarget): string {
  if (target.path.length > 0) {
    return target.path[target.path.length - 1]
  }
  // TODO Parsing safety: If a user messes up the CSSTarget metadata, they
  // are going to crash the app, which is definitely not a good experience
  throw new Error('CSSTarget can not be empty')
}

export function getCSSTargetIndex(targetPath: Array<string>, allTargets: Array<CSSTarget>) {
  return allTargets.findIndex((t) => Utils.shallowEqual(t.path, targetPath))
}
