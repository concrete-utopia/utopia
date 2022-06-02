import React from 'react'
import { usePubSubAtom, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { useColorTheme, UtopiaStyles } from '../../../uuiui'
import { CompanionIconTypeAtom, NavigatorWidthAtom } from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
import { ModeSelectButton } from '../mode-select-buttons'

export const CompanionSelector = React.memo(() => {
  const navigatorVisible = useEditorState(
    (store) => !store.editor.navigator.minimised,
    'ConversionHighlightOutline navigatorVisible',
  )
  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom, (): boolean => false)
  const colorTheme = useColorTheme()

  const [selectedIcons, updateSelectedIcons] = usePubSubAtom(CompanionIconTypeAtom)

  return (
    <div
      style={{
        paddingTop: 4,
        paddingLeft: navigatorVisible ? navigatorWidth + 4 : 4,
        display: 'flex',
      }}
    >
      <div
        style={{
          height: 29,
          display: 'flex',
          alignItems: 'center',
          paddingLeft: 4,
          paddingRight: 4,
          gap: 4,
          borderRadius: 4,
          background: colorTheme.bg0.value,
          boxShadow: UtopiaStyles.popup.boxShadow,
          cursor: 'pointer',
        }}
      >
        Flex
        <ModeSelectButton
          selected={selectedIcons.flexReorder === 1}
          title={'1'}
          onMouseDown={React.useCallback(
            () => updateSelectedIcons({ flexReorder: 1, absoluteMove: selectedIcons.absoluteMove }),
            [updateSelectedIcons, selectedIcons.absoluteMove],
          )}
        />
        <ModeSelectButton
          selected={selectedIcons.flexReorder === 2}
          title={'2'}
          onMouseDown={React.useCallback(
            () => updateSelectedIcons({ flexReorder: 2, absoluteMove: selectedIcons.absoluteMove }),
            [updateSelectedIcons, selectedIcons.absoluteMove],
          )}
        />
        | Move
        <ModeSelectButton
          selected={selectedIcons.absoluteMove === 1}
          title={'1'}
          onMouseDown={React.useCallback(
            () => updateSelectedIcons({ flexReorder: selectedIcons.flexReorder, absoluteMove: 1 }),
            [updateSelectedIcons, selectedIcons.flexReorder],
          )}
        />
        <ModeSelectButton
          selected={selectedIcons.absoluteMove === 2}
          title={'2'}
          onMouseDown={React.useCallback(
            () => updateSelectedIcons({ flexReorder: selectedIcons.flexReorder, absoluteMove: 2 }),
            [updateSelectedIcons, selectedIcons.flexReorder],
          )}
        />
      </div>
    </div>
  )
})
