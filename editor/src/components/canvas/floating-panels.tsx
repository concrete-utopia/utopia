import React from 'react'
import { NO_OP } from '../../core/shared/utils'
import { UtopiaTheme } from '../../uuiui'
import { LeftPanelMinWidth } from '../editor/store/editor-state'
import { LeftPaneComponent } from '../navigator/left-pane'
import { CodeEditorPane, ResizableRightPane } from './design-panel-root'
import type { Menu, Pane, PanelData } from './floating-panels-state-2'

export const FloatingPanelsContainer = React.memo(() => {
  const orderedPanels = React.useMemo<Array<PanelData>>(() => {
    return [
      { name: 'code-editor', index: 0, span: 6, type: 'pane', order: 1 },
      { name: 'navigator', index: 0, span: 6, type: 'menu', order: 0 },
      { name: 'inspector', index: -1, span: 12, type: 'menu', order: 0 },
    ]
  }, [])

  return (
    <div
      data-testid='floating-panels-container'
      style={{
        position: 'absolute',
        display: 'grid',
        width: '100%',
        height: '100%',
        gridTemplateColumns: '[col] 260px [col] 260px [canvas] 1fr [col] 260px [col] 260px [end]',
        gridTemplateRows: 'repeat(12, 1fr)',
        gridAutoFlow: 'dense',
      }}
    >
      {orderedPanels.map((pane) => {
        return <FloatingPanel key={pane.name} pane={pane} />
      })}
    </div>
  )
})

interface FloatingPanelProps {
  pane: PanelData
}

export const FloatingPanel = React.memo<FloatingPanelProps>((props) => {
  const { name, index, span, order } = props.pane

  const draggablePanelComponent = (() => {
    switch (name) {
      case 'code-editor':
        return (
          <CodeEditorPane
            small={false}
            width={0}
            height={0}
            onResize={NO_OP}
            setIsResizing={NO_OP}
            resizableConfig={{
              enable: {
                right: true,
              },
            }}
          />
        )
      case 'inspector':
        return (
          <ResizableRightPane
            width={0}
            height={0}
            onResize={NO_OP}
            setIsResizing={NO_OP}
            resizableConfig={{
              snap: {
                x: [UtopiaTheme.layout.inspectorSmallWidth, UtopiaTheme.layout.inspectorLargeWidth],
              },
              enable: {
                left: true,
              },
            }}
          />
        )
      case 'navigator':
        return (
          <LeftPaneComponent
            width={0}
            height={0}
            onResize={NO_OP}
            setIsResizing={NO_OP}
            resizableConfig={{
              minWidth: LeftPanelMinWidth,
              enable: {
                right: true,
              },
            }}
          />
        )
      default:
        return null
    }
  })()

  return (
    <>
      <div
        style={{
          gridColumn: `col ${index > -1 ? index + 1 : index}`,
          gridRow: `span ${span}`,
          order: order,
          display: 'flex',
          flexDirection: 'column',
          contain: 'layout',
        }}
      >
        {draggablePanelComponent}
      </div>
    </>
  )
})
