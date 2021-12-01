import React from 'react'
import {
  Section,
  FlexColumn,
  InspectorSubsectionHeader,
  FlexRow,
  Tooltip,
  UtopiaTheme,
} from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import { TargetSelectorPanel, CSSTarget } from './header-section/target-selector'

export interface TargetSelectorSectionProps {
  targets: Array<CSSTarget>
  selectedTargetPath: Array<string>
  onSelectTarget: (targetPath: Array<string>) => void
  style?: React.CSSProperties
  className?: string
  onStyleSelectorRename: (renameTarget: CSSTarget, label: string) => void
  onStyleSelectorDelete: (deleteTarget: CSSTarget) => void
  onStyleSelectorInsert: (parent: CSSTarget, label: string) => void
}

export const TargetSelectorSection = betterReactMemo(
  'TargetSelectorSection',
  (props: TargetSelectorSectionProps) => {
    return (
      <Section className={props.className}>
        <FlexColumn className='titledSectionContentColumn'>
          {/* <TargetSelectorPanel
            targets={props.targets}
            selectedTargetPath={props.selectedTargetPath}
            onSelect={props.onSelectTarget}
            onStyleSelectorRename={props.onStyleSelectorRename}
            onStyleSelectorDelete={props.onStyleSelectorDelete}
            onStyleSelectorInsert={props.onStyleSelectorInsert}
          /> */}
          <InspectorSubsectionHeader>
            <div style={{ flexGrow: 1 }}>Styling</div>
            <div>{props.selectedTargetPath}</div>
          </InspectorSubsectionHeader>
          <FlexRow style={{ height: UtopiaTheme.layout.rowHeight.normal, gap: 8, paddingLeft: 8 }}>
            <Tooltip title='Changes effect inline style properties'>
              <div
                onClick={() => props.onSelectTarget(['style'])}
                style={{
                  backgroundColor: 'hsl(0,0%,94%)',
                  padding: '2px 4px',
                  textAlign: 'center',
                  outline: props.selectedTargetPath.includes('style')
                    ? '1px solid #007AFF'
                    : 'none',
                  borderRadius: 4,
                }}
              >
                🦥 Style
              </div>
            </Tooltip>
            <Tooltip title='Editing emotion CSS properties'>
              <div
                onClick={() => props.onSelectTarget(['css'])}
                style={{
                  backgroundColor: 'hsl(0,0%,94%)',
                  padding: '2px 4px',
                  textAlign: 'center',
                  outline: props.selectedTargetPath.includes('css') ? '1px solid #007AFF' : 'none',
                  borderRadius: 4,
                }}
              >
                🦚 CSS
              </div>
            </Tooltip>
            <Tooltip title='Changes below add css:&hover effects'>
              <div
                onClick={() => props.onSelectTarget(['css:&hover'])}
                style={{
                  backgroundColor: 'hsl(0,0%,94%)',
                  padding: '2px 4px',
                  textAlign: 'center',
                  outline: props.selectedTargetPath.includes('css:&hover')
                    ? '1px solid #007AFF'
                    : 'none',
                  borderRadius: 4,
                }}
              >
                🐹 CSS:&hover
              </div>
            </Tooltip>
          </FlexRow>
          <div style={{ borderBottom: '1px solid hsl(0,0%,90%)', width: '100%' }}></div>
          <div
            style={{
              borderTop: '1px solid hsl(0,0%,90%)',
              borderLeft: '1px solid hsl(0,0%,90%)',
              transform: 'rotate(45deg)',
              width: 5,
              height: 5,
              position: 'relative',
              top: -4,
              background: 'white',
              left: props.selectedTargetPath.includes('style')
                ? 30
                : props.selectedTargetPath.includes('css')
                ? 86
                : 160,
            }}
          ></div>
        </FlexColumn>
      </Section>
    )
  },
)
