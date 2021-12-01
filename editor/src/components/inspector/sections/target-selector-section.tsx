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
            <Tooltip title='style'>
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
            <Tooltip title='css'>
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
            <Tooltip title='css:&hover'>
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
        </FlexColumn>
      </Section>
    )
  },
)
