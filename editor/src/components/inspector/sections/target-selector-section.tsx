import * as React from 'react'
import { colorTheme, Section, FlexColumn } from '../../../uuiui'
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

const HeaderTitledSectionStyle = { backgroundColor: colorTheme.inspectorEmphasizedBackground.value }

export const TargetSelectorSection = betterReactMemo(
  'TargetSelectorSection',
  (props: TargetSelectorSectionProps) => {
    return (
      <Section style={HeaderTitledSectionStyle} className={props.className}>
        <FlexColumn className='titledSectionContentColumn'>
          <TargetSelectorPanel
            targets={props.targets}
            selectedTargetPath={props.selectedTargetPath}
            onSelect={props.onSelectTarget}
            onStyleSelectorRename={props.onStyleSelectorRename}
            onStyleSelectorDelete={props.onStyleSelectorDelete}
            onStyleSelectorInsert={props.onStyleSelectorInsert}
          />
        </FlexColumn>
      </Section>
    )
  },
)
