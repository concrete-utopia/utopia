import React from 'react'
import { Section, FlexColumn } from '../../../uuiui'
import type { CSSTarget } from './header-section/target-selector'
import { TargetSelectorPanel } from './header-section/target-selector'

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

export const TargetSelectorSection = React.memo((props: TargetSelectorSectionProps) => {
  return (
    <Section className={props.className}>
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
})
