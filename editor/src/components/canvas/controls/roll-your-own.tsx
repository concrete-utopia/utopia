import { useAtom } from 'jotai'
import React from 'react'
import {
  defaultExperimentalGridFeatures,
  experimentalGridFeatures,
  gridFeaturesExplained,
} from './grid-controls'
import { setFeatureEnabled } from '../../../utils/feature-switches'
import { Button, FlexColumn, FlexRow, Section, useColorTheme } from '../../../uuiui'
import { when } from '../../../utils/react-conditionals'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'

const sections = ['Grid'] as const
type Section = (typeof sections)[number]

export const RollYourOwnPane = React.memo(() => {
  const [currentSection, setCurrentSection] = React.useState<Section | null>(null)

  const onChangeSection = React.useCallback((e: React.ChangeEvent<HTMLSelectElement>) => {
    const maybeSectionValue = e.target.value as Section
    if (sections.includes(maybeSectionValue)) {
      setCurrentSection(maybeSectionValue)
    } else {
      setCurrentSection(null)
    }
  }, [])

  return (
    <FlexColumn
      id='leftPaneRollYourOwn'
      key='leftPaneRollYourOwn'
      style={{
        display: 'relative',
        alignItems: 'stretch',
        paddingBottom: 50,
        overflowY: 'scroll',
        alignSelf: 'stretch',
      }}
    >
      <Section>
        <FlexRow
          style={{
            margin: 8,
            gap: 12,
            height: 22,
          }}
        >
          <span style={{ fontWeight: 600 }}>Roll Your Own</span>
        </FlexRow>
        <FlexRow style={{ gap: 12, margin: 8 }}>
          <select
            value={currentSection ?? undefined}
            style={{ flex: 1 }}
            onChange={onChangeSection}
          >
            <option value=''>–</option>
            {sections.map((section) => {
              return (
                <option key={`section-${section}`} value={section}>
                  {section}
                </option>
              )
            })}
          </select>
        </FlexRow>

        {when(currentSection === 'Grid', <GridSection />)}
      </Section>
    </FlexColumn>
  )
})
RollYourOwnPane.displayName = 'RollYourOwnPane'

const GridSection = React.memo(() => {
  const [gridFeatures, setGridFeatures] = useAtom(experimentalGridFeatures)

  const onClickResetDefaults = React.useCallback(() => {
    for (const [feat, value] of Object.entries(gridFeatures)) {
      if (typeof value === 'boolean') {
        setFeatureEnabled(`Grid move - ${feat}` as any, value)
      }
    }
    setGridFeatures(defaultExperimentalGridFeatures)
  }, [gridFeatures, setGridFeatures])

  const colorTheme = useColorTheme()

  return (
    <FlexColumn style={{ gap: 10 }}>
      <UIGridRow padded variant='<-------------1fr------------->'>
        <Button spotlight highlight onClick={onClickResetDefaults}>
          Reset defaults
        </Button>
      </UIGridRow>
      {Object.entries(gridFeatures).map(([feat, value]) => {
        const isDefault = (defaultExperimentalGridFeatures as any)[feat] === true
        return (
          <UIGridRow padded key={feat} variant='<--1fr--><--1fr-->'>
            <FlexColumn>
              <div
                style={{
                  fontWeight: isDefault ? 'bolder' : 'normal',
                }}
              >
                {feat} {isDefault ? '(default)' : ''}
              </div>
              <div
                style={{
                  color: colorTheme.fg7.value,
                  fontSize: 9,
                  whiteSpace: 'normal',
                }}
              >
                {gridFeaturesExplained[feat]}
              </div>
            </FlexColumn>
            <FlexRow style={{ justifyContent: 'flex-end' }}>
              {typeof value === 'boolean' && (
                <input
                  type='checkbox'
                  checked={value}
                  onChange={(e) => {
                    e.stopPropagation()
                    setGridFeatures({ ...gridFeatures, [feat]: e.target.checked })
                    setFeatureEnabled(`Grid move - ${feat}` as any, e.target.checked) // terrible hacks on top of terrible hacks
                  }}
                />
              )}
              {typeof value === 'string' && (
                <input
                  type='text'
                  style={{ width: 100 }}
                  value={value}
                  onChange={(e) => {
                    e.stopPropagation()
                    setGridFeatures({ ...gridFeatures, [feat]: e.target.value })
                  }}
                />
              )}
              {typeof value === 'number' && (
                <input
                  type='number'
                  style={{ width: 100 }}
                  value={value}
                  onChange={(e) => {
                    e.stopPropagation()
                    setGridFeatures({
                      ...gridFeatures,
                      [feat]: parseFloat(e.target.value),
                    })
                  }}
                />
              )}
            </FlexRow>
          </UIGridRow>
        )
      })}
    </FlexColumn>
  )
})
GridSection.displayName = 'GridSection'
