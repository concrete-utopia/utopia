import React from 'react'
import { FlexColumn, FlexRow, Section } from '../../../uuiui'
import { when } from '../../../utils/react-conditionals'

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
  return <FlexColumn style={{ gap: 10 }}>{/* TODO */}</FlexColumn>
})
GridSection.displayName = 'GridSection'
