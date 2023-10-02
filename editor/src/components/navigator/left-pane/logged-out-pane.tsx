/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import {
  Button,
  FlexRow,
  Icons,
  Section,
  SectionBodyArea,
  SectionTitleRow,
  Subdued,
  Title,
  colorTheme,
} from '../../../uuiui'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'
import { FlexCol } from 'utopia-api'

export const LoggedOutPane = React.memo(() => {
  return (
    <Section data-name='Storyboards' tabIndex={-1}>
      <SectionBodyArea minimised={false}>
        <UIGridRow
          padded
          variant='<-------------1fr------------->'
          style={{
            height: 'inherit',
            wordWrap: 'normal',
            whiteSpace: 'normal',
            alignItems: 'flex-start',
            minHeight: 34,
            paddingTop: 8,
            paddingLeft: 8,
            paddingRight: 8,
            paddingBottom: 8,
            letterSpacing: 0.1,
            lineHeight: '17px',
            fontSize: '11px',
          }}
        >
          <Title>Sign In To:</Title>
          <ul style={{ paddingLeft: 16 }}>
            <li>Design and code from anywhere</li>
            <li>Save and preview your projects</li>
            <li>Use custom assets, fonts, and more</li>
            <li>Load and save projects on Github</li>
          </ul>
        </UIGridRow>
        <FlexCol css={{ padding: '0 8px', gap: 8, alignItems: 'center', justifyContent: 'center' }}>
          <Button
            style={{
              background: colorTheme.dynamicBlue.value,
              color: colorTheme.bg0.value,
              width: 180,
            }}
          >
            <b>Sign In</b>&nbsp;
            <Icons.ExternalLinkSmaller color='on-light-main' />
          </Button>
          <Subdued>Free and Open Source</Subdued>
        </FlexCol>
      </SectionBodyArea>
    </Section>
  )
})
