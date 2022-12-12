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
} from '../../../uuiui'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'

export const LoggedOutPane = React.memo(() => {
  return (
    <Section data-name='Storyboards' tabIndex={-1}>
      <SectionTitleRow minimised={false} hideButton>
        <FlexRow flexGrow={1} style={{ position: 'relative' }}>
          <Title>Sign in to</Title>
        </FlexRow>
      </SectionTitleRow>
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
          <ul style={{ paddingLeft: 16 }}>
            <li>Design and code from anywhere</li>
            <li>Save and preview your projects</li>
            <li>Use custom assets, fonts, and more</li>
            <li>Load and save projects on Github</li>
          </ul>
        </UIGridRow>
        <UIGridRow style={{ gap: 8 }} padded variant='<--1fr--><--1fr-->'>
          <Button primary highlight>
            <b>Sign In</b>&nbsp;
            <Icons.ExternalLinkSmaller color='on-highlight-main' />
          </Button>
          <Subdued>Free and Open Source</Subdued>
        </UIGridRow>
      </SectionBodyArea>
    </Section>
  )
})
