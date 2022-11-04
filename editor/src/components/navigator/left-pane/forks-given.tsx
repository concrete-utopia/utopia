/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { auth0Url } from '../../../common/env-vars'
import { projectEditorURL } from '../../../common/server'
import {
  Avatar,
  Button,
  FlexRow,
  Icons,
  Section,
  SectionBodyArea,
  SectionTitleRow,
  Subdued,
  Title,
  useColorTheme,
} from '../../../uuiui'
import { User } from '../../../uuiui-deps'
import { Link } from '../../../uuiui/link'
import { useGetProjectMetadata } from '../../common/server-hooks'
import { useTriggerForkProject } from '../../editor/persistence-hooks'
import { useEditorState } from '../../editor/store/store-hook'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'

const ForkButton = React.memo(() => {
  const onClickOnForkProject = useTriggerForkProject()

  return (
    <Button
      primary
      highlight
      style={{
        height: 24,
        backgroundImage: 'linear-gradient(3deg, #92ABFF 0%, #1FCCB7 99%)',
        boxShadow: 'inset 0 0 0 1px rgba(94,94,94,0.20)',
        borderRadius: 2,
      }}
      onClick={onClickOnForkProject}
    >
      <b>Fork</b>&nbsp;this project
    </Button>
  )
})

export const ForksGiven = React.memo(() => {
  const colorTheme = useColorTheme()

  const { id, projectName, description, isLoggedIn, forkedFrom } = useEditorState((store) => {
    return {
      id: store.editor.id,
      projectName: store.editor.projectName,
      description: store.editor.projectDescription,
      isLoggedIn: User.isLoggedIn(store.userState.loginState),
      forkedFrom: store.editor.forkedFromProjectId,
    }
  }, 'ForkPanel')

  const projectOwnerMetadata = useGetProjectMetadata(id)
  const forkedFromMetadata = useGetProjectMetadata(forkedFrom)

  const onClickLoginNewTab = React.useCallback(() => {
    window.open(auth0Url('auto-close'), '_blank')
  }, [])

  const forkedFromText =
    forkedFrom == null ? null : (
      <React.Fragment>
        Forked from <Link href={projectEditorURL(forkedFrom)}>{forkedFromMetadata?.title}</Link>
      </React.Fragment>
    )

  return (
    <Section data-name='Fork' tabIndex={-1}>
      <SectionTitleRow minimised={false}>
        <FlexRow flexGrow={1} style={{ position: 'relative' }}>
          <Title>Project</Title>
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
          <div>
            <span
              style={{
                paddingLeft: 4,
                paddingRight: 4,
                paddingTop: 2,
                paddingBottom: 2,
                background: colorTheme.primary.value,
                color: colorTheme.neutralInvertedForeground.value,
                borderRadius: 2,
              }}
            >
              <b>{projectName}</b>&nbsp;
            </span>
            &nbsp;
            <Subdued>{id}</Subdued>
          </div>
          <p>{description}</p>
        </UIGridRow>
        <UIGridRow
          padded
          variant='|--32px--|<--------auto-------->'
          style={{ gap: 12, marginTop: 8 }}
        >
          <div
            role='avatar'
            style={{
              width: 28,
              height: 28,
              borderRadius: '50%',
              boxShadow: `inset 0px 0px 0px 1px ${colorTheme.subduedForeground.o(50).value}`,
              background: colorTheme.subtleBackground.value,
            }}
          >
            <Avatar
              isLoggedIn={isLoggedIn}
              userPicture={projectOwnerMetadata?.ownerPicture ?? null}
              size={28}
            />
          </div>

          <div style={{ whiteSpace: 'normal' }}>
            Created by <b>{projectOwnerMetadata?.ownerName ?? ''}</b>
            <br />
            {forkedFromText}
          </div>
        </UIGridRow>

        <UIGridRow style={{ gap: 8, marginTop: 8 }} padded variant='<--1fr--><--1fr-->'>
          <ForkButton />
          {isLoggedIn ? null : (
            <Button
              outline
              highlight
              style={{
                height: 24,
              }}
              onClick={onClickLoginNewTab}
            >
              <b>Sign in</b>&nbsp;to edit&nbsp;
              <Icons.ExternalLinkSmaller />
            </Button>
          )}
        </UIGridRow>
      </SectionBodyArea>
    </Section>
  )
})
