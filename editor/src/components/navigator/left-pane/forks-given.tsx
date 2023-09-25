/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { projectEditorURL } from '../../../common/server'
import { Avatar, Section, SectionBodyArea, Subdued, useColorTheme } from '../../../uuiui'
import { User } from '../../../uuiui-deps'
import { Link } from '../../../uuiui/link'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'

export const ForksGiven = React.memo(() => {
  const colorTheme = useColorTheme()

  const isLoggedIn = useEditorState(
    Substores.restOfStore,
    (store) => User.isLoggedIn(store.userState.loginState),
    'ForksGiven isLoggedIn',
  )

  const { id, forkedFrom } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        id: store.editor.id,
        forkedFrom: store.editor.forkedFromProjectId,
      }
    },
    'ForkPanel',
  )

  const projectOwnerMetadata = useEditorState(
    Substores.projectServerState,
    (store) => store.projectServerState.projectData,
    'ForksGiven projectOwnerMetadata',
  )

  const forkedFromMetadata = useEditorState(
    Substores.projectServerState,
    (store) => store.projectServerState.forkedFromProjectData,
    'ForksGiven forkedFromProjectData',
  )

  const forkedFromText =
    forkedFrom == null ? null : (
      <React.Fragment>
        Forked from{' '}
        <Link href={projectEditorURL(forkedFrom)}>{forkedFromMetadata?.title ?? forkedFrom}</Link>
      </React.Fragment>
    )

  return (
    <Section data-name='Fork' tabIndex={-1}>
      <SectionBodyArea minimised={false}>
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
              boxShadow: `inset 0px 0px 0px 1px ${colorTheme.subduedForeground.value}`,
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
          <Subdued>{id}</Subdued>
        </UIGridRow>
      </SectionBodyArea>
    </Section>
  )
})
