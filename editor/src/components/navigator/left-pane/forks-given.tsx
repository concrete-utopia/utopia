/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { projectEditorURL } from '../../../common/server'
import { Avatar, Section, SectionBodyArea, Subdued, useColorTheme } from '../../../uuiui'
import { User } from '../../../uuiui-deps'
import { Link } from '../../../uuiui/link'
import { useGetProjectMetadata } from '../../common/server-hooks'
import { useEditorState } from '../../editor/store/store-hook'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'

export const ForksGiven = React.memo(() => {
  const colorTheme = useColorTheme()

  const { id, isLoggedIn, forkedFrom } = useEditorState((store) => {
    return {
      id: store.editor.id,
      isLoggedIn: User.isLoggedIn(store.userState.loginState),
      forkedFrom: store.editor.forkedFromProjectId,
    }
  }, 'ForkPanel')

  const projectOwnerMetadata = useGetProjectMetadata(id)
  const forkedFromMetadata = useGetProjectMetadata(forkedFrom)

  const forkedFromText =
    forkedFrom == null ? null : (
      <React.Fragment>
        Forked from <Link href={projectEditorURL(forkedFrom)}>{forkedFromMetadata?.title}</Link>
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
          <Subdued>{id}</Subdued>
        </UIGridRow>
      </SectionBodyArea>
    </Section>
  )
})
