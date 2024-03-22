import { Button, ContextMenu, Flex, Text } from '@radix-ui/themes'
import moment from 'moment'
import React from 'react'
import { ProjectActionsMenu } from '../components/projectActionContextMenu'
import { Spinner } from '../components/spinner'
import { useProjectsStore } from '../store'
import { sprinkles } from '../styles/sprinkles.css'
import type { Collaborator, ProjectListing } from '../types'
import { AccessLevel, asAccessLevel } from '../types'
import { useProjectEditorLink } from '../util/links'
import { when } from '../util/react-conditionals'
import { UnknownPlayerName, multiplayerInitialsFromName } from '../util/strings'
import { getOwnerName, ProjectBadge } from './projects'

export const ProjectCard = React.memo(
  ({
    project,
    isSharedWithMe,
    collaborators,
    selected,
    onSelect,
  }: {
    project: ProjectListing
    isSharedWithMe: boolean
    collaborators: Collaborator[]
    selected: boolean
    onSelect: () => void
  }) => {
    const projectEditorLink = useProjectEditorLink()

    const openProject = React.useCallback(() => {
      window.open(projectEditorLink(project.proj_id), '_blank')
    }, [project.proj_id, projectEditorLink])

    const activeOperations = useProjectsStore((store) =>
      store.operations.filter((op) => op.projectId === project.proj_id && !op.errored),
    )

    const projectTitle = React.useMemo(() => {
      const renaming = activeOperations.find((op) => op.type === 'rename')
      return renaming?.type === 'rename' ? renaming.newTitle : project.title
    }, [project, activeOperations])

    const ownerName = React.useMemo(() => {
      return getOwnerName(project.owner_id, collaborators)
    }, [collaborators, project])

    const onOpenShareDialog = useOpenShareDialog(project)

    return (
      <ContextMenu.Root>
        <ContextMenu.Trigger>
          <div
            style={{
              minHeight: 200,
              flex: 1,
              width: '100%',
              height: 'min-content',
              display: 'flex',
              flexDirection: 'column',
              gap: 5,
              filter: activeOperations.length > 0 ? 'grayscale(1)' : undefined,
            }}
          >
            <div
              style={{
                border: selected ? '2px solid #0090FF' : '2px solid transparent',
                borderRadius: 10,
                overflow: 'hidden',
                height: '100%',
                aspectRatio: 1.6,
                background: 'linear-gradient(#a1a1a130, #a1a1a115)',
                backgroundAttachment: 'local',
                backgroundRepeat: 'no-repeat',
                position: 'relative',
                filter: project.deleted === true ? 'grayscale(1)' : undefined,
              }}
              onMouseDown={onSelect}
              onDoubleClick={openProject}
            >
              {when(
                project.hasPendingRequests === true,
                <Button
                  radius='full'
                  variant='solid'
                  color='red'
                  size='1'
                  style={{
                    position: 'absolute',
                    right: 8,
                    top: 8,
                    fontWeight: 600,
                    fontSize: 10,
                    cursor: 'pointer',
                    height: 20,
                  }}
                  onClick={onOpenShareDialog}
                >
                  New Requests
                </Button>,
              )}
              {when(
                project.ProjectAccess?.access_level === AccessLevel.COLLABORATIVE,
                <div style={{ position: 'absolute', right: 8, bottom: 8, display: 'flex', gap: 2 }}>
                  {collaborators.map((collaborator) => {
                    return (
                      <div
                        key={`collaborator-${project.id}-${collaborator.id}`}
                        style={{
                          borderRadius: '100%',
                          width: 24,
                          height: 24,
                          backgroundImage: `url("${collaborator.avatar}")`,
                          backgroundSize: 'cover',
                          display: 'flex',
                          justifyContent: 'center',
                          alignItems: 'center',
                          fontSize: '.9em',
                          fontWeight: 700,
                        }}
                        title={collaborator.name ?? UnknownPlayerName}
                        className={sprinkles({
                          boxShadow: 'shadow',
                          color: 'white',
                          backgroundColor: 'primary',
                        })}
                      >
                        {when(
                          collaborator.avatar === '',
                          multiplayerInitialsFromName(collaborator.name),
                        )}
                      </div>
                    )
                  })}
                </div>,
              )}
              {when(
                activeOperations.length > 0,
                <div
                  style={{
                    position: 'absolute',
                    top: 0,
                    left: 0,
                    right: 0,
                    bottom: 0,
                    display: 'flex',
                    alignItems: 'center',
                    justifyContent: 'center',
                    pointerEvents: 'none',
                  }}
                >
                  <Spinner className={sprinkles({ backgroundColor: 'primary' })} />
                </div>,
              )}
            </div>
            <div
              style={{
                display: 'flex',
                flexDirection: 'row',
                justifyContent: 'space-between',
                alignItems: 'center',
              }}
            >
              <div
                style={{ display: 'flex', flexDirection: 'column', padding: 10, gap: 5, flex: 1 }}
              >
                <div style={{ display: 'flex', gap: 5, flex: 1 }}>
                  <Text
                    size='1'
                    style={{
                      flexGrow: 1,
                      fontWeight: 500,
                    }}
                  >
                    {projectTitle}
                  </Text>
                  <ProjectBadge
                    onOpenShareDialog={onOpenShareDialog}
                    accessLevel={
                      asAccessLevel(project.ProjectAccess?.access_level) ?? AccessLevel.PRIVATE
                    }
                  />
                </div>
                <Flex>
                  <Text size='1' style={{ opacity: 0.5, flex: 1 }}>
                    {moment(project.modified_at).fromNow()}
                  </Text>
                  {when(
                    isSharedWithMe && ownerName != null,
                    <Text size='1' style={{ opacity: 0.5 }}>
                      By {ownerName}
                    </Text>,
                  )}
                </Flex>
              </div>
            </div>
          </div>
        </ContextMenu.Trigger>
        <ProjectActionsMenu project={project} />
      </ContextMenu.Root>
    )
  },
)
