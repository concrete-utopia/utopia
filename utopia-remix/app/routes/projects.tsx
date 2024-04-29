import {
  ArchiveIcon,
  ArrowDownIcon,
  ArrowUpIcon,
  AvatarIcon,
  CubeIcon,
  DashboardIcon,
  GitHubLogoIcon,
  GlobeIcon,
  HamburgerMenuIcon,
  LockClosedIcon,
  MagnifyingGlassIcon,
  PersonIcon,
} from '@radix-ui/react-icons'
import { Badge, Button, ContextMenu, DropdownMenu, Flex, Text, TextField } from '@radix-ui/themes'
import { json, type LoaderFunctionArgs } from '@remix-run/node'
import { useFetcher, useLoaderData } from '@remix-run/react'
import moment from 'moment'
import type { UserDetails } from 'prisma-client'
import React, { useCallback } from 'react'
import { ProjectActionsMenu } from '../components/projectActionContextMenu'
import { SharingDialogWrapper } from '../components/sharingDialog'
import { SortingContextMenu } from '../components/sortProjectsContextMenu'
import { Spinner } from '../components/spinner'
import { useCleanupOperations } from '../hooks/useCleanupOperations'
import { useIsDarkMode } from '../hooks/useIsDarkMode'
import { useOpenShareDialog } from '../hooks/useOpenShareDialog'
import {
  listDeletedProjects,
  listProjects,
  listSharedWithMeProjectsAndCollaborators,
} from '../models/project.server'
import { getCollaborators } from '../models/projectCollaborators.server'
import { button } from '../styles/button.css'
import { projectCards, projectRows, projectTemplate } from '../styles/projects.css'
import { projectCategoryButton, userName } from '../styles/sidebarComponents.css'
import { sprinkles } from '../styles/sprinkles.css'
import type { Collaborator, CollaboratorsByProject, Operation, ProjectListing } from '../types'
import { AccessLevel, asAccessLevel, getOperationDescription } from '../types'
import { requireUser } from '../util/api.server'
import { assertNever } from '../util/assertNever'
import { auth0LoginURL } from '../util/auth0.server'
import { useProjectEditorLink } from '../util/links'
import { unless, when } from '../util/react-conditionals'
import { UnknownPlayerName, multiplayerInitialsFromName } from '../util/strings'
import {
  useProjectIsOnActiveOperation,
  useProjectMatchesQuery,
  useSortCompareProject,
} from '../util/use-sort-compare-project'
import { githubRepositoryPrettyName } from '../util/github'
import type { OperationWithKey } from '../stores/projectsStore'
import { createProjectsStore, ProjectsContext, useProjectsStore } from '../stores/projectsStore'
import { UserAvatar } from '../components/userAvatar'
import { UserContextMenu } from '../components/user-context-menu'

const SortOptions = ['title', 'dateCreated', 'dateModified'] as const
export type SortCriteria = (typeof SortOptions)[number]

const Categories = [
  'allProjects',
  'archive',
  'private',
  'public',
  'sharing',
  'sharedWithMe',
] as const

function isCategory(category: unknown): category is Category {
  return Categories.includes(category as Category)
}

export type Category = (typeof Categories)[number]

const categories: {
  [key in Category]: { name: string; icon: React.ReactNode; description: string }
} = {
  allProjects: {
    name: 'All My Projects',
    icon: <CubeIcon width='16' height='16' />,
    description: 'Projects you create or open will show up here.',
  },
  private: {
    name: 'Private',
    icon: <LockClosedIcon width='16' height='16' />,
    description: 'Projects you create that are private to you will show up here.',
  },
  sharing: {
    name: 'Sharing',
    icon: <PersonIcon width='16' height='16' />,
    description: 'Projects that you share with other collaborators will show up here.',
  },
  sharedWithMe: {
    name: 'Shared With Me',
    icon: <AvatarIcon width='16' height='16' />,
    description: 'Projects that you have been added to as a collaborator will show up here.',
  },
  public: {
    name: 'Public',
    icon: <GlobeIcon width='16' height='16' />,
    description: 'Public projects you own will show up here.',
  },
  archive: {
    name: 'Archive',
    icon: <ArchiveIcon width='16' height='16' />,
    description: 'Archived projects are kept here until you delete them for good.',
  },
}

export async function loader(args: LoaderFunctionArgs) {
  const user = await requireUser(args.request, { redirect: auth0LoginURL() })

  const [projects, deletedProjects, sharedWithMe] = await Promise.all([
    listProjects({ ownerId: user.user_id }),
    listDeletedProjects({ ownerId: user.user_id }),
    listSharedWithMeProjectsAndCollaborators({ userId: user.user_id }),
  ])
  const collaborators = await getCollaborators({
    ids: [...projects, ...deletedProjects].map((p) => p.proj_id),
    userId: user.user_id,
  })

  return json(
    {
      user: user,
      projects: projects,
      deletedProjects: deletedProjects,
      projectsSharedWithMe: sharedWithMe.projects,
      collaborators: { ...collaborators, ...sharedWithMe.collaborators },
    },
    { headers: { 'cache-control': 'no-cache' } },
  )
}

type LoaderData = {
  user: UserDetails
  projects: ProjectListing[]
  deletedProjects: ProjectListing[]
  projectsSharedWithMe: ProjectListing[]
  collaborators: CollaboratorsByProject
}

const ProjectsPage = React.memo(() => {
  const data = useLoaderData() as unknown as LoaderData

  const store = React.useRef(createProjectsStore({ myUser: data.user })).current

  return (
    <ProjectsContext.Provider value={store}>
      <ProjectsPageInner />
    </ProjectsContext.Provider>
  )
})
ProjectsPage.displayName = 'ProjectsPage'

export default ProjectsPage

const ProjectsPageInner = React.memo(() => {
  useCleanupOperations()

  const data = useLoaderData() as unknown as LoaderData
  const searchQuery = useProjectsStore((store) => store.searchQuery)
  const selectedCategory = useProjectsStore((store) => store.selectedCategory)

  const activeProjects = React.useMemo(() => {
    switch (selectedCategory) {
      case 'allProjects':
        return data.projects
      case 'public':
        return data.projects.filter((p) => p.ProjectAccess?.access_level === AccessLevel.PUBLIC)
      case 'private':
        return data.projects.filter(
          (p) => (p.ProjectAccess?.access_level ?? AccessLevel.PRIVATE) === AccessLevel.PRIVATE,
        )
      case 'sharing':
        return data.projects.filter(
          (p) => p.ProjectAccess?.access_level === AccessLevel.COLLABORATIVE,
        )
      case 'archive':
        return data.deletedProjects
      case 'sharedWithMe':
        return data.projectsSharedWithMe
      default:
        assertNever(selectedCategory)
    }
  }, [data.projects, data.deletedProjects, data.projectsSharedWithMe, selectedCategory])

  const sortCompareProject = useSortCompareProject()
  const projectMatchesQuery = useProjectMatchesQuery()
  const projectIsOnActiveOperation = useProjectIsOnActiveOperation()

  const isAllProjects = selectedCategory === 'allProjects'

  const filteredProjects = React.useMemo(
    () =>
      activeProjects
        // filter out projects that are part of active operations
        .filter(projectIsOnActiveOperation)
        // filter out projects that don't match the search query
        .filter(projectMatchesQuery)
        // sort them out according to the selected strategy
        .sort(sortCompareProject),

    [activeProjects, projectMatchesQuery, sortCompareProject, projectIsOnActiveOperation],
  )

  const sharingProjectId = useProjectsStore((store) => store.sharingProjectId)
  const sharingProject = React.useMemo(() => {
    return activeProjects.find((p) => p.proj_id === sharingProjectId) ?? null
  }, [activeProjects, sharingProjectId])

  const setSelectedProjectId = useProjectsStore((store) => store.setSelectedProjectId)

  const containerRef = React.useRef<HTMLDivElement | null>(null)

  const onContainerMouseDown = React.useCallback(
    (e: React.MouseEvent) => {
      const target = e.target as HTMLElement
      // check if target is inside the projects' container
      // since this handler fires also on context menu clicks
      if (
        containerRef.current != null &&
        containerRef.current.contains(target) &&
        target.closest('[data-role="project"]') == null
      ) {
        setSelectedProjectId(null)
      }
    },
    [setSelectedProjectId],
  )

  return (
    <div
      style={{
        height: '100vh',
        width: '100vw',
        gap: 45,
        overflow: 'hidden',
        boxSizing: 'border-box',
        display: 'flex',
        flexDirection: 'row',
        userSelect: 'none',
      }}
      ref={containerRef}
      onMouseDown={onContainerMouseDown}
    >
      <Sidebar user={data.user} />
      <div
        className='projects-container'
        style={{
          display: 'flex',
          flexGrow: 1,
          flexDirection: 'column',
          gap: 30,
          margin: '30px 30px 0px 0',
          overflowY: isAllProjects ? 'auto' : 'hidden',
          scrollbarColor: '#a1a1a130 transparent',
        }}
      >
        <TopActionBar />
        {when(isAllProjects && searchQuery == '', <ProjectTemplates />)}
        <ProjectsHeader projects={filteredProjects} />
        <Projects projects={filteredProjects} collaborators={data.collaborators} />
        <ActiveOperations projects={activeProjects} />
        <SharingDialogWrapper project={sharingProject} />
      </div>
    </div>
  )
})
ProjectsPageInner.displayName = 'ProjectsPageInner'

const Sidebar = React.memo(({ user }: { user: UserDetails }) => {
  const searchQuery = useProjectsStore((store) => store.searchQuery)
  const setSearchQuery = useProjectsStore((store) => store.setSearchQuery)
  const selectedCategory = useProjectsStore((store) => store.selectedCategory)
  const setSelectedCategory = useProjectsStore((store) => store.setSelectedCategory)
  const setSelectedProjectId = useProjectsStore((store) => store.setSelectedProjectId)
  const setSharingProjectId = useProjectsStore((store) => store.setSharingProjectId)

  const isDarkMode = useIsDarkMode()

  const logoPic = React.useMemo(() => {
    return isDarkMode ? 'url(/assets/pyramid_dark.png)' : 'url(/assets/pyramid_light.png)'
  }, [isDarkMode])

  const handleSelectCategory = React.useCallback(
    (category: string) => () => {
      if (isCategory(category)) {
        setSelectedCategory(category)
        setSearchQuery('')
        setSelectedProjectId(null)
        setSharingProjectId(null)
      }
    },
    [setSelectedCategory, setSearchQuery, setSelectedProjectId, setSharingProjectId],
  )

  const onChangeSearchQuery = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      setSearchQuery(e.target.value)
    },
    [setSearchQuery],
  )

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        width: 230,
        flexShrink: 0,
        justifyContent: 'space-between',
        margin: '30px 0 30px 30px',
      }}
    >
      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          gap: 30,
        }}
      >
        <DropdownMenu.Root>
          <DropdownMenu.Trigger>
            <div
              style={{
                display: 'flex',
                alignItems: 'center',
                gap: 10,
                padding: '0 6px',
              }}
            >
              <UserAvatar
                picture={user.picture}
                size={32}
                name={user.name ?? user.email ?? user.user_id}
                className={sprinkles({ borderRadius: 'medium' })}
              />
              <div className={userName({})}>{user.name}</div>
            </div>
          </DropdownMenu.Trigger>
          <UserContextMenu />
        </DropdownMenu.Root>
        <TextField.Root>
          <TextField.Slot>
            <MagnifyingGlassIcon height='16' width='16' style={{ paddingLeft: 8 }} />
          </TextField.Slot>
          <TextField.Input
            radius='full'
            placeholder='Search…'
            id='search-input'
            autoFocus={true}
            value={searchQuery}
            onChange={onChangeSearchQuery}
            style={{ fontSize: 12 }}
          />
        </TextField.Root>
        <Flex direction='column' gap='2'>
          {Object.entries(categories).map(([category, data]) => {
            return (
              <button
                key={`category-${category}`}
                className={projectCategoryButton({
                  color:
                    category === selectedCategory && searchQuery === '' ? 'selected' : 'neutral',
                })}
                onClick={handleSelectCategory(category)}
              >
                {data.icon}
                <Text size='1'>{data.name}</Text>
              </button>
            )
          })}
        </Flex>
      </div>
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          alignItems: 'center',
          gap: 10,
          fontFamily: 'Reckless',
          fontSize: 22,
          padding: '0 6px',
        }}
      >
        <div
          style={{
            height: 40,
            width: 30,
            backgroundSize: '30px',
            backgroundRepeat: 'no-repeat',
            backgroundImage: logoPic,
          }}
        />
        Utopia
      </div>
    </div>
  )
})
Sidebar.displayName = 'Sidebar'

const TopActionBar = React.memo(() => {
  const projectEditorLink = useProjectEditorLink()
  const newProjectButtons = React.useMemo(() => {
    return [
      {
        id: 'createProject',
        title: '+ New Project',
        onClick: () => window.open(projectEditorLink(null), '_blank'),
        color: 'blue',
      },
      {
        id: 'createPublicProject',
        title: '+ New Public Project',
        onClick: () => window.open(`${projectEditorLink(null)}?accessLevel=public`, '_blank'),
        color: 'green',
      },
    ] as const
  }, [projectEditorLink])

  return (
    <div
      style={{
        height: 60,
        flex: 0,
        display: 'flex',
        justifyContent: 'flex-end',
        flexDirection: 'row',
        gap: 15,
      }}
    >
      {newProjectButtons.map((p) => (
        <Button key={p.id} color={p.color} onClick={p.onClick}>
          {p.title}
        </Button>
      ))}
    </div>
  )
})
TopActionBar.displayName = 'TopActionBar'

const ProjectsHeader = React.memo(({ projects }: { projects: ProjectListing[] }) => {
  const searchQuery = useProjectsStore((store) => store.searchQuery)
  const setSearchQuery = useProjectsStore((store) => store.setSearchQuery)
  const selectedCategory = useProjectsStore((store) => store.selectedCategory)

  const sortCriteria = useProjectsStore((store) => store.sortCriteria)
  const sortAscending = useProjectsStore((store) => store.sortAscending)

  const gridView = useProjectsStore((store) => store.gridView)
  const setGridView = useProjectsStore((store) => store.setGridView)
  const setGridViewFalse = React.useCallback(() => {
    setGridView(false)
  }, [setGridView])
  const setGridViewTrue = React.useCallback(() => {
    setGridView(true)
  }, [setGridView])

  const convertToTitleCase = (str: string): string => {
    return str
      .replace(/([A-Z])/g, ' $1')
      .trim()
      .replace(/^\w/, (c) => c.toUpperCase())
  }

  function clearSearchInput() {
    const inputElement = document.getElementById('search-input') as HTMLInputElement
    if (inputElement) {
      inputElement.value = ''
    }
  }

  const onClearSearch = React.useCallback(() => {
    return () => {
      setSearchQuery('')
      clearSearchInput()
    }
  }, [setSearchQuery])

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 5 }}>
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'space-between',
          height: 32,
          flexShrink: 0,
        }}
      >
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            gap: 10,
          }}
        >
          <div style={{ fontSize: 16, fontWeight: 600 }}>
            {when(
              searchQuery !== '',
              <span>
                <span style={{ color: 'gray', paddingRight: 3 }}>
                  <span onClick={onClearSearch} style={{ cursor: 'pointer' }}>
                    ←{' '}
                  </span>{' '}
                  Search results for
                </span>
                <span> "{searchQuery}"</span>
              </span>,
            )}
            {when(
              searchQuery === '',
              <div style={{ flex: 1 }}>{categories[selectedCategory].name}</div>,
            )}
          </div>
          {when(
            projects.length > 0 && searchQuery === '',
            <div
              style={{
                display: 'flex',
                flexDirection: 'row',
                alignItems: 'center',
                gap: 10,
              }}
            >
              <CategoryActions projects={projects} />
            </div>,
          )}
        </div>
        <div style={{ display: 'flex', flexDirection: 'row', gap: 20, alignItems: 'center' }}>
          {when(
            projects.length > 1,
            <DropdownMenu.Root>
              <DropdownMenu.Trigger>
                <Button color='gray' variant='ghost' highContrast style={{ fontSize: 12 }}>
                  {convertToTitleCase(sortCriteria)}
                  {sortAscending ? <ArrowUpIcon /> : <ArrowDownIcon />}
                </Button>
              </DropdownMenu.Trigger>
              <SortingContextMenu />
            </DropdownMenu.Root>,
          )}
          {when(
            projects.length > 0,
            <div style={{ display: 'flex', flexDirection: 'row', gap: 1, alignItems: 'center' }}>
              <HamburgerMenuIcon
                onClick={setGridViewFalse}
                className={button({
                  size: 'square',
                  color: !gridView ? 'selected' : 'transparent',
                })}
              />
              <DashboardIcon
                onClick={setGridViewTrue}
                className={button({
                  size: 'square',
                  color: gridView ? 'selected' : 'transparent',
                })}
              />
            </div>,
          )}
        </div>
      </div>
    </div>
  )
})
ProjectsHeader.displayName = 'CategoryHeader'

const CategoryActions = React.memo(({ projects }: { projects: ProjectListing[] }) => {
  const selectedCategory = useProjectsStore((store) => store.selectedCategory)

  switch (selectedCategory) {
    case 'allProjects':
    case 'public':
    case 'private':
    case 'sharing':
    case 'sharedWithMe':
      return null
    case 'archive':
      return <CategoryArchiveActions projects={projects} />
    default:
      assertNever(selectedCategory)
  }
})
CategoryActions.displayName = 'CategoryActions'

const CategoryArchiveActions = React.memo(({ projects }: { projects: ProjectListing[] }) => {
  const fetcher = useFetcher()

  const handleEmptyTrash = React.useCallback(() => {
    const ok = window.confirm(
      'Are you sure? ALL projects in the archive will be deleted permanently.',
    )
    if (ok) {
      fetcher.submit({}, { method: 'POST', action: `/internal/projects/destroy` })
    }
  }, [fetcher])

  return (
    <Button
      onClick={handleEmptyTrash}
      style={{ display: projects.length === 0 ? 'none' : 'block', fontSize: 12 }}
      color='gray'
      variant='soft'
      highContrast
    >
      Delete All
    </Button>
  )
})
CategoryArchiveActions.displayName = 'CategoryArchiveActions'

const Projects = React.memo(
  ({
    projects,
    collaborators,
  }: {
    projects: ProjectListing[]
    collaborators: CollaboratorsByProject
  }) => {
    const myUser = useProjectsStore((store) => store.myUser)
    const gridView = useProjectsStore((store) => store.gridView)

    const selectedProjectId = useProjectsStore((store) => store.selectedProjectId)
    const setSelectedProjectId = useProjectsStore((store) => store.setSelectedProjectId)

    const handleProjectSelect = React.useCallback(
      (project: ProjectListing) => setSelectedProjectId(project.proj_id),
      [setSelectedProjectId],
    )

    const selectedCategory = useProjectsStore((store) => store.selectedCategory)

    const isAllProjects = selectedCategory === 'allProjects'
    const scrollStyle = isAllProjects ? { overflow: 'initial' } : {}

    return (
      <>
        {when(
          projects.length > 0 && !gridView,
          <div className={projectRows()} style={{ ...scrollStyle }}>
            {projects.map((project) => (
              <ProjectRow
                key={project.proj_id}
                project={project}
                isSharedWithMe={project.owner_id !== myUser?.user_id}
                selected={project.proj_id === selectedProjectId}
                /* eslint-disable-next-line react/jsx-no-bind */
                onSelect={() => handleProjectSelect(project)}
                collaborators={collaborators[project.proj_id]}
              />
            ))}
          </div>,
        )}
        {when(
          projects.length > 0 && gridView,
          <div className={projectCards()} style={{ ...scrollStyle }}>
            {projects.map((project) => (
              <ProjectCard
                key={project.proj_id}
                project={project}
                isSharedWithMe={project.owner_id !== myUser?.user_id}
                selected={project.proj_id === selectedProjectId}
                /* eslint-disable-next-line react/jsx-no-bind */
                onSelect={() => handleProjectSelect(project)}
                collaborators={collaborators[project.proj_id]}
              />
            ))}
          </div>,
        )}
        {when(projects.length === 0, <NoProjectsMessage />)}
      </>
    )
  },
)
Projects.displayName = 'Projects'

const NoProjectsMessage = React.memo(() => {
  const selectedCategory = useProjectsStore((store) => store.selectedCategory)
  const searchQuery = useProjectsStore((store) => store.searchQuery)

  const description = React.useMemo(() => {
    return searchQuery !== '' ? 'No projects found.' : categories[selectedCategory].description
  }, [searchQuery, selectedCategory])

  return <div style={{ padding: '30px 0px', color: 'gray' }}>{description}</div>
})
NoProjectsMessage.displayName = 'NoProjectsMessage'

const ProjectCard = React.memo(
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

    const isDarkMode = useIsDarkMode()

    const openShareDialog = useOpenShareDialog(project.proj_id)

    const canOpenShareDialog = React.useMemo(() => {
      return project.deleted !== true && !isSharedWithMe
    }, [project, isSharedWithMe])

    const onOpenShareDialog = React.useCallback(() => {
      if (canOpenShareDialog) {
        openShareDialog()
      }
    }, [openShareDialog, canOpenShareDialog])

    const onMouseDown = useCallback(
      (e: React.MouseEvent) => {
        e.stopPropagation()
        onSelect()
      },
      [onSelect],
    )

    return (
      <ContextMenu.Root>
        <ContextMenu.Trigger>
          <div
            data-role='project'
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
            onMouseDown={onMouseDown}
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
                      <UserAvatar
                        key={`collaborator-${project.id}-${collaborator.id}`}
                        size={24}
                        starBadge={collaborator.id === project.owner_id}
                        picture={collaborator.avatar}
                        name={collaborator.name ?? UnknownPlayerName}
                        className={sprinkles({
                          boxShadow: 'shadow',
                          color: 'white',
                          backgroundColor: 'primary',
                          borderRadius: 'full',
                        })}
                      />
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
                  <Flex
                    style={{
                      flexGrow: 1,
                      flex: 1,
                    }}
                    gap='2'
                    align={'center'}
                    justify={'start'}
                  >
                    <Text
                      size='1'
                      style={{
                        fontWeight: 500,
                      }}
                    >
                      {projectTitle}
                    </Text>
                    {when(
                      project.github_repository != null,
                      <Flex title={githubRepositoryPrettyName(project.github_repository)}>
                        <GitHubLogoIcon
                          color={isDarkMode ? 'white' : 'black'}
                          height={14}
                          width={14}
                        />
                      </Flex>,
                    )}
                  </Flex>
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
ProjectCard.displayName = 'ProjectCard'

const ProjectRow = React.memo(
  ({
    project,
    collaborators,
    selected,
    isSharedWithMe,
    onSelect,
  }: {
    project: ProjectListing
    collaborators: Collaborator[]
    selected: boolean
    isSharedWithMe: boolean
    onSelect: () => void
  }) => {
    const projectEditorLink = useProjectEditorLink()

    const openProject = React.useCallback(() => {
      window.open(projectEditorLink(project.proj_id), '_blank')
    }, [project.proj_id, projectEditorLink])

    const ownerName = React.useMemo(() => {
      return getOwnerName(project.owner_id, collaborators)
    }, [collaborators, project])

    const activeOperations = useProjectsStore((store) =>
      store.operations.filter((op) => op.projectId === project.proj_id && !op.errored),
    )

    const projectTitle = React.useMemo(() => {
      const renaming = activeOperations.find((op) => op.type === 'rename')
      return renaming?.type === 'rename' ? renaming.newTitle : project.title
    }, [project, activeOperations])

    const isDarkMode = useIsDarkMode()

    const openShareDialog = useOpenShareDialog(project.proj_id)

    const canOpenShareDialog = React.useMemo(() => {
      return project.deleted !== true && !isSharedWithMe
    }, [project, isSharedWithMe])

    const onOpenShareDialog = React.useCallback(() => {
      if (canOpenShareDialog) {
        openShareDialog()
      }
    }, [openShareDialog, canOpenShareDialog])

    const onMouseDown = useCallback(
      (e: React.MouseEvent) => {
        e.stopPropagation()
        onSelect()
      },
      [onSelect],
    )

    return (
      <ContextMenu.Root>
        <ContextMenu.Trigger>
          <div style={{ padding: '8px 0' }}>
            <div
              data-role='project'
              style={{
                height: 60,
                display: 'flex',
                flexDirection: 'row',
                alignItems: 'center',
                justifyContent: 'space-between',
                border: selected ? '2px solid #0090FF' : '2px solid transparent',
                borderRadius: 10,
                padding: 4,
                paddingRight: 10,
                transition: `.1s background-color ease-in-out`,
              }}
              onMouseDown={onMouseDown}
              onDoubleClick={openProject}
            >
              <div
                style={{
                  display: 'flex',
                  flexDirection: 'row',
                  alignItems: 'center',
                  justifyContent: 'space-between',
                  flex: 1,
                }}
              >
                <Flex gap='6' style={{ alignItems: 'center' }}>
                  <div
                    style={{
                      borderRadius: 6,
                      overflow: 'hidden',
                      height: 56,
                      width: 90,
                      background: 'linear-gradient(#a1a1a130, #a1a1a115)',
                      backgroundAttachment: 'local',
                      backgroundRepeat: 'no-repeat',
                      position: 'relative',
                      filter: project.deleted === true ? 'grayscale(1)' : undefined,
                    }}
                  />
                  <Flex style={{ flexDirection: 'column', gap: 0 }}>
                    <Flex
                      align='center'
                      justify='start'
                      gap='2'
                      style={{
                        flexGrow: 1,
                        minWidth: 180,
                      }}
                    >
                      <Text
                        size='1'
                        style={{
                          fontWeight: 500,
                        }}
                      >
                        {projectTitle}
                      </Text>
                      {when(
                        project.github_repository != null,
                        <Flex title={githubRepositoryPrettyName(project.github_repository)}>
                          <GitHubLogoIcon
                            color={isDarkMode ? 'white' : 'black'}
                            height={14}
                            width={14}
                          />
                        </Flex>,
                      )}
                    </Flex>
                    {when(
                      isSharedWithMe && ownerName != null,
                      <Text size='1' style={{ opacity: 0.5 }}>
                        By {ownerName}
                      </Text>,
                    )}
                  </Flex>
                </Flex>
                <Text size='1' style={{ width: 100, opacity: 0.5 }}>
                  {moment(project.modified_at).fromNow()}
                </Text>
                <div
                  style={{
                    flex: 1,
                    maxWidth: 200,
                    minWidth: 100,
                    display: 'flex',
                    gap: 6,
                  }}
                >
                  {when(
                    project.ProjectAccess?.access_level === AccessLevel.COLLABORATIVE,
                    collaborators.map((collaborator) => {
                      return (
                        <div
                          key={`collaborator-${project.id}-${collaborator.id}`}
                          style={{
                            borderRadius: '100%',
                            width: 24,
                            height: 24,
                            backgroundColor: '#0075F9',
                            backgroundImage: `url("${collaborator.avatar}")`,
                            backgroundSize: 'cover',
                            color: 'white',
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
                    }),
                  )}
                </div>
                <div
                  style={{
                    width: 110,
                    height: 20,
                  }}
                >
                  {when(
                    project.hasPendingRequests === true,
                    <Button
                      radius='full'
                      variant='solid'
                      color='red'
                      size='1'
                      style={{
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
                </div>
                <ProjectBadge
                  accessLevel={
                    asAccessLevel(project.ProjectAccess?.access_level) ?? AccessLevel.PRIVATE
                  }
                  onOpenShareDialog={onOpenShareDialog}
                />
              </div>
            </div>
          </div>
        </ContextMenu.Trigger>
        <ProjectActionsMenu project={project} />
      </ContextMenu.Root>
    )
  },
)
ProjectRow.displayName = 'ProjectRow'

export const ProjectBadge = React.memo(
  ({
    accessLevel,
    onOpenShareDialog,
  }: {
    accessLevel: AccessLevel
    onOpenShareDialog: () => void
  }) => {
    const [color, backgroundColor] = React.useMemo(() => {
      switch (accessLevel) {
        case AccessLevel.PRIVATE:
          return ['rgb(209 78 0)', 'rgb(249 144 0 / 15%)']
        case AccessLevel.PUBLIC:
          return ['rgb(0 130 77)', 'rgb(0 155 0 / 9%)']
        case AccessLevel.WITH_LINK:
          return ['rgb(0 114 222)', 'rgb(0 132 241 / 9%)']
        case AccessLevel.COLLABORATIVE:
          return ['rgb(0 114 222)', 'rgb(0 132 241 / 9%)']
        default:
          return ['gray', 'lightgray']
      }
    }, [accessLevel])

    const text = React.useMemo(() => {
      switch (accessLevel) {
        case AccessLevel.PRIVATE:
          return 'Private'
        case AccessLevel.PUBLIC:
          return 'Public'
        case AccessLevel.WITH_LINK:
          return 'With Link'
        case AccessLevel.COLLABORATIVE:
          return 'Collaborative'
        default:
          return 'Unknown'
      }
    }, [accessLevel])
    return (
      <div
        style={{ width: 80, display: 'flex', justifyContent: 'flex-end' }}
        onClick={onOpenShareDialog}
      >
        <Badge
          style={{
            backgroundColor: backgroundColor,
            color: color,
            fontSize: 10,
            cursor: 'pointer',
          }}
        >
          {text}
        </Badge>
      </div>
    )
  },
)
ProjectBadge.displayName = 'ProjectBadge'

const ActiveOperations = React.memo(({ projects }: { projects: ProjectListing[] }) => {
  const operations = useProjectsStore((store) =>
    store.operations.sort((a, b) => b.startedAt - a.startedAt),
  )

  const getOperationProject = React.useCallback(
    (operation: Operation) => {
      return projects.find((project) => project.proj_id === operation.projectId)
    },
    [projects],
  )

  return (
    <div
      style={{
        position: 'fixed',
        bottom: 0,
        right: 0,
        margin: 10,
        display: 'flex',
        flexDirection: 'column',
        gap: 10,
      }}
    >
      {operations.map((operation) => {
        const project = getOperationProject(operation)
        if (project == null) {
          return null
        }
        return <ActiveOperationToast operation={operation} key={operation.key} project={project} />
      })}
    </div>
  )
})
ActiveOperations.displayName = 'ActiveOperations'

const ActiveOperationToast = React.memo(
  ({ operation, project }: { operation: OperationWithKey; project: ProjectListing }) => {
    const removeOperation = useProjectsStore((store) => store.removeOperation)

    const dismiss = React.useCallback(() => {
      if (!operation.errored) {
        return
      }
      removeOperation(operation.key)
    }, [removeOperation, operation])

    return (
      <div
        style={{
          padding: 10,
          display: 'flex',
          gap: 10,
          alignItems: 'center',
          userSelect: 'none',
        }}
        className={sprinkles({
          boxShadow: 'shadow',
          borderRadius: 'small',
          backgroundColor: operation.errored ? 'error' : 'primary',
          color: 'white',
        })}
      >
        {unless(operation.errored, <Spinner className={sprinkles({ backgroundColor: 'white' })} />)}
        {when(
          operation.errored,
          <>
            <button
              className={`${button({ color: 'selected' })} ${sprinkles({
                color: 'white',
              })}`}
              onClick={dismiss}
            >
              Dismiss
            </button>
            <div
              style={{
                fontWeight: 'bold',
                textTransform: 'uppercase',
              }}
            >
              Failed
            </div>
          </>,
        )}
        <div>{getOperationDescription(operation, project)}</div>
      </div>
    )
  },
)
ActiveOperationToast.displayName = 'ActiveOperation'

function getOwnerName(ownerId: string, collaborators: Collaborator[]) {
  const collaborator = collaborators.find((c) => c.id === ownerId)
  return collaborator?.name ?? 'Utopia user' // This is a fallback required in case we have misconfigured user details
}

function createCloneLink(
  accessLevel: AccessLevel,
  { cloneRepo, cloneBranch }: { cloneBranch?: string; cloneRepo?: string } = {},
): string {
  let accessLevelParam
  switch (accessLevel) {
    case AccessLevel.PUBLIC:
      accessLevelParam = 'public'
      break
    case AccessLevel.WITH_LINK:
      accessLevelParam = 'withLink'
      break
    case AccessLevel.COLLABORATIVE:
      accessLevelParam = 'collaborative'
      break
    case AccessLevel.PRIVATE:
      accessLevelParam = 'private'
      break
    default:
      assertNever(accessLevel)
  }
  const cloneRepoParam = cloneRepo != null ? `&clone=${cloneRepo}` : ''
  const cloneBranchParam = cloneBranch != null ? `&github_branch=${cloneBranch}` : ''
  return `/p?accessLevel=${accessLevelParam}${cloneRepoParam}${cloneBranchParam}`
}

const projectTemplates = [
  {
    title: 'Public Project',
    onClick: () => window.open(createCloneLink(AccessLevel.PUBLIC), '_blank'),
    thumbnail: '/assets/thumbnail-new.png',
  },
  {
    title: 'Remix Project',
    onClick: () =>
      window.open(
        createCloneLink(AccessLevel.PUBLIC, {
          cloneRepo: 'concrete-utopia/utopia-remix-starter',
        }),
        '_blank',
      ),
    thumbnail: '/assets/thumbnail-remix.png',
  },
  {
    title: 'Hydrogen Project',
    onClick: () =>
      window.open(
        createCloneLink(AccessLevel.PUBLIC, { cloneRepo: 'concrete-utopia/hydrogen-november' }),
        '_blank',
      ),
    thumbnail: '/assets/thumbnail-hydrogen.png',
  },
]
function ProjectTemplates() {
  const isDarkMode = useIsDarkMode()
  return (
    <Flex
      direction='column'
      gap='5'
      style={{
        padding: '10px 20px',
        backgroundColor: isDarkMode ? '#232323' : 'rgba(0,0,0,0.05)',
        borderRadius: '10px',
      }}
    >
      <Text
        style={{
          fontSize: '16px',
          fontWeight: 600,
          display: 'flex',
          flex: 'none',
          alignItems: 'center',
          height: '32px',
        }}
      >
        Create From Template
      </Text>
      <Flex gap='4'>
        {projectTemplates.map((template) => (
          <Flex
            key={template.title}
            gap='5'
            direction='column'
            align='center'
            style={{
              flex: 1,
              width: '100%',
              maxWidth: '200px',
              height: 'min-content',
              display: 'flex',
              flexDirection: 'column',
              gap: 5,
              cursor: 'pointer',
            }}
            onClick={template.onClick}
          >
            <div
              className={projectTemplate()}
              style={{
                backgroundImage: `url(${template.thumbnail})`,
                backgroundSize: 'cover',
                backgroundPosition: 'center',
                backgroundRepeat: 'no-repeat',
              }}
            ></div>
            <Flex
              direction='column'
              gap='5'
              align='start'
              style={{
                width: '100%',
                padding: '10px',
              }}
            >
              <Text
                size='1'
                align='left'
                style={{
                  fontWeight: 500,
                  alignSelf: 'flex-start',
                }}
              >
                {template.title}
              </Text>
            </Flex>
          </Flex>
        ))}
      </Flex>
    </Flex>
  )
}
