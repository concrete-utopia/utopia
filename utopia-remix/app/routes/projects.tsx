import {
  ArrowDownIcon,
  ArrowUpIcon,
  CubeIcon,
  DashboardIcon,
  HamburgerMenuIcon,
  MagnifyingGlassIcon,
  TrashIcon,
} from '@radix-ui/react-icons'
import { Badge, Button, ContextMenu, DropdownMenu, Flex, Text, TextField } from '@radix-ui/themes'
import { type LoaderFunctionArgs, json } from '@remix-run/node'
import { useFetcher, useLoaderData } from '@remix-run/react'
import moment from 'moment'
import type { UserDetails } from 'prisma-client'
import React from 'react'
import { ProjectActionsMenu } from '../components/projectActionContextMenu'
import { SortingContextMenu } from '../components/sortProjectsContextMenu'
import { Spinner } from '../components/spinner'
import { useCleanupOperations } from '../hooks/useCleanupOperations'
import { useIsDarkMode } from '../hooks/useIsDarkMode'
import { listDeletedProjects, listProjects } from '../models/project.server'
import { getCollaborators } from '../models/projectCollaborators.server'
import type { OperationWithKey } from '../store'
import { useProjectsStore } from '../store'
import { button } from '../styles/button.css'
import { projectCards, projectRows } from '../styles/projects.css'
import { projectCategoryButton, userName } from '../styles/sidebarComponents.css'
import { sprinkles } from '../styles/sprinkles.css'
import type {
  Collaborator,
  CollaboratorsByProject,
  Operation,
  ProjectWithoutContent,
} from '../types'
import {
  AccessLevel,
  getOperationDescription,
  asAccessLevel,
  isProjectAccessRequestWithUserDetailsArray,
} from '../types'
import type { ProjectAccessRequestWithUserDetails } from '../types'
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
import { SharingDialogWrapper } from '../components/sharingDialog'

const SortOptions = ['title', 'dateCreated', 'dateModified'] as const
export type SortCriteria = (typeof SortOptions)[number]

const Categories = ['allProjects', 'trash'] as const

function isCategory(category: unknown): category is Category {
  return Categories.includes(category as Category)
}

export type Category = (typeof Categories)[number]

const categories: { [key in Category]: { name: string; icon: React.ReactNode } } = {
  allProjects: { name: 'All My Projects', icon: <CubeIcon width='16' height='16' /> },
  trash: { name: 'Trash', icon: <TrashIcon width='16' height='16' /> },
}

export async function loader(args: LoaderFunctionArgs) {
  const user = await requireUser(args.request, { redirect: auth0LoginURL() })

  const projects = await listProjects({
    ownerId: user.user_id,
  })
  const deletedProjects = await listDeletedProjects({
    ownerId: user.user_id,
  })
  const collaborators = await getCollaborators({
    ids: [...projects, ...deletedProjects].map((p) => p.proj_id),
    userId: user.user_id,
  })

  return json(
    { projects, deletedProjects, user, collaborators },
    { headers: { 'cache-control': 'no-cache' } },
  )
}

const ProjectsPage = React.memo(() => {
  useCleanupOperations()

  const data = useLoaderData() as unknown as {
    projects: ProjectWithoutContent[]
    user: UserDetails
    deletedProjects: ProjectWithoutContent[]
    collaborators: CollaboratorsByProject
  }

  const selectedCategory = useProjectsStore((store) => store.selectedCategory)

  const activeProjects = React.useMemo(() => {
    switch (selectedCategory) {
      case 'allProjects':
        return data.projects
      case 'trash':
        return data.deletedProjects
      default:
        assertNever(selectedCategory)
    }
  }, [data.projects, data.deletedProjects, selectedCategory])

  const sortCompareProject = useSortCompareProject()
  const projectMatchesQuery = useProjectMatchesQuery()
  const projectIsOnActiveOperation = useProjectIsOnActiveOperation()

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
    >
      <Sidebar user={data.user} />
      <div
        style={{
          display: 'flex',
          flexGrow: 1,
          flexDirection: 'column',
          gap: 30,
          margin: '30px 30px 0px 0',
        }}
      >
        <TopActionBar />
        <ProjectsHeader projects={filteredProjects} />
        <Projects projects={filteredProjects} collaborators={data.collaborators} />
        <ActiveOperations projects={activeProjects} />
      </div>
    </div>
  )
})
ProjectsPage.displayName = 'ProjectsPage'

export default ProjectsPage

const Sidebar = React.memo(({ user }: { user: UserDetails }) => {
  const searchQuery = useProjectsStore((store) => store.searchQuery)
  const setSearchQuery = useProjectsStore((store) => store.setSearchQuery)
  const selectedCategory = useProjectsStore((store) => store.selectedCategory)
  const setSelectedCategory = useProjectsStore((store) => store.setSelectedCategory)
  const setSelectedProjectId = useProjectsStore((store) => store.setSelectedProjectId)

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
      }
    },
    [setSelectedCategory, setSearchQuery, setSelectedProjectId],
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
        <div
          style={{
            display: 'flex',
            alignItems: 'center',
            gap: 10,
            padding: '0 6px',
          }}
        >
          <img
            className={sprinkles({ borderRadius: 'medium' })}
            style={{ width: 32 }}
            src={user.picture ?? undefined}
            referrerPolicy='no-referrer'
          />
          <div className={userName({})}>{user.name}</div>
        </div>
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

const ProjectsHeader = React.memo(({ projects }: { projects: ProjectWithoutContent[] }) => {
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

const CategoryActions = React.memo(({ projects }: { projects: ProjectWithoutContent[] }) => {
  const selectedCategory = useProjectsStore((store) => store.selectedCategory)

  switch (selectedCategory) {
    case 'allProjects':
      return null
    case 'trash':
      return <CategoryTrashActions projects={projects} />
    default:
      assertNever(selectedCategory)
  }
})
CategoryActions.displayName = 'CategoryActions'

const CategoryTrashActions = React.memo(({ projects }: { projects: ProjectWithoutContent[] }) => {
  const fetcher = useFetcher()

  const handleEmptyTrash = React.useCallback(() => {
    const ok = window.confirm(
      'Are you sure? ALL projects in the trash will be deleted permanently.',
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
      Empty Trash
    </Button>
  )
})
CategoryTrashActions.displayName = 'CategoryTrashActions'

const Projects = React.memo(
  ({
    projects,
    collaborators,
  }: {
    projects: ProjectWithoutContent[]
    collaborators: CollaboratorsByProject
  }) => {
    const gridView = useProjectsStore((store) => store.gridView)

    const selectedProjectId = useProjectsStore((store) => store.selectedProjectId)
    const setSelectedProjectId = useProjectsStore((store) => store.setSelectedProjectId)

    const handleProjectSelect = React.useCallback(
      (project: ProjectWithoutContent) =>
        setSelectedProjectId(project.proj_id === selectedProjectId ? null : project.proj_id),
      [setSelectedProjectId, selectedProjectId],
    )

    return (
      <>
        {when(
          projects.length > 0 && !gridView,
          <div className={projectRows()}>
            {projects.map((project) => (
              <ProjectRow
                key={project.proj_id}
                project={project}
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
          <div className={projectCards()}>
            {projects.map((project) => (
              <ProjectCard
                key={project.proj_id}
                project={project}
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

  function getCategorySubtitle(cat: Category) {
    switch (cat) {
      case 'allProjects':
        return 'Projects you create or open will show up here.'
      case 'trash':
        return 'Deleted projects are kept here until you destroy them for good.'
      default:
        assertNever(cat)
    }
  }

  const subtitle = searchQuery !== '' ? 'No projects found.' : getCategorySubtitle(selectedCategory)

  return <div style={{ padding: '30px 0px', color: 'gray' }}>{subtitle}</div>
})
NoProjectsMessage.displayName = 'NoProjectsMessage'

const ProjectCard = React.memo(
  ({
    project,
    collaborators,
    selected,
    onSelect,
  }: {
    project: ProjectWithoutContent
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

    const accessRequestsFetcher = useFetcher()
    const [accessRequests, setAccessRequests] = React.useState<
      ProjectAccessRequestWithUserDetails[]
    >([])

    React.useEffect(() => {
      if (accessRequestsFetcher.state === 'idle' && accessRequestsFetcher.data != null) {
        if (isProjectAccessRequestWithUserDetailsArray(accessRequestsFetcher.data)) {
          setAccessRequests(accessRequestsFetcher.data)
        }
      }
    }, [accessRequestsFetcher])

    const handleSortMenuOpenChange = React.useCallback(() => {
      const action = `/internal/projects/${project.proj_id}/access/requests`
      accessRequestsFetcher.submit({}, { method: 'GET', action: action })
    }, [accessRequestsFetcher, project])

    return (
      <ContextMenu.Root onOpenChange={handleSortMenuOpenChange}>
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
              }}
              onMouseDown={onSelect}
              onDoubleClick={openProject}
            >
              <div style={{ position: 'absolute', right: 6, bottom: 6, display: 'flex', gap: 2 }}>
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
                        filter: project.deleted === true ? 'grayscale(1)' : undefined,
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
              </div>
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
                    accessLevel={
                      asAccessLevel(project.ProjectAccess?.access_level) ?? AccessLevel.PRIVATE
                    }
                  />
                </div>
                <Text size='1' style={{ opacity: 0.5 }}>
                  {moment(project.modified_at).fromNow()}
                </Text>
              </div>
            </div>
          </div>
        </ContextMenu.Trigger>
        <ProjectActionsMenu project={project} accessRequests={accessRequests} />
        <SharingDialogWrapper project={project} accessRequests={accessRequests} />
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
    onSelect,
  }: {
    project: ProjectWithoutContent
    collaborators: Collaborator[]
    selected: boolean
    onSelect: () => void
  }) => {
    const projectEditorLink = useProjectEditorLink()

    const openProject = React.useCallback(() => {
      window.open(projectEditorLink(project.proj_id), '_blank')
    }, [project.proj_id, projectEditorLink])

    const accessRequestsFetcher = useFetcher()
    const [accessRequests, setAccessRequests] = React.useState<
      ProjectAccessRequestWithUserDetails[]
    >([])

    React.useEffect(() => {
      if (accessRequestsFetcher.state === 'idle' && accessRequestsFetcher.data != null) {
        if (isProjectAccessRequestWithUserDetailsArray(accessRequestsFetcher.data)) {
          setAccessRequests(accessRequestsFetcher.data)
        }
      }
    }, [accessRequestsFetcher])

    const onContextMenuOpenChange = React.useCallback(() => {
      const action = `/internal/projects/${project.proj_id}/access/requests`
      accessRequestsFetcher.submit({}, { method: 'GET', action: action })
    }, [accessRequestsFetcher, project])

    return (
      <ContextMenu.Root onOpenChange={onContextMenuOpenChange}>
        <ContextMenu.Trigger>
          <div style={{ padding: '8px 0' }}>
            <div
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
              onMouseDown={onSelect}
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
                <Flex gap='6'>
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
                    }}
                  />
                  <Text
                    size='1'
                    style={{
                      display: 'flex',
                      gap: '10px',
                      alignItems: 'center',
                      flexGrow: 1,
                      minWidth: 180,
                      fontWeight: 500,
                    }}
                  >
                    {project.title}
                  </Text>
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
                  {collaborators.map((collaborator) => {
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
                          filter: project.deleted === true ? 'grayscale(1)' : undefined,
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
                </div>
                <ProjectBadge
                  accessLevel={
                    asAccessLevel(project.ProjectAccess?.access_level) ?? AccessLevel.PRIVATE
                  }
                />
              </div>
            </div>
          </div>
        </ContextMenu.Trigger>
        <ProjectActionsMenu project={project} accessRequests={accessRequests} />
      </ContextMenu.Root>
    )
  },
)
ProjectRow.displayName = 'ProjectRow'

const ProjectBadge = React.memo(({ accessLevel }: { accessLevel: AccessLevel }) => {
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
    <div style={{ width: 80, display: 'flex', justifyContent: 'flex-end' }}>
      <Badge
        style={{
          backgroundColor: backgroundColor,
          color: color,
          fontSize: 9,
        }}
      >
        {text}
      </Badge>
    </div>
  )
})
ProjectBadge.displayName = 'ProjectBadge'

const ActiveOperations = React.memo(({ projects }: { projects: ProjectWithoutContent[] }) => {
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
  ({ operation, project }: { operation: OperationWithKey; project: ProjectWithoutContent }) => {
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
