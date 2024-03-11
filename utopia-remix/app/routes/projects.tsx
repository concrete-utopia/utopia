import {
  Root as DropdownMenuRoot,
  Trigger as DropdownMenuTrigger,
} from '@radix-ui/react-dropdown-menu'
import {
  CubeIcon,
  DashboardIcon,
  DotsHorizontalIcon,
  HamburgerMenuIcon,
  MagnifyingGlassIcon,
  TrashIcon,
} from '@radix-ui/react-icons'
import { LoaderFunctionArgs, json } from '@remix-run/node'
import { useFetcher, useLoaderData } from '@remix-run/react'
import moment from 'moment'
import { UserDetails } from 'prisma-client'
import React from 'react'
import { ProjectContextMenu } from '../components/projectActionContextMenu'
import { SortingContextMenu } from '../components/sortProjectsContextMenu'
import { Spinner } from '../components/spinner'
import { useIsDarkMode } from '../hooks/useIsDarkMode'
import { listDeletedProjects, listProjects } from '../models/project.server'
import { getCollaborators } from '../models/projectCollaborators.server'
import { OperationWithKey, useProjectsStore } from '../store'
import { button } from '../styles/button.css'
import { newProjectButton } from '../styles/newProjectButton.css'
import { projectCategoryButton, userName } from '../styles/sidebarComponents.css'
import { projectCards, projectRows } from '../styles/projects.css'
import { sprinkles } from '../styles/sprinkles.css'
import {
  Collaborator,
  CollaboratorsByProject,
  Operation,
  ProjectWithoutContent,
  getOperationDescription,
} from '../types'
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
import { useCleanupOperations } from '../hooks/useCleanupOperations'
import { Text, Button } from '@radix-ui/themes'

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

const MarginSize = 30
const SidebarRowHeight = 30

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
        margin: MarginSize,
        height: `calc(100vh - ${MarginSize * 2}px)`,
        width: `calc(100vw - ${MarginSize * 2}px)`,
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
          gap: MarginSize,
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
  const [isSearchFocused, setIsSearchFocused] = React.useState(false)

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

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        width: 230,
        flexShrink: 0,
        justifyContent: 'space-between',
      }}
    >
      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          gap: 20,
        }}
      >
        <div
          style={{
            display: 'flex',
            alignItems: 'center',
            gap: 10,
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
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            border: `1px solid ${isSearchFocused ? '#0075F9' : 'transparent'}`,
            borderBottom: `1px solid ${isSearchFocused ? '#0075F9' : 'gray'}`,
            borderRadius: isSearchFocused ? 3 : undefined,
            overflow: 'visible',
            padding: '0px 14px',
            gap: 10,
          }}
        >
          <MagnifyingGlassIcon width='16' height='16' />
          <input
            id='search-input'
            autoFocus={true}
            value={searchQuery}
            onChange={(e) => {
              setSearchQuery(e.target.value)
            }}
            onFocus={() => setIsSearchFocused(true)}
            onBlur={() => setIsSearchFocused(false)}
            style={{
              border: 'none',
              background: 'transparent',
              outline: 'none',
              height: SidebarRowHeight,
              display: 'flex',
              flexDirection: 'row',
              alignItems: 'center',
              fontSize: 12,
            }}
            placeholder='Search…'
          />
        </div>
        <div style={{ display: 'flex', flexDirection: 'column', gap: 5 }}>
          {Object.entries(categories).map(([category, data]) => {
            return (
              <Button
                radius='full'
                key={`category-${category}`}
                onClick={handleSelectCategory(category)}
                variant={category === selectedCategory && searchQuery === '' ? 'solid' : 'outline'}
                style={{
                  justifyContent: 'flex-start',
                  padding: '0 14px',
                  gap: 10,
                }}
              >
                {data.icon}
                <Text size='1'>{data.name}</Text>
              </Button>
            )
          })}
        </div>
      </div>
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          alignItems: 'center',
          gap: 14,
          fontFamily: 'Reckless',
          fontSize: 34,
        }}
      >
        <div
          style={{
            height: 60,
            width: 45,
            backgroundSize: '45px',
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
        title: '+ Blank Project',
        onClick: () => window.open(projectEditorLink(null), '_blank'),
        color: 'blue',
      },
      // {
      //   title: '+ Project On GitHub',
      //   onClick: () => {},
      //   color: 'pink',
      // },
      // {
      //   title: '+ Import From GitHub',
      //   onClick: () => {},
      //   color: 'purple',
      // },
      // {
      //   title: '+ Remix Project',
      //   onClick: () => {},
      //   color: 'blue',
      // },
      // {
      //   title: '+ Shopify Store',
      //   onClick: () => {},
      //   color: 'green',
      // },
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

  const [sortMenuOpen, setSortMenuOpen] = React.useState(false)
  const handleSortMenuOpenChange = React.useCallback(() => {
    setSortMenuOpen((prevSortMenuOpen) => !prevSortMenuOpen)
  }, [])

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 5 }}>
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'space-between',
          height: 40,
          flexShrink: 0,
        }}
      >
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            gap: 10,
            padding: '5px 10px',
          }}
        >
          <div style={{ fontSize: 16, fontWeight: 600 }}>
            {when(
              searchQuery !== '',
              <span>
                <span style={{ color: 'gray', paddingRight: 3 }}>
                  <span
                    onClick={() => {
                      setSearchQuery('')
                      clearSearchInput()
                    }}
                    style={{ cursor: 'pointer' }}
                  >
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
        <div style={{ display: 'flex', flexDirection: 'row', gap: 10 }}>
          {when(
            projects.length > 1,
            <DropdownMenuRoot onOpenChange={handleSortMenuOpenChange}>
              <DropdownMenuTrigger asChild>
                <div
                  className={button()}
                  style={{
                    justifyContent: 'flex-end',
                    gap: 10,
                    background: sortMenuOpen ? '#a4a4a430' : 'inherit',
                  }}
                >
                  <Text size='1'>{convertToTitleCase(sortCriteria)} </Text>
                  <div>{sortAscending ? '↑' : '↓'}</div>
                </div>
              </DropdownMenuTrigger>
              <SortingContextMenu />
            </DropdownMenuRoot>,
          )}
          {when(
            projects.length > 0,
            <div style={{ display: 'flex', flexDirection: 'row', gap: 1, alignItems: 'center' }}>
              <HamburgerMenuIcon
                onClick={() => {
                  setGridView(false)
                }}
                className={button({
                  size: 'square',
                  color: !gridView ? 'selected' : 'transparent',
                })}
              />
              <DashboardIcon
                onClick={() => {
                  setGridView(true)
                }}
                className={button({
                  size: 'square',
                  color: gridView ? 'selected' : 'transparent',
                })}
              />
            </div>,
          )}
        </div>
      </div>
      {when(projects.length === 0, <NoProjectsMessage />)}
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
      style={{ display: projects.length === 0 ? 'none' : 'block' }}
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
                onSelect={() => handleProjectSelect(project)}
                collaborators={collaborators[project.proj_id]}
              />
            ))}
          </div>,
        )}
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

  return <div style={{ padding: '0px 10px' }}>{subtitle}</div>
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

    return (
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
            border: selected ? '2px solid #0075F9' : '2px solid transparent',
            borderRadius: 10,
            overflow: 'hidden',
            height: '100%',
            aspectRatio: 1.6,
            background: 'linear-gradient(#a4a4a4, #a4a4a410)',
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
                  {when(collaborator.avatar === '', multiplayerInitialsFromName(collaborator.name))}
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
          <div style={{ display: 'flex', flexDirection: 'column', padding: 10, gap: 5, flex: 1 }}>
            <Text
              size='1'
              style={{
                flexGrow: 1,
                fontWeight: 500,
              }}
            >
              {project.title}
            </Text>
            <Text size='1' style={{ opacity: 0.5 }}>
              {moment(project.modified_at).fromNow()}
            </Text>
          </div>
          <ProjectCardActions project={project} />
        </div>
      </div>
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

    return (
      <div style={{ padding: '8px 0' }}>
        <div
          style={{
            height: 40,
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            justifyContent: 'space-between',
            border: selected ? '2px solid #0075F9' : '2px solid transparent',
            borderRadius: 10,
            padding: '4px 30px 4px 4px',
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
              gap: 15,
              flex: 1,
            }}
          >
            <div
              style={{
                borderRadius: 8,
                overflow: 'hidden',
                height: 40,
                width: 70,
                background: 'linear-gradient(#a4a4a4, #a4a4a410)',
                backgroundAttachment: 'local',
                backgroundRepeat: 'no-repeat',
                position: 'relative',
              }}
            />

            <Text
              size='1'
              style={{
                flexGrow: 1,
                minWidth: 180,
                maxWidth: 380,
                fontWeight: 500,
              }}
            >
              {project.title}
            </Text>
            <Text size='1' style={{ width: 220, opacity: 0.5 }}>
              {moment(project.modified_at).fromNow()}
            </Text>
            <div
              style={{
                maxWidth: 480,
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
          </div>
          <ProjectCardActions project={project} />
        </div>
      </div>
    )
  },
)
ProjectRow.displayName = 'ProjectRow'

const ProjectCardActions = React.memo(({ project }: { project: ProjectWithoutContent }) => {
  const [sortMenuOpen, setSortMenuOpen] = React.useState(false)
  const handleSortMenuOpenChange = React.useCallback(() => {
    setSortMenuOpen((prevSortMenuOpen) => !prevSortMenuOpen)
  }, [])

  return (
    <div style={{ display: 'flex', alignItems: 'center', gap: 5 }}>
      <DropdownMenuRoot onOpenChange={handleSortMenuOpenChange}>
        <DropdownMenuTrigger asChild>
          <DotsHorizontalIcon
            className={button()}
            style={{ background: sortMenuOpen ? '#a4a4a430' : 'inherit' }}
          />
        </DropdownMenuTrigger>
        <ProjectContextMenu project={project} />
      </DropdownMenuRoot>
    </div>
  )
})
ProjectCardActions.displayName = 'ProjectCardActions'

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
