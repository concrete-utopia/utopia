import {
  Root as DropdownMenuRoot,
  Trigger as DropdownMenuTrigger,
} from '@radix-ui/react-dropdown-menu'
import { DashboardIcon, DotsHorizontalIcon, HamburgerMenuIcon } from '@radix-ui/react-icons'
import React from 'react'
import { LoaderFunctionArgs, json } from '@remix-run/node'
import { useFetcher, useLoaderData } from '@remix-run/react'
import moment from 'moment'
import { UserDetails } from 'prisma-client'
import { ProjectContextMenu } from '../components/projectActionContextMenu'
import { SortingContextMenu } from '../components/sortProjectsContextMenu'
import { useIsDarkMode } from '../hooks/useIsDarkMode'
import { listDeletedProjects, listProjects } from '../models/project.server'
import { getCollaborators } from '../models/projectCollaborators.server'
import { useProjectsStore } from '../store'
import { button } from '../styles/button.css'
import { newProjectButton } from '../styles/newProjectButton.css'
import { projectCategoryButton, userName } from '../styles/sidebarComponents.css'
import { sprinkles } from '../styles/sprinkles.css'
import { Collaborator, CollaboratorsByProject, ProjectWithoutContent } from '../types'
import { requireUser } from '../util/api.server'
import { assertNever } from '../util/assertNever'
import { projectEditorLink } from '../util/links'
import { when } from '../util/react-conditionals'
import { multiplayerInitialsFromName } from '../util/strings'
import { useProjectMatchesQuery, useSortCompareProject } from '../util/use-sort-compare-project'

const SortOptions = ['title', 'dateCreated', 'dateModified'] as const
export type SortCriteria = (typeof SortOptions)[number]

const Categories = ['allProjects', 'trash'] as const

function isCategory(category: unknown): category is Category {
  return Categories.includes(category as Category)
}

export type Category = (typeof Categories)[number]

const categories: { [key in Category]: { name: string } } = {
  allProjects: { name: 'All My Projects' },
  trash: { name: 'Trash' },
}

const MarginSize = 30
const SidebarRowHeight = 30

export async function loader(args: LoaderFunctionArgs) {
  const user = await requireUser(args.request)

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

  const filteredProjects = React.useMemo(
    () =>
      activeProjects
        // filter out projects that don't match the search query
        .filter(projectMatchesQuery)
        // sort them out according to the selected strategy
        .sort(sortCompareProject),

    [activeProjects, projectMatchesQuery, sortCompareProject],
  )

  return (
    <div
      style={{
        margin: MarginSize,
        height: `calc(100vh - ${MarginSize * 2}px)`,
        width: `calc(100vw - ${MarginSize * 2}px)`,
        gap: MarginSize,
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
        <ProjectsHeader projects={activeProjects} />
        <ProjectCards projects={filteredProjects} collaborators={data.collaborators} />
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
            style={{ width: 40 }}
            src={user.picture ?? undefined}
            referrerPolicy='no-referrer'
          />
          <div className={userName({})}>{user.name}</div>
        </div>

        <input
          id='search-input'
          autoFocus={true}
          value={searchQuery}
          onChange={(e) => {
            setSearchQuery(e.target.value)
          }}
          style={{
            border: 'none',
            background: 'transparent',
            outline: 'none',
            color: 'grey',
            height: SidebarRowHeight,
            borderBottom: '1px solid gray',
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            padding: '0 14px',
          }}
          placeholder='Search…'
        />
        <div style={{ display: 'flex', flexDirection: 'column', gap: 5 }}>
          {Object.entries(categories).map(([category, data]) => {
            return (
              <button
                key={`category-${category}`}
                className={projectCategoryButton({
                  color: category === selectedCategory ? 'selected' : 'neutral',
                })}
                onClick={handleSelectCategory(category)}
              >
                <span>{data.name}</span>
              </button>
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
  const newProjectButtons = [
    {
      id: 'createProject',
      title: '+ Blank Project',
      onClick: () => window.open(projectEditorLink(null), '_blank'),
      color: 'orange',
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

  return (
    <div
      style={{
        height: 60,
        flex: 0,
        display: 'flex',
        flexDirection: 'row',
        gap: 15,
      }}
    >
      {newProjectButtons.map((p) => (
        <button key={p.id} className={newProjectButton({ color: p.color })} onClick={p.onClick}>
          <span>{p.title}</span>
        </button>
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

  return (
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
                    const inputElement = document.getElementById('search-input') as HTMLInputElement
                    if (inputElement) {
                      inputElement.value = ''
                    }
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
      </div>

      {when(
        projects.length > 0,
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            gap: 10,
          }}
        >
          <CategoryActions projects={projects} />
          {when(
            projects.length > 1,
            <div style={{ display: 'flex', flexDirection: 'row', gap: 10 }}>
              <DropdownMenuRoot>
                <DropdownMenuTrigger asChild>
                  <div
                    className={button()}
                    style={{
                      justifyContent: 'flex-end',
                      gap: 10,
                    }}
                  >
                    <div>{convertToTitleCase(sortCriteria)} </div>
                    <div>{sortAscending ? '↑' : '↓'}</div>
                  </div>
                </DropdownMenuTrigger>
                <SortingContextMenu />
              </DropdownMenuRoot>
              <div style={{ display: 'flex', flexDirection: 'row', gap: 1 }}>
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
              </div>
            </div>,
          )}
        </div>,
      )}
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
    <>
      <div
        className={button({ color: 'subtle' })}
        onClick={handleEmptyTrash}
        style={{ display: projects.length === 0 ? 'none' : 'block' }}
      >
        Empty Trash
      </div>
    </>
  )
})
CategoryTrashActions.displayName = 'CategoryTrashActions'

const ProjectCards = React.memo(
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
        {!gridView ? (
          <div
            style={{
              display: 'flex',
              flexDirection: 'column',
              flexGrow: 1,
              overflowY: 'scroll',
              scrollbarColor: 'lightgrey transparent',
              gap: 10,
            }}
          >
            {projects.map((project) => (
              <ProjectRow
                key={project.proj_id}
                project={project}
                selected={project.proj_id === selectedProjectId}
                onSelect={() => handleProjectSelect(project)}
                collaborators={collaborators[project.proj_id]}
              />
            ))}
          </div>
        ) : (
          <div
            style={{
              display: 'flex',
              flexWrap: 'wrap',
              alignContent: 'flex-start',
              gap: MarginSize,
              flexGrow: 1,
              flexDirection: 'row',
              overflowY: 'scroll',
              scrollbarColor: 'lightgrey transparent',
            }}
          >
            {projects.map((project) => (
              <ProjectCard
                key={project.proj_id}
                project={project}
                selected={project.proj_id === selectedProjectId}
                onSelect={() => handleProjectSelect(project)}
                collaborators={collaborators[project.proj_id]}
              />
            ))}
          </div>
        )}
      </>
    )
  },
)
ProjectCards.displayName = 'ProjectCards'

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
    const openProject = React.useCallback(() => {
      window.open(projectEditorLink(project.proj_id), '_blank')
    }, [project.proj_id])

    return (
      <div
        style={{
          height: 200,
          width: 300,
          display: 'flex',
          flexDirection: 'column',
          gap: 5,
        }}
      >
        <div
          style={{
            border: selected ? '2px solid #0075F9' : '2px solid transparent',
            borderRadius: 10,
            overflow: 'hidden',
            height: 180,
            width: '100%',
            background: 'linear-gradient(rgba(77, 255, 223, 0.4), rgba(255,250,220,.8))',
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
                  title={collaborator.name}
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
            <div style={{ fontWeight: 600 }}>{project.title}</div>
            <div>{moment(project.modified_at).fromNow()}</div>
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
    const openProject = React.useCallback(() => {
      window.open(projectEditorLink(project.proj_id), '_blank')
    }, [project.proj_id])

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
                background: 'linear-gradient(rgba(77, 255, 223, 0.4), rgba(255,250,220,.8))',
                backgroundAttachment: 'local',
                backgroundRepeat: 'no-repeat',
                position: 'relative',
              }}
            />
            <div
              style={{
                flexGrow: 1,
                minWidth: 180,
                maxWidth: 380,
                fontWeight: 600,
              }}
            >
              {project.title}
            </div>
            <div style={{ width: 220 }}>{moment(project.modified_at).fromNow()}</div>
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
                      backgroundImage: `url("${collaborator.avatar}")`,
                      backgroundSize: 'cover',
                      display: 'flex',
                      justifyContent: 'center',
                      alignItems: 'center',
                      fontSize: '.9em',
                      fontWeight: 700,
                      filter: project.deleted === true ? 'grayscale(1)' : undefined,
                    }}
                    title={collaborator.name}
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
  return (
    <div style={{ display: 'flex', alignItems: 'center', gap: 5 }}>
      <DropdownMenuRoot>
        <DropdownMenuTrigger asChild>
          <DotsHorizontalIcon className={button()} />
        </DropdownMenuTrigger>
        <ProjectContextMenu project={project} />
      </DropdownMenuRoot>
    </div>
  )
})
ProjectCardActions.displayName = 'ProjectCardActions'
