import * as DropdownMenu from '@radix-ui/react-dropdown-menu'
import { LoaderFunctionArgs, json } from '@remix-run/node'
import { useFetcher, useLoaderData } from '@remix-run/react'
import moment from 'moment'
import { UserDetails } from 'prisma-client'
import React, { useState } from 'react'
import { ProjectContextMenu } from '../components/projectActionContextMenu'
import { listDeletedProjects, listProjects } from '../models/project.server'
import { useStore } from '../store'
import { button } from '../styles/button.css'
import { newProjectButton } from '../styles/newProjectButton.css'
import { projectCategoryButton, userName } from '../styles/sidebarComponents.css'
import { sprinkles } from '../styles/sprinkles.css'
import { ProjectWithoutContent } from '../types'
import { requireUser } from '../util/api.server'
import { assertNever } from '../util/assertNever'
import { projectEditorLink } from '../util/links'
import { when } from '../util/react-conditionals'

export async function loader(args: LoaderFunctionArgs) {
  const user = await requireUser(args.request)

  const projects = await listProjects({
    ownerId: user.user_id,
  })

  const deletedProjects = await listDeletedProjects({
    ownerId: user.user_id,
  })

  return json({ projects, deletedProjects, user })
}

const Categories = ['allProjects', 'trash'] as const

export type Category = (typeof Categories)[number]

const categories: { [key in Category]: { name: string } } = {
  allProjects: { name: 'All My Projects' },
  trash: { name: 'Trash' },
}

const ProjectsPage = React.memo(() => {
  const marginSize = 30
  const rowHeight = 30

  const data = useLoaderData() as unknown as {
    projects: ProjectWithoutContent[]
    user: UserDetails
    deletedProjects: ProjectWithoutContent[]
  }

  const fetcher = useFetcher()

  const [projects, setProjects] = React.useState<ProjectWithoutContent[]>(data.projects)
  const [searchQuery, setSearchQuery] = useState<string>('')
  const [isDarkMode, setIsDarkMode] = useState<boolean>(false)

  const filteredProjects = React.useMemo(() => {
    const sanitizedQuery = searchQuery.trim().toLowerCase()
    if (sanitizedQuery.length === 0) {
      return projects
    }
    return projects.filter((project) => project.title.toLowerCase().includes(sanitizedQuery))
  }, [projects, searchQuery])

  const selectedProjectId = useStore((store) => store.selectedProjectId)
  const setSelectedProjectId = useStore((store) => store.setSelectedProjectId)
  const selectedCategory = useStore((store) => store.selectedCategory)
  const setCategory = useStore((store) => store.setSelectedCategory)

  const updateProjects = React.useCallback(
    (category: Category) => {
      switch (category) {
        case 'allProjects':
          setProjects(data.projects)
          break
        case 'trash':
          setProjects(data.deletedProjects)
          break
        default:
          assertNever(category)
      }
    },
    [data.projects, data.deletedProjects],
  )

  const handleEmptyTrash = React.useCallback(() => {
    const ok = window.confirm(
      'Are you sure? ALL projects in the trash will be deleted permanently.',
    )
    if (ok) {
      fetcher.submit({}, { method: 'POST', action: `/internal/projects/destroy` })
    }
  }, [fetcher])

  const handleProjectSelect = React.useCallback(
    (project: ProjectWithoutContent) => {
      if (project.deleted) {
        return
      }
      setSelectedProjectId(project.proj_id === selectedProjectId ? null : project.proj_id)
    },
    [selectedProjectId, setSelectedProjectId],
  )

  const udpateCategory = React.useCallback(
    (category: Category) => {
      setCategory(category)
      setSearchQuery('')
      setSelectedProjectId(null)
      updateProjects(category)
    },
    [selectedCategory, setSelectedProjectId, updateProjects],
  )

  const handleCategoryClick = React.useCallback(
    (category: React.SetStateAction<string>) => {
      udpateCategory(category as Category)
    },
    [udpateCategory],
  )

  // when the media query changes, update the theme
  React.useEffect(() => {
    const handleColorSchemeChange = (event: {
      matches: boolean | ((prevState: boolean) => boolean)
    }) => {
      setIsDarkMode(event.matches)
    }

    const mediaQuery = window.matchMedia('(prefers-color-scheme: dark)')
    setIsDarkMode(mediaQuery.matches)
    mediaQuery.addListener(handleColorSchemeChange)

    return () => {
      mediaQuery.removeListener(handleColorSchemeChange)
    }
  }, [])

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

  const logoPic = isDarkMode ? 'url(/assets/pyramid_dark.png)' : 'url(/assets/pyramid_light.png)'

  return (
    <div
      style={{
        margin: marginSize,
        height: `calc(100vh - ${marginSize * 2}px)`,
        width: `calc(100vw - ${marginSize * 2}px)`,
        gap: marginSize,
        overflow: 'hidden',
        boxSizing: 'border-box',
        display: 'flex',
        flexDirection: 'row',
        userSelect: 'none',
      }}
    >
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
              src={data.user.picture ?? undefined}
              referrerPolicy='no-referrer'
            />
            <div className={userName({})}>{data.user.name}</div>
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
              height: rowHeight,
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
                  onClick={() => handleCategoryClick(category)}
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
      <div
        style={{
          display: 'flex',
          flexGrow: 1,
          flexDirection: 'column',
          gap: marginSize,
        }}
      >
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
              fontSize: 16,
              fontWeight: 600,
              padding: '5px 10px',
            }}
          >
            <div>
              {when(
                searchQuery !== '',
                <span>
                  <span style={{ color: 'gray', paddingRight: 3 }}>
                    <span
                      onClick={() => {
                        setSearchQuery('')
                        const inputElement = document.getElementById(
                          'search-input',
                        ) as HTMLInputElement
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
            <div>
              {when(
                selectedCategory === 'trash',
                <button
                  className={button({ size: 'small' })}
                  onClick={handleEmptyTrash}
                  disabled={filteredProjects.length === 0}
                >
                  Empty trash
                </button>,
              )}
            </div>
          </div>
          <div style={{ display: 'flex', flexDirection: 'row' }}>
            <div>Sort</div>
          </div>
        </div>

        <div
          style={{
            display: 'flex',
            flexWrap: 'wrap',
            alignContent: 'flex-start',
            gap: marginSize,
            flexGrow: 1,
            flexDirection: 'row',
            overflowY: 'scroll',
            scrollbarColor: 'lightgrey transparent',
          }}
        >
          {filteredProjects.map((project) => (
            <ProjectCard
              key={project.proj_id}
              project={project}
              selected={project.proj_id === selectedProjectId}
              onSelect={() => handleProjectSelect(project)}
            />
          ))}
        </div>
      </div>
    </div>
  )
})
ProjectsPage.displayName = 'ProjectsPage'

export default ProjectsPage

type ProjectCardProps = {
  project: ProjectWithoutContent
  selected: boolean
  onSelect: () => void
}

const ProjectCard: React.FC<ProjectCardProps> = ({ project, selected, onSelect }) => {
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
        }}
        onMouseDown={onSelect}
        onDoubleClick={openProject}
      />
      <ProjectActions project={project} />
    </div>
  )
}

const ProjectActions = React.memo(({ project }: { project: ProjectWithoutContent }) => {
  return (
    <div style={{ display: 'flex', alignItems: 'center' }}>
      <div style={{ display: 'flex', flexDirection: 'column', padding: 10, gap: 5, flex: 1 }}>
        <div style={{ fontWeight: 600 }}>{project.title}</div>
        <div>{moment(project.modified_at).fromNow()}</div>
      </div>
      <div style={{ display: 'flex', alignItems: 'center', gap: 5 }}>
        <DropdownMenu.Root>
          <DropdownMenu.Trigger asChild>
            <button className={button()}>…</button>
          </DropdownMenu.Trigger>
          <ProjectContextMenu project={project} />
        </DropdownMenu.Root>
      </div>
    </div>
  )
})
ProjectActions.displayName = 'ProjectActions'
