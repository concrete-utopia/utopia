import React, { useEffect, useState } from 'react'
import { LoaderFunctionArgs, json } from '@remix-run/node'
import { useFetcher, useLoaderData } from '@remix-run/react'
import moment from 'moment'
import { UserDetails } from 'prisma-client'
import { listProjects } from '../models/project.server'
import { ensure, requireUser } from '../util/api.server'
import { Status } from '../util/statusCodes.server'
import { sprinkles } from '../styles/sprinkles.css'
import { button } from '../styles/button.css'
import { projectCategoryButton } from '../styles/projectCategoryButton.css'
import { newProjectButton } from '../styles/newProjectButton.css'

const PAGINATION_LIMIT = 20

export async function loader(args: LoaderFunctionArgs) {
  const user = await requireUser(args.request)

  const url = new URL(args.request.url)
  const offset = parseInt(url.searchParams.get('offset') ?? '0')
  ensure(offset >= 0, 'offset cannot be negative', Status.BAD_REQUEST)

  const projects = await listProjects({
    ownerId: user.user_id,
    limit: PAGINATION_LIMIT,
    offset: offset,
  })

  return json({ projects, user })
}

type ProjectsPageState = {
  selectedProjectId: string | null
}

const ProjectsPage = React.memo(() => {
  const marginSize = 30
  const rowHeight = 30

  const [selectedProject, setSelectedProject] = useState<ProjectsPageState>({
    selectedProjectId: null,
  })

  const handleProjectSelect = (projectId: string) => {
    setSelectedProject({ selectedProjectId: projectId })
  }
  const clearSelectedProject = () => setSelectedProject({ selectedProjectId: null })

  const [selectedCategory, setSelectedCategory] = useState('All My Projects')

  const handleCategoryClick = (category: React.SetStateAction<string>) => {
    setSelectedCategory(category)
  }

  const data = useLoaderData() as unknown as {
    projects: Project[]
    user: UserDetails
  }

  const [searchValue, setSearchValue] = useState('')
  const [filteredProjects, setFilteredProjects] = useState<Project[]>(data.projects)

  const filterProjects = () => {
    if (searchValue === '') {
      setFilteredProjects(data.projects)
    } else {
      const filteredProjects = data.projects.filter((project) =>
        project.title.toLowerCase().includes(searchValue.toLowerCase()),
      )
      setFilteredProjects(filteredProjects)
    }
  }

  React.useEffect(() => {
    filterProjects()
  }, [searchValue, data.projects])

  const [projects, setProjects] = React.useState<Project[]>([])

  const projectsFetcher = useFetcher()
  const [reachedEnd, setReachedEnd] = React.useState(false)

  const loadMore = React.useCallback(
    (offset: number) => () => {
      projectsFetcher.load(`?offset=${offset}`)
    },
    [],
  )

  const createNewProject = () => {
    window.open(`/project/`, '_self')
  }

  React.useEffect(() => {
    setProjects(data.projects)
    if (data.projects.length < PAGINATION_LIMIT) {
      setReachedEnd(true)
    }
  }, [data.projects])

  React.useEffect(() => {
    if (projectsFetcher.data == null || projectsFetcher.state === 'loading') {
      return
    }
    const newProjects = (projectsFetcher.data as { projects: Project[] }).projects
    setProjects((projects) => [...projects, ...newProjects])
    if (newProjects.length < PAGINATION_LIMIT) {
      setReachedEnd(true)
    }
  }, [projectsFetcher.data])

  const categories = [
    { name: 'All My Projects', color: 'selected' },
    // { name: 'Private', color: 'neutral' },
    // { name: 'Public', color: 'neutral' },
    // { name: 'Shared With Me', color: 'neutral' },
    // { name: 'Trash', color: 'neutral' },
  ]

  const newProjectButtons = [
    {
      title: '+ Blank Project',
      onClick: createNewProject,
      color: 'orange',
    },
    // {
    //   title: '+ Project On GitHub',
    //   onClick: createNewProject,
    //   color: 'pink',
    // },
    // {
    //   title: '+ Import From GitHub',
    //   onClick: createNewProject,
    //   color: 'purple',
    // },
    // {
    //   title: '+ Remix Project',
    //   onClick: createNewProject,
    //   color: 'blue',
    // },
    // {
    //   title: '+ Shopify Store',
    //   onClick: createNewProject,
    //   color: 'green',
    // },
  ] as const

  const [isDarkMode, setIsDarkMode] = useState(false)

  useEffect(() => {
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

  const backgroundImage = isDarkMode
    ? 'url(https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_dark.png?raw=true)'
    : 'url(https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_light.png?raw=true)'

  const logoStyle = {
    height: 60,
    width: 45,
    backgroundSize: '45px',
    backgroundRepeat: 'no-repeat',
    backgroundImage: backgroundImage,
  }

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
      }}
    >
      <div
        onMouseDown={clearSelectedProject}
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
            <div style={{ fontSize: 12, fontWeight: 500 }}>{data.user.name}</div>
          </div>

          <input
            autoFocus={true}
            onChange={(e) => setSearchValue(e.target.value)}
            onKeyPress={(e) => {
              if (e.key === 'Enter') {
                // Call a function to filter the projects based on the search value
                filterProjects()
              }
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
            placeholder='Search...'
            // value={this.state.projectTitleFilter || ''}
          />
          <div style={{ display: 'flex', flexDirection: 'column', gap: 5 }}>
            {categories.map((category, index) => (
              <button
                key={index}
                className={projectCategoryButton({
                  color: category.name === selectedCategory ? 'selected' : 'neutral',
                })}
                onClick={() => handleCategoryClick(category.name)}
              >
                <span>{category.name}</span>
              </button>
            ))}
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
          <div style={logoStyle} />
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
          onMouseDown={clearSelectedProject}
          style={{
            height: 60,
            flex: 0,
            display: 'flex',
            flexDirection: 'row',
            gap: 15,
          }}
        >
          {newProjectButtons.map((p) => (
            <button className={newProjectButton({ color: p.color })} onClick={p.onClick}>
              <span>{p.title}</span>
            </button>
          ))}
        </div>
        <div
          onMouseDown={clearSelectedProject}
          style={{ fontSize: 16, fontWeight: 600, padding: '5px 10px' }}
        >
          {selectedCategory}
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
              project={project as Project}
              selected={project.proj_id === selectedProject.selectedProjectId}
              onSelect={() => handleProjectSelect(project.proj_id)}
            />
          ))}
          {!reachedEnd ? (
            <button onClick={loadMore(filteredProjects.length)} style={{ width: 200, height: 60 }}>
              Load More
            </button>
          ) : null}
        </div>
      </div>
    </div>
  )
})
ProjectsPage.displayName = 'ProjectsPage'

export default ProjectsPage

type Project = {
  proj_id: string
  title: string
  modified_at: Date
}

type ProjectCardProps = {
  project: Project
  selected: boolean
  onSelect: () => void
}

const ProjectCard: React.FC<ProjectCardProps> = ({ project, selected, onSelect }) => {
  const openProject = React.useCallback(() => {
    window.open(`${window.ENV.EDITOR_URL}/p/${project.proj_id}`, '_blank')
  }, [project.proj_id])

  return (
    <div
      key={project.proj_id}
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
      <div style={{ display: 'flex', flexDirection: 'column', padding: 10, gap: 5 }}>
        <div style={{ fontWeight: 600 }}>{project.title}</div>
        <div>{moment(project.modified_at).fromNow()}</div>
      </div>
    </div>
  )
}
