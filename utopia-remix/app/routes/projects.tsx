import React, { useState } from 'react'
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

const PAGINATION_LIMIT = 10

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

const ProjectsPage = React.memo(() => {
  const marginSize = 30
  const rowHeight = 30

  const [selectedCategory, setSelectedCategory] = useState('All My Projects')

  const handleCategoryClick = (category: React.SetStateAction<string>) => {
    setSelectedCategory(category)
  }

  const data = useLoaderData() as unknown as {
    projects: Project[]
    user: UserDetails
  }

  const [projects, setProjects] = React.useState<Project[]>([])

  const projectsFetcher = useFetcher()
  const [reachedEnd, setReachedEnd] = React.useState(false)

  const loadMore = React.useCallback(
    (offset: number) => () => {
      projectsFetcher.load(`?offset=${offset}`)
    },
    [],
  )

  const openProject = React.useCallback(
    (projectId: string) => () => {
      window.open(`${window.ENV.EDITOR_URL}/p/${projectId}`, '_blank')
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
    { name: 'Private', color: 'neutral' },
    { name: 'Public', color: 'neutral' },
    { name: 'Shared With Me', color: 'neutral' },
    { name: 'Trash', color: 'neutral' },
  ]

  const newProjectButtons = [
    {
      title: '+ Blank Project',
      onClick: createNewProject,
      color: 'orange',
    },
    {
      title: '+ Project On GitHub',
      onClick: createNewProject,
      color: 'pink',
    },
    {
      title: '+ Import From GitHub',
      onClick: createNewProject,
      color: 'purple',
    },
    {
      title: '+ Remix Project',
      onClick: createNewProject,
      color: 'blue',
    },
    {
      title: '+ Shopify Store',
      onClick: createNewProject,
      color: 'green',
    },
  ] as const

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
          <div
            style={{
              height: rowHeight,
              borderBottom: '1px solid gray',
              display: 'flex',
              flexDirection: 'row',
              alignItems: 'center',
              padding: '0 14px',
            }}
          >
            Search...
          </div>
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
          <div
            style={{
              height: 60,
              width: 45,
              backgroundSize: 45,
              backgroundRepeat: 'no-repeat',
              backgroundImage:
                'url(https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_light.png?raw=true)',
            }}
          ></div>
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
        <div style={{ fontSize: 16, fontWeight: 600, padding: '5px 10px' }}>{selectedCategory}</div>
        <div
          style={{
            display: 'flex',
            flexWrap: 'wrap',
            gap: marginSize,
            flexGrow: 1,
            flexDirection: 'row',
            overflowY: 'scroll',
            scrollbarColor: 'lightgrey transparent',
          }}
        >
          {projects.map((project) => (
            <ProjectCard key={project.proj_id} project={project as Project} />
          ))}
          {!reachedEnd ? (
            <button
              className={button({ size: 'medium' })}
              onClick={loadMore(projects.length)}
              style={{ width: 300 }}
            >
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
}

const ProjectCard: React.FC<ProjectCardProps> = ({ project }) => {
  const openProject = React.useCallback(() => {
    window.open(`${window.ENV.EDITOR_URL}/p/${project.proj_id}`, '_blank')
  }, [project.proj_id])

  return (
    <div
      key={project.proj_id}
      style={{
        overflow: 'hidden',
        height: 200,
        width: 300,
        display: 'flex',
        flexDirection: 'column',
        gap: 5,
      }}
    >
      <div
        style={{
          border: '1px solid grey',
          borderRadius: 10,
          flex: 1,
          background: `linear-gradient(0deg, rgba(77, 255, 223, 0.4) 0%, rgba(255,250,220,.8) 100%)`,
        }}
        onClick={openProject}
      />
      <div style={{ display: 'flex', flexDirection: 'column', padding: 10, gap: 5 }}>
        <div style={{ fontWeight: 600 }}>{project.title}</div>
        <div>{moment(project.modified_at).fromNow()}</div>
      </div>
    </div>
  )
}
