import { LoaderFunctionArgs, json } from "@remix-run/node";
import { useFetcher, useLoaderData } from "@remix-run/react";
import moment from "moment";
import { Project, UserDetails } from "prisma-client";
import React from "react";
import { listProjects } from "../models/project.server";
import { ensure, requireUser } from "../util/api.server";
import { Status } from "../util/statusCodes.server";
import { sprinkles } from "../styles/sprinkles.css";
import { button } from "../styles/button.css";
import { projectCategoryButton } from "../styles/projectCategoryButton.css";

const PAGINATION_LIMIT = 10;

export async function loader(args: LoaderFunctionArgs) {
  const user = await requireUser(args.request);

  const url = new URL(args.request.url);
  const offset = parseInt(url.searchParams.get("offset") ?? "0");
  ensure(offset >= 0, "offset cannot be negative", Status.BAD_REQUEST);

  const projects = await listProjects({
    ownerId: user.user_id,
    limit: PAGINATION_LIMIT,
    offset: offset,
  });

  return json({ projects, user });
}

const ProjectsPage = React.memo(() => {
  const marginSize = 30;
  const rowHeight = 30;

  const data = useLoaderData() as unknown as {
    projects: Project[];
    user: UserDetails;
  };

  const [projects, setProjects] = React.useState<Project[]>([]);

  const projectsFetcher = useFetcher();
  const [reachedEnd, setReachedEnd] = React.useState(false);

  const loadMore = React.useCallback(
    (offset: number) => () => {
      projectsFetcher.load(`?offset=${offset}`);
    },
    [],
  );

  const openProject = React.useCallback(
    (projectId: string) => () => {
      window.open(`${window.ENV.EDITOR_URL}/p/${projectId}`, "_blank");
    },
    [],
  );

  React.useEffect(() => {
    setProjects(data.projects);
    if (data.projects.length < PAGINATION_LIMIT) {
      setReachedEnd(true);
    }
  }, [data.projects]);

  React.useEffect(() => {
    if (projectsFetcher.data == null || projectsFetcher.state === "loading") {
      return;
    }
    const newProjects = (projectsFetcher.data as { projects: Project[] })
      .projects;
    setProjects((projects) => [...projects, ...newProjects]);
    if (newProjects.length < PAGINATION_LIMIT) {
      setReachedEnd(true);
    }
  }, [projectsFetcher.data]);

  return (
    <div
      style={{
        margin: marginSize,
        height: `calc(100vh - ${marginSize * 2}px)`,
        width: `calc(100vw - ${marginSize * 2}px)`,
        gap: marginSize,
        overflow: "hidden",
        boxSizing: "border-box",
        display: "flex",
        flexDirection: "row",
      }}
    >
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          width: 230,
          justifyContent: "space-between",
        }}
      >
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            gap: 20,
          }}
        >
          <div
            style={{
              display: "flex",
              alignItems: "center",
              gap: 10,
            }}
          >
            <img
              className={sprinkles({ borderRadius: "rounded" })}
              style={{ width: 40 }}
              src={data.user.picture ?? undefined}
              referrerPolicy="no-referrer"
            />
            <div style={{ fontSize: 12, fontWeight: 500 }}>
              {data.user.name}
            </div>
          </div>
          <div
            style={{
              height: rowHeight,
              borderBottom: "1px solid gray",
              display: "flex",
              flexDirection: "row",
              alignItems: "center",
              padding: "0 14px",
            }}
          >
            Search...
          </div>
          <div style={{ display: "flex", flexDirection: "column", gap: 5 }}>
            <button
              className={projectCategoryButton({
                color: "selected",
                size: "medium",
              })}
            >
              <span>All My Projects</span>
            </button>
            <button
              className={projectCategoryButton({
                color: "neutral",
                size: "medium",
              })}
            >
              <span>Private</span>
            </button>
            <button
              className={projectCategoryButton({
                color: "neutral",
                size: "medium",
              })}
            >
              <span>Public</span>
            </button>
            <button
              className={projectCategoryButton({
                color: "neutral",
                size: "medium",
              })}
            >
              <span>Shared With Me</span>
            </button>
            <button
              className={projectCategoryButton({
                color: "neutral",
                size: "medium",
              })}
            >
              <span>Trash</span>
            </button>
          </div>
        </div>
        <div
          style={{
            display: "flex",
            flexDirection: "row",
            alignItems: "center",
            gap: 14,
            fontFamily: "Reckless",
            fontSize: 34,
          }}
        >
          <div
            style={{
              height: 60,
              width: 45,
              backgroundSize: 45,
              backgroundRepeat: "no-repeat",
              backgroundImage:
                "url(https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_light.png?raw=true)",
            }}
          ></div>
          Utopia
        </div>
      </div>

      <div
        style={{
          display: "flex",
          flexGrow: 1,
          flexDirection: "column",
          background: "orange",
          overflowY: "scroll",
        }}
      >
        <table>
          <thead>
            <tr>
              <th>preview</th>
              <th>title</th>
              <th>modified</th>
              <th>owner</th>
              <th>actions</th>
            </tr>
          </thead>
          <tbody>
            {projects.map((project) => {
              return (
                <tr key={project.proj_id}>
                  <td>
                    <img
                      style={{ width: 100, height: 100 }}
                      className={sprinkles({ borderRadius: "rounded" })}
                      src={`/v1/thumbnail/${project.proj_id}`}
                    />
                  </td>
                  <td>{project.title}</td>
                  <td>{moment(project.modified_at).fromNow()}</td>
                  <td>
                    {project.owner_id === data.user.user_id
                      ? "You"
                      : "Somebody else"}
                  </td>
                  <td>
                    <button
                      className={button({
                        color: "accent",
                        size: "medium",
                      })}
                      onClick={openProject(project.proj_id)}
                    >
                      <span>↗️</span>
                      <span>Open</span>
                    </button>
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
        {!reachedEnd ? (
          <button
            className={button({ color: "accent", size: "medium" })}
            onClick={loadMore(projects.length)}
          >
            Load more
          </button>
        ) : null}
      </div>
    </div>
  );
});
ProjectsPage.displayName = "ProjectsPage";

export default ProjectsPage;
