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
    <div>
      <div
        style={{
          display: "flex",
          alignItems: "center",
          gap: 10,
        }}
      >
        <img
          className={sprinkles({ borderRadius: "roundedFull" })}
          style={{ width: 36 }}
          src={data.user.picture ?? undefined}
          referrerPolicy="no-referrer"
        />
        <div>{data.user.email}</div>
      </div>
      <h1>Your projects</h1>
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
  );
});
ProjectsPage.displayName = "ProjectsPage";

export default ProjectsPage;
