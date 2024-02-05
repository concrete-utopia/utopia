import { LoaderFunctionArgs, json } from "@remix-run/node";
import React from "react";
import { listProjects } from "../models/project.server";
import { useLoaderData } from "@remix-run/react";
import { requireUser } from "../util/api.server";

export async function loader(args: LoaderFunctionArgs) {
  const user = await requireUser(args.request);
  const projects = await listProjects({ ownerId: user.user_id });
  return json({ projects });
}

const ProjectsPage = React.memo(() => {
  const data = useLoaderData<typeof loader>();
  return (
    <div>
      <h1>Projects</h1>
      <div>
        {data.projects.map((project) => {
          return <div key={project.id}>{project.title}</div>;
        })}
      </div>
    </div>
  );
});
ProjectsPage.displayName = "ProjectsPage";

export default ProjectsPage;
