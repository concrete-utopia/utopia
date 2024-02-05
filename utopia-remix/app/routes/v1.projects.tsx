import { LoaderFunctionArgs } from "@remix-run/node";
import { listProjects } from "../models/project.server";
import { getManyUserDetails } from "../models/userDetails.server";
import { ensure, handle, handleOptions } from "../util/api.server";
import { Status } from "../util/statusCodes.server";
import { ListProjectsResponse } from "../types";

export async function loader(args: LoaderFunctionArgs) {
  return handle(args.request, {
    OPTIONS: handleOptions,
    GET: handleListProjects,
  });
}

export async function handleListProjects(): Promise<ListProjectsResponse> {
  const projects = await listProjects({});

  const userIds = new Set(projects.map((p) => p.owner_id));
  const userDetails = await getManyUserDetails(Array.from(userIds));

  const projectsWithOwnerData = projects.map((p) => {
    const user = userDetails.find((details) => details.user_id === p.owner_id);
    ensure(user != null, "owner not found", Status.NOT_FOUND);

    return {
      id: p.proj_id,
      ownerName: user?.name ?? null,
      ownerPicture: user?.picture ?? null,
      title: p.title,
      description: null,
      createdAt: p.created_at.toISOString(),
      modifiedAt: p.modified_at.toISOString(),
    };
  });

  return {
    projects: projectsWithOwnerData,
  };
}
