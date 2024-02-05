import { Project } from "prisma-client";
import { prisma } from "../db.server";

const defaultLimit = 10;

export async function listProjects(params: {
  ownerId: string;
  offset?: number;
  limit?: number;
}): Promise<Project[]> {
  return prisma.project.findMany({
    where: { owner_id: params.ownerId },
    orderBy: { modified_at: "desc" },
    take: params.limit ?? defaultLimit,
    skip: params.offset,
  });
}
