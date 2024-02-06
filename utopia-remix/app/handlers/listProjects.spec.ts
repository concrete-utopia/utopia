import { prisma } from "../db.server";
import {
  createTestProject,
  createTestSession,
  createTestUser,
  truncateTable,
} from "../test-util";
import { ApiError, SESSION_COOKIE_NAME } from "../util/api.server";
import { handleListProjects } from "./listProjects";

describe("handleListProjects", () => {
  afterEach(async () => {
    // cleanup
    await truncateTable([
      prisma.projectID,
      prisma.project,
      prisma.userDetails,
      prisma.persistentSession,
    ]);
  });

  beforeEach(async () => {
    // seed
    await createTestUser(prisma, { id: "bob", name: "Bob Bobson" });
    await createTestUser(prisma, { id: "alice", name: "Alice Alisson" });
    await createTestProject(prisma, { id: "foo", ownerId: "bob" });
    await createTestProject(prisma, { id: "bar", ownerId: "bob" });
    await createTestProject(prisma, { id: "baz", ownerId: "alice" });
    await createTestProject(prisma, { id: "qux", ownerId: "bob" });
  });

  it("requires a session cookie", async () => {
    const req = new Request("http://localhost:8002/v1/projects");

    let error: any = null;
    try {
      await handleListProjects(req);
    } catch (err) {
      error = err;
    }

    expect(error instanceof ApiError).toBe(true);
    const apiError = error as ApiError;
    expect(apiError.status).toBe(401);
    expect(apiError.message).toBe("missing session cookie");
  });

  it("requires a user for the session cookie", async () => {
    const req = new Request("http://localhost:8002/v1/projects");
    req.headers.set("cookie", `${SESSION_COOKIE_NAME}=wrong`);

    let error: any = null;
    try {
      await handleListProjects(req);
    } catch (err) {
      error = err;
    }

    expect(error instanceof ApiError).toBe(true);
    const apiError = error as ApiError;
    expect(apiError.status).toBe(401);
    expect(apiError.message).toBe("session not found");
  });

  describe("with an authorized user", () => {
    beforeEach(async () => {
      await createTestSession(prisma, { key: "bobs-key", userId: "bob" });
    });

    it("returns the list of projects", async () => {
      const req = new Request("http://localhost:8002/v1/projects");
      req.headers.set("cookie", `${SESSION_COOKIE_NAME}=bobs-key`);
      const got = await handleListProjects(req);
      expect(got.projects.length).toBe(3);
      expect(
        got.projects.map((p) => ({
          id: p.id,
          ownerName: p.ownerName,
          title: p.title,
        })),
      ).toEqual([
        { id: "qux", ownerName: "Bob Bobson", title: "qux" },
        { id: "bar", ownerName: "Bob Bobson", title: "bar" },
        { id: "foo", ownerName: "Bob Bobson", title: "foo" },
      ]);
    });
  });
});
