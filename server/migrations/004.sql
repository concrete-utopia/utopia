CREATE TABLE "public"."project_collaboration" (
    "project_id" character varying NOT NULL,
    "collaboration_editor" character varying NOT NULL,
    "last_seen_timeout" timestamp with time zone NOT NULL
);

CREATE INDEX "project_collaboration_project_id_index" ON "project_collaboration" ("project_id");

CREATE INDEX "project_collaboration_collaboration_editor_index" ON "project_collaboration" ("collaboration_editor");
