CREATE TABLE if not exists project_access (
    id serial,
    project_id text not null references project(proj_id) ON DELETE CASCADE,
    access_level integer not null default 0,
    modified_at timestamp with time zone not null default now()
);

ALTER TABLE ONLY "project_access" 
    ADD CONSTRAINT "unique_project_access" UNIQUE ("project_id");