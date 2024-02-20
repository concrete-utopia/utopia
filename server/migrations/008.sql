CREATE TABLE project_access (
    id serial,
    project_id text not null references project(proj_id),
    access_level integer not null default 0,
    modified_at timestamp with time zone not null default now()
)