create table project_access {
    id serial primary key,
    project_id text not null references projects(project_id),
    access_level integer not null default 0,
    modified_at timestamp with time zone not null default now(),
}