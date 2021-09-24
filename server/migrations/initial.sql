CREATE TABLE "public"."persistent_session" (
    "key" character varying NOT NULL,
    "auth_id" "bytea",
    "session" "bytea" NOT NULL,
    "created_at" timestamp with time zone NOT NULL,
    "accessed_at" timestamp with time zone NOT NULL
);

CREATE TABLE "public"."project" (
    "id" bigint NOT NULL,
    "proj_id" character varying NOT NULL,
    "owner_id" character varying NOT NULL,
    "title" character varying NOT NULL,
    "created_at" timestamp with time zone NOT NULL,
    "modified_at" timestamp with time zone NOT NULL,
    "content" "bytea" NOT NULL,
    "deleted" boolean
);

CREATE TABLE "public"."project_i_d" (
    "id" bigint NOT NULL,
    "proj_id" character varying NOT NULL
);

CREATE SEQUENCE "public"."project_i_d_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE "public"."project_i_d_id_seq" OWNED BY "public"."project_i_d"."id";

CREATE SEQUENCE "public"."project_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE "public"."project_id_seq" OWNED BY "public"."project"."id";

CREATE TABLE "public"."showcase" (
    "id" bigint NOT NULL,
    "proj_id" character varying NOT NULL,
    "index" bigint NOT NULL
);

CREATE SEQUENCE "public"."showcase_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE "public"."showcase_id_seq" OWNED BY "public"."showcase"."id";

CREATE TABLE "public"."user_configuration" (
    "id" bigint NOT NULL,
    "user_id" character varying NOT NULL,
    "shortcut_config" character varying
);

CREATE SEQUENCE "public"."user_configuration_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE "public"."user_configuration_id_seq" OWNED BY "public"."user_configuration"."id";

CREATE TABLE "public"."user_details" (
    "id" bigint NOT NULL,
    "user_id" character varying NOT NULL,
    "email" character varying,
    "name" character varying,
    "picture" character varying
);

CREATE SEQUENCE "public"."user_details_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE "public"."user_details_id_seq" OWNED BY "public"."user_details"."id";

ALTER TABLE ONLY "public"."project" ALTER COLUMN "id" SET DEFAULT "nextval"('"public"."project_id_seq"'::"regclass");

ALTER TABLE ONLY "public"."project_i_d" ALTER COLUMN "id" SET DEFAULT "nextval"('"public"."project_i_d_id_seq"'::"regclass");

ALTER TABLE ONLY "public"."showcase" ALTER COLUMN "id" SET DEFAULT "nextval"('"public"."showcase_id_seq"'::"regclass");

ALTER TABLE ONLY "public"."user_configuration" ALTER COLUMN "id" SET DEFAULT "nextval"('"public"."user_configuration_id_seq"'::"regclass");

ALTER TABLE ONLY "public"."user_details" ALTER COLUMN "id" SET DEFAULT "nextval"('"public"."user_details_id_seq"'::"regclass");

ALTER TABLE ONLY "public"."persistent_session"
    ADD CONSTRAINT "persistent_session_pkey" PRIMARY KEY ("key");

ALTER TABLE ONLY "public"."project_i_d"
    ADD CONSTRAINT "project_i_d_pkey" PRIMARY KEY ("id");

ALTER TABLE ONLY "public"."project"
    ADD CONSTRAINT "project_pkey" PRIMARY KEY ("id");

ALTER TABLE ONLY "public"."showcase"
    ADD CONSTRAINT "showcase_pkey" PRIMARY KEY ("id");

ALTER TABLE ONLY "public"."project"
    ADD CONSTRAINT "unique_project" UNIQUE ("proj_id");

ALTER TABLE ONLY "public"."project_i_d"
    ADD CONSTRAINT "unique_project_i_d" UNIQUE ("proj_id");

ALTER TABLE ONLY "public"."user_configuration"
    ADD CONSTRAINT "unique_user_configuration" UNIQUE ("user_id");

ALTER TABLE ONLY "public"."user_details"
    ADD CONSTRAINT "unique_user_details" UNIQUE ("user_id");

ALTER TABLE ONLY "public"."user_configuration"
    ADD CONSTRAINT "user_configuration_pkey" PRIMARY KEY ("id");

ALTER TABLE ONLY "public"."user_details"
    ADD CONSTRAINT "user_details_pkey" PRIMARY KEY ("id");

