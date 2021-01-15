CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL UNIQUE,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL
);
CREATE TABLE roles (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    rolename TEXT NOT NULL UNIQUE
);
CREATE TABLE userroles (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    refuser UUID NOT NULL,
    refrole UUID NOT NULL
);
CREATE TYPE history_type AS ENUM ('historytype_tariff', 'historytype_contract', 'historytype_partner');
CREATE TYPE workflow_type AS ENUM ('wftype_new', 'wftype_update');
CREATE TABLE workflows (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    refuser UUID DEFAULT uuid_generate_v4() NOT NULL,
    history_type history_TYPE NOT NULL,
    workflow_type workflow_type NOT NULL,
    progress JSONB NOT NULL,
    validfrom DATE NOT NULL,
    workflow_status TEXT DEFAULT 'initial' NOT NULL,
    createdat TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    progressb BYTEA NOT NULL
);
CREATE TABLE histories (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    latestversion INT DEFAULT 0 NOT NULL,
    history_type history_type NOT NULL
);
CREATE TABLE versions (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    refhistory UUID NOT NULL,
    validfrom DATE DEFAULT NOW() NOT NULL,
    createdat TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    "committed" BOOLEAN DEFAULT false NOT NULL
);
CREATE TABLE contracts (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    validfromversion INT NOT NULL,
    validthruversion INT DEFAULT NULL,
    refhistory UUID DEFAULT uuid_generate_v4() NOT NULL,
    content TEXT NOT NULL
);
CREATE TABLE partners (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    validfromversion INT NOT NULL,
    validthruversion INT DEFAULT NULL,
    refhistory UUID DEFAULT uuid_generate_v4() NOT NULL,
    content TEXT NOT NULL
);
CREATE TABLE tariffs (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    validfromversion INT NOT NULL,
    validthruversion INT DEFAULT NULL,
    refhistory UUID DEFAULT uuid_generate_v4() NOT NULL,
    content TEXT NOT NULL
);
ALTER TABLE contracts ADD CONSTRAINT contracts_ref_refhistory FOREIGN KEY (refhistory) REFERENCES histories (id) ON DELETE CASCADE;
ALTER TABLE contracts ADD CONSTRAINT contracts_ref_validfromversion FOREIGN KEY (validfromversion) REFERENCES versions (id) ON DELETE NO ACTION;
ALTER TABLE contracts ADD CONSTRAINT contracts_ref_validthruversion FOREIGN KEY (validthruversion) REFERENCES versions (id) ON DELETE SET NULL;
ALTER TABLE partners ADD CONSTRAINT partners_ref_refhistory FOREIGN KEY (refhistory) REFERENCES histories (id) ON DELETE CASCADE;
ALTER TABLE partners ADD CONSTRAINT partners_ref_validfromversion FOREIGN KEY (validfromversion) REFERENCES versions (id) ON DELETE NO ACTION;
ALTER TABLE partners ADD CONSTRAINT partners_ref_validthruversion FOREIGN KEY (validthruversion) REFERENCES versions (id) ON DELETE SET NULL;
ALTER TABLE userroles ADD CONSTRAINT userroles_ref_refrole FOREIGN KEY (refrole) REFERENCES roles (id) ON DELETE CASCADE;
ALTER TABLE userroles ADD CONSTRAINT userroles_ref_refuser FOREIGN KEY (refuser) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE versions ADD CONSTRAINT versions_ref_refhistory FOREIGN KEY (refhistory) REFERENCES histories (id) ON DELETE CASCADE;
ALTER TABLE workflows ADD CONSTRAINT workflows_ref_refuser FOREIGN KEY (refuser) REFERENCES users (id) ON DELETE NO ACTION;
