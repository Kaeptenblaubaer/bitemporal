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
    ref_user UUID NOT NULL,
    ref_role UUID NOT NULL
);
CREATE TYPE history_type AS ENUM ('historytype_tariff', 'historytype_contract', 'historytype_partner');
CREATE TYPE workflow_type AS ENUM ('wftype_new', 'wftype_update');
CREATE TABLE workflows (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    ref_user UUID DEFAULT uuid_generate_v4() NOT NULL,
    history_type history_TYPE NOT NULL,
    workflow_type workflow_type NOT NULL,
    progress JSONB NOT NULL,
    validfrom DATE NOT NULL,
    workflow_status TEXT DEFAULT 'initial' NOT NULL,
    createdat TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE TABLE histories (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    latestversion INT DEFAULT 0 NOT NULL,
    history_type history_type NOT NULL,
    ref_owned_by_workflow UUID DEFAULT uuid_generate_v4()
);
CREATE TABLE versions (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    ref_history UUID NOT NULL,
    validfrom DATE DEFAULT NOW() NOT NULL,
    createdat TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    "committed" BOOLEAN DEFAULT false NOT NULL,
    ref_shadowedby INT DEFAULT NULL
);
CREATE TABLE contract_states (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    ref_validfromversion INT NOT NULL,
    ref_validthruversion INT DEFAULT NULL,
    ref_history UUID DEFAULT uuid_generate_v4() NOT NULL,
    content TEXT NOT NULL
);
CREATE TABLE partner_states (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    ref_validfromversion INT NOT NULL,
    ref_validthruversion INT DEFAULT NULL,
    ref_history UUID DEFAULT uuid_generate_v4() NOT NULL,
    content TEXT NOT NULL
);
CREATE TABLE tariff_states (
    id BIGSERIAL PRIMARY KEY NOT NULL,
    ref_validfromversion INT NOT NULL,
    ref_validthruversion INT DEFAULT NULL,
    ref_history UUID DEFAULT uuid_generate_v4() NOT NULL,
    content TEXT NOT NULL
);
ALTER TABLE contract_states ADD CONSTRAINT contracts_ref_Validfromversion FOREIGN KEY (ref_validfromversion) REFERENCES versions (id) ON DELETE NO ACTION;
ALTER TABLE contract_states ADD CONSTRAINT contracts_ref_Validthruversion FOREIGN KEY (ref_validthruversion) REFERENCES versions (id) ON DELETE SET NULL;
ALTER TABLE contract_states ADD CONSTRAINT contracts_ref_history FOREIGN KEY (ref_history) REFERENCES histories (id) ON DELETE CASCADE;
ALTER TABLE histories ADD CONSTRAINT histories_ref_OwnedByWorkflow FOREIGN KEY (ref_owned_by_workflow) REFERENCES workflows (id) ON DELETE NO ACTION;
ALTER TABLE partner_states ADD CONSTRAINT partners_ref_Validfromversion FOREIGN KEY (ref_validfromversion) REFERENCES versions (id) ON DELETE CASCADE;
ALTER TABLE partner_states ADD CONSTRAINT partners_ref_history FOREIGN KEY (ref_history) REFERENCES histories (id) ON DELETE CASCADE;
ALTER TABLE partner_states ADD CONSTRAINT partners_ref_validthruversion FOREIGN KEY (ref_validthruversion) REFERENCES versions (id) ON DELETE SET NULL;
ALTER TABLE tariff_states ADD CONSTRAINT tariffs_ref_ref_history FOREIGN KEY (ref_history) REFERENCES histories (id) ON DELETE NO ACTION;
ALTER TABLE tariff_states ADD CONSTRAINT tariffs_ref_ref_validfromversion FOREIGN KEY (ref_validfromversion) REFERENCES versions (id) ON DELETE NO ACTION;
ALTER TABLE tariff_states ADD CONSTRAINT tariffs_ref_ref_validthruversion FOREIGN KEY (ref_validthruversion) REFERENCES versions (id) ON DELETE NO ACTION;
ALTER TABLE userroles ADD CONSTRAINT userroles_ref_refrole FOREIGN KEY (ref_role) REFERENCES roles (id) ON DELETE CASCADE;
ALTER TABLE userroles ADD CONSTRAINT userroles_ref_refuser FOREIGN KEY (ref_user) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE versions ADD CONSTRAINT versions_ref_refhistory FOREIGN KEY (ref_history) REFERENCES histories (id) ON DELETE CASCADE;
ALTER TABLE versions ADD CONSTRAINT versions_ref_shadowedby FOREIGN KEY (ref_shadowedby) REFERENCES versions (id) ON DELETE NO ACTION;
ALTER TABLE workflows ADD CONSTRAINT workflows_ref_refuser FOREIGN KEY (ref_user) REFERENCES users (id) ON DELETE NO ACTION;
