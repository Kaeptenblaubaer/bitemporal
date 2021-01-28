

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.histories DISABLE TRIGGER ALL;

INSERT INTO public.histories (id, latestversion, history_type) VALUES ('ab9e7316-8f63-4c18-ac28-15a0a87d1d4f', 0, 'historytype_contract');


ALTER TABLE public.histories ENABLE TRIGGER ALL;


ALTER TABLE public.versions DISABLE TRIGGER ALL;

INSERT INTO public.versions (id, refhistory, validfrom, createdat, committed) VALUES (7, 'ab9e7316-8f63-4c18-ac28-15a0a87d1d4f', '2021-01-28', '2021-01-28 12:13:36.932894+01', false);
INSERT INTO public.versions (id, refhistory, validfrom, createdat, committed) VALUES (8, 'ab9e7316-8f63-4c18-ac28-15a0a87d1d4f', '2021-01-30', '2021-01-28 12:14:02.861046+01', false);


ALTER TABLE public.versions ENABLE TRIGGER ALL;


ALTER TABLE public.contracts DISABLE TRIGGER ALL;

INSERT INTO public.contracts (id, validfromversion, validthruversion, refhistory, content) VALUES (7, 7, NULL, 'ab9e7316-8f63-4c18-ac28-15a0a87d1d4f', 'a');
INSERT INTO public.contracts (id, validfromversion, validthruversion, refhistory, content) VALUES (8, 8, NULL, 'ab9e7316-8f63-4c18-ac28-15a0a87d1d4f', 'aa');


ALTER TABLE public.contracts ENABLE TRIGGER ALL;


ALTER TABLE public.partners DISABLE TRIGGER ALL;



ALTER TABLE public.partners ENABLE TRIGGER ALL;


ALTER TABLE public.roles DISABLE TRIGGER ALL;



ALTER TABLE public.roles ENABLE TRIGGER ALL;


ALTER TABLE public.tariffs DISABLE TRIGGER ALL;



ALTER TABLE public.tariffs ENABLE TRIGGER ALL;


ALTER TABLE public.users DISABLE TRIGGER ALL;

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts) VALUES ('8a3639ac-aa00-49f4-82fd-cf5b6dea0c3d', 'm.f@hamburg.de', 'sha256|17|sUxRVj5bbutZnMi4EANj0A==|Br+dKaTVmReuWCvs5xX2dQEvxussBdDJc1u6CJ8W/dI=', NULL, 0);


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.userroles DISABLE TRIGGER ALL;



ALTER TABLE public.userroles ENABLE TRIGGER ALL;


ALTER TABLE public.workflows DISABLE TRIGGER ALL;

INSERT INTO public.workflows (id, refuser, history_type, workflow_type, progress, validfrom, workflow_status, createdat, progressb) VALUES ('d278d58f-2a7a-4027-a0fa-f3c44b71fdbf', '8a3639ac-aa00-49f4-82fd-cf5b6dea0c3d', 'historytype_contract', 'wftype_new', '{"partner": null, "contract": {"state": 7, "history": "ab9e7316-8f63-4c18-ac28-15a0a87d1d4f", "version": 7}}', '2021-01-28', 'initial', '2021-01-28 12:13:31.641867+01', '\x');
INSERT INTO public.workflows (id, refuser, history_type, workflow_type, progress, validfrom, workflow_status, createdat, progressb) VALUES ('c9e7df48-48bf-4bb9-80c0-94e3ab3d0aaa', '8a3639ac-aa00-49f4-82fd-cf5b6dea0c3d', 'historytype_contract', 'wftype_update', '{"partner": null, "contract": {"state": null, "history": "ab9e7316-8f63-4c18-ac28-15a0a87d1d4f", "version": 8}}', '2021-01-30', 'initial', '2021-01-28 12:13:54.294846+01', '\x');
INSERT INTO public.workflows (id, refuser, history_type, workflow_type, progress, validfrom, workflow_status, createdat, progressb) VALUES ('2e45cf3d-424c-41d9-a0e6-5a4d0fb40245', '8a3639ac-aa00-49f4-82fd-cf5b6dea0c3d', 'historytype_contract', 'wftype_update', '{"partner": null, "contract": {"state": null, "history": "ab9e7316-8f63-4c18-ac28-15a0a87d1d4f", "version": null}}', '2021-01-29', 'initial', '2021-01-28 12:14:34.2122+01', '\x');


ALTER TABLE public.workflows ENABLE TRIGGER ALL;


SELECT pg_catalog.setval('public.contracts_id_seq', 8, true);



SELECT pg_catalog.setval('public.partners_id_seq', 1, false);



SELECT pg_catalog.setval('public.tariffs_id_seq', 1, false);



SELECT pg_catalog.setval('public.versions_id_seq', 8, true);



