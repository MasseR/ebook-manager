CREATE TABLE public.users (
  email text NOT NULL,
  username text NOT NULL,
  "password" bytea NOT NULL,
  CONSTRAINT users_pkey PRIMARY KEY (email)
)
WITH (
  OIDS=FALSE
) ;
