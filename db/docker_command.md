If you want to run the database for development, use this docker command.
We'll change this to be done within docker compose later.

`docker run --name pda_database -d -p 27017:27017 mongo:latest`
