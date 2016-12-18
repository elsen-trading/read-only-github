# read-only-github

Read only mirror of a single github project. Mostly me learning how to use servant and elm.

Use the [github projects api](https://developer.github.com/v3/projects/) to expose a single project to the web. The app is protected using basic authentication with the credentials set in the config file.

## Getting Started

1. Install [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Create the configuration file

  ```
  cp backend/sample.cfg backend/project.cfg
  < edit backend/project.cfg as you please >
  ```

3. Build and start the backend server

  ```
  cd backend
  stack build
  stack exec backend
  ```

4. Build and start the frontend server

  ```
  npm install -g elm
  elm package install
  npm install
  gulp
  ```

## TODO
- streamline deployment (better secret management, add nginx info)
- style date format
- style login view
- style issue table
