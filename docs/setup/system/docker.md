# Docker

You can use docker to compile the project as well.  Note that the Dockerfiles in the project often fall out of maitenance because no one on the main development team uses them, when this is noticed, they are deleted.

```sh
docker build -f docker/Ubuntu-20.04.Dockerfile -t jak .
```

This will create an image with all required dependencies and already built.

```sh
docker run -v "$(pwd)"/build:/home/jak/jak-project/build -it jak bash
```

Note: If you change the content of the `build/` directory you'll need to rerun the `docker build` command. Alternatively you can get the build via `docker cp`.

This will link your `build/` folder to the images so can validate your build or test it on an external device.

Docker images can be linked into your IDE (e.g. CLion) to help with codesniffing, static analysis, run tests and continuous build.

Unfortunately you'll still need task runner on your local machine to run the game or instead, manually run the game via the commands found in `Taskfile.yml`.
