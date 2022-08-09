# MLDonkey: cross-platform multi-network peer-to-peer daemon

![Build](https://github.com/ygrek/mldonkey/workflows/Build/badge.svg?branch=master)

## Use with Docker

To create a docker image with the latest version for your platform (tested with `amd64` and `aarch64`), checkout this git repo and run:


```
docker build -t mldonkey:latest .
```

To run the container with docker-compose, edit the docker-compose.yml file to match your network and storage settings. Then run:

```
docker-compose up
```

Make sure the user id 1000 has write access to the volume mount point (`/srv/mlnet` in this example).


http://mldonkey.sourceforge.net
