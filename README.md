# Adrian Sieber

My personal website and blog.


## Development

Check out all available tasks in the [makefile].


## Deployment of Gitea

Running on DigitalOcean droplet (1CPU, 1GB, Ubuntu + Docker) and
available at <https://code.adriansieber.com>

1. Create DigitalOcean droplet
1. `ssh root@$IP_ADDRESS`
1. `vim docker-compose.yaml`
    ```yaml
    version: "2"
    networks:
      gitea:
        external: false
    services:
      server:
        image: gitea/gitea:1.4
        environment:
          - USER_UID=1000
          - USER_GID=1000
        restart: always
        networks:
          - gitea
        volumes:
          - ./gitea:/data
        ports:
          - "80:3000"
          - "222:22"
    ```
1. Disable firewall for port 80 and 443
    - `ufw allow 80/tcp`
    - `ufw allow 443/tcp`
