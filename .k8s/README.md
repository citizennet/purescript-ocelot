# Deploying into local Kubernetes cluster

If you've got a local Kubernetes cluster that you'd like to deploy Ocelot to, the files here should make that easy to do via `helm`.

First, if you haven't created a Docker build, run the following from the project root:

```bash
docker build -t ocelot-local .
```

With that out of the way, run the following to kick off a local deployment:

```bash
helm upgrade --install ocelot .k8s/ --set ingress.host=$(hostname),persistence.path=/home/$USER/src/purescript-ocelot
```

If you don't have `hostname` configured or your project repo is not set up in `~/src`, then you'll have to manually substitute the value for `ingress.host` with your hostname, and the value for `persistence.path` with your actual project location.

Configuring your `hostname` is as simple as:

```bash
hostname dude.dev.citizennet.com
hostname > /etc/hostname
```

If you don't want to mess with your system's `hostname` settings, just hardcode that part in the `helm install` command.

And that's it! You can verify your deployment by running `kubectl get pods` and looking for `ocelot-...`. To view the logs for either, run `kubectl logs <pod name> <container name>` where `<container name>` is either `ocelot` for the http server logs.

## Helm config options

There are a few options you can pass to `helm` via the `--set` flag when deploying:

- `ingress.host` **required**: the hostname for your local deployment, e.g. `dude.dev.citizennet.com`
- `persistence.path` **required**: the local path to your Ocelot repository, e.g. `/home/dude/src/purescript-ocelot`
