# Dhall infrastructure

This directory contains code for Dhall's shared infrastructure, including:

* [dhall-lang.org](https://dhall-lang.org) - The website that serves as an
  entrypoint to the language

* [discourse.dhall-lang.org](https://discourse.dhall-lang.org) - Our Discourse
  forum

* [prelude.dhall-lang.org](https://prelude.dhall-lang.org) - The standard
  Prelude

* [hydra.dhall-lang.org](https://hydra.dhall-lang.org) - Continuous integration
  (CI) server for Dhall repositories

* [cache.dhall-lang.org](https://cache.dhall-lang.org) - Binary cache serving
  anything built by CI

The only exception is:

* [docs.dhall-lang.org](https://docs.dhall-lang.org) - Documentation for the
  Dhall ecosystem

... whose code is located in the [`./docs`](../docs/README) subdirectory.

All of these services are hosted on the same server.

This infrastructure is managed using continuous delivery, meaning that the
server updates itself every hour to pick up changes merged into the `master`
branch of this repository.

That means that all you need to do to update any of these services is to create
a pull request and once your work is merged the server will auto-update itself
on the hour.

## Updating `dhall-lang.org`

To update the website, create a pull request to amend
[`./nixops/index.html`](./index.html).

You can build the website using Nix to test your changes locally before
submitting them.  Run the following command:

```bash
$ nix build --file ./release.nix website
```

... then open `./result/index.html` in your web browser.

## Updating `docs.dhall-lang.org`

To update the documentation, create a pull request to amend the
[`./docs`](../docs/README) directory.

You can build the documentation using Nix to test your changes locally before
submitting them.  Run the following command:

```bash
$ nix build --file ./release.nix docs
```

... then open `./result/index.html` in your web browser.

## Updating `{discourse,hydra,prelude,cache}.dhall-lang.org`

All of these services are configured in
[`./nixops/logical.nix`](./logical.nix), and you can update them by creating a
pull request to amend that file.

Verify that the machine configuration builds correctly by running this
command from the project root (ie `dhall-lang/`, not
`dhall-lang/nixops/`):

```bash
$ nix build --file ./release.nix machine
```

See the section [Testing](#testing) on how to run the built configuration locally
in a virtual machine.

If you have SSH access to the machine then you can also do a test deploy by
running:

```bash
$ ssh dhall-lang.org

$ nix-shell --packages git

$ git clone https://github.com/dhall-lang/dhall-lang.git

$ cd dhall-lang

$ git checkout "${THE_BRANCH_YOU_WANT_TO_TEST}"

$ nix build --file ./release.nix machine

$ sudo result/bin/switch-to-configuration test
```

## Obtaining SSH access to `dhall-lang.org`

Add yourself as an authorized user on the server by creating a pull request to
add your account and public SSH key here:

* [`users.users`](https://github.com/dhall-lang/dhall-lang/blob/b20476014e508be2218adbafa656996d0fd217bc/nixops/logical.nix#L532-L540)

After your pull request is merged and the server updates itself you should be
able to log in as:

```bash
$ ssh dhall-lang.org
```

... and you should have `sudo` privileges if you included yourself in the
`wheel` group.

## Testing

If you have a Linux machine with Nix installed, you can test the same
configuration inside of a VM using this script:

```bash
./scripts/test-vm.sh
```

You can then log into the VM using the `root` user with an empty password.

You can also forward a port from the guest machine to the host machine using the
following command:

```bash
QEMU_NET_OPTS="hostfwd=tcp::${HOST_PORT}-:${GUEST_PORT}" ./scripts/test-vm.sh 
```

For example, you can browse the website hosted by the VM by running the
following command:

```bash
QEMU_NET_OPTS='hostfwd=tcp::8443-:443' ./scripts/test-vm.sh 
```

... and then opening https://localhost:8443 in your browser.

## Bootstrapping

This section records how the `dhall-lang.org` server was bootstrapped in case we
somehow need to recreate everything from scratch:

*   Provision a NixOS machine with the following specs

    * 4 cores
    * 8 GB RAM
    * 160 GB disk
    * static IP address

    ... that you can `ssh` into as an account with `sudo` privileges

*   Set `dhall-lang.org` and all subdomains to point to the static IP address of
    the above server

    Note that currently @Gabriel439 owns the `dhall-lang.org` domain.  Contact
    him if you need to update the domain.

    If you are interested in setting up the same infrastructure underneath a
    different domain that you have purchased, then replace all references in
    the code and these instructions with your preferred domain name.

*   Run the following commands:

    ```bash
    $ ssh dhall-lang.org

    $ nix-shell --packages git

    $ git clone https://github.com/dhall-lang/dhall-lang.git

    $ cd dhall-lang

    $ nix build --file ./release.nix machine

    $ sudo result/bin/switch-to-configuration switch
    ```

    From this point on the server will track the `master` branch of this
    repository.

    This initial deployment step will enable some services but others will fail
    due to insufficient permissions, which the following steps will address.

*   Enable Discourse by saving the `discourse@dhall-lang.org` account password

    Save the account password using:

    ```bash
    $ echo -n "${PASSWORD}" > /var/lib/self-deploy/dhall-lang/nixops/discourseSmtpPassword
    ```

    If you are creating this account for the first time , then pick any password
    and then update the
    `mailserver.loginAccounts."discourse@dhall-lang.org".hashedPassword`
    NixOS configuration option in [`nixops/logical.nix`](./logical.nix) to
    match.

*   Get a personal access token for the
    [`@dhall-bot`](https://github.com/dhall-bot) user with the `repo`
    permission.  This token is used by CI to authenticate GitHub requests.

    This GitHub account is currently operated by @Gabriel439.  Ask him if you
    need to obtain access to this account.

    If you are setting up your own parallel infrastructure then you can use
    a personal access token for any GitHub account of your own.

*   Save the personal access token to `/etc/hydra/authorization` on the `hydra`
    machine:

    ```bash
    $ echo "${TOKEN}" > /etc/hydra/authorization/dhall-lang
    ```

*   Update NixOS configuration to match the SSH public key for `dhall-lang.org`

    Note that this configuration change needs to be merged into `master` by
    creating a pull request to modify the following option:

    * [`programs.ssh.knownHosts`](https://github.com/dhall-lang/dhall-lang/blob/b20476014e508be2218adbafa656996d0fd217bc/nixops/logical.nix#L106-L109)

*   Create an administrative user for `hydra`:

    ```bash
    $ hydra-create-user "${USERNAME}" --fullname "${FULL_NAME}" --email "${EMAIL}" --password "${PASSWORD}" --role admin
    ```

*   Log into [Hydra](https://hydra.dhall-lang.org) with the account created
    in the previous step

*   Create one Hydra project for each Dhall project using the following
    settings (replacing `${project}` with the project name, such as
    `dhall-haskell`):

    *   **Enabled**: True
    *   **Visible in the list of projects**: True
    *   **Identifier**: `${project}`
    *   **Display name**: `${project}`
    *   **Description**: `CI for for ${project}`
    *   **Owner**: `${USERNAME}`
    *   **Declarative spec file**: `${project}.json`
    *   **Declarative input**:
        *   **type**: `Local path`
        *   **value**: `/etc/hydra`

*   Restart any builds that fail due to "Output limit exceeded".  This is a
    harmless error

*   Set up the mail server to not be rejected as spam

    Follow the instructions here:

    * [`nixos-mailserver` - A Complete Setup Guide](https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/wikis/A-Complete-Setup-Guide)

*   Optional - Redeploy the server

    ... by running the following command on the server:

    ```bash
    $ sudo systemctl restart self-deploy
    ```

    Or just wait.  The `self-deploy` service runs every hour and updates the
    server to match the `master` branch of this repository.

*   Hopefully, you're done!

    There may have been setup steps that I have forgotten about.  If something
    is still not working check [`./nixops/logical.nix`](./logical.nix) to see
    if there are any hard-coded values that need to be fixed via pull
    requests.
