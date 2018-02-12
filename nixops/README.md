# Dhall infrastructure

This directory contains a NixOps specification for hosting Dhall infrastructure,
which currently consists of:

* An IPFS mirror for the Dhall Prelude
    * This can be deployed by anybody without any special credentials or
      permissions as long as you're willing to pay for your own `t2.nano` AWS
      instance
* A Hydra server for Dhall CI
    * This requires an account with read/write access to the Dhall repositories
      and also requires access to the `hydra.dhall-lang.org` server.  This
      exists mainly to document the current deployment, but you can adapt the
      deployment to your own machines/projects/accounts if desired.

You can deploy your own copy of this infrastructure using the following
instructions:

*   Installing Nix by following these instructions:

    [https://nixos.org/nix/download.html](https://nixos.org/nix/download.html)

*   Installing the necessary toolchain, either permanently:

    ```bash
    $ nix-env --install --attr nixops
    $ nix-env --install --attr awscli
    ```

    ... or transiently:

    ```bash
    $ nix-shell --packages nixops awscli
    ```

*   Set up AWS credentials under the `default` profile:

    ```bash
    $ aws configure
    ```

*   Deploy the infrastructure using `nixops`:

    First, create the deployment specification:

    ```bash
    $ nixops create -d dhall logical.nix physical.nix
    ```

    THen you can deploy the IPFS server by running:
    ```bash
    $ EC2_ACCESS_KEY=default nixops deploy -d dhall --include ipfs
    ```

    ... and deploy the Hydra server (if you have the credentials to do so) by
    running:

    ```bash
    $ nixops deploy -d dhall --include hydra
    ```

*   Create an administrative user for `hydra`:

    ```bash
    $ nixops ssh -d dhall hydra hydra-create-user "${USERNAME}" --fullname "${FULL_NAME}" --email "${EMAIL}" --password "${PASSWORD}" --role admin
    ```

*   Get a personal access token for the
    [`@dhall-bot`](https://github.com/dhall-bot) user with the `repo` permission

*   Save the personal access token to `/etc/hydra/authorization` on the `hydra`
    machine:

    ```bash
    $ nixops ssh -d dhall hydra 'cat > /etc/hydra/authorization/dhall-lang' <<< "${TOKEN}"
    ```

*   Add `hydra` to the `hydra-queue-runner` user's known hosts:

    ```bash
    $ nixops ssh -d dhall hydra 'sudo -u hydra-queue-runner ssh -i /etc/keys/hydra-queue-runner/hydra-queue-runner_rsa hydra-queue-runner@hydra.dhall-lang.org'
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

*   You're done!
