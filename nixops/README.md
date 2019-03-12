# Dhall infrastructure

This directory contains a NixOps specification for hosting Dhall infrastructure,
which currently consists of a Hydra server for Dhall's continuous integration.

Deploying the Hydra server requires an account with read/write access to the
Dhall repositories and also requires non-interactive `root` SSH access to the
`hydra.dhall-lang.org` server (currently provisioned out-of-band via Linode).
This directory exists mainly to document the current deployment, but you can
adapt the deployment to your own machines/projects/accounts if desired.

You can deploy your own copy of this infrastructure using the following
instructions:

*   Installing Nix by following these instructions:

    [https://nixos.org/nix/download.html](https://nixos.org/nix/download.html)

*   Installing the necessary toolchain, either permanently:

    ```bash
    $ nix-env --install --attr nixops
    ```

    ... or transiently:

    ```bash
    $ nix-shell --packages nixops
    ```

*   Provide the `discourse@dhall-lang.org` account password

    Save the account password using:

    ```bash
    $ echo -n "${PASSWORD}" > nixops/discourseSmtpPassword
    ```

    If you are creating this account for the first time , then pick any password
    and then update the
    `mailserver.loginAccounts."discourse@dhall-lang.org".hashedPassword`
    NixOS configuration option in `nixops/logical.nix` to match.

*   Deploy the infrastructure using `nixops`:

    First, create the deployment specification:

    ```bash
    $ nixops create -d dhall logical.nix physical.nix
    ```

    Then you can deploy the Hydra server by running:

    ```bash
    $ nixops deploy --deployment dhall
    ```

*   Create an administrative user for `hydra`:

    ```bash
    $ nixops ssh --deployment dhall hydra hydra-create-user "${USERNAME}" --fullname "${FULL_NAME}" --email "${EMAIL}" --password "${PASSWORD}" --role admin
    ```

*   Get a personal access token for the
    [`@dhall-bot`](https://github.com/dhall-bot) user with the `repo` permission

*   Save the personal access token to `/etc/hydra/authorization` on the `hydra`
    machine:

    ```bash
    $ nixops ssh --deployment dhall hydra 'cat > /etc/hydra/authorization/dhall-lang' <<< "${TOKEN}"
    ```

*   Add `hydra` to the `hydra-queue-runner` user's known hosts:

    ```bash
    $ nixops ssh --deployment dhall hydra 'sudo -u hydra-queue-runner ssh -i /etc/keys/hydra-queue-runner/hydra-queue-runner_rsa hydra-queue-runner@hydra.dhall-lang.org'
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

*   You're done!
