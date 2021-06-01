# development

## Obelisk

Install obelisk as described in detail on the
[obelisk homepage](https://github.com/obsidiansystems/obelisk).
On NixOS I add the binary cache and do

    > nix-env -f https://github.com/obsidiansystems/obelisk/archive/master.tar.gz -iA command

## MySQL

You need a local sql database.
The connection info is defined in

    config/backend/params

## Json Web Key

Use this independent [simple code to generate a json web key](https://github.com/rubenmoor/generate-jwk/tree/master) and store it in the configuration:

    > path-to-binary/generate-jwk > config/backend/jwk

## run the code

Run local development server, it will appear on http://localhost:8000:

    > ob run --no-interpret .obelisk/impl

Note: The `--no-interpret` flag became necessary after I forked obelisk with

    > ob thunk unpack ./.obelisk/impl

in order to apply local changes.
These changes add a `mysql` service to the deployment
and do not affect obelisk at all.

# production

Choose a folder for your deployment.
Apparently this is preferably located next to the directory that has the code.

## initialize

    > mkdir ../deployment
    > ob deploy init --ssh-key $PATH_TO_PEM_FILE --hostname $EC2_URL --route $PAGE_URL --admin-email $SOME_EMAIL ../deployment
    > cd ../deployment

## configuration

In the deployment directory you find the `config/` folder with production configuration.
You have to provide a jwk file for production as well as database connection info.


## updating

The `ob deploy ...` commands rely on a version of the obelisk project
that has been pushed to some remote repository (i.e. github.com).
The whole process when updating the deployed code therefore goes like:

    > git commit ...
    > git push
    > cd ../deployment
    > ob deploy update

## actual deployment

    > ob deploy push
