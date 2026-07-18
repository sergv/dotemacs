This is yet another Emacs configuration among countless alternatives,
a speck of dust drifting in the ever expanding githubverse...

My daily tool, a treasure trove of things that somewhat work most of
the time and mostly work some of the time. There are many like it, but
this one is mine.

NOTE: No warranties, either express or implied, are hereby given. All
software is supplied as is, without guarantee.  The user assumes all
responsibility for damages resulting from the use of these features,
including, but not limited to, frustration, disgust, system abends, disk
head-crashes, general malfeasance, floods, fires, shark attack, nerve
gas, locust infestation, cyclones, hurricanes, tsunamis, local
electromagnetic disruptions, hydraulic brake system failure, invasion,
hashing collisions, normal wear and tear of friction surfaces, cosmic
radiation, inadvertent destruction of sensitive electronic components,
windstorms, the Riders of Nazgul, infuriated chickens, malfunctioning
mechanical or electrical sexual devices, premature activation of the
distant early warning system, peasant uprisings, halitosis, artillery
bombardment, explosions, cave-ins, and/or frogs falling from the sky.

## Environment variables
- `EMACS_ROOT`          - path
- `EMACS_COMPILED_ROOT` - path, where config compilation results go
- `EMACS_WRITABLE_ROOT` - path
- `EMACS_SYSTEM_TYPE`   - e.g. `(linux home)`
- `EMACS_ENV_DEFS`      - redirect .bashrc
- `EMACS_SKIP_ELC`      - `0` or `1`
- `EMACS_NIX_STORE_DIR` - unused legacy?
- `EMACS_NIX_STATE_DIR` - unused legacy?

## Environment variables take 2

   Emacs uses following environment variables for configuration:

   1. EMACS_ROOT - path to .emacs.d directory.
   2. EMACS_ENV_DEFS - paths to .bash_env file - shell script that sets up environment variables on the system for current user.
   3. BASHRC_ENV_LOADED - whether ~/.bash_env was already loaded.
   4. EMACS_COMPILED_ROOT
