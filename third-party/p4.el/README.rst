=====
p4.el
=====
Perforce/Emacs integration


Introduction
------------
**p4.el** integrates the `Perforce`_ software version management system into `GNU Emacs`_. It provides Emacs interfaces that map directly to Perforce commands, and so is most useful if you are already familiar with Perforce and want to access it from Emacs. (If, on the other hand, you are already familiar with the Emacs `VC`_ interface, and want to add Perforce as a VC backend, then you might look at Jonathan Kamens’ `VC-P4`_.)

.. _Perforce: http://www.perforce.com/
.. _GNU Emacs: http://www.gnu.org/software/emacs/
.. _VC: http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html
.. _VC-P4: http://public.perforce.com/wiki/Emacs_VC-P4


Installation
------------
Download `p4.el`_. Then, in your ``.emacs``:

.. _p4.el: https://github.com/gareth-rees/p4.el/blob/master/p4.el

1. Ensure the directory containing ``p4.el`` is on your ``load-path``::

    (add-to-list 'load-path "/full/path/to/dir/containing/p4.el")

2. Load the library::

    (require 'p4)

By default, the P4 global key bindings start with ``C-x p``. If you
prefer a different key prefix, then you should customize the setting
``p4-global-key-prefix``.

To compile the Perforce help text into the Emacs documentation
strings for each command, you must byte-compile ``p4.el``::

    $ emacs -Q -batch -f batch-byte-compile p4.el


Use
---
*Note:* This section assumes that you are using the default key
prefix, ``C-x p``. If you’ve customized ``p4-global-key-prefix``, then
change the key sequences accordingly.

``p4.el`` provides an Emacs command for nearly all Perforce commands,
and they have the same name: for example the Perforce command ``p4
edit`` corresponds to the Emacs command ``p4-edit``. You can type
``C-x p C-h`` to see a list of all key bindings (but not every
Perforce command has a key binding).

Commands in ``p4.el`` operate on the “current” file by default -- this
is the file you’re visiting in the current buffer, if any; the marked
file(s) in a Dired buffer, if any; or the file on the current line in
a Dired buffer. But if they are given a prefix argument then you can
enter any arguments to the command. For example ``C-x p e`` opens the
current file for edit. But ``C-u C-x p e * RET`` opens all files in
the current directory for edit.

Commands that prompt you for a Perforce entity name provide ``TAB``
completion on the available entity names. (“Entities” include
branches, pending changelists, clients, filespecs, groups, jobs,
labels, and users.)

These are the most useful commands:

================  ============  ===========================================
Perforce command  Key sequence  Description
================  ============  ===========================================
``add``           ``C-x p a``   Open file for add.
``annotate``      ``C-x p V``   Annotate each line with the revision it was
                                last updated.
``client``        ``C-x p c``   Edit client workspace mapping.
``edit``          ``C-x p e``   Open file for edit.
``delete``        ``C-x p x``   Open file for delete.
``diff``          ``C-x p =``   Diff local file against the depot.
``filelog``       ``C-x p f``   Show revision history of file.
``move``          ``C-x p m``   Move (rename) a file that’s open for edit.
``opened``        ``C-x p o``   List open files.
``reconcile``     ``C-x p z``   Reconcile client with workspace changes.
``revert``        ``C-x p r``   Revert file, discarding local changes.
``status``        ``C-x p s``   Identify differences between the workspace
                                and the depot.
``submit``        ``C-x p S``   Submit changes to the depot.
``update``        ``C-x p g``   Get files from depot.
================  ============  ===========================================

Commands like ``submit`` and ``client`` open a form for editing in
Emacs. When done, submit the form to the Perforce server by typing
``C-c C-c``. In the special case of a ``submit`` form, you can change
your mind and type ``C-c C-p`` to save the change description as a
pending changelist (instead of submitting it); in the special case of
a ``change`` form, you can change your mind and type ``C-c C-s`` to
submit the change (instead of saving it).


Customization
-------------

Type ``M-x customize-group RET p4 RET`` to see all the options. The
most important options are ``p4-executable`` (the location of the
Perforce client executable, in case it can’t be found on your
``PATH``), and ``p4-global-key-prefix`` (the prefix for all Perforce
key bindings, in case the default ``C-x p`` is unsuitable).

If you run Perforce through a wrapper script or want to modify the
command line arguments passed to ``p4`` in some other way, you can use
the low-level ``p4-modify-args-function`` customization option. For
example::

    (defun modify-args-for-my-p4-wrapper (args)
      (cons "--my-wrapper-option" args))
    (setq p4-executable "/usr/bin/my-p4-wrapper"
          p4-modify-args-function #'modify-args-for-my-p4-wrapper)

If your workflow makes heavy use of numbered pending changelists, then
you may wish to turn on the option ``p4-open-in-changelist``, so that
you are prompted for a pending changelist number (with ``TAB``
completion) each time you open a file.


Keychain access
---------------

The user setting ``p4-password-source`` tells ``p4.el`` what to do
when Perforce requires a password. By default it prompts you for a
password, but it can be configured to fetch your password from your
system’s password manager.

#. On OS X, ``p4.el`` can automatically fetch your Perforce passwords
   from the login keychain. For each Perforce account, use Keychain
   Access to create an application password where the “Account” is the
   Perforce user name (``P4USER``) and “Where” is the Perforce server
   setting (``P4PORT``). Then in Emacs, customize the user setting
   ``p4-password-source`` and select “Fetch password from OS X
   Keychain.”

#. If your system is supported by the keyring_ Python module, then
   install that module (it is installed by default on Ubuntu) and for
   each Perforce account, run::

       python -c "import keyring,sys;keyring.set_password(*sys.argv[1:])" \
           P4PORT P4USER PASSWORD

   replacing ``P4PORT`` with the Perforce server setting, ``P4USER``
   with the Perforce user name, and ``PASSWORD`` with the password.
   Then in Emacs, customize the user setting ``p4-password-source``
   and select “Fetch password from Python keyring.”

.. _keyring: https://pypi.python.org/pypi/keyring


What’s new
----------

If you’ve been using the `old Emacs-P4`_ from SourceForge, then here
are the significant new and improved features in this version:

.. _old Emacs-P4: http://p4el.sourceforge.net/

- The “mode check”, and most user commands, run Perforce in the
  background, so that Emacs does not hang when the Perforce server is
  unavailable or when operations take a long time. You can control
  which commands get run in the background and which get run in the
  foreground by customizing the setting ``p4-synchronous-commands``.
- You no longer lose unsaved changes in a buffer when you run the
  ``p4-edit`` command. Instead, you are prompted as to whether you
  want to revert the buffer.
- If you are logged out of Perforce, running any Perforce command
  prompts you to log in.
- If your Perforce server is untrusted, running any Perforce command
  asks you whether you want to trust the server.
- When you revert a file with changes, you get shown the diffs that
  you are about to revert.
- Errors from Perforce commands are shown to you reliably.
- New interfaces to Perforce commands ``flush``, ``grep``, ``move``,
  ``reconcile``, ``status``, ``shelve``, ``tickets``, ``unshelve``,
  and ``update``.
- The ``p4-blame`` command makes use of ``p4 annotate`` if your server
  supports it, and so is much faster. The annotation also includes a
  snippet from the change description if there’s space.
- Diffs are opened in diff-mode, and you can jump from a line in a
  hunk to the corresponding line in the source.
- Support for Unicode characters. (If the Perforce server is not
  Unicode enabled, the UTF-8 encoding is used so that in future the
  server can be converted to Unicode.)

These features have been removed:

- The “notify” feature: superseded by the Perforce review daemon.
- ``p4-colorized-diffs``: superseded by ``global-font-lock-mode``.


License
-------
This program is free software; you can redistribute it and/or modify
it under the terms of the `GNU General Public License`_ as published by
the `Free Software Foundation`_; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the `GNU
General Public License`_ for more details.

.. _GNU General Public License: http://www.gnu.org/copyleft/gpl.html
.. _Free Software Foundation: http://www.fsf.org/
