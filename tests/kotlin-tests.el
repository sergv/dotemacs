;; kotlin-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  4 August 2024
;; Description:

(require 'kotlin-setup)
(require 'kotlin-ts-mode)
(require 'treesit-setup)

(require 'ert)
(require 'tests-utils)

(defun kotlin-tests/initialise-buffer ()
  (kotlin-ts-mode)
  (setq-local indent-tabs-mode nil))

(ert-deftest kotlin-tests/indentation-1 ()
  (skip-if-no-treesitter!)
  (tests-utils--test-buffer-contents
   :action
   (indent-region (point-min) (point-max))
   :contents
   (tests-utils--multiline
    "package test"
    ""
    "abstract class Test _|_{"
    "protected inner class Test2 {"
    "private fun controlSound(prefix : String, url: String, repeat: boolean) {"
    "if (replacedUrl != mSoundPlayer.currentAudioUri ||"
    "mSoundPlayer.isCurrentAudioFinished ||"
    "mSoundPlayer.isRepeat() != repeat"
    ")"
    "{"
    "onCurrentAudioChanged(replacedUrl, repeat)"
    "} else {"
    "mSoundPlayer.playOrPauseSound()"
    "}"
    "}"
    "}"
    "}")
   :expected-value
   (tests-utils--multiline
    "package test"
    ""
    "abstract class Test _|_{"
    "    protected inner class Test2 {"
    "        private fun controlSound(prefix : String, url: String, repeat: boolean) {"
    "            if (replacedUrl != mSoundPlayer.currentAudioUri ||"
    "                mSoundPlayer.isCurrentAudioFinished ||"
    "                mSoundPlayer.isRepeat() != repeat"
    "                )"
    "            {"
    "                onCurrentAudioChanged(replacedUrl, repeat)"
    "            } else {"
    "                mSoundPlayer.playOrPauseSound()"
    "            }"
    "        }"
    "    }"
    "}")
   :initialisation (kotlin-tests/initialise-buffer)
   :buffer-id kotlin-tests))

(provide 'kotlin-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; kotlin-tests.el ends here
