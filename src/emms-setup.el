;; emms-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  1 January 2012
;; Keywords:
;; Requirements:
;; Status:

(require 'set-up-paths)
(add-to-list 'load-path (path-concat +emacs-standalone-path+
                                     "emms"
                                     "lisp"))

(setf emms-cache-file (concat +prog-data-path+ "/emms/cache")
      emms-directory (concat +prog-data-path+ "/emms"))

(require 'emms-setup)
(emms-standard)
(emms-default-players)

(defun emms-setup-mode ()
  (def-keys-for-map emms-playlist-mode-map
    +control-x-prefix+
    ("u"        emms-playlist-mode-undo)
    ("t"        next-line)
    ("n"        previous-line)
    ("s"        nil)
    ("h"        nil)
    ("<left>"   nil)
    ("<right>"  nil)
    ("<up>"     previous-line)
    ("<down>"   next-line)
    ("p"        emms-playlist-mode-yank)

    ("SPC"      emms-pause)
    ("C-k"      nil)
    ("k"        emms-playlist-mode-kill-track)
    ("K"        emms-playlist-mode-kill-entire-track)
    ("d"        emms-playlist-mode-kill-track)
    ("D"        emms-playlist-mode-kill-entire-track)

    (","        nil)
    (", b"      ibuffer)
    (";"        vim:ex-read-command)

    ("g x"      smex)
    ("g X"      smex-major-mode-commands)
    ("g f"      find-file)
    ("g <"      vim-mock:motion-go-to-first-non-blank-beg)
    ("g >"      vim-mock:motion-go-to-first-non-blank-end)
    ("g k"      remove-buffer)
    ("g K"      remove-buffer-and-window)

    ("z"        nil)
    ("z t"      vim-mock:scroll-line-to-top)
    ("z z"      vim-mock:scroll-line-to-center)
    ("z b"      vim-mock:scroll-line-to-bottom)

    ("<insert>" vim:scroll-line-up)
    ("<delete>" vim:scroll-line-down)))

(add-hook 'emms-playlist-mode-hook #'emms-setup-mode)


(setf *elisp-do-not-move-files*
      (append *elisp-do-not-move-files*
              '("emms-cue.el"
                "emms-lastfm-client.el"
                "emms-playlist-mode.el"
                "emms-player-xine.el"
                "emms-player-vlc.el"
                "emms-score.el"
                "emms-playlist-sort.el"
                "emms-playlist-limit.el"
                "jack.el"
                "emms-player-mplayer.el"
                "emms-history.el"
                "emms-player-mpd.el"
                "emms-auto.el"
                "emms-url.el"
                "emms-streams.el"
                "emms-info-mp3info.el"
                "emms-tag-editor.el"
                "emms-browser.el"
                "emms-i18n.el"
                "emms-setup.el"
                "emms.el"
                "emms-maint.el"
                "emms-info-metaflac.el"
                "emms-stream-info.el"
                "emms-volume-amixer.el"
                "emms-lastfm-scrobbler.el"
                "emms-bookmarks.el"
                "emms-last-played.el"
                "emms-metaplaylist-mode.el"
                "emms-player-simple.el"
                "emms-info-ogginfo.el"
                "emms-info-libtag.el"
                "emms-source-playlist.el"
                "emms-source-file.el"
                "later-do.el"
                "emms-player-mpg321-remote.el"
                "emms-playing-time.el"
                "emms-mode-line-icon.el"
                "emms-lyrics.el"
                "emms-mark.el"
                "emms-mode-line.el"
                "emms-info.el"
                "emms-compat.el"
                "emms-cache.el"
                "emms-volume.el"
                "tq.el")))

;; Local Variables:
;; End:

;; emms-setup.el ends here
