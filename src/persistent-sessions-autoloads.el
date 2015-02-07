;; persistent-sessions-autoloads.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  7 February 2015
;; Description:

(autoload 'sessions/load-buffers "persistent-sessions"
          "Load session from FILE's contents."
          t)

(autoload 'sessions/save-buffers "persistent-sessions"
          "Save all buffers that have physical file assigned into FILE."
          t)

(provide 'persistent-sessions-autoloads)

;; Local Variables:
;; End:

;; persistent-sessions-autoloads.el ends here
