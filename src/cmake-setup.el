;; cmake-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday,  6 November 2012
;; Description:

(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

(autoload 'cmake-mode "cmake-mode" nil t)

(defparameter cmake-keywords-re
  (rx (group
       symbol-start
       (or "add_custom_command"
           "add_custom_target"
           "add_definitions"
           "add_dependencies"
           "add_executable"
           "add_library"
           "add_subdirectory"
           "add_test"
           "aux_source_directory"
           "break"
           "build_command"
           "cmake_minimum_required"
           "cmake_policy"
           "configure_file"
           "create_test_sourcelist"
           "define_property"
           "else"
           "elseif"
           "enable_language"
           "enable_testing"
           "endforeach"
           "endfunction"
           "endif"
           "endmacro"
           "endwhile"
           "execute_process"
           "export"
           "file"
           "find_file"
           "find_library"
           "find_package"
           "find_path"
           "find_program"
           "fltk_wrap_ui"
           "foreach"
           "function"
           "get_cmake_property"
           "get_directory_property"
           "get_filename_component"
           "get_property"
           "get_source_file_property"
           "get_target_property"
           "get_test_property"
           "if"
           "include"
           "include_directories"
           "include_external_msproject"
           "include_regular_expression"
           "install"
           "link_directories"
           "list"
           "load_cache"
           "load_command"
           "macro"
           "mark_as_advanced"
           "math"
           "message"
           "option"
           "project"
           "qt_wrap_cpp"
           "qt_wrap_ui"
           "remove_definitions"
           "return"
           "separate_arguments"
           "set"
           "set_directory_properties"
           "set_property"
           "set_source_files_properties"
           "set_target_properties"
           "set_tests_properties"
           "site_name"
           "source_group"
           "string"
           "target_link_libraries"
           "try_compile"
           "try_run"
           "unset"
           "variable_watch"
           "while")
       symbol-end)
      (group
       (* whitespace)))
  "Regexp to mach CMake keywords followed by whitespace.")

(defun cmake-normalize-style ()
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward cmake-keywords-re
                                nil
                                t)
        (replace-match (concat (upcase (match-string-no-properties 1))
                               " ")
                       t)))))

(defun cmake-setup ()
  (init-common :use-yasnippet nil
               :use-whitespace 'tabs-only))

(add-hook 'cmake-mode-hook #'cmake-setup)


(provide 'cmake-setup)

;; Local Variables:
;; End:

;; cmake-setup.el ends here
