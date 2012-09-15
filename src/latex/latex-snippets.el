;; latex-snippets.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 10 May 2012
;; Description:

(require 'yasnippet)


(defun yas/define-latex-snippets ()
  (labels ((def-snips (keys body &optional description)
             (loop
               for k in keys
               collect (list k
                             (if (listp body)
                               (mapconcat #'identity
                                          body
                                          "\n")
                               body)
                             description
                             nil
                             nil
                             nil
                             "latex-snippets.el"))))
    (yas/define-snippets
     major-mode
     (append
      (def-snips '("\\b" "beg" "\\beg" "begin" "\\begin")
          '("\\begin{${1:env}}"
            "    $0"
            "\\end{$1}")
        "\begin{...} ... \end{...}")
      (def-snips '("enum")
          '("\\begin{enumerate}"
            "  \\item $0"
            "\\end{enumerate}")
        "\begin{enumerate} ... \end{enumerate}")

      (def-snips '("eq")
          '("\\begin{equation}"
            "    $0"
            "\\end{equation}")
        "\begin{equation} ... \end{equation}")
      (def-snips '("eqn" "eq*")
          '("\\begin{equation*}"
            "    $0"
            "\\end{equation*}")
        "\begin{equation*} ... \end{equation*}")

      (def-snips '("eqa")
          '("\\begin{eqnarray}"
            "    $1 & $2 & $0"
            "\\end{eqnarray}")
        "\begin{eqnarray} ... \end{eqnarray}")
      (def-snips '("eqa*" "eqn")
          '("\\begin{eqnarray*}"
            "    $1 & $2 & $0"
            "\\end{eqnarray*}")
        "\begin{eqnarray*} ... \end{eqnarray*}")

      (def-snips '("frac" "\\frac")
        "\frac{${1:numerator}}{${2:denominator}}$0"
        "\frac{}{}")

      (def-snips '("figure")
          '("\\begin{figure}[htbp]"
            "  \\begin{center}"
            "    \\includegraphics[keepaspectratio=true,scale=${4:0.5}]{${1:filename}}"
            "  \\end{center}"
            "  \\caption{${2:caption}}"
            "  \\label{fig:${3:label}}"
            "\\end{figure}"
            "$0")
        "figure")

      (def-snips '("inline-listing")
          '("\\begin{flushleft}"
            "  Листинг \\ref{lst:${2:label}} -- ${1:name}"
            "\\end{flushleft}"
            "\\begin{lstlisting}[aboveskip=0cm,belowskip=0cm,label=lst:$2]"
            "${3:program text}"
            "\\end{lstlisting}")
        "inline-listing")))))




(provide 'latex-snippets)

;; Local Variables:
;; End:

;; latex-snippets.el ends here
