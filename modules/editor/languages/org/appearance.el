
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  )

(use-package org-pretty-table
  :straight ( :host github :repo "fuco1/org-pretty-table")
  :hook (org-mode . org-pretty-table-mode)
  )
