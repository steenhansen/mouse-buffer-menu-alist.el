   
;; start mouse-1 click menu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(defun menu-read-modifed (a-buff)
 (let ((read-only-char "")
       (modified-char ""))
    (with-current-buffer a-buff
      (if buffer-read-only 
        (setq read-only-char "%")))
    (if (buffer-modified-p a-buff)
      (setq modified-char "*"))
    (concat read-only-char modified-char)))

(defun label-core-clj(a-buff) 
  "format clojure files, highlight if starts with 'core'"
  (format "%1$s%2$s%3$s%4$s" 
    (menu-read-modifed a-buff)
    (if (string-prefix-p "test" (buffer-name a-buff) ) 
      "      " 
      "")                     ;                  test_file.clj
    (buffer-name a-buff)
    (if (string-prefix-p "core" (buffer-name a-buff) )
      " ***" 
      "")))                   ; core.clj ***

(defun menu-no-case-sort (buff-1 buff-2)
  "ensure mouse-buffer-menu is sorted with no-case"
  (string> (downcase (buffer-name buff-1)) (downcase (buffer-name buff-2))))

(defun menu-is-ag (name-of-buffer)
  "is the buffer an ag-search buffer?"
  (if (string-prefix-p "*ag " name-of-buffer)
     t
     nil))
     
(defun menu-is-clj (name-of-buffer)
  "is the buffer a clojure buffer?"
  (if (string-suffix-p ".clj" name-of-buffer)
     t
     nil))     
     
(defun menu-no-start-blank (name-of-buffer)
  "ignore buffers starting with white space"
  (if (/= (aref name-of-buffer 0) ?\s)
     t
     nil))
      
(defun label-default(a-buffer) 
  "format mouse-buffer entry as a default file"
  (format "  %1$s%2$s%3$s" 
    (menu-read-modifed a-buffer)
    (buffer-name a-buffer)    ; readme.txt ---
    " ---"))                 
     
(defun label-search(name-of-buffer) 
  "format mouse-buffer entry as an ag-search"
    (setq split-list (split-string name-of-buffer "text:\\| dir:"))
    (setq search-for (nth 1 split-list))
    (format "%1$s%2$s" 
      "~~~search : " 
      search-for))
      
(defun menu-buff-show (name-of-buffer)
  "ignore emacs buffers on mouse-buffer-menu"
  (if (or (equal name-of-buffer "*dashboard*" )
          (equal name-of-buffer "*scratch*")
          (string-prefix-p "*nrepl-server _clojure/" name-of-buffer)  
          (string-prefix-p ":" name-of-buffer  )
          (equal name-of-buffer "*:Buffers:*")
          (equal name-of-buffer "*Messages*"))
      nil
      t))     
     
(defun mouse-buffer-menu-alist (the-buffers)      
  "Buffer menu displayed when control left click"
  (let ((mouse-buf-menu)                                                              
        (the-dashboard-buffer))
    (setq sorted-buffers (sort the-buffers `menu-no-case-sort))
    (dolist (a-buffer sorted-buffers mouse-buf-menu)
      (progn
        (setq name-of-buffer (buffer-name a-buffer))
        (if (eq "*dashboard*" name-of-buffer)
          (setq the-dashboard-buffer a-buffer))
        (if (menu-buff-show name-of-buffer)
          (if (menu-no-start-blank name-of-buffer)
            (progn
              (setq menu-label (cond 
              	((menu-is-ag name-of-buffer)  (label-search name-of-buffer))         
                ((menu-is-clj name-of-buffer) (label-core-clj a-buffer))                
                (t                            (label-default a-buffer))))
              (setq menu-entry (cons menu-label a-buffer))                       
              (setq mouse-buf-menu (cons menu-entry mouse-buf-menu)))))))
    (if (eq 0 (length mouse-buf-menu))
        (cons "*dashboard*" the-dashboard-buffer) ; if dashboard only buffer
        mouse-buf-menu)))
   
(setq mouse-buffer-menu-mode-mult 1234)  ;; deter sub-menus on control-click 
