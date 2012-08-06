(defsystem mobile-phone
  :depends-on (ol-utils
               cl-gtk2-gtk)
  :serial t
  :components ((:file "gtk-interface")))
