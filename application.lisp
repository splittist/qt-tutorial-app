(defpackage #:qt-tutorial-app
  (:use #:cl #:qt)
  (:export #:main))

(in-package #:qt-tutorial-app)
(named-readtables:in-readtable :qt)

(defvar *qapp*)

(defclass main-window ()
  ((current-file :accessor current-file)
   (text-edit :accessor text-edit)
   (file-menu :accessor file-menu)
   (edit-menu :accessor edit-menu)
   (help-menu :accessor help-menu)
   (file-tool-bar :accessor file-tool-bar)
   (edit-tool-bar :accessor edit-tool-bar)
   (new-action :accessor new-action)
   (open-action :accessor open-action)
   (save-action :accessor save-action)
   (save-as-action :accessor save-as-action)
   (exit-action :accessor exit-action)
   (cut-action :accessor cut-action)
   (copy-action :accessor copy-action)
   (paste-action :accessor paste-action)
   (about-action :accessor about-action)
   (about-qt-action :accessor about-qt-action))
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow")
  (:slots ("newFile()" (lambda (this) 
                         (when (maybe-save this)
                           (#_clear (text-edit this))
                           (setf (current-file this) ""))))
          ("open()" (lambda (this)
                      (when (maybe-save this)
                        (let ((file-name (#_getOpenFileName "QFileDialog" this)))
                          (unless (string= file-name "")
                            (load-file this file-name))))))
          ("save()" (lambda (this)
                      (if (string= (current-file this) "")
                        (#_saveAs this)
                        (save-file this (current-file this)))))
          ("saveAs()" (lambda (this)
                        (let ((file-name (#_getSaveFileName "QFileDialog" this)))
                          (unless (string= file-name "")
                            (save-file this file-name)))))
          ("about()" (lambda (this)
                       (#_about "QMessageBox" this "About Application"
                        "The <b>Application</b> example demonstrates how to write modern GUI applications using QT, with a menu bar, toolbars, and a status bar.")))
          ("documentWasModified()" (lambda (this)
                                     (#_setWindowModified this
                                      (#_isModified (#_document (text-edit this)))))))
  (:override ("closeEvent" (lambda (this event)
                             (cond ((maybe-save this)
                                    (write-settings this)
                                    (#_accept event))
                                   (t (#_ignore event)))))))

(defmethod initialize-instance :after ((instance main-window) &key parent)
  (if parent
    (new instance parent)
    (new instance))
  (setf (text-edit instance) (#_new QPlainTextEdit)
        (current-file instance) "")
  (#_setCentralWidget instance (text-edit instance))
  (create-actions instance)
  (create-menus instance)
  (create-tool-bars instance)
  (create-status-bar instance)
  (read-settings instance)
  (#_connect "QObject"
             (#_document (text-edit instance)) (QSIGNAL "contentsChanged()")
             instance (QSLOT "documentWasModified()"))
  (setf (current-file instance) ""))

(defun create-action (instance name slot-owner slot
                           &key icon shortcut status-tip (signal "triggered()"))
  (let ((action (if icon
                  (#_new QAction (#_new QIcon icon) name instance)
                  (#_new QAction name instance))))
    (when shortcut (#_setShortcut action (#_new QKeySequence shortcut)))
    (when status-tip (#_setStatusTip action status-tip))
    (#_connect "QObject"
               action (QSIGNAL signal)
               slot-owner (QSLOT slot))
    action))

(defun create-actions (instance)
  (setf (new-action instance)
        (create-action instance "&New" instance "newFile()"
                       :shortcut "Ctrl+N"
                       :icon "images/new.png"
                       :status-tip "Create a new file"))
  (setf (open-action instance)
        (create-action instance "&Open..." instance "open()"
                       :shortcut "Ctrl+O"
                       :icon "images/open.png"
                       :status-tip "Open an existing file"))
  (setf (save-action instance)
        (create-action instance "&Save" instance "save()"
                       :shortcut "Ctrl+S"
                       :icon "images/save.png"
                       :status-tip "Save the document to disk"))
  (setf (save-as-action instance)
        (create-action instance "Save &As..." instance "saveAs()"
                       :status-tip "Save the document under a new name"))
  (setf (exit-action instance)
        (create-action instance "E&xit" instance "close()"
                       :shortcut "Ctrl+Q"
                       :status-tip "Exit the application"))
  (setf (cut-action instance)
        (create-action instance "Cu&t" (text-edit instance) "cut()"
                       :shortcut "Ctrl+X"
                       :icon "images/cut.png"
                       :status-tip "Cut the current selection's contents to the clipboard"))
  (setf (copy-action instance)
        (create-action instance "&Copy" (text-edit instance) "copy()"
                       :shortcut "Ctrl+C"
                       :icon "images/copy.png"
                       :status-tip "Copy the current selection's contents to the clipboard"))
  (setf (paste-action instance)
        (create-action instance "&Paste" (text-edit instance) "paste()"
                       :shortcut "Ctrl+V"
                       :icon "images/paste.png"
                       :status-tip "Paste the clipboard's contents into the current selection"))
  (setf (about-action instance)
        (create-action instance "&About" instance "about()"
                       :status-tip "Show the application's About box"))
  (setf (about-qt-action instance)
        (create-action instance "About &Qt" *qapp* "aboutQt()"
                       :status-tip "Show the Qt library's About box"))
  (#_setEnabled (cut-action instance) nil)
  (#_setEnabled (copy-action instance) nil)
  (#_connect "QObject"
             (text-edit instance) (QSIGNAL "copyAvailable(bool)")
             (cut-action instance) (QSLOT "setEnabled(bool)"))
  (#_connect "QObject"
             (text-edit instance) (QSIGNAL "copyAvailable(bool)")
             (copy-action instance) (QSLOT "setEnabled(bool)")))

(defun create-menu (instance name &rest actions)
  (let ((menu (#_addMenu (#_menuBar instance) name)))
    (dolist (action actions)
      (if action
        (#_addAction menu action)
        (#_addSeparator menu)))
    menu))

(defun create-menus (instance)
  (with-slots (new-action open-action save-action save-as-action exit-action
               cut-action copy-action paste-action
               about-action about-qt-action) instance
    (setf (file-menu instance)
          (create-menu instance "&File"
                       new-action open-action save-action save-as-action
                       nil
                       exit-action))
    (setf (edit-menu instance)
          (create-menu instance "&Edit"
                       cut-action copy-action paste-action))
    (#_addSeparator (#_menuBar instance))
    (setf (help-menu instance)
          (create-menu instance "&Help"
                       about-action about-qt-action))))

(defun create-tool-bar (instance name &rest actions)
  (let ((tool-bar (#_addToolBar instance name)))
    (dolist (action actions)
      (#_addAction tool-bar action))
    tool-bar))

(defun create-tool-bars (instance)
  (with-slots (new-action open-action save-action
               cut-action copy-action paste-action) instance
    (setf (file-tool-bar instance)
          (create-tool-bar instance "File" new-action open-action save-action))
    (setf (edit-tool-bar instance)
          (create-tool-bar instance "Edit" cut-action copy-action paste-action))))

(defun create-status-bar (instance)
  (#_showMessage (#_statusBar instance) "Ready"))

(defun read-settings (instance)
  (with-objects ((settings (#_new QSettings "Trolltech" "Application Example"))
                 (default-pos (#_new QVariant (#_new QPoint 200 200)))
                 (default-size (#_new QVariant (#_new QSize 400 400))))
    (with-objects ((pos (#_toPoint (#_value settings "pos" default-pos)))
                   (size (#_toSize (#_value settings "size" default-size))))
       (#_resize instance size)
       (#_move instance pos))))

(defun write-settings (instance)
  (with-objects ((settings (#_new QSettings "Trolltech" "Application Example")))
    (#_setValue settings "pos" (#_new QVariant (#_pos instance)))
    (#_setValue settings "size" (#_new QVariant (#_size instance)))))

(defun maybe-save (instance)
  (if (#_isModified (#_document (text-edit instance)))
    (with-object (msg-box (#_new QMessageBox))
      (#_setText msg-box "The document has been modified")
      (#_setInformativeText msg-box "Do you want to save your changes?")
      (#_setStandardButtons msg-box (logior (primitive-value (#_QMessageBox::Save))
                                            (primitive-value (#_QMessageBox::Discard))
                                            (primitive-value (#_QMessageBox::Cancel))))
      (#_setDefaultButton msg-box (#_QMessageBox::Save))
      (let ((ret (#_exec msg-box)))
        (cond ((= ret (primitive-value (#_QMessageBox::Save)))
               (#_save instance))
              ((= ret (primitive-value (#_QMessageBox::Cancel)))
               nil)
              (t t))))
    t))

(defun load-file (instance file-name)
  (handler-case
    (with-open-file (f file-name :direction :input)
      (let ((text (with-output-to-string (s)
                    (loop for line = (read-line f nil)
                          while line do (write-line line s)))))
        (#_setPlainText (text-edit instance) text)))
    (error (e)
      (with-object (msg-box (#_new QMessageBox (#_QMessageBox::Warning)
                             "Application"
                             (format nil "Cannot read file ~A: ~A" file-name e)))
        (#_exec msg-box)))
    (:no-error (ignore)
      (declare (ignore ignore))
      (progn (setf (current-file instance) file-name)
             (#_showMessage (#_statusBar instance) "File loaded" 2000)))))

(defun save-file (instance file-name)
  (handler-case
    (with-open-file (f file-name :direction :output :if-exists :supersede)
      (write-string (#_toPlainText (text-edit instance)) f))
    (error (e)
      (with-object (msg-box (#_new QMessageBox (#_QMessageBox::Warning)
                             "Application"
                             (format nil "Cannot write file ~A: ~A" file-name e)))
        (#_exec msg-box)))
    (:no-error (ignore)
      (declare (ignore ignore))
      (progn (setf (current-file instance) file-name)
             (#_showMessage (#_statusBar instance) "File saved" 2000)))))

(defmethod (setf current-file) :after (newval (instance main-window))
  (#_setModified (#_document (text-edit instance)) nil)
  (#_setWindowModified instance nil)
  (let ((shown-name (if (string= newval "")
                      "untitled.txt"
                      (file-namestring newval))))
    (#_setWindowTitle instance (format nil "~A[*] - ~A" shown-name "Application"))))

(defun main ()
  (setf *qapp* (make-qapplication))
  (let ((main-win (make-instance 'main-window)))
    (#_show main-win)
    (unwind-protect
      (#_exec *qapp*)
      (#_hide main-win))))

