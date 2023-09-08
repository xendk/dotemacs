;; -*- lexical-binding: t -*-

(require 'php-mode)

(require 'tests/xen-test-helpers)

(require 'xen-php)

;; todo:
;; doc-comments:
;; figure out the fully qualified name of classes.
;; regular comments:

(describe "PHP mode helpers"
  (describe "doc-comment"
    (describe "for variables"
      (it "should add empty @var"
        (xen-test-with-temp-php-buffer
         "
|
$var = somefunc();
"
         (execute-kbd-macro (kbd "/**"))
         ;; (xen-php-handle-docstring)
         (xen-expect-buffer-equals "
/** @var | $var */
$var = somefunc();
")))
      )

    (describe "for classes/interfaces"
      (it "should add empty doc-comment"
        (xen-test-with-temp-php-buffer
         "
|
class Test {}
"
       (execute-kbd-macro (kbd "/**"))
       ;; (xen-php-handle-docstring)
       (xen-expect-buffer-equals "
/**
 * |
 */
class Test {}
"))))

    (describe "for properties"
      (it "should add empty @var"
        (xen-test-with-temp-php-buffer
         "
|
protected $var;
"
         (execute-kbd-macro (kbd "/**"))
         ;; (xen-php-handle-docstring)
         (xen-expect-buffer-equals "
/**
 * @var |
 */
protected $var;
")))

      (it "should get @var type from constructor"
        (xen-test-with-temp-php-buffer
         "
|
protected $var;

function __construct(int $var) {}
"
         (execute-kbd-macro (kbd "/**"))
         ;; (xen-php-handle-docstring)
         (xen-expect-buffer-equals "
/**
 * @var |int
 */
protected $var;

function __construct(int $var) {}
")))
      (it "should fully qualify type"
        (xen-test-with-temp-php-buffer
         "
use Name\\Sub\\Space\\Class;
|
protected $var;

function __construct(Class $var) {}
"
         (execute-kbd-macro (kbd "/**"))
         ;; (xen-php-handle-docstring)
         (xen-expect-buffer-equals "
use Name\\Sub\\Space\\Class;
/**
 * @var |\\Name\\Sub\\Space\\Class
 */
protected $var;

function __construct(Class $var) {}
"))))

    (describe "for functions"
      (it "should work at the end of buffer"
        (xen-test-with-temp-php-buffer
         "|"
         (execute-kbd-macro (kbd "/**"))
         ;; (xen-php-handle-docstring)
         (xen-expect-buffer-equals "/**
 * |
 */")))

      (it "adds argument doc-comments when required"
        (xen-test-with-temp-php-buffer
         "
|
function banana(array $x): int {
}
"
         (execute-kbd-macro (kbd "/**"))
         ;; (xen-php-handle-docstring)
         (xen-expect-buffer-equals "
/**
 * @param |array<> $x
 */
function banana(array $x): int {
}
")))

      (it "adds return doc-comments when required"
        (xen-test-with-temp-php-buffer
         "
|
function banana(int $x): array {
}
"
         (execute-kbd-macro (kbd "/**"))
         ;; (xen-php-handle-docstring)
         (xen-expect-buffer-equals "
/**
 * @return |array<>
 */
function banana(int $x): array {
}
")))

      (it "adds both argument and return doc-comments when required"
        (xen-test-with-temp-php-buffer
         "
|
function banana(array $x): array {
}
"
         (execute-kbd-macro (kbd "/**"))
         ;; (xen-php-handle-docstring)
         (xen-expect-buffer-equals "
/**
 * @param |array<> $x
 *
 * @return array<>
 */
function banana(array $x): array {
}
")))

      (it "groups parameters and return as it should"
        (xen-test-with-temp-php-buffer
         "
|
function banana(array $x, array $y): array {
}
"
         (execute-kbd-macro (kbd "/**"))
         ;; (xen-php-handle-docstring)
         (xen-expect-buffer-equals "
/**
 * @param |array<> $x
 * @param array<> $y
 *
 * @return array<>
 */
function banana(array $x, array $y): array {
}
")))

      (it "adds a newline and * when no doc-comment are needed"
        (xen-test-with-temp-php-buffer
         "
|
function banana(int $x): int {
}
"
         (execute-kbd-macro (kbd "/**"))
         ;; (xen-php-handle-docstring)
         (xen-expect-buffer-equals "
/**
 * |
 */
function banana(int $x): int {
}
")))

      (it "adds empty doc-comments for unknown types"
        (xen-test-with-temp-php-buffer
         "
|
function banana($x) {
}
"
         (execute-kbd-macro (kbd "/**"))
         ;; (xen-php-handle-docstring)

         (xen-expect-buffer-equals (concat "
/**
 * @param | $x
 *
 * @return " ;; The trailing space is significant.
                                           "
 */
function banana($x) {
}
"))))
      (it "doesn't add void return type"
        (xen-test-with-temp-php-buffer
         "
|
function banana(): void {
}
"
         (execute-kbd-macro (kbd "/**"))
         ;; (xen-php-handle-docstring)
         (xen-expect-buffer-equals "
/**
 * |
 */
function banana(): void {
}
")))
      (it "doesn't add return type for constructor"
          (xen-test-with-temp-php-buffer
           "
|
function __construct() {
}
"
           (execute-kbd-macro (kbd "/**"))
           ;; (xen-php-handle-docstring)
           (xen-expect-buffer-equals "
/**
 * |
 */
function __construct() {
}
")))))

  (describe "xen-php-qualify-type"
    (it "should get the type from simple use statements"
      (xen-test-with-temp-php-buffer
       "use Name\\Space\\Class;"
       (expect (xen-php-qualify-type "Class") :to-equal "\\Name\\Space\\Class")
       ;; (xen-php-handle-docstring)
       )
      )

    (it "should get the type from use as statements"
      (xen-test-with-temp-php-buffer
       "use Name\\Space\\Balls as Class;"
       (expect (xen-php-qualify-type "Class") :to-equal "\\Name\\Space\\Balls")
       ;; (xen-php-handle-docstring)
       )
      )
    )

  (describe "xen-php-setup-tools"
    (describe "sets the proper tools in the simple case"
      (before-each
        (spy-on 'buffer-file-name :and-return-value "/home/xen/project/src/File.php")
        (spy-on 'locate-dominating-file :and-call-fake (lambda (file name)
                                                         (cond
                                                          ((equal file "/home/xen/project/src/File.php")
                                                           "/home/xen/project/")
                                                          (t nil))))
        (spy-on 'file-exists-p :and-call-fake (lambda (file)
                                                (cond
                                                 ((equal file "/home/xen/project/vendor/bin/phpcs") t)
                                                 ((equal file "/home/xen/project/vendor/bin/phpstan") t)
                                                 (t nil))))
        (xen-php-setup-tools))
      (it "sets the phpcs and phpstan executables properly"
        (expect flycheck-php-phpcs-executable :to-equal "/home/xen/project/vendor/bin/phpcs")
        (expect phpstan-executable :to-equal "/home/xen/project/vendor/bin/phpstan")))

    (describe "handles nested projects"
      (before-each
        (spy-on 'buffer-file-name :and-return-value "/home/xen/project/vendor/xendk/subproject/src/File.php")
        (spy-on 'locate-dominating-file :and-call-fake (lambda (file name)
                                                         (cond
                                                          ((equal file "/home/xen/project/vendor/xendk/subproject/src/File.php")
                                                           "/home/xen/project/vendor/xendk/subproject/")
                                                          ;; Shouldn't hit this one, or we'll infinite loop.
                                                          ((equal file "/home/xen/project/vendor/xendk/subproject")
                                                           "/home/xen/project/vendor/xendk/subproject/")
                                                          ((equal file "/home/xen/project/vendor/xendk/")
                                                           "/home/xen/project/")
                                                          (t nil))))
        (spy-on 'file-exists-p :and-call-fake
                (lambda (file)
                  (cond
                   ((equal file "/home/xen/project/vendor/xendk/subproject/vendor/bin/phpcs") t)
                   ((equal file "/home/xen/project/vendor/xendk/subproject/vendor/bin/phpstan") nil)
                   ((equal file "/home/xen/project/vendor/bin/phpcs") nil)
                   ((equal file "/home/xen/project/vendor/bin/phpstan") t)
                   (t nil))))
        (xen-php-setup-tools))
      (it "sets phpcs executable to the one found in the subproject"
        (expect flycheck-php-phpcs-executable :to-equal "/home/xen/project/vendor/xendk/subproject/vendor/bin/phpcs"))
      (it "sets phpstan executable to the one found in the parent project"
        (expect phpstan-executable :to-equal "/home/xen/project/vendor/bin/phpstan")
        (message "%S" (spy-calls-all-args 'locate-dominating-file))))))
