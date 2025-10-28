;;; -*- lexical-binding: t; -*-

(require 'buttercup)

(require 'tests/+helpers)
(require '+php-mode)

(describe "+php-make-use"
  (it "should move class to use block"
    (+test-with-temp-php-buffer
     "
use Something

func(\\Namespaced\\Cl|ass);
"
     (+php-make-use)
     (+expect-buffer-equals "
use Namespaced\\Class;
use Something

func(Class);
")))

  (it "should do nothing if not on a valid FQ-name"
    (+test-with-temp-php-buffer
     "
use Something

func(\\Cl|ass);
"
     (+php-make-use)
     (+expect-buffer-equals "
use Something

func(\\Class);
")))

  (it "should not duplicate uses"
    (+test-with-temp-php-buffer
     "
use Namespaced\\Class;
use Something

func(\\Namespaced\\Cl|ass);
"
     (+php-make-use)
     (+expect-buffer-equals "
use Namespaced\\Class;
use Something

func(Class);
")))

  (it "should add in a use block if none exists"
    (+test-with-temp-php-buffer
     "
func(\\Namespaced\\Cl|ass);
"
     (+php-make-use)
     (+expect-buffer-equals "
use Namespaced\\Class;

func(Class);
")))

  (it "should add in a use block after namespace if none exists"
    (+test-with-temp-php-buffer
     "<?php

namespace Banana;

func(\\Namespaced\\Cl|ass);
"
     (+php-make-use)
     (+expect-buffer-equals "<?php

namespace Banana;

use Namespaced\\Class;

func(Class);
"))))

(describe "+php-handle-docstring"
  (describe "doc-comment"
    (describe "for variables"
      (it "should add empty @var"
        (+test-with-temp-php-buffer
         "
|
$var = somefunc();
"
         (execute-kbd-macro (kbd "/**"))
         (+expect-buffer-equals "
/** @var | $var */
$var = somefunc();
")))
      )

    (describe "for classes/interfaces"
      (it "should add empty doc-comment"
        (+test-with-temp-php-buffer
         "
|
class Test {}
"
       (execute-kbd-macro (kbd "/**"))
       (+expect-buffer-equals "
/**
 * |
 */
class Test {}
"))))

    (describe "for properties"
      (it "should add empty @var"
        (+test-with-temp-php-buffer
         "
|
protected $var;
"
         (execute-kbd-macro (kbd "/**"))
         (+expect-buffer-equals "
/**
 * @var |
 */
protected $var;
")))

      (it "should get @var type from constructor"
        (+test-with-temp-php-buffer
         "
|
protected $var;

function __construct(int $var) {}
"
         (execute-kbd-macro (kbd "/**"))
         (+expect-buffer-equals "
/**
 * @var |int
 */
protected $var;

function __construct(int $var) {}
")))

      (it "should fully qualify type"
        (+test-with-temp-php-buffer
         "
use Name\\Sub\\Space\\Class;
|
protected $var;

function __construct(Class $var) {}
"
         (execute-kbd-macro (kbd "/**"))
         (+expect-buffer-equals "
use Name\\Sub\\Space\\Class;
/**
 * @var |\\Name\\Sub\\Space\\Class
 */
protected $var;

function __construct(Class $var) {}
"))))

    (describe "for functions"
      (it "should work at the end of buffer"
        (+test-with-temp-php-buffer
         "|"
         (execute-kbd-macro (kbd "/**"))
         (+expect-buffer-equals "/**
 * |
 */")))

      (it "adds argument doc-comments when required"
        (+test-with-temp-php-buffer
         "
|
function banana(array $x): int {
}
"
         (execute-kbd-macro (kbd "/**"))
         (+expect-buffer-equals "
/**
 * @param |array<> $x
 */
function banana(array $x): int {
}
")))

      (it "adds return doc-comments when required"
        (+test-with-temp-php-buffer
         "
|
function banana(int $x): array {
}
"
         (execute-kbd-macro (kbd "/**"))
         (+expect-buffer-equals "
/**
 * @return |array<>
 */
function banana(int $x): array {
}
")))

      (it "adds both argument and return doc-comments when required"
        (+test-with-temp-php-buffer
         "
|
function banana(array $x): array {
}
"
         (execute-kbd-macro (kbd "/**"))
         (+expect-buffer-equals "
/**
 * @param |array<> $x
 *
 * @return array<>
 */
function banana(array $x): array {
}
")))

      (it "groups parameters and return as it should"
        (+test-with-temp-php-buffer
         "
|
function banana(array $x, array $y): array {
}
"
         (execute-kbd-macro (kbd "/**"))
         (+expect-buffer-equals "
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
        (+test-with-temp-php-buffer
         "
|
function banana(int $x): int {
}
"
         (execute-kbd-macro (kbd "/**"))
         (+expect-buffer-equals "
/**
 * |
 */
function banana(int $x): int {
}
")))

      (it "adds empty doc-comments for unknown types"
        (+test-with-temp-php-buffer
         "
|
function banana($x) {
}
"
         (execute-kbd-macro (kbd "/**"))

         (+expect-buffer-equals (concat "
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
        (+test-with-temp-php-buffer
         "
|
function banana(): void {
}
"
         (execute-kbd-macro (kbd "/**"))
         (+expect-buffer-equals "
/**
 * |
 */
function banana(): void {
}
")))

      (it "doesn't add return type for constructor"
        (+test-with-temp-php-buffer
         "
|
function __construct() {
}
"
           (execute-kbd-macro (kbd "/**"))
           (+expect-buffer-equals "
/**
 * |
 */
function __construct() {
}
"))))))

(describe "+php-qualify-type"
  (it "should get the type from simple use statements"
    (+test-with-temp-php-buffer
     "use Name\\Space\\Class;"
     (expect (+php-qualify-type "Class") :to-equal "\\Name\\Space\\Class")))

  (it "should get the type from use as statements"
    (+test-with-temp-php-buffer
     "use Name\\Space\\Balls as Class;"
     (expect (+php-qualify-type "Class") :to-equal "\\Name\\Space\\Balls"))))

(describe "+php-mode-backend-namespace"
  (it "should return nothing if no parent src directory"
    (spy-on 'locate-dominating-file :and-return-value nil)
    (expect (+php-mode-backend-namespace "namespace ") :to-be nil))

  (it "should handle files in src"
    (spy-on 'buffer-file-name :and-return-value "/home/user/project/src/File.php")
    (spy-on 'locate-dominating-file :and-return-value "/home/user/project/")
    (expect (+php-mode-backend-namespace "namespace ") :to-equal "namespace ;"))

  (it "should use the path in src as namespace"
    (spy-on 'buffer-file-name :and-return-value "/home/user/project/src/Name/Space/File.php")
    (spy-on 'locate-dominating-file :and-return-value "/home/user/project/")
    (expect (+php-mode-backend-namespace "namespace ") :to-equal "namespace Name\\Space;"))

  (it "should handle Drupal modules"
    (spy-on 'buffer-file-name :and-return-value "/home/user/project/src/Name/Space/File.php")
    (spy-on 'file-exists-p :and-call-fake (lambda (file) (equal file "/home/user/project/project.info.yml")))
    (spy-on 'locate-dominating-file :and-return-value "/home/user/project/")
    (expect (+php-mode-backend-namespace "namespace ") :to-equal "namespace Drupal\\project\\Name\\Space;"))

  (it "should handle Drupal modules files in root"
    (spy-on 'buffer-file-name :and-return-value "/home/user/project/src/File.php")
    (spy-on 'file-exists-p :and-call-fake (lambda (file) (equal file "/home/user/project/project.info.yml")))
    (spy-on 'locate-dominating-file :and-return-value "/home/user/project/")
    (expect (+php-mode-backend-namespace "namespace ") :to-equal "namespace Drupal\\project;")))
