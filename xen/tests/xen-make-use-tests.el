;; -*- lexical-binding: t -*-

(require 'php-mode)
(require 'tests/xen-test-helpers)
(require 'xen-php)

(describe "xen-make-use"
  (it "should move class to use block"
    (xen-test-with-temp-php-buffer
     "
use Something

func(\\Namespaced\\Cl|ass);
"
     (xen-php-make-use)
     (xen-expect-buffer-equals "
use Namespaced\\Class;
use Something

func(Class);
")))

  (it "should do nothing if not on a valid FQ-name"
    (xen-test-with-temp-php-buffer
     "
use Something

func(\\Cl|ass);
"
     (xen-php-make-use)
     (xen-expect-buffer-equals "
use Something

func(\\Class);
")))

  (it "should not duplicate uses"
    (xen-test-with-temp-php-buffer
     "
use Namespaced\\Class;
use Something

func(\\Namespaced\\Cl|ass);
"
     (xen-php-make-use)
     (xen-expect-buffer-equals "
use Namespaced\\Class;
use Something

func(Class);
"))))
