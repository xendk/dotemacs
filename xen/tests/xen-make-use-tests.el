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
")))
  (it "should add in a use block if none exists"
      (xen-test-with-temp-php-buffer
       "
func(\\Namespaced\\Cl|ass);
"
       (xen-php-make-use)
       (xen-expect-buffer-equals "
use Namespaced\\Class;

func(Class);
")))
  (it "should add in a use block after namespace if none exists"
      (xen-test-with-temp-php-buffer
       "<php

namespace Banana;

func(\\Namespaced\\Cl|ass);
"
       (xen-php-make-use)
       (xen-expect-buffer-equals "<php

namespace Banana;

use Namespaced\\Class;

func(Class);
"))))
