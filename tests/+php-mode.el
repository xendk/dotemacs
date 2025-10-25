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
