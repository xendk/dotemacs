Feature: Mached pairs deletion
  In order to speeed up editing
  As a user
  I want the matching pair to be deleted

  Background:
    Given I am in buffer "test"
    And the buffer is empty
    And I insert:
      """
      <?php

      if (empty($var));
      """
    And I load the following:
      """
      (require 'php-mode)
      (require 'xen-paired-delete)
      """
    And I turn on php-mode
    And I quietly turn on xen-paired-delete-mode
    And I quietly turn on smartparens-mode

  Scenario: Forward deleting start of pair.
    When I place the cursor after "if ("
    And I press "<backspace>"
    Then the buffer should contain:
      """
      <?php

      if empty($var);
      """

  Scenario: Forward deleting end of pair.
    When I place the cursor after "var))"
    And I press "<backspace>"
    Then the buffer should contain:
      """
      <?php

      if empty($var);
      """


  Scenario: Backwards deleting start of pair.
    When I place the cursor after "if "
    And I press "<deletechar>"
    Then the buffer should contain:
      """
      <?php

      if empty($var);
      """

  Scenario: Backwards deleting end of pair.
    When I place the cursor after "var)"
    And I press "<deletechar>"
    Then the buffer should contain:
      """
      <?php

      if empty($var);
      """
