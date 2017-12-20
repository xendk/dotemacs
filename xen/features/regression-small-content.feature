Feature Almost empty pair regression test
  Deleting the $ in dpm($) deleted the pair instead.

  Background:
    Given I am in buffer "test"
    And the buffer is empty
    And I insert:
      """
      dpm($);
      """
    And I load the following:
      """
      (require 'php-mode)
      (require 'smartparens)
      """
    And I turn on php-mode
    And I quietly turn on smartparens-mode

  @regression
  Scenario: Deleting one char content.
    When I place the cursor after "($"
    And I press "<backspace>"
    Then the buffer should contain:
      """
      dpm();
      """

  Scenario: Deleting one char content backwards.
    When I place the cursor after "("
    And I press "<deletechar>"
    Then the buffer should contain:
      """
      dpm();
      """

  Scenario: Deleting one char content backwards.
    When I place the cursor after "dpm"
    And I press "<backspace>"
    Then the buffer should contain:
      """
      dp($);
      """

  Scenario: Deleting outer char content backwards.
    When I place the cursor after ")"
    And I press "<deletechar>"
    Then the buffer should contain:
      """
      dpm($)
      """

  Scenario: Deleting even outer content.
    When I place the cursor after ");"
    And I press "<backspace>"
    Then the buffer should contain:
      """
      dpm($)
      """

  Scenario: Deleting even outer content backwards.
    When I place the cursor after "dp"
    And I press "<deletechar>"
    Then the buffer should contain:
      """
      dp($);
      """
