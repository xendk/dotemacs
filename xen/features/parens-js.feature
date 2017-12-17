Feature: js-mode mached pairs deletion
  In order to speeed up editing
  As a user
  I want the matching pair to be deleted

  This tests in js-mode specifically

  Background:
    Given I am in buffer "test"
    And the buffer is empty
    And I insert:
      """
      if (fun(var));
      """
    And I load the following:
      """
      (require 'js)
      (require 'smartparens)
      """
    And I turn on js-mode
    And I quietly turn on smartparens-mode

  Scenario: JS Forward deleting start of pair.
    When I place the cursor after "if ("
    And I press "<backspace>"
    Then the buffer should contain:
      """
      if fun(var);
      """

  Scenario: JS Forward deleting end of pair.
    When I place the cursor after "var))"
    And I press "<backspace>"
    Then the buffer should contain:
      """
      if fun(var);
      """


  Scenario: JS Backwards deleting start of pair.
    When I place the cursor after "if "
    And I press "<deletechar>"
    Then the buffer should contain:
      """
      if fun(var);
      """

  Scenario: JS Backwards deleting end of pair.
    When I place the cursor after "var)"
    And I press "<deletechar>"
    Then the buffer should contain:
      """
      if fun(var);
      """
