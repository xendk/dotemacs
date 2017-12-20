Feature JS comments deletion regression test
  Pair finding code was too eager.

  @regression
  Scenario: Deleting comments.
    Given I am in buffer "test"
    And the buffer is empty
    And I insert:
    """
    function test() {
      // test
    }
    """
    And I load the following:
    """
    (require 'js)
    (require 'smartparens)
    """
    And I turn on js-mode
    And I quietly turn on smartparens-mode
    When I place the cursor after "//"
    And I press "<backspace>"
    And I press "<backspace>"
    Then the buffer should contain:
    """
    function test() {
       test
    }
    """
