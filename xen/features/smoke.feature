Feature: Test test

  Background:
    Given I am in buffer "test"
    And the buffer is empty
    And I insert:
      """
      1234
      """

  @test
  Scenario: Test backspace
    When I place the cursor after "2"
    And I press "<backspace>"
    Then the buffer should contain:
      """
      134
      """

  @test
  Scenario: Test delete
    When I place the cursor after "2"
    And I press "<deletechar>"
    Then the buffer should contain:
      """
      124
      """
