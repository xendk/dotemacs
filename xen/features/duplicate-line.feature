Feature: Duplicate line
  Function that duplicates current line.

  Scenario: Basic duplication
    Given I am in buffer "test"
    And the buffer is empty
    And I insert:
      """
      First line
      Second line
      """
    When I place the cursor after "First "
    And I call "xen-duplicate-current-line"
    Then the buffer should contain:
      """
      First line
      First line
      Second line
      """

  Scenario: End of buffer duplication
    Given I am in buffer "test"
    And the buffer is empty
    And I insert:
      """
      First line
      Second line
      """
    When I place the cursor after "Second"
    And I call "xen-duplicate-current-line"
    Then the buffer should contain:
      """
      First line
      Second line
      Second line
      """

  Scenario: End of buffer duplication 2
    Given I am in buffer "test"
    And the buffer is empty
    And I insert:
      """
      First line
      Second line

      """
    When I place the cursor after "Second"
    And I call "xen-duplicate-current-line"
    Then the buffer should contain:
      """
      First line
      Second line
      Second line

      """
