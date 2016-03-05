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

    1(2
    3(4
    5)6
    7)8
    """
    And I load the following:
    """
    (require 'php-mode)
    (require 'smartparens)
    """
    And I turn on php-mode    
    And I quietly turn on smartparens-mode    

  Scenario: Forward deleting start of pair.
    When I place the cursor after "1"
    And I press "<deletechar>"
    Then the buffer should contain:
    """
    <?php

    12
    3(4
    5)6
    78
    """
    
  Scenario: Forward deleting end of pair.
    When I place the cursor after "7"
    And I press "<deletechar>"
    Then the buffer should contain:
    """
    <?php

    12
    3(4
    5)6
    78
    """


  Scenario: Backwards deleting start of pair.
    When I place the cursor before "2"
    And I press "<delete>"
    Then the buffer should contain:
    """
    <?php

    12
    3(4
    5)6
    78
    """
    
  Scenario: Backwards deleting end of pair.
    When I place the cursor before "8"
    And I press "<delete>"
    Then the buffer should contain:
    """
    <?php

    12
    3(4
    5)6
    78
    """
    
