Feature Regression test
  Inserting closing parentesis deletes another opening.

  type "hide()" after "$(this)." and "each(" shouldn't loose it's parethesis.
  (it's the ")" that triggers it, sp has allready inserted one, so it deletes
  one)

  @regression
  Scenario: Banana
    Given I am in buffer "test"
    And the buffer is empty
    And I insert:
    """    
    (function ($) {
    Drupal.test = function(class) {
    $('.ask-vopros-tab').each(function() {
        $(this).
    });
    }
    })(jQuery);

    """
    And I load the following:
    """
    (require 'js)
    (require 'smartparens)
    """
    And I turn on js-mode
    And I quietly turn on smartparens-mode
    When I place the cursor after "$(this)."
    And I type "hide();"
    Then the buffer should contain:
    """    
    (function ($) {
    Drupal.test = function(class) {
    $('.ask-vopros-tab').each(function() {
        $(this).hide();
    });
    }
    })(jQuery);

    """
