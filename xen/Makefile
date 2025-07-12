
.PHONY: test
test:
	cask exec buttercup -L tests \
                    -L site-lisp \
                    -L files \
                    --eval "(setq undercover--send-report nil)" \
                    ${BUTTERCUP_OPTIONS} tests
