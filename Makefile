.PHONY: test
test: ## Run poor man's snapshot tests
	@gleam test --target=javascript -- --capture=yes 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_js_capture_yes.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff snapshots/new_js_capture_yes.txt snapshots/js_capture_yes.txt
	@rm snapshots/new_js_capture_yes.txt
	@gleam test --target=javascript -- --capture=no 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_js_capture_no.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff snapshots/new_js_capture_no.txt snapshots/js_capture_no.txt
	@rm snapshots/new_js_capture_no.txt
	@gleam test --target=javascript -- --capture=mixed 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_js_capture_mixed.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff snapshots/new_js_capture_mixed.txt snapshots/js_capture_mixed.txt
	@rm snapshots/new_js_capture_mixed.txt
	@gleam test --target=erlang -- --capture=yes 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_erlang_capture_yes.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff snapshots/new_erlang_capture_yes.txt snapshots/erlang_capture_yes.txt
	@rm snapshots/new_erlang_capture_yes.txt