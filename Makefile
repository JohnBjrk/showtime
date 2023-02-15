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
	@gleam test --target=javascript -- --ignore=ignore 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_js_ignore_ignore.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff snapshots/new_js_ignore_ignore.txt snapshots/js_ignore_ignore.txt
	@rm snapshots/new_js_ignore_ignore.txt
	@gleam test --target=javascript -- --modules=subfolder/sub_test 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_js_modules_subtest.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff snapshots/new_js_modules_subtest.txt snapshots/js_modules_subtest.txt
	@rm snapshots/new_js_modules_subtest.txt
	@gleam test --target=erlang -- --capture=yes 2>&1 | sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_erlang_capture_yes.txt; \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff snapshots/new_erlang_capture_yes.txt snapshots/erlang_capture_yes.txt
	@rm snapshots/new_erlang_capture_yes.txt
	@gleam test --target=erlang -- --capture=no 2> snapshots/new_erlang_capture_no_stderr.txt |(sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_erlang_capture_no.txt); \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff snapshots/new_erlang_capture_no.txt snapshots/erlang_capture_no.txt
	diff snapshots/new_erlang_capture_no_stderr.txt snapshots/erlang_capture_no_stderr.txt
	@rm snapshots/new_erlang_capture_no.txt
	@rm snapshots/new_erlang_capture_no_stderr.txt
	@gleam test --target=erlang -- --capture=mixed 2> snapshots/new_erlang_capture_mixed_stderr.txt |(sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_erlang_capture_mixed.txt); \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff snapshots/new_erlang_capture_mixed.txt snapshots/erlang_capture_mixed.txt
	diff snapshots/new_erlang_capture_mixed_stderr.txt snapshots/erlang_capture_mixed_stderr.txt
	@rm snapshots/new_erlang_capture_mixed.txt
	@rm snapshots/new_erlang_capture_mixed_stderr.txt
	@gleam test --target=erlang -- --ignore=ignore 2> snapshots/new_erlang_ignore_ignore_stderr.txt |(sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_erlang_ignore_ignore.txt); \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff snapshots/new_erlang_ignore_ignore.txt snapshots/erlang_ignore_ignore.txt
	diff snapshots/new_erlang_ignore_ignore_stderr.txt snapshots/erlang_ignore_ignore_stderr.txt
	@rm snapshots/new_erlang_ignore_ignore.txt
	@rm snapshots/new_erlang_ignore_ignore_stderr.txt
	@gleam test --target=erlang -- --modules=subfolder/sub_test 2> snapshots/new_erlang_modules_subtest_stderr.txt |(sed '1,/.*Running showtime_test.main.*/d' > snapshots/new_erlang_modules_subtest.txt); \
	if [ $$? -ne 0 ]; \
	then \
			echo "Expected test to exit with code 1"; \
			false; \
	fi
	diff snapshots/new_erlang_modules_subtest.txt snapshots/erlang_modules_subtest.txt
	diff snapshots/new_erlang_modules_subtest_stderr.txt snapshots/erlang_modules_subtest_stderr.txt
	@rm snapshots/new_erlang_modules_subtest.txt
	@rm snapshots/new_erlang_modules_subtest_stderr.txt